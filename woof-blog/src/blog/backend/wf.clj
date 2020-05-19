(ns blog.backend.wf
  (:require
    [clojure.core.async :as async :refer [go go-loop]]
    [clojure.stacktrace :as stacktrace]

    [taoensso.timbre :as timbre :refer [info error]]

    [woof.base :as base]
    [woof.wfs.evt-loop :as evt-loop]

    [blog.backend.ws :as ws]
    [blog.backend.fs :as fs]

    [blog.core.templates :as templates]
    [blog.core.common :as core]
    [blog.core.posts :as posts]
    [blog.backend.srv :as srv]

    ))

;; blog backend workflow


;;
;; CONFIGURATION

(defn config-impl []
  {
   :init (fn [params]
           {
             ;; where markdown files are stored
             :posts-dir  "/Users/ndrw/m/ndrew.github.io/src/posts"

             ;; where to output the generated html
             :deploy-root-dir "/Users/ndrw/m/ndrew.github.io/"

             :preview-port 8081
            })
   }
  )



(defn common-impl []
  {:ctx (fn [params]
          {
           :prn           {:fn (fn [v]
                                 (prn v)
                                 v)
                           }

           :wait-and-put  {:fn       (fn [[ch-map data]]
                                       (async/put! (:chan ch-map) data)
                                       data)
                           :collect? true
                           }

           :save-in-state {:fn       (fn [[k v]]
                                       (let [state (base/&state params)]
                                         (swap! state assoc (:k k) v)))
                           :collect? true
                           }
           }
          )
   :opts
   ;; log first error, prior to any other opts
        (fn [params] {:op-handlers-map {:error (fn [err]
                                                 (error ::wf-error
                                                        err
                                                        (stacktrace/print-stack-trace err)
                                                        ))}})

   }
  )




;; expose ::#INDEX-POSTS#
(defn posts-steps-fn [params]
  {
   ;; `open` posts directory
   ::posts-root    [:file (:posts-dir params)]

   ;; add the include filter to the post
   ::md-filter     [:id {:include #".+md$"}]
   ::md-root       [:kv-merge* [::posts-root ::md-filter]]

   ;; get list of markdown files to be converted
   ::md-files*     [:ls* ::md-root]

   ;; kv-zip found posts
   ::md-files*-k   [:mem-k* ::md-files*]
   ::mds*          [:*kv-zip [::md-files*-k ::md-files*]]
   ::posts*        [:enrich-posts* ::mds*]

   ;; ::posts* should be narrowed to contain only last steps
   ::ready-posts*  [:final-posts* ::posts*]

   ;; sort posts for index
   ::#INDEX-POSTS# [:sort-posts ::ready-posts*]
   }
  )




(defn routes-steps-fn [params]
  (let [ ;; get things from params
        *STATE (base/&state params)
        CHAN-FACTORY (base/&chan-factory params)
        EVT-LOOP (evt-loop/&evt-loop params)

        ;; hardcoded stuff

        BLOG-INDEX-URL "/blog/"
        POST-URL "/blog/posts/:id"
        STATIC-URL "/"

        ;; kind of internal API
        ;; retrieve post from ::#INDEX-POSTS#
        GET-POST (fn [post-id]
                    (let [INDEX-POSTS (::#INDEX-POSTS# @*STATE)]
                      (if-not (or (nil? post-id) (nil? INDEX-POSTS))
                        (nth INDEX-POSTS post-id nil))))


        ;; expand new steps to event loop
        ;; - uses blocking wait, without timeout
        ;; - pollutes the results

        EMIT-n-WAIT (fn [work-step]
                       (let [ch (base/make-chan CHAN-FACTORY (base/rand-sid "evt-"))
                             ch-sid (base/rand-sid "ch-")
                             work-sid (base/rand-sid "work-")
                             out-sid (base/rand-sid "out-")]

                         (async/put! EVT-LOOP {
                                               ch-sid   [:id {:chan ch}]
                                               work-sid work-step
                                               out-sid  [:wait-and-put [ch-sid work-sid]]
                                               })
                         (async/<!! ch)))


        ]
    ;; todo: <?> inject post handler
    {


     ::fake-index-html [:fake-index-page {:some :data}]
     ::fake-index-handler [:wrap-to-handler ::fake-index-html]

     ::fake-index-kv-cfg       [:kv* [:method :get
                                      :path "/"]]
     ::fake-index-route-map    [:kv-merge* [::fake-index-kv-cfg ::fake-index-handler]]
     ::fake-index-route        [:route-map ::fake-index-route-map]


     ;; save ::#INDEX-POSTS# in state
     ::index-save-id      [:id {:k ::#INDEX-POSTS#}]
     ::save-index-posts   [:save-in-state [::index-save-id ::#INDEX-POSTS#]]
     ;; todo: how to ensure that posts will be saved in state before they'll be needed

     ;; blog index page (/)
     ::index-route        [:route-map ::index-route-map]
     ::index-route-map    [:kv-merge* [::index-kv-cfg ::index-page-handler]]
     ::index-kv-cfg       [:kv* [:method :get
                                 :path BLOG-INDEX-URL]]

     ::index-page-handler [:wrap-to-handler ::blog-index-html]
     ::blog-index-html    [:blog-index-page ::#INDEX-POSTS#]

     ;; post preview page
     ::post-route         [:route-map ::post-route-map]
     ::post-route-map     [:kv* [:method :get
                                 :path POST-URL
                                 :handler (fn [req]
                                            (let [raw-id (get-in req [:params :id])
                                                  POST (GET-POST (core/parse-int raw-id))]
                                              (if (nil? POST)
                                                (str "NO SUCH POST " raw-id)
                                                (EMIT-n-WAIT [:post-page POST])))
                                            )
                                 ]]

     ::md-route           [:route-map {:method  :get
                                       :path    "/md/posts/:id"
                                       :handler (fn [req]
                                                  (let [raw-id (get-in req [:params :id])
                                                        POST (GET-POST (core/parse-int raw-id))]
                                                    (if (nil? POST)
                                                      (str "NO SUCH POST " raw-id)
                                                      (:contents POST)
                                                      ))
                                                  )
                                       }]

     ;; static preview
     ::preview-route      [:static-route [STATIC-URL (get params :deploy-root-dir)]]

     ;; serve figwheel js
     ::frontend-js-route  [:static-route ["/cljs-out" "/Users/ndrw/m/woof/woof-blog/target/public/cljs-out"]]

     ;; ws route
     ::_ws-route-kv       [:id {:method :get
                                :path   "/ws"
                                }]
     ::_ws-handler-kv     [:wrap-to-handler (ws/gen-ws-request-fn params)]

     ::ws-route-map       [:kv-merge* [::_ws-route-kv ::_ws-handler-kv]]
     ::ws-route           [:route-map ::ws-route-map]
     ;; how to expose



     ;; export
     ::#ROUTES#           [:routes [::fake-index-route

                                    ::index-route
                                    ::post-route
                                    ::preview-route

                                    ::frontend-js-route
                                    ::md-route

                                    ::ws-route
                                    ]]

     ::regular-update     [:each-10-s 20]
     ::ws-broadcast       [:fire-update ::regular-update]

     }
    ))


(defn server-steps-fn [params]
  ; (info :SERVER_STEPS params)

  (let [PORT (get params :preview-port 8081)]
    {
     ;; prepare server config map
     ::server-cfg [:kv* [::_port-k ::port-v
                         ::_handler-k ::#ROUTES#]]

     ::_port-k    [:id :port] ::port-v [:id PORT]
     ::_handler-k [:id :handler]

     ;; start server
     ::start!     [:start-server ::server-cfg]

     ;; disabled for now - open browser page, after server had been started
     ;;::url-to-open [:wait-others [::..url-to-open ::start!]]
     ;;  ::..url-to-open   [:id "http://localhost:8081/blog/posts/1"]
     ;;::open-browser  [:open-browser ::url-to-open]
     }))



;; todo: build-evt-loop-init-fn

(defn evt-loop-impl [CHAN-FACTORY]


  (let [EVT-LOOP     (base/make-chan CHAN-FACTORY (base/rand-sid "server-loop"))]
    {


     ; :init (fn [params] {::EVT-LOOP EVT-LOOP})
     :init  (evt-loop/build-evt-loop-init-fn EVT-LOOP)
     :steps (fn [params]
              {
               ::#EVT-LOOP# [:evt-loop EVT-LOOP]
               })

     }
    )
  )

;; prepares a blog workflow
(defn init-blog-backend-wf [*STATE]
  ;; one day this should be configurable

  (let [;; way for wf to create channels
        *CHAN-STORAGE (atom {})
        CHAN-FACTORY (base/chan-factory *CHAN-STORAGE)

        ;; todo: maybe evt loop should be used only from the params

        ;; cfg
        _evt-loop-impl (evt-loop-impl CHAN-FACTORY)

        _cfg (config-impl)
        _common (common-impl)
        _ws-impl  (ws/ws-impl CHAN-FACTORY)
        _fs-impl (fs/fs-impl)
        _templates-impl (templates/templating-impl)
        _posts-impl (posts/posts-impl)
        _srv-impl (srv/server-impl)

        init-fns [

                  ;; user provided init functions
                  (:init _cfg)                   ;; provide posts path and other configuration specific for blog
                  (:init _ws-impl)               ;; initialization for web-socket communication
                  ;; technical stuff
                  (:init _evt-loop-impl) ;; Expose evt loop expose channel factory

                  (base/build-init-state-fn *STATE)                   ;; Provide state atom for a workflow. <?> should state atom be internal or global is okay
                  (base/build-init-chan-factory-fn CHAN-FACTORY)      ;; Provide a channel factory for a workflow.

                  ]

        ctx-fns [
                 core/common-ctx-fn     ;; common/general use context handlers
                 (:ctx _common)      ;; blog wf generic stuff, like debug, etc

                 (:ctx _fs-impl)     ;; files traversal stuff
                 (:ctx _posts-impl)  ;; read and return posts with metadata
                 (:ctx _templates-impl)    ;; templating and rendering

                 (:ctx _ws-impl)   ;; ws communication
                 (:ctx _srv-impl)   ;; preview server (stateful)
                 ]

        ;; boo-1 (info init-fns)
        ;; foo-1 (info ctx-fns)


        ]

    (let [wf (base/wf! :init init-fns
                       :ctx ctx-fns
                       :opts [
                              ;; log first error, prior to any other opts
                              (:opts _common)
                              (:opts _srv-impl)

                              (base/build-opt-state-fn *STATE)
                              (base/build-opts-chan-factory-fn CHAN-FACTORY)

                              ;; last opts for state clean up
                              (base/build-opt-on-done (fn [params result] (reset! *STATE {})))
                              ]

                       :steps [
                               posts-steps-fn    ;; retrieves posts => ::#INDEX-POSTS#
                               routes-steps-fn   ;; prepare server routes (static/post preview/post index) => ::#ROUTES#
                               server-steps-fn   ;; starts server with ::#ROUTES#
                               (:steps _evt-loop-impl) ;; evt loop to keep wf running for => ::#EVT-LOOP#
                               ]
                       )

          on-wf-stop (fn [stop-chan]
                       ;; fixme: wait for full server stop
                       (info :stopping stop-chan)
                       )

          api-map {
                   ;; todo: should we expose some api here?
                   ;; :some-api-method (fn [msg] (prn "boo"))
                   }]

      (base/stateful-wf *STATE wf on-wf-stop api-map)
      )
    )
  )