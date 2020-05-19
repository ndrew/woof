(ns ^:figwheel-hooks blog.frontend
  (:require
    [goog.object]
    [goog.dom :as gdom]
    [goog.dom.classlist :as gclasslist]

    [cljsjs.commonmark]

    [cljs.core.async :refer [go] :as async]

    [clojure.string :as str]
    [clojure.edn :as edn]

    [rum.core :as rum]

    [woof.base :as base]
    [woof.utils :as utils]
    [woof.wfs.evt-loop :as evt-loop]
    [woof.wfs.kv :as kv]


    ;; use playground ui for now
    [woof.client.playground.ui :as ui]
    [woof.client.ws :as ws]


    [blog.frontend.util :refer [GET]]
    [blog.frontend.md :as md ]
    )
  (:import
    [goog.net.XhrIo ResponseType])
  )

;; blog ui for

(enable-console-print!)



(rum/defcs <editor-ui> < (rum/local -1 ::selected-i)
  [local data]
  (let [post-meta (:meta data)
        meta-meta (meta post-meta)
        md (:md data)
        html (:html data)

        ]


    [:div.editor-root

     ;; how to keep styles of playground UI
     (ui/menubar "actions"
                 [
                  ["toggle backend generated" (fn []
                                              ;; md/backend-html/js-html
                                                (gclasslist/toggle (.-body js/document) "hide-backend-md")
                                                )]
                  ["show full md" (fn[]

                                    )]

                  []
                  ["send ws" (fn []
                               (let [evt-loop-chan (:evt-loop data)
                                     socket (:socket data)
                                     ]

                                    (let [msg {:socket socket
                                               :msg "AZAZAZA"
                                               }]

                                      (async/put! evt-loop-chan
                                                  {
                                                   (base/rand-sid) [:ws-send! msg]
                                                   }
                                                  )

                                      )


                                    )
                               )]])

     ; [:pre (base/pretty! meta-meta)]
     ; [:pre (base/pretty! post-meta)]
     ; [:pre.md-preview md]

     ;; full html preview
     #_[:div.html-preview
      {:dangerouslySetInnerHTML {:__html html}}]


     (let [selected-i @(::selected-i local)]

       ;; markdown blocks
       (map-indexed (fn [i node]
                      (if (= i selected-i)
                          (rum/with-key
                            (md/<selected-md-block>
                              {:select-fn (fn [e] (reset! (::selected-i local) i))
                               :md md
                               } node)
                            i
                            )
                          ;; normal component, can be fragment used?
                          [:div.md-block
                           {:key i
                            :dangerouslySetInnerHTML {:__html (.render md/renderer node)}
                            :on-click (fn [e] (reset! (::selected-i local) i))
                            }])
                      ) (:nodes data))
       )
     ]
    )
  )


;; WF

(defn common-ctx [params]
  {
   :id            {:fn identity}

   :first-collect {
                   :fn       (fn [v]
                               (if (= :nil v)
                                   (utils/throw! "empty sid list provided for :first-collect"))

                               (if (and (seq? v) (seq v))
                                   (first v)
                                   v))
                   :collect? true
                   }

   :GET*          {:fn       (fn [url]
                               (let [ch (async/chan)]
                                      (GET url (fn [resp-text]
                                                 (async/put! ch
                                                           {(base/rand-sid) [:id resp-text]})
                                                 ))

                                    ch
                                    )

                               )
                   :expands? true}
   }
  )



(defn ws-ctx-fn [params]
  {

   ;;
   :ws-connect! {
                  :fn (fn [url]
                        (ws/chan-connect url
                                         :chan (base/make-chan (base/&chan-factory params) (base/rand-sid "ws-"))
                                         :on-message (fn [payload]
                                                      (let [msg (ws/read-transit payload)]
                                                           (prn "GOT message" msg)))
                                        ))
                 }

   :ws-send!   {
                 :fn (fn [msg]
                       (prn "SENDING " msg)
                       (ws/send-transit! (:socket msg)
                                         (:msg msg))
                       ::sent
                       )
                 }
   })

;;


;; for now store backend generated html for markdown
(defonce *backend-generated-html (volatile! ""))


(defn blog-ctx [params]
  {

   :current-post-id {:fn (fn [_]
                           (let [url (.-pathname js/location)]
                                (last (str/split url "/"))))}
   :md-url {:fn (fn [post-id]
                  (str "/md/posts/" post-id))}

   :hide-backend-md! {:fn (fn [_]
                            (when (nil? (gdom/getElement "js-md"))
                                  (let [backend-generated-el (gdom/getElement "backend-md")]
                                    ;; take backend generated html and store it, just in case
                                    (vreset! *backend-generated-html (.-innerHTML backend-generated-el))

                                    ;;(gdom/removeNode backend-generated-el)
                                    )
                                  ))}

   :create-js-md-el {:fn (fn [md-model]
                           (if-let [root-el (gdom/getElement "js-md")]
                                   (do
                                     (rum/mount (<editor-ui> md-model) root-el)
                                     )
                                   (let [el (gdom/createElement "article")]
                                     (set! (.-id el) "js-md")
                                     (gdom/appendChild (.-body js/document) el)
                                     (rum/mount (<editor-ui> md-model) el)))
                           )}

   :md-model {:fn (fn [md]
                    (md/md->html md))}
   }
  )


(defonce *wf-result (atom {}))

(defonce chan-factory (base/chan-factory (atom {})))

(defonce *running-wf (atom nil))

(defn store-running-wf-opts [params]
  {:before-process  (fn [wf-chan xtor]
                      (reset! *running-wf xtor)
                      :ok)})


(defn start-posts-ui-wf! []
  (let [EVT-LOOP-chan (base/make-chan chan-factory (base/rand-sid "evt-"))
        STEPS {
               ::post-id [:current-post-id nil]
               ::md-source-url [:md-url ::post-id]

               ::_md [:GET* ::md-source-url]
               ::md [:first-collect ::_md]

               ::hide-backend-md! [:hide-backend-md! ::md]

               ::md-model [:md-model ::md]

               ::ws [:ws-connect! "ws:localhost:8081/ws"]

               ::_socket [:id :socket]

               ::evt-loop-kv [:kv [:evt-loop EVT-LOOP-chan]]

               ::socket-kv [:kv* [::_socket ::ws]]

               ::md-model-w-socket [:kv-merge* [::md-model
                                                ::socket-kv
                                                ::evt-loop-kv
                                                ]]


               ::ui [:create-js-md-el ::md-model-w-socket]


               ;; we need to store ws socket so we can use it later

               ::evt-loop [:evt-loop EVT-LOOP-chan]
               }

        ]

    ;; keep-xtor - to end wf properly

    (base/run-wf! (base/wf!
                    :init [(base/build-init-chan-factory-fn chan-factory)
                           (evt-loop/build-evt-loop-init-fn EVT-LOOP-chan)]
                    :opts [
                           ;; how to handle on done, for infinite workflows
                           (base/build-opt-on-done (fn [params result]
                                                     (.log js/console result)
                                                     (reset! *wf-result result)))
                           (base/build-opts-chan-factory-fn chan-factory)
                           store-running-wf-opts
                           ]
                    :ctx [common-ctx
                          kv/ctx-fn
                          evt-loop/evt-loop-ctx-fn
                          ws-ctx-fn
                          blog-ctx]

                    :steps STEPS
                    ))
    )
  )

(defn ui-wf [ui-wf-fn]

  (.clear js/console)

  ;; stop previous wf
  (if-let [prev-wf @*running-wf]
    (let [ch (base/end! prev-wf)]
      ; (.log js/console "ending previous wf")
      (go
        (let [v (async/<! ch)]
          ; (.log js/console "actually ended" v)
          (ui-wf-fn))))
    (ui-wf-fn)
    )



  (let [EVT-LOOP-chan (base/make-chan chan-factory (base/rand-sid "evt-"))]

    ;; keep-xtor - to end wf properly

    (base/run-wf! (base/wf!
                    :init [(base/build-init-chan-factory-fn chan-factory)
                           (evt-loop/build-evt-loop-init-fn EVT-LOOP-chan)]
                    :opts [(base/build-opt-on-done (fn [params result]
                                                     (.log js/console result)
                                                     (reset! *wf-result result)))
                           (base/build-opts-chan-factory-fn chan-factory)
                           (fn [params]
                             {:before-process  (fn [wf-chan xtor]
                                                 (reset! *running-wf xtor)
                                                 :ok)})
                           ]
                    :ctx [common-ctx
                          kv/ctx-fn
                          evt-loop/evt-loop-ctx-fn

                          ws-ctx-fn
                          blog-ctx]
                    :steps {
                            ::post-id [:current-post-id nil]
                            ::md-source-url [:md-url ::post-id]

                            ::_md [:GET* ::md-source-url]
                            ::md [:first-collect ::_md]

                            ::hide-backend-md! [:hide-backend-md! ::md]

                            ::md-model [:md-model ::md]

                            ::ws [:ws-connect! "ws:localhost:8081/ws"]

                            ::_socket [:id :socket]

                            ::evt-loop-kv [:kv [:evt-loop EVT-LOOP-chan]]

                            ::socket-kv [:kv* [::_socket ::ws]]

                            ::md-model-w-socket [:kv-merge* [::md-model
                                                             ::socket-kv
                                                             ::evt-loop-kv
                                                             ]]


                            ::ui [:create-js-md-el ::md-model-w-socket]



                            ;; we need to store ws socket so we can use it later

                            ::evt-loop [:evt-loop EVT-LOOP-chan]
                            ;; add evt loop now?

                            }
                    ))
    )


  )


;;


(if-let [blog (goog.object/get js/window "BLOG")]
  (cond
    ;; fake index
    (= "index" blog) (set! (.-onload js/window) (fn []
                                                  (.log js/console "Frontend: initialize fake index")
                                                  ))
    ;; post page
    (= "post" blog) (set! (.-onload js/window)
                          (partial ui-wf start-posts-ui-wf!))
    )

  (do
    (.log js/console "not running from page with global BLOG not set.")
    )
  )

(defn ^:after-load on-js-reload []
  (when-let [blog (goog.object/get js/window "BLOG")]
    ;; (prn "RELOAD")
    (cond
      ;; fake index
      (= "index" blog) (do
                         (.log js/console "DO NOTHING")
                         ) #_(set! (.-onload js/window) (fn []
                                                    (.log js/console "Frontend: initialize fake index")
                                                    ))
      ;; post page
      (= "post" blog)
      (ui-wf start-posts-ui-wf!)
      )


    )
  ) ;; re-mount app on js reload


