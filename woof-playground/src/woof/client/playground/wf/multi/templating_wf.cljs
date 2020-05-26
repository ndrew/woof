(ns ^:figwheel-hooks
  woof.client.playground.wf.multi.templating-wf
  (:require
    [cljs.core.async :as async]

    [rum.core :as rum]

    [woof.client.playground.wf.common :as cmn]
    [woof.client.playground.ui.wf :as wf-ui]
    [woof.client.playground.ui :as ui]

    [woof.base :as wf]
    [woof.utils :as u]
    [woof.u :as base]
    [woof.data :as d]


    ;; needed for rendering
    ["react-dom/server" :as dom-server]


    ;; for now use the simplest markdown processor
    [markdown.core :refer [md->html]]
    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))



;; hiccup tree ui
(rum/defc <hiccup> < rum/static
  [root]

  (let [[t & r] root

        has-attrs? (map? (first r))
        attrs (if has-attrs? (first r) {})
        body (if has-attrs? (rest r) r)
        container? (and
                     (not (empty? body))
                     (some vector? body))

        sub? (= :SUB t)
        ]

    (if sub?
      ;; substitution
      [:div.el.sub
       [:span.t (:sid attrs)]

       (map (fn [b]
              (if (vector? b)
                  (<hiccup> b)
                  [:span.text-node b])
              ) body)
       ]
      ;; normal hiccup
      [:div.el
       ;{:class (if no-children "node")}
       {:class (if container? "c" "n")}

       [:span.t (pr-str t)]
       (if-not (= {} attrs)
         [:span.attrs (pr-str attrs)])

       (map (fn [b]
              (if (vector? b)
                  (<hiccup> b)
                  [:span.text-node b])
              ) body)
       ]
      )
    )

  )

(rum/defcs <ui-hiccup-preview> < rum/static
                                 (rum/local {} ::inline-results?)

  [local *wf]

  (let [wf @*wf
        results (get wf :result {:TEMPLATE/html "no results yet"
                                 :TEMPLATE/hiccup [:div "No HICCUP"]})

        html   (get results :TEMPLATE/html)
        hiccup (get results :TEMPLATE/hiccup)
        hiccup-annotated (get results :TEMPLATE/hiccup-annotated)
        ]

    [:div.templating-workflow

     [:div

      [:.section [:header "HICCUP (with substitutions)"]
       (<hiccup>
         (if hiccup-annotated hiccup-annotated
                              hiccup)
         )]

      [:.section [:header "RAW HICCUP"]
       [:pre (d/pretty! hiccup)]]

      [:.section [:header "HTML PREVIEW"]
       ;; how will this work with head and other attributes?
       [:div.html-preview
        {:dangerouslySetInnerHTML {:__html html}}]]

      [:.section
       [:header "HTML String"]
       [:pre
        html]
       ]
      ]




     [:hr]

     (wf-ui/<default-body> *wf)

     ]
    )

  )



(rum/defc <template>
  [markup]
  markup
  )



(defn complex-hiccup-example []
  [:html
   [:head


    [:meta {:charset "utf-8"}]
    [:meta {:name "author" :content "Andrew Sernyak"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]

    [:title "Andrew Sernyak Blog"]

    [:link {:rel "stylesheet" :href "/css.css"}]
    [:link {:rel "shortcut icon" :sizes "16x16 24x24 32x32 48x48 72x72" :href "/favicon.ico"}]
    [:link {:rel "image_src" :href "../logo.png"}]

    [:link {:rel "alternate" :type "application/rss+xml" :title "RSS" :href "atom.xml"}]


    [:script "var BLOG = 'index';"]
    [:link {:rel "stylesheet" :href "http://localhost:9500/css/base.css"}]
    [:script {:src "http://localhost:9500/cljs-out/blog-main.js"}]

    ]
   [:body {:style {:color "black"}}
    "HELLO, I AM SERVER RENDERED PAGE."

    ;[:SUBSTITUTION {:params :yo}]


    [:div#blog-dashboard

     [:p {:color "blue"} "I am a paragraph!"]
     ]
    ]
   ]
  )


(defn do-substitute [subs-map sid]

  ;;
  (let [sym-name (name sid)
        sym-ns (namespace sid)

        ;; build a regex for substitution, todo: maybe only if contains wildcard?
        pattern-str (str ":" sym-ns "/" sym-name)
        re (re-pattern pattern-str)
        ]

    ;; return values only
    (let [filtered-kv (filter (fn [[k v]]
                                #_(.log js/console k
                                        (re-find re (str k))
                                        )

                                (re-find re (str k))
                                ) subs-map)
          filtered-vals (vals filtered-kv)]

      ;; if the filtered map returned sequence, so we need to concat, instead of conj
      (reduce (fn [a b]
                (if (seq? b)
                    (concat a b)
                    (conj a b))
                ) '() filtered-vals)

      )
    )
  )



(defn normal-sub [subs-map
                  hiccup sub-sid]
  (let [subs (do-substitute subs-map sub-sid)]
    (concat hiccup subs)))


(defn placeholder-sub [subs-map
                       hiccup sub-sid]

  (let [subs (do-substitute subs-map sub-sid)]
    (conj hiccup (into [:SUB {:sid (str sub-sid)}] subs)))
  )


(defn substitute-hiccup [subs-fn root]
  (let [[t & r] root
        has-attrs? (map? (first r))

        attrs (if has-attrs?
                (first r)
                {})

        body (if has-attrs?
               (rest r)
               r)


        ]

    (let [result (reduce (fn [a b]
                        (if (vector? b)
                            (conj a (substitute-hiccup subs-fn b))
                            (if (base/sid? b)
                              (subs-fn a b)
                              (conj a b))
                            )
                        ) '() body) ]

      (into [t attrs]
            result)
      )

    )
  )

(defn template-ctx [params]
  {

   ;; config process, basically merges different configs into a single one
   :CFG* {:expands? true
          :fn (fn [cfg]
                {
                 ;; each ns exposed step is considered as contract?
                 :CFG/template-cfg [:id {:template :hiccup
                                         :author "ndrw"}]
                 :CFG/deploy-cfg [:id {:deploy :github}]
                 :CFG/content-cfg [:id cfg]
                 ;; for now just merge different sub-configs into a single config
                 :CFG/cfg* [:kv-merge* [:CFG/template-cfg :CFG/deploy-cfg :CFG/content-cfg]]
                 }
                )}



   ;; just simplest way - concat head and body
   :head-body {:fn (fn [[head body]]

                     [:html
                      head
                      body
                      ]
                     )
               :collect? true
               }


   ;; generator step for returning available meta tags
   :expand-meta {
                 :fn (fn [cfg]
                       {
                        :META/meta-author   [:id [:meta {:name "author" :content "Andrew Sernyak"}]]
                        :META/meta-encoding [:id [:meta {:charset "utf-8"}]]
                        }
                       )
                 :expands? true
                 }

   :md {
        :fn (fn [md]
              (md->html md)
              )
        }



   ; normal subst
   :subst {
          :fn (fn [vs]
                (let [[template & r] vs
                      ; merge substitions into a single map
                      subst-map (apply merge {} r)

                      subs-fn (partial normal-sub subst-map)
                      ;subs-fn (partial placeholder-sub subst-map)
                      ]

                     (substitute-hiccup subs-fn template)
                     )
                )
          :collect? true
          }

   :placeholder-subst {
           :fn (fn [vs]
                 (let [[template & r] vs
                       ; merge substitions into a single map
                       subst-map (apply merge {} r)

                       subs-fn (partial placeholder-sub subst-map)]

                      (.log js/console subst-map)

                      (substitute-hiccup subs-fn template)
                      )
                 )
           :collect? true
           }



   ;; templating process

   :TEMPLATE {:expands? true
          :fn (fn [cfg]
                {

                 ;; generate and collect meta tags
                 :META/META [:*kv-zip [:META/_meta-keys :META/available-meta]]
                   :META/available-meta [:expand-meta {}]
                   :META/_meta-keys [:mem-k* :META/available-meta]

                 :HEAD/title [:id {:HEAD/title [:title "title"]}]

                 :HEAD/favicon [:id {:HEAD/favicon '(
                                     [:link {:rel "shortcut icon" :sizes "16x16 24x24 32x32 48x48 72x72" :href "/favicon.ico"}]
                                     [:link {:rel "image_src" :href "../logo.png"}])}]

                 ;; substitutions are sids
                 ;; substitution can go as 'body' in hiccup
                 :SUBST/head-template [:id [:head
                                            ;; multi substitution - all values from ns META/ will be included
                                              :META/*
                                             ;; single substitution (if the order is important)
                                              :HEAD/title

                                              :HEAD/favicon
                                            ]]


                 ;; build <head> via hiccup substitution
                 :SUBST/head [:subst [:SUBST/head-template
                                      :META/META :HEAD/title :HEAD/favicon]]

                 ;; how to track substitutions? substitute into intermedary
                 :SUBST/head-annotated [:placeholder-subst
                                          [:SUBST/head-template
                                           :META/META :HEAD/title :HEAD/favicon]]





                 :CONTENT/md [:md "**booo**"]

                 :HTML/body-template [:id [:body
                                           "This is a body"

                                           :CONTENT/md

                                           ]]


                 ;; todo: add substitutions for body also

                 :HTML/BODY [:id [:body "boo"]]


                 ;; just concat head and body
                 ;; + head/body can be async

                 :HTML/hiccup [:head-body [:SUBST/head :HTML/BODY]]
              ;; return the version with substitutions also
                 :HTML/hiccup-annotated [:head-body [:SUBST/head-annotated :HTML/BODY]]


                 }
                )}




   ;; convert hiccup markup to html
   :hiccup->html    {
                     :fn (fn [data]
                           ;; see https://github.com/tonsky/rum/issues/210
                           (let [markup (rum/render-static-markup (<template> data))]
                                markup))
                     }


   }
  )

(defn hiccup-steps [params]
  {


;   :TEMPLATE/hiccup [:id :DUMMY/sample-hiccup]
     ; :DUMMY/sample-hiccup [:id [:html [:body [:div {:style {:color "red"}} "boo"]]]]
     ; :DUMMY/sample-hiccup [:id (complex-hiccup-example)]

   ;; run substitution as separate 'process'
   :TEMPLATE/process [:TEMPLATE {}]

   ;;
   :TEMPLATE/hiccup [:id :HTML/hiccup]
   :TEMPLATE/hiccup-annotated [:id :HTML/hiccup-annotated]

   ;; these are hardcoded steps for hiccup generation and preview

   :TEMPLATE/html [:hiccup->html :TEMPLATE/hiccup]


   ;; todo: test feeding the template from other process
   ;   :CFG/process [:CFG* {:index {:contents "index c o n t e n t"}}]

   }
  )




(defn hiccup-template-wf-initializer [*SWF]
  (let [EVT-LOOP  (cmn/make-chan (wf/rand-sid "evt-loop"))]
    {

     :title       "Templating via hiccup"
     :explanation [:div.explanation
                   [:p "exploring ways how to do templating via hiccup"]
                   [:p "describe substitution in hiccup template as sid"]
                   [:p "placeholder sids (like :META/*) allow to substitute many values"]

                   ]
     ;;
     :init-fns    [cmn/chan-factory-init-fn
                   (fn [params] {::EVT-LOOP EVT-LOOP})    ;; expose evt loop for other workflows
                   ]
     ;;
     :ctx-fns     [cmn/common-ctx-fn
                   template-ctx]

     ;;
     :steps-fns   [(fn [params] { ::#EVT-LOOP# [:evt-loop (::EVT-LOOP params)]})
                   hiccup-steps]

     :opt-fns     [cmn/chan-factory-opts-fn]

     ;; persistent filters in UI - to hide/show un-needed steps
     :ui-fn       (partial wf-ui/<wf-UI> <ui-hiccup-preview>)
     })
  )






;;
;; example: static site generator

;; by definition, ssg means that we produce a set of html/js/css/img files.
;; these later are being deployed to a web server, have nice properties and bla-bla..

;; idea of ssg is that we have a process that will generate these files
;;
;; ssg -> files -> PROFIT
;;    (which is basically the same as writing the site by hand)
;;
;; but for different sites we will need different set files!
;; so we need to parametrize our ssg algorithm
;;
;; ssg(...) -> files
;;
;; side-note regarding files,
;; file can be a value (think memory file) or file can be a place (think filename), or can be some combination of both.

;; idea: show, don't tell - there need to be a way to see data

;; for ssg - it's preview.
;; note that preview may be not 100% same as production - no need for analytics scripts, other urls, etc.

;; so in woof we'll have SSG process

;; ::SSG [:ssg-impl* ...]
;; => {
;;        :SSG/*FILES (::index.html ::css|style.css, ... )  - ordered list (or tree) of files
;;
;;        ;; use file generated from memory
;;        ::css|style.css [:memory-file {:file-name "style.css" :contents "...." }]
;;        ;; use existing file from file system
;;        ::img|image-1.jpg [:fs-file {:file-name "/Users/.../image-1.jpg" }]
;;
;;        ;; or use steps generated by other sub-processes
;;        ::index.html [:files* :INDEX/PROCESS]
;;
;;    }

;; and PREVIEW process linking to ::SSG process

;; ::PREVIEW [:preview-impl* [::SSG]]

;;
;; section 1 - FILES
;;
;; in memory file [:memory-file {:content ... }]
;; fs file [:fs-file {:file-name ..}]
;; url-file [:url-file {:url "" }]
;;
;; <?> file bundle [:files* [::file-1 ::file-2 ...]]
;;

;; section 2 - defining which files to return?

;; usually SSG consists of more processes

;; template + content => ([:mem-file "template-generated"] [:fs-file "asset-1"] [:fs-file "asset2])

;; handling assets ?