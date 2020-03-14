(ns ^:figwheel-hooks ^:figwheel-always
  woof.browser
  (:require
    [woof.lib :as lib]
    [goog.object]
    [goog.dom :as dom]
    [goog.object]
    [goog.dom.query :as query]
    [goog.dom.classes :as classes]

    [woof.data :as d]

    [woof.base :as base]
    [woof.wf :as wf]
    [woof.utils :as u]

    [woof.scraper :as scraper]
    ))


;; ns for doing in-browser scraping

;; for now, build as :browser-min and inject in the host page as
;   (function() { var $script = document.createElement('script'); $script.setAttribute("type","text/javascript"); $script.setAttribute("src", "http://localhost:9500/cljs-out/browser-main.js"); document.body.appendChild($script); })()

;; or copy page contents into browser.html


(enable-console-print!)



(defn simple-ctx [params]
  {
   ;; step-handler-id is a simple keyword
   :step-handler {:fn (fn [v] v)}

   :log {:fn (fn[v]
               (.log js/console v)
               v)}

   :identity {:fn identity }



   }
  )


(defn simple-steps [params]
  {

   }
  )

(defn simple-opt[params]
  {
   :op-handlers-map {
                     :done  (fn [result]
                              (.log js/console result)

                              (.log js/console
                                    "RESULT"
                                    (::RESULT result))
                              ;(ready)
                              )

                     :error (fn [result]
                              (.error js/console result))

                     }

   })


(defn domik-ctx [params]
  (let [*state (atom {})]
    {

     ;; gets html elements
     :query-selector-all {
                          :fn (fn [selector]
                                (array-seq (.querySelectorAll (.-body js/document) selector)))
                          }

     ;; splits elements to a separate step
     :expand*            (base/make-expand-steps-sbody :identity)

     :collect            {
                          :fn       (fn [xs]
                                      ; (.warn js/console xs)
                                      xs)
                          :collect? true
                          }

     ;; splits sid-list into
     :iterate*           (base/make-expand-steps-sbody :process)

     :process-old        {
                          :fn (fn [el]
                                {:t (u/now)}
                                )
                          }

     :process*            {
                           :fn (fn [el]
                                 ;; {:t (u/now)}

                                 { (base/rand-sid "ppp-") [:process-old el]}

                                 )
                           :expands? true
                           }

     :process            {
                              :fn (fn [el]
                                    (scraper/parse-listing el)

                                    )
                              }


     :reduce-k             {
                            :fn       (fn [o]
                                        { (base/rand-sid "K-") [:identity {:k o}] })
                            :expands? true
                            }

     ;; kv zipping - joins keys with values
     :zip {
           :fn       (fn [[[k] vs]]
                       (let [ks (:k k)]
                            (apply assoc {}  (interleave ks vs))
                            ))
           :collect? true
           }

     ;; another way of doing kv zipping
     :reduce-v             {
                            :fn       (fn [o]
                                        {
                                         (base/rand-sid "V-") [:identity {:v o}]
                                         }
                                        )

                            :expands? true
                            :collect? true
                            }

     :zip-implicit {
                    :fn       (fn [[[k] [v]]]

                                (let [ks (:k k)
                                      vs (:v v)]
                                     (apply assoc {}  (interleave ks vs))
                                     )
                                )
                    :collect? true
                    }


     ;; todo: convenience wrapper for working with collection with single
     }
    )

  )

;; avoiding duplicates:
;; a) not returning via expand*
;; b) not including during kv-zipping


(defn domik-steps [params]
  {
   ::all [:query-selector-all ".cnt .objava"]

   ::expand-id [:expand* ::all]

   ::processed-elements [:iterate* ::expand-id]

   ::RESULT [:collect ::processed-elements]




   ;; hacky way to pass the key as a value

;   ::k [:reduce-k ::processed-elements]
;   ::KV [:zip [::k ::processed-elements]]

   ;; ::v [:reduce-v ::processed-elements]
   ;::KV [:zip-implicit [::k ::v]]


 ;!!! ::RESULT [:identity ::KV]

   ;::collected [:collect ::processed-elements]

   ;::RESULT [:identity ::collected]


   }
  )



(def init-fns   [])
(def ctx-fns    [simple-ctx domik-ctx

                 ;;
                 (fn [params]
                   {
                    :post-process {
                                   :fn (fn [listings]
                                         (sort-by
                                           :uah
                                           (map #(get % :price) listings)
                                           )

                                         )
                                   }
                    }
                   )

                 ])
(def steps-fns  [simple-steps domik-steps

                 (fn [params]
                   {
                    ::post-process [:post-process ::RESULT]
                    ::str-post-process [:log ::post-process]
                    }
                   )
                 ])
(def opt-fns    [simple-opt])



(defn ^:export run_workflow []
  ;; this will start the wf
  (let [wf-impl
        (base/parametrized-wf!
          (base/combine-init-fns init-fns)
          identity ; wf-params-fn
          identity ; opt-params-fn
          (base/combine-fns opt-fns :merge-results base/merge-opts-maps)
          (base/combine-fns ctx-fns)
          (base/combine-fns steps-fns))
        ]
    (base/run-wf! wf-impl identity)
    )
  )





(defn ^:export domik []
  #_(let [;els (array-seq (dom/getElementsByClass "objava_content"))
        els (array-seq (.querySelectorAll (.-body js/document) "#divListObjects .objava_content"))
        ]
    ;; iterate through each objava_content
    (let [_data (map parse-objava els)
          data (prepare-data _data)]

      ;; enrich ui
      (doseq [d data]
        (when-let [existing-el (.querySelector (.-body js/document) (str "a[clickcntid='" (:id d) "']"))]
          (let [parent (dom/getAncestorByClass existing-el "objava_content")]

            (if-let [zzz (.querySelector parent "zzz")]
              (dom/setTextContent zzz (d/pretty d))
              (dom/insertChildAt parent (dom/createDom "pre" "zzz" (d/pretty d)) 0)
              )

            ;; set flexbox order
            ;(if [z parent])
            )
          )
        )
      )
    )
  )


(defn ^:export domik1 []

  (.log js/console (d/pretty (domik)))
  )

;; todo: add autoscroll




(when-not (goog.object/get js/window "PLAYGROUND")

  ;(.clear js/console)
  ;(run_workflow)

  ; (.querySelectorAll (.-body js/document) "#divListObjects .objava_content")
  ;;
  #_(let [;els (array-seq (dom/getElementsByClass "objava_content"))
        els (array-seq (.querySelectorAll (.-body js/document) "#divListObjects .objava_content"))
        ]
    ;; iterate through each objava_content

    (let [data (map parse-objava els)]
      (doseq [d data]
        (when-let [existing-el (.querySelector (.-body js/document) (str "a[clickcntid='" (:id d) "']"))]
          (let [parent (dom/getAncestorByClass existing-el "objava_content")]

            (if-let [zzz (.querySelector parent "zzz")]
              (dom/setTextContent zzz (d/pretty d))
              (dom/insertChildAt parent (dom/createDom "pre" "zzz" (d/pretty d)) 0)
              )

            ;; set flexbox order
            ;(if [z parent])
            ;;
            ;;; parent
            )
          )
        )
      )
    )
  )
