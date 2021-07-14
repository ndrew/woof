(ns woof.client.browser.yt.parser
  (:require
    [goog.dom :as dom]
    [goog.object :as gobj]

    [goog.dom.classes :as classes]
    [goog.dom.dataset :as dataset]

    [clojure.string :as str]
    [cljs.core.async :refer [go go-loop] :as async]

    [woof.base :as base :refer [sid make-chan &chan-factory]]
    [woof.data :as d]

    [woof.client.dom :as wdom]
    [woof.client.dbg :as dbg :refer [__log]]

    [woof.utils :as u]

    [cljs-time.core :as time]
    [cljs-time.format :as time-fmt]

    [woof.client.dom :as woof-dom :refer [txt q q*]]))



(defn history-day-selector []

  ;
  ; "#contents .ytd-section-list-renderer"
  ; - continuation list renderer has this type

  ;; <ytd-item-section-renderer> - contains list of videos per day
  "#contents ytd-item-section-renderer:not(.WOOF-WIP)"

  )



(defn safe-txt [el selector]
  (if-let [sub-el (woof-dom/q el selector)]
    ;; don't trim for now
    (woof-dom/txt sub-el)
    (do
      (.log js/console "can't find element for selector: " selector "parent-el" el)
      (classes/add el "parsed-error")
      ""
      )
    )
  )


(defn safe-href [el selector]
  (if-let [sub-el (wdom/q el selector)]
    (wdom/attr sub-el "href")
    (do
      (.log js/console "can't find element for selector: " selector "parent-el" el)
      (classes/add el "parsed-error")
      ""
      )
    )
  )




(defn history-day-scrape-impl [el]

  ;; should this be in the
  ;(classes/add el "WOOF-WIP")

  (let [; $date (woof-dom/q el "#header #title")
        date (safe-txt el "#header #title")
        ]

    ;; (.log js/console "parsing" date)

    (merge
      {
       :d date
       }
      (if-let [$contents (q el "#contents")]
        (let [videos (q* $contents "#contents ytd-video-renderer")
              imgs   (q* $contents "#thumbnail #img")

              ready? (and
                       (= (count imgs) (count videos))
                       (every? #(.-complete %) imgs))
              ]

          (if ready?
            ;; parse
            (let [
                  $videos (q* $contents "#contents > ytd-video-renderer")
                  ;;"YTD-THUMBNAIL"

                  videos (map (fn [$root]
                                (if (classes/has $root "PROCESSED-VIDEO")
                                  (classes/add $root "DOUBLE-PROCESSED-VIDEO")
                                  )

                                (classes/add $root "PROCESSED-VIDEO")


                    (merge
                      ;;  YTD-VIDEO-META-BLOCK

                      (if-let [$el (q $root "YTD-VIDEO-META-BLOCK YTD-CHANNEL-NAME YT-FORMATTED-STRING A")]
                        {
                         :channel-url (.getAttribute $el "href")
                         :channel (txt $el)
                         }
                        {})

                      (if-let [$el (q $root "YTD-VIDEO-META-BLOCK > DIV > DIV > SPAN")]
                        {
                         :views (txt $el)
                         }
                        {})
                      ;
                      {
                       :url (if-let [$el (q $root "YTD-THUMBNAIL > A")]
                              (.getAttribute $el "href") "")

                       :duration (if-let [$el (q $root "YTD-THUMBNAIL-OVERLAY-TIME-STATUS-RENDERER SPAN")]
                                   (txt $el) "")

                       :title (if-let [$el (q $root "H3 YT-FORMATTED-STRING")]
                                (txt $el) "")

                       :description (if-let [$el (q $root "#description-text")]
                                (txt $el) "")
                       })
                    ) $videos)
                  ]

              ;; ytd-thumbnail-overlay-resume-playback-renderer

              ;(.log js/console "VIDEOS" $videos)
              {
                :videos videos
                :ready? true
               }
              )

            {:ready? false}
            )

          )
        {:ready? false}
        )
      )
    )

  )



(defn is-scraped? [el]
  (dataset/has el "woof_id"))


(defn mark-scraped! [el]
  ;; todo: configure
  (let [ALLOW-DOUBLE-PARSE false]

    ;; mark element as processed
    (if-not ALLOW-DOUBLE-PARSE
      (dataset/set el "woof_id" (sid)))
    ))


;;;;;;;;



(defn _observe [mut-chan muts]
  ; (.warn js/console muts)
  (doseq [mut muts]
    (async/put! mut-chan mut)))

(defn make-observer [mut-chan]
  (js/MutationObserver. (partial _observe mut-chan)))

;;
;;
(defn _history-scrape-fn [params el]

  ; todo: should this be a separate woof workflow?

  ; mark el as WIP
  (classes/add el "WOOF-WIP")

  (let [mut-chan (make-chan (&chan-factory params) (sid "MUT/"))
        out-chan (make-chan (&chan-factory params) (sid "OUT/"))

        *WF-UI (base/& params :wf/*UI ":wf/*UI should be provided")

        ;; store last parse result
        *result (volatile! {})
        *count (volatile! 0)

        parse! (fn []
                 ;; each sub video should be marked as parsed
                 (let [result (history-day-scrape-impl el)]
                   (.log js/console "PARSE:" result)

                   (if (:ready? result)
                     (do
                       (swap! *WF-UI assoc :SCRAPE/WIP (:d result) (count (:videos result)))
                       (vreset! *result result)
                       )
                     (do
                       (.warn js/console "can't parse" el)
                       )
                     )
                   ))

        mut-observer (make-observer mut-chan)

        ; INITIAL_EL el
        ]

    ;; todo: maybe mutation observer is not needed, we can re-use ytd-continuation-item-renderer
    (.observe mut-observer
      el
      #js {:characterData true
           :childList true
           :subtree true})

    ;; parse in different 'thread'
    (js/setTimeout parse!)

    (go-loop []
      (let [v (async/alt!
                mut-chan ([result] :mutation)
                (u/timeout 1000) :tick)]

        (if (= :mutation v)
          (do
            ;; reset the try counter on mutation
            (.log js/console "MUTATION")
            (classes/add el "WOOF-UPD")
            (vswap! *count 0)

            (js/setTimeout parse!)

            (recur)
            )
          (if (= :tick v)
            (let [result @*result
                  c @*count]

              (if (or ; has loading spinner
                      (woof-dom/q el "ytd-continuation-item-renderer")
                      ; not parseable yet
                      (not (get result :ready? false)))
                (do
                  (if (< 10 c)
                    (do
                      ;; try harder)
                      (.log js/console "WAITING..." el result)
                      (recur))
                    (do ;; not ready after N tries
                      (.warn js/console "CAN'T PROCESS" el
                             (pr-str result))

                      (classes/add el "WOOF-ERROR")
                      (async/put! out-chan {:error true
                                            ;; provide some reason why
                                            :tag (.-outerHTML el)

                                            })
                      (.disconnect mut-observer)
                      )
                    )
                  )
                (do
                  ;; PARSED

                  (classes/add el "WOOF-DONE")
                  (swap! *WF-UI dissoc :SCRAPE/WIP (:d result))
                  (swap! *WF-UI update :RESULTS merge {(:d result) result})

                  (async/put! out-chan result)
                  (.disconnect mut-observer)

                  )
                )
              )
            )
          )))
    out-chan
    )
  )



;;;;

(defn parse-playlist-video [el]
(let [id_$ "A:nth-child(1)"
        ;channel_$ "DIV:nth-child(3) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1) > YTD-CHANNEL-NAME:nth-child(1) > DIV:nth-child(1) > DIV:nth-child(1) > YT-FORMATTED-STRING > A"
                 ;_  "DIV:nth-child(3) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1) > YTD-CHANNEL-NAME:nth-child(1) > DIV:nth-child(1) > DIV:nth-child(1) > YT-FORMATTED-STRING > A"
        ;title_$ "DIV:nth-child(3) > H3:nth-child(1) > SPAN:nth-child(2)"


        ]

    (if-let [id (safe-href el id_$)]
      (merge
        {
         :id id

         ;; todo: exctract WL index from the URL

         ;:channel (safe-href el channel_$)
         ;:title (safe-txt el title_$)


         ;; :el-map (map #(dissoc % :el) (wdom/el-map el :top-selector-fn (fn [base el] { :nth-child (:i base)})))
         }

        (if-let [n (wdom/q el "YTD-CHANNEL-NAME YT-FORMATTED-STRING > A")]
          {:channel-href (wdom/attr n "href")  }
          {"YTD-CHANNEL-NAME YT-FORMATTED-STRING > A" false })

        (if-let [n (wdom/q el "A YT-IMG-SHADOW:nth-child(1) IMG")]
          {:img-src (wdom/attr n "src")  }
          {"A YT-IMG-SHADOW:nth-child(1) IMG" false		   })

        (if-let [n (wdom/q el "#channel-name #text a")]
          {:channel-title (.-innerText n)  }
          {"#channel-name #text a" false   })

        (if-let [n (wdom/q el "DIV:nth-child(3)  H3:nth-child(1)  SPAN:nth-child(2)")]
          {:title (.-innerText n)}
          {"DIV:nth-child(3)  H3:nth-child(1)  SPAN:nth-child(2)" false})

        (if-let [n (wdom/q el "YTD-THUMBNAIL-OVERLAY-TIME-STATUS-RENDERER > SPAN:nth-child(2)")]
          {:duration (.-innerText n)}
          {"YTD-THUMBNAIL-OVERLAY-TIME-STATUS-RENDERER > SPAN:nth-child(2)" false})


        #_(if-let [n (wdom/q el "DIV:nth-child(3) > YTD-VIDEO-META-BLOCK:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1) > YTD-CHANNEL-NAME:nth-child(1) > DIV:nth-child(1) > DIV:nth-child(1) > YT-FORMATTED-STRING > A")]
          {:innerText (.-innerText n)}
          {"DIV:nth-child(3) > H3:nth-child(1) > SPAN:nth-child(2)" false})


        )

      (do
        (.warn js/console "cannot find the id for element " el)
        )
      )


    )
)
