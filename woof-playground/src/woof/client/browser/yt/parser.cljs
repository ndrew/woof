(ns woof.client.browser.yt.parser
  (:require
    [goog.dom :as dom]
    [goog.object :as gobj]

    [goog.dom.classes :as classes]
    [goog.dom.dataset :as dataset]

    [clojure.string :as str]
    [cljs.core.async :refer [go go-loop] :as async]

    [woof.base :as base]
    [woof.data :as d]

    [woof.client.dom :as wdom]
    [woof.client.dbg :as dbg :refer [__log]]

    [woof.utils :as u]

    [cljs-time.core :as time]
    [cljs-time.format :as time-fmt]

    [woof.client.dom :as woof-dom :refer [txt q q*]]))



#_(defn history-day-selector []
  "#contents .ytd-section-list-renderer"
  )



(defn history-day-selector []
  ;;
  "#contents .ytd-section-list-renderer:not(.PROCESSED-SECTION)"
 ; "#contents .ytd-section-list-renderer"
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




(defn history-day-scrape-impl [el]

  ; (.log js/console "parsing" el)

  ;"DIV:nth-child(3) > YTD-VIDEO-RENDERER:nth-child(1) > DIV:nth-child(1) > DIV:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1) > H3:nth-child(1) > A:nth-child(2) > YT-FORMATTED-STRING:nth-child(2)"
  ;"DIV:nth-child(3) > YTD-VIDEO-RENDERER:nth-child(1) > DIV:nth-child(1) > DIV:nth-child(2) > DIV:nth-child(1) > DIV:nth-child(1) > H3:nth-child(1) > YTD-BADGE-SUPPORTED-RENDERER:nth-child(1) > DIV:nth-child(1) > SPAN:nth-child(2)"

  #_(let [$date "DIV:nth-child(1) > YTD-ITEM-SECTION-HEADER-RENDERER > DIV > DIV:nth-child(1)"
        $duration "DIV:nth-child(3) > YTD-VIDEO-RENDERER:nth-child(1) > DIV:nth-child(1) > YTD-THUMBNAIL:nth-child(1) > A > DIV:nth-child(2) > YTD-THUMBNAIL-OVERLAY-TIME-STATUS-RENDERER:nth-child(2) > SPAN:nth-child(2)"]
    {
     :date (safe-txt el $date)

     :duration (safe-txt el $duration)
     :aria-duration (if-let [$el (woof-dom/q el $duration)]
                      ;; don't trim for now
                      (.getAttribute $el "aria-label")
                      ""
                      #_(do
                        (.log js/console "can't find element for selector: " selector "parent-el" el)
                        (classes/add el "parsed-error")
                        ""
                        )
                      )

     :url (if-let [$el (woof-dom/q el "DIV:nth-child(3) > YTD-VIDEO-RENDERER:nth-child(1) > DIV:nth-child(1) > YTD-THUMBNAIL:nth-child(1) > A")]
           (.getAttribute $el "href")
           ""
           )
     }


    )

  (let [; $date (woof-dom/q el "#header #title")
        date (safe-txt el "#header #title")
        ]

    (.log js/console "parsing" date)

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



;;
;; main parsing function
;;  - parses block of videos per day
;;  - marks block as parsed after all videos had been processed
;;
(defn _history-day-scrape [params el]
  (try
    (let [result (history-day-scrape-impl el)
          *UI (get params :wf/*UI)]
      (if (get result :ready? false)
        (let [t (u/now)
              ;; chunk updates
              _t (quot t 10000)

              time-class (str "ttt-" _t)

              ]
          (swap! *UI update :RESULTS merge {(:d result) result})


          ; (.log js/console "PROCESSED:" (pr-str result))
          (classes/add el "PROCESSED-SECTION")

          (classes/add el time-class)
          (swap! *UI assoc :upd t)

          (swap! *UI update :upd-class (fnil conj #{}) _t)


          ;; hide the parsed data in order to speed up the
          (classes/add (woof-dom/q el "#contents") "HIDE-HIDE-HIDE")

          ;(woof-dom/html! (woof-dom/q el "#contents") (str (u/now)))

          :parsed-OK
          )
        :waiting
        )

      ;; todo: ...
      ; (swap! *SCRAPED-DATA conj result)
      ; (swap! *WF-scrape-data conj result)
      )
    (catch js/Error e
      (do
        (classes/add el "PROCESSED-ERROR")
        (.error js/console e)

        ;; (swap! *FAILED conj (woof-dom/el-map el))
        )
      )
    )
  )



#_(defn _history-day-scrape-async [params el]

  (let [cf (base/&chan-factory params)
        chan (base/make-chan cf (base/rand-sid))]
    (go
      (async/<! (u/timeout (rand-int 500)))
      (async/put! chan
                  (_history-day-scrape params el))

      )
    chan
    )
  )

(defn is-scraped? [el]
  (dataset/has el "woof_id"))


(defn mark-scraped! [el]
  (let [sid (base/rand-sid)
        ALLOW-DOUBLE-PARSE true
        ]
    ;; mark element as processed
    (if-not ALLOW-DOUBLE-PARSE
      (dataset/set el "woof_id" sid))
    ))
