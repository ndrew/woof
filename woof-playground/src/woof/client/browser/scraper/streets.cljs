(ns woof.client.browser.scraper.streets
  (:require

    [woof.base :as base]
    [woof.client.dom :as woof-dom :refer [q q* html! btn!]]
    [woof.client.dbg :as dbg :refer [__log]]
    [woof.data :as d]
    [woof.utils :as u]

    [goog.dom :as dom]
    [goog.dom.classes :as classes]

    [goog.dom.dataset :as dataset]
    ;; common wf

    [woof.wfs.evt-loop :as evt-loop]
    [woof.wfs.watcher :as watcher]

    [woof.client.ws :as ws]
    [woof.utils :as u]
    [woof.client.browser.scraper.session :as ss]
    [woof.client.browser.scraper.scraping-ui :as sui]

    [cljs.core.async :refer [go] :as async]
    [cljs.core.async.interop :refer-macros [<p!]]
    [clojure.string :as str]))

(defn clean-css-init [params]

  ;; remove previosly added styles
  (woof-dom/remove-added-css)

  ;; remove classes for previously added elements
  (doseq [s* ["woof-el"
              "woof-start"
              "woof-tmp"
              "woof-err"
              "woof-processed"
              ]
          ]
    (doseq [el (q* (str "." s*))]
      (classes/remove el s*)
      )
    )

  {
   ;; ::state *state
   })


(defn save-streets-edn [edn]

  (let [a (.createElement js/document "a")]
    (.appendChild (.-body js/document) a)
    (set! (.-style a) "display: none")

    (let [s (pr-str edn)
          blob (js/Blob. (clj->js [s])
                         (js-obj "type" "octet/stream"))
          url (.createObjectURL (.-URL js/window) blob)]

      (set! (.-href a) url)
      (set! (.-download a) (str "streets-" (u/now) ".edn"))

      (.click a)
      (.revokeObjectURL (.-URL js/window) url)
      ;; todo: remove a element
      )
    )

  )


(defn process-street [el]
  (try

    ;; parse street definition from element text

    (classes/add el "woof-el")

    ;; \s\((.*)\)\.\s(.*)
    (let [text (.-textContent el)
          ;regexp (re-pattern "(?ms)(.*);\\s(.*);\\s(.*)\\s(\\d+)\\s\\((.*)\\)\\.\\s(.*)")


          ;; non-greedy one

          ;; "(?ms)(.*?);\\s+(.*?);\\s+(.*?)\\s+(\\d+)\\s\\((.*?)\\)[\\.\\s]*(.+)"
          ;; define regexp parts
          parts ["(.+);" ;; (ua name);
                 "\\s+(.+);" ;; (ru name);
                 "\\s+(.+?)"  ;; (en name)
                 "\\s+(\\d+)" ;; (idx)
                 "\\s\\((.*?)\\)" ;; \( districts \)
                 "[\\.\\s]*(.+)" ;; other
                 ]


          regexp (re-pattern (str "(?ms)" (apply str parts)))
          ;regexp (re-pattern "(?ms)(.*);\\s+(.*);\\s+(.*)\\s+(\\d+)\\s+\\((.*)\\)\\.\\s+(.*)")
          matches (re-matches regexp text)]

      (let [[_ ua ru en idx __district __other] matches

            _district (if (nil? __district) "" (str/trim-newline __district))

            districts (str/split _district #",\s")

            _other (if (nil? __other) ""  (str/trim-newline __other))
            ]


        (if (nil? ua)
          (do
            ;; (js-debugger)



            (loop [parts (vec (drop-last parts))]

              (let [pattern-s (str "(?ms)" (apply str (conj parts "(.+)")))
                    regexp (re-pattern pattern-s)]

                #_(.log js/console
                      (str
                        pattern-s
                        "\n\n"
                        text
                        "\n========\n"
                        )
                      (re-matches regexp text)

                      )

                (if (> (count parts) 1)
                  (recur (vec (drop-last parts)))
                  ""
                  )

                )

              )


            ;; (js-debugger)

            (classes/add el "woof-err")
            (classes/add el "woof-err-parse")
            )


          ;; regexp had been parsed
          (do
            (let [has-errors
                  (cond



                    (re-find #"[а-яА-Я]" en)
                    (do
                        (.log js/console
                              "regex: "
                              (str "(?ms)" (apply str parts))

                              "matches: " matches

                              "\ntext:\n"
                              text

                              )
                        (classes/add el "woof-err-en") true
                        )

                    (not= 5 (count idx))
                    (do

                      (.log js/console 5)


                      (classes/add el "woof-err-idx") true
                      )


                    (#{"Голосіївський" "Солом'янський"} (first districts))
                    (do
                      (classes/add el "woof-err-idx") true
                      )

                    (re-find #"\n" _district)
                    (do
                      (.log js/console _district)
                      (classes/add el "woof-err-district") true
                      )


                    :else false
                    )]

              (if has-errors
                (do
                  (.warn js/console {
                                     :ua        ua
                                     :ru        ru
                                     :en        en
                                     :idx       idx

                                     :district  (first districts)
                                     :districts districts

                                     ;; extract directions
                                     :other     _other

                                     ;; store raw text if there had been a problem

                                     })
                  (classes/add el "woof-err")
                  )

                (classes/add el "woof-processed")
                )

              )
            )

          )



        {
         :ua        ua
         :ru        ru
         :en        en
         :idx       idx

         :district  (first districts)
         :districts districts

         ;; extract directions
         :other     _other

         ;; store raw text if there had been a problem

         }
        )
      )


    ;; (parse-listing el)

    (catch js/Error e
      (do
        (.warn js/console
               "error during parsing" el
               e)
        {:error :error}
        )))
  )

(defn wf! [*wf-state meta-info]

  {
   :init    [clean-css-init]

   :ctx     [watcher/watcher-ctx
             (fn [params]
               {

                :process*   (base/expand-into :process)
                :process    {:fn process-street}

                :download-edn {:fn save-streets-edn}


                :custom-log {:fn (fn [v]
                                   (if (seq v)
                                     (do
                                       (.groupCollapsed js/console "STREETS")
                                       (doseq [a v]
                                         (.log js/console a))
                                       (.groupEnd js/console)
                                       )

                                     (.log js/console v)
                                     )

                                   )}

                :aggregate {:fn (fn [streets]

                                  (let [a (volatile! #{})]
                                    (doseq [st streets]
                                      ;(vswap! a into (:districts st))
                                      (vswap! a conj (first (:districts st)))
                                      )

                                    (doseq [d (sort @a)]
                                      (.warn js/console d)
                                      )


                                    )

                                  )}

                }
               )
             ]

   :steps   [
             (fn [params]
               {

                ;  ::css-1 [:css-rule ".woof-processed { display: none; }"]

                ::selector           [:v "P"]
                ;; get the elements to be parsed
                ::els                [:query-selector-all ::selector]

                ;; process the elements similar to pmap
                ::processed-elements [:process* ::els]

                ::collect [:collect ::processed-elements]



                ::save-results [:download-edn ::collect]
                ;; ::log [:custom-log ::collect]

                ;; post validate that all parsed are coherent
                ::validate [:aggregate ::collect]

                }

               )


             ;;
             (fn [params]
               {
                ; ::css-1 [:css-rule ".woof-scrape-panel { height: 150px; width: 100%; }"]

                :css/custom-css [:css-file "http://localhost:9500/css/streets.css"]
                }
               )
             ]

   ;; р-н СТ "Арсеналець"
   :otps    [
             ;;
             (fn [params]
               ;; update
               {:execute (partial base/_timed-execute-fn 50)}
               )
             ]

   :api     {}

   :on-stop (fn [state]
              (__log "STREETS EXTRACTOR: ON STOP")
              ;; (.log js/console state)


              ;; todo: does copying prev state makes sense here
              #_(when-let [*old-state (get-in state [:WF/params ::state])]
                  (.log js/console "copying prev state")
                  (reset! *state @*old-state)
                  )

              ;; can return channel
              )
   }

  )