(ns blog.frontend.md
  (:require
    [clojure.string :as str]
    [clojure.edn :as edn]

    [cljsjs.commonmark]

    [rum.core :as rum]

    [woof.client.playground.ui :as ui]
    )
  )

;; MD

;; use global renderer for now
(defonce renderer (new js/commonmark.HtmlRenderer (clj->js {
                                                            "sourcepos" true ;; source position information for block-level elements will be rendered in the data-sourcepos attribute (for HTML)
                                                            })))

(defn md->html [md-text]
  (let [parser-opts (clj->js {
                              "smart" true ;; straight quotes will be made curly, -- will be changed to an en dash, --- will be changed to an em dash, and ... will be changed to ellipses
                              ; "safe" false - no raw html
                              })
        parser (new js/commonmark.Parser parser-opts)]
    ;;
    (let [md-root (.parse parser md-text)
          walker (.walker md-root)
          *meta (volatile! {})

          *tree (volatile! [])

          ; *cursor (volatile! [])
          ; *last-parent (volatile! "")
          ]

      ;; split this into rum controls

      (loop [evt (.next walker)]
        (when evt
          ;(.log js/console evt)

          (let [node (.-node evt)]

            (if (and (= "code_block" (.-type node))
                     (= "blog-post" (.-info node )))
              (do
                (vreset! *meta
                         (with-meta
                           (edn/read-string (.-literal node))
                           {:src (js->clj (.-sourcepos node))})
                         )

                (.unlink node)))

            (if (and
                  (.-entering evt)
                  (.-parent node)
                  (= "document" (.. node -parent -type))
                  )
              (vswap! *tree conj node)
              )
            )

          (recur (.next walker))))

      ;; (.warn js/console @*tree)

      ; (.render renderer md-root)
      {
       :meta @*meta
       :md md-text

       ;; for now return rendered html
       :html (.render renderer md-root)

       :nodes  @*tree
       }

      ))
  )


;; UI

(rum/defcs <selected-md-block> < (rum/local :md ::display)
  [local {
          select-fn :select-fn
          md :md
          } node]
  [:div.md-block.selected
   {
    :on-click select-fn
    :style {:background-color "rgba(0,0,0,.06333)"}
    :class (if (= :md @(::display local)) "md-editor")
    }

   ; menu -
   (ui/menubar "display as:" [["md" (fn [] (reset! (::display local) :md))]
                              ["html" (fn [] (reset! (::display local) :html))]])

   ;; should we render html in component, or it should be passed from outside
   (if (= :html @(::display local))
     [:div.md-block {:dangerouslySetInnerHTML {:__html (.render renderer node)}}]
     (let [
           [[_start-line _start-x] [_end-line _end-x]] (js->clj (.-sourcepos node))

           start-line (dec _start-line)
           end-line (dec _end-line)

           start-x (dec _start-x)
           end-x (dec _end-x)
           ; should we take start-x and end-x to account for top level elements?
           ;fst (subs (first used-lines) start-x)
           ;lst (subs (last used-lines) 0 end-x)

           lines (str/split-lines md)

           used-lines (vec (take
                             (inc (- end-line start-line))
                             (drop start-line lines)))

           md-content (reduce #(str %1 %2 "\n") "" used-lines)
           ]

       (cond
         (= "code_block" (.-type node)) ;; handle indent?
         [:pre [:code md-content]]

         :else [:p
                md-content
                ]
         )
       )
     )

   ]
  )
