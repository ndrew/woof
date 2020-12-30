(ns woof.client.browser.scraper.actions
  (:require
    [woof.base :as base]
    [woof.client.dom :as woof-dom]
    ))


;; (woof-dom/chord 83 :shift true :meta true)
(defn chord-action [chord action-name f ]
  [action-name
   (with-meta f {:shortcut chord} )
   ;; #^{:shortcut chord} f
   ])


(defn actions-impl! [API on-chord]

  (let [SHORTCUTS (reduce (fn [a [k f]]
                            (if f
                              (if-let [m (meta f)]
                                (if (contains? m :shortcut)
                                  (assoc a (:shortcut m) f))
                                a
                                )
                              a
                              )
                            ) {} API)

        klog (fn [e]
               (let [chord (into {} (for [[key attr] {:shift "shiftKey" :ctrl "ctrlKey" :alt "altKey" :meta "metaKey" :code  "keyCode"}]
                                      [key (aget e attr)]))]

                 (on-chord chord SHORTCUTS)

                 )
               )
        ]
    {
     :init [(fn [params]
              (js/addEventListener "keydown" klog false)
              {}
              )]
     :opts [(base/build-opt-on-done (fn [_ _]
                                      ; (.warn js/console result)
                                      (js/removeEventListener "keydown" klog false)))
            ]
     }
    )
  )


;;
;; WORKFLOW KEY ACTIONS
(defn default-on-chord [chord SHORTCUTS]
  ;; (.log js/console chord SHORTCUTS)
  (cond
    ;; cmd + shift + up
    (= chord {:shift true :ctrl false :alt false :meta true :code  38})
    (woof-dom/scraping-ui__inc 50)

    ;; cmd + shift + down
    (= chord {:shift true :ctrl false :alt false :meta true :code  40})
    (woof-dom/scraping-ui__inc -50)

    (contains? SHORTCUTS chord) ((get SHORTCUTS chord))

    :else (do)
    )
  )
