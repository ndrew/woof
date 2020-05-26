(ns ^:figwheel-hooks
  woof.client.playground.wf.multi.wf
  (:require
    [cljs.core.async :as async]

    [woof.client.playground.wf.common :as cmn]
    [woof.client.playground.ui.wf :as wf-ui]


    [woof.base :as wf]
    [woof.utils :as u]
    [woof.u :as base]

    [woof.client.playground.wf.multi.wf-ui :as ui]
    )

  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


;; exploring idea that expand step in a workflow can represent a process:
;;
;;   can return multiple results:
;;     * having intermediate results
;;     * history of changes
;;     * user-friendly / machine-friendly data
;;   provide contract/spec via hardcoded ns-specific steps
;;     * start with simplest (hardcoded) implementation
;;     * how to deal with intermediary steps
;;   expand step can be substituted by separate workflow

;; initially expand steps were intended to be used to return collections,
;; that could be processed in parallel.

;; infinite expand can be used as event loop


;; example: process chaining

  ;; cfg process []
  ;;   - read config from fs - parse - RETURN
  ;; content process [cfg]
  ;;   - return list of files
  ;;   - process these files
  ;; ui process [content]
  ;;   - display the results




;; global state, for easier storing wf stuff and passing it to the UI
(defonce initial-ui-state {:log []
                           :ready? false})

(defonce *UI-STATE (atom initial-ui-state))






;;
;; CONFIG process 

(defn CFG-ctx [params]
  {

   ;; config process, basically merges different configs into a single one
   :CFG* {:expands? true
           :fn (fn [cfg]
                 {

                  ;; return intermediary config for specific process as separate step
                  :cfg/template-cfg [:id {:template :hiccup}]
                  :cfg/deploy-cfg [:id {:deploy :github}]
                  :cfg/content-cfg [:id cfg]

                  ;; main contract - return all sub-configs merged
                  :cfg/CONFIG [:kv-merge* [:cfg/template-cfg :cfg/deploy-cfg :cfg/content-cfg]]
                  }
                 )}

   }
  )

;;
;; CONTENT process

(defn CONTENT-ctx [params]
  {

   ;; content process - return file names and processed file
   :CONTENT* {
              :expands? true
              :fn (fn [config-map]
                    {

                     ;; finds markdown files in the specified directory
                     :content/FILE-NAMES [:md-files* (get config-map :posts-dir)]
                     
                     ;; processes each file as sub-process
                     :content/FILE-CONTENTS [:process-files* :content/FILE-NAMES]
                     ;; this process adds intermediary steps for processing assets.

                     ;;    <?> should we link sids to root file and kv-zip contents
                     ;; or <?> should we use unique asset id for that? so root file won't be waiting for assets


                     :ui/store-md-files [:ui-log [:ui/__md-caption :content/FILE-NAMES]]
                       :ui/__md-caption [:id "file list"]

                     :ui/store-file-contents [:ui-log [:ui/__file-contents-caption :content/FILE-CONTENTS]]
                       :ui/__file-contents-caption [:id "processed files"]

                     }


                    )
              }



   ;; simulate finding file names for further processing
   :md-files* {
                   :fn (fn [dir]
                         (let [N (+ (rand-int 10) 2)
                               fs (map (fn [x] (str "post-" x ".md")) (range N))
                               ]

                              (reduce (fn [a f]
                                        (assoc a (keyword "md" f) [:id {:f f}])) {} fs)

                              )
                         )
                   :expands? true
                   }


   ;; simulate reading file contents and extracting meta-data for it
   :process-files* {
                          :fn (fn [files]
                                (reduce (fn [a f]
                                          (let [file-path (:f f)
                                                assets-N (rand-int 5)

                                                ;; todo: now all assets are unique per root file, how to make them overlap

                                                assets (reduce (fn [A item]
                                                                 (assoc A (base/rand-sid (str "asset-" file-path "-"))
                                                                        [:id {:f (str "asset-" file-path "__" item ".jpg")}]
                                                                        )
                                                                 ) {} (range assets-N))
                                                processed-file (assoc f
                                                                 :f file-path
                                                                 :content (str (base/rand-sid))
                                                                 ;; can't use sids here - as they'll be lost without kv zipping
                                                                 :assets (vec (map #(get (second %) :f) (vals assets)))
                                                                 )
                                                ]
                                               (merge
                                                 (assoc a (base/rand-sid "p-") [:id processed-file])
                                                 assets)

                                               )

                                          )
                                        {} files)
                                )
                          :expands? true
                          :collect? true
                     }

   })

;;
;; ALL PROCESSES JOINED
;;
(defn steps-fn [params]
  {

   ;; RUN config process
   :cfg/process [:CFG*
                 {
                  :posts-dir "/some/dir"
                  }] ;; => :cfg/CONFIG

   ;; process can be replaced by providing export fields directly — :cfg/CONFIG [:id {:cfg }]


   ;; RUN content process

   ;; use content config as input for content process
   :content/config [:id :cfg/CONFIG]
   ; this allows to substitute input process with value

   :content/process [:CONTENT* :content/config]

   ;; RUN UI process

   :ui/ready [:ready [:content/process]]

   
   ;; pass content kv-zip to UI
   :ui/__posts [:ui-assoc [:ui/__content-key :ui/CONTENT_KV_ZIP]]
     :ui/__content-key [:id :POSTS]

   :ui/CONTENT_KV_ZIP     [:*kv-zip [:ui/__mem-k__content :content/process]]
     :ui/__mem-k__content   [:mem-k* :content/process]

   }
  )


;; UI PROCESS context
(defn ui-ctx [params]
  {

   :ui-log {:fn (fn [[id v]]
                  ;; linearization: store intermediary values and display them in UI
                  (swap! *UI-STATE update-in [:log] conj [id v nil])
                  )
            :collect? true}

   ;; puts kv data in the UI state
   :ui-assoc {:fn (fn [[id v]]
                    (swap! *UI-STATE assoc id v)
                    )
              :collect? true}


   ;; tells UI that wf is ready
   :ready {:fn (fn [trigger]
                 (swap! *UI-STATE assoc :ready? true)
                 )
           :collect? true
           }

   }
  )


;;

(defn wf-as-process-initializer [*SWF]
  (let [EVT-LOOP  (cmn/make-chan (wf/rand-sid "evt-loop"))]
    {

     :title       "Expand steps"
     :explanation [:div.explanation
                   [:p "figuring stuff out"]]
     ;;
     :init-fns    [(fn [params] (reset! *UI-STATE initial-ui-state) {} ) ;; rewrite global ui state, on start
                   cmn/chan-factory-init-fn
                   (fn [params] {::EVT-LOOP EVT-LOOP})    ;; expose evt loop for other workflows
                   ]
     ;;
     :ctx-fns     [cmn/common-ctx-fn
                   CFG-ctx
                   CONTENT-ctx
                   ui-ctx
                   ]

     ;;
     :steps-fns   [(fn [params]
                     {
                      ::#EVT-LOOP# [:evt-loop (::EVT-LOOP params)]
                      })
                   steps-fn
                   ]

     :opt-fns     [cmn/chan-factory-opts-fn]

     :ui-fn       (partial wf-ui/<wf-UI> (partial ui/<ui-prototype-1> *UI-STATE))
     })
  )

