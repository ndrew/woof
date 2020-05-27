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
    [woof.data :as d])

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


                     ;:ui/store-md-files [:ui-log [:ui/__md-caption :content/FILE-NAMES]]
                     ;  :ui/__md-caption [:id "file list"]

                     ;:ui/store-file-contents [:ui-log [:ui/__file-contents-caption :content/FILE-CONTENTS]]
                     ;  :ui/__file-contents-caption [:id "processed files"]

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
                                                                        [:id {
                                                                              :f (str "asset-" file-path "__" item ".jpg")
                                                                              :type :asset
                                                                              }]
                                                                        )
                                                                 ) {} (range assets-N))
                                                processed-file (assoc f
                                                                 :f file-path
                                                                 :type :md
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
;; RENDER PROCESS

(defn RENDER-init [params]
  {
   :render/*ASSETS-CACHE (atom {})
   }
  )

(defn RENDER-ctx [params]
  {

   :post-template {:fn (fn [cfg]
                         {
                          :template (fn [f]
                                      (assoc f
                                             :html (str "<RENDER>" (d/pretty! (:content f)) "</RENDER>" ))
                                      )
                          }
                         )
                   }

   :render! {
                  :fn (fn [[template data]]
                        (let [template-fn (get template :template)]
                             (template-fn data))
                        )
                  :collect? true
                  }

   :prepare-assets! {
                     :fn (fn [post]
                           (let [asset-ids (get post :assets [])]

                                (.log js/console "prepare assets" asset-ids)
                                {:processed-assets asset-ids}
                                )

                           )

                     }

   :render-result  {:fn (fn [f]
                          (assoc f ::ready true)
                          )
                          }



   :render-many {
                 :fn (fn [[template posts]]
                       (reduce (fn [a post]
                                 (let [&post (base/rand-sid "render")
                                       &assets (base/rand-sid "render")
                                       &post+assets (base/rand-sid "render")
                                       &template (base/rand-sid "render")
                                       &render (base/rand-sid "render")
                                       ]
                                      (merge a
                                             {
                                              &post [:id post]
                                              &assets [:prepare-assets! &post]
                                              &post+assets [:kv-merge* [&post &assets]]
                                              &template [:id template]

                                              &render [:render! [&template
                                                                 &post+assets]]

                                              ;; export the result
                                              (base/rand-sid "render") [:render-result &render]

                                              }
                                             )

                                      )
                                 )
                         {} posts)



                       )
                 :expands? true
                 :collect? true
                 }

   :render-results {
                    :fn (fn [rendered-files]
                          (reduce
                            (fn [a item]
                              (if (::ready item)
                                   (conj a (dissoc item ::ready))
                                  a)
                              )
                            []
                            rendered-files)
                          )
                    :collect? true
                    }

   :RENDER*       {:expands? true
                   :fn       (fn [[CFG CONTENT]]
                               (let [all-files (get CONTENT :content/FILE-CONTENTS)

                                     files-by-type (group-by #(get % :type :unknown) all-files)

                                     assets (get files-by-type :asset [])
                                     posts (get files-by-type :md [])
                                     ]


                                    {
                                     ;; todo: handle case expand step returns nothing

                                     :render/template [:post-template CFG]

                                     ;; all posts / assets
                                     :render/ASSETS [:id assets]
                                     :render/POSTS  [:id posts]



                                     ;; render 1 file
                                     ;:render/dummy-file [:id {:f "file" :type :md :contents "booo!" }]
                                     ;:render/TEST-RENDER [:render! [:render/template
                                     ;                               :render/dummy-file]]

                                     ; render all posts
                                     ;; problem - for collect - we will return many data
                                     :render/_render-many [:render-many [:render/template :render/POSTS]]
                                     :render/TEST-RENDER [:render-results :render/_render-many]

                                     }

                                    )
                               )
                   :collect? true
                   }

   }
  )


;;
;; ALL PROCESSES JOINED
;;
(defn steps-fn [params]
  {

;; CONFIG process
   :cfg/PROCESS [:CFG*
                 {
                  :posts-dir "/some/dir"
                  }] ;; => :cfg/CONFIG

   ;; process can be replaced by providing export fields directly — :cfg/CONFIG [:id {:cfg }]

   :ui/CFG_KV_ZIP     [:*kv-zip [:ui/__mem-k__cfg :cfg/PROCESS]]
     :ui/__mem-k__cfg   [:mem-k* :cfg/PROCESS]
     :ui/__cfg [:ui-assoc [:ui/__cfg-key :ui/CFG_KV_ZIP]]
        :ui/__cfg-key [:id :CFG]


;; CONTENT process

   ;; use content config as input for content process
   :content/config [:id :cfg/CONFIG]
   ; this allows to substitute input process with value

   :content/PROCESS [:CONTENT* :content/config]

   ;; pass content kv-zip to UI
   :ui/__posts [:ui-assoc [:ui/__content-key :ui/CONTENT_KV_ZIP]]
     :ui/__content-key [:id :POSTS]
     :ui/CONTENT_KV_ZIP     [:*kv-zip [:ui/__mem-k__content :content/PROCESS]]
       :ui/__mem-k__content   [:mem-k* :content/PROCESS]


   ;; RENDER process
   :render/config [:id :cfg/CONFIG]         ;; IN contract - value
   :render/content [:id :ui/CONTENT_KV_ZIP] ;; IN contract - process with multiple values

   :render/PROCESS [:RENDER* [:render/config :render/content] ]

   :ui/RENDER_KV_ZIP     [:*kv-zip [:ui/__mem-k__render :render/PROCESS]]
     :ui/__mem-k__render   [:mem-k* :render/PROCESS]
     :ui/__render [:ui-assoc [:ui/__render-key :ui/RENDER_KV_ZIP]]
        :ui/__render-key [:id :RENDER]


   ;; UI process

   :ui/ready [:ready [:content/PROCESS]]

   

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

                   RENDER-init
                   ]
     ;;
     :ctx-fns     [cmn/common-ctx-fn
                   CFG-ctx
                   CONTENT-ctx
                   RENDER-ctx
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

