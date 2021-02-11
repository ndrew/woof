(ns woof.server.scraper.fs
  "woof scraper filesystem"
  (:require
    [clojure.core.async :as async :refer [go go-loop]]
    [clojure.java.io :as io]
    [clojure.string :as str :refer [join]]

    [clojure.java.io :as io]
    [semantic-csv.core :as sc]
    [clojure.data.csv :as cd-csv]

    ;;     [me.raynes.fs :as fs]

    [taoensso.timbre :as timbre :refer [log trace debug info warn error fatal report logf tracef debugf infof warnf errorf fatalf reportf spy get-env]]

    ))


;; hard-coded rank map for CSV export
(defonce ranks
         (array-map
           :ok 1
           :id 10
           :url 11

           :USD 20
           :USD_M2 21

           :UAH 30
           :UAH_M2 31

           :commission 32

           ;; addr
           :addr 40
           :addr_street 41
           :addr_house 42
           :addr_district 43
           :addr_district_1 44
           :addr_subway 45

           ;:district_1
           ;:district_1_url

           :info 50   ;; why ????

           :img-1 51
           :img-2 52
           :img-3 53

           ;:imgs 51
           ;:img-n 52
           ;:img-alt 53

           :rooms 60
           :floor 61
           :floor_total 62

           :area 64
           :area_total 65
           :area_kitchen 66
           :area_living 67

           :house_walls 68
           :house_new 69

           ;; meta
           :source 70
           :added 71
           :upd 72

           :paid 73
           :paid_info 74
           :agent-id 75
           :agent-name 76

           ;; other meta
           :_url_district 80
           ))


;;

;; provide your base dir - if needed
(def base-dir "")


(defn get-ids [src]
  ;; todo: nicer string joining
  (let [file-name (str base-dir
                       (name src) "_" "ids.edn")]
    (when-not (.exists (io/file file-name))
      (spit file-name #{})
      (info file-name "created"))

    (read-string (slurp file-name))))



(defn save-listings [data]
  (let [*log (volatile! [])]

    (doseq [{src :src
             listings :listings
             ids :ids} data]

      (let [keyfn (fn [k] (get ranks k 10000))

            sort-fn (partial sort-by keyfn)

            VECT-OPTS {
                       ;; todo: add missing rows if needed
                       :header (keys ranks) #_(-> listings
                                                  first
                                                  keys
                                                  set
                                                  (into #{:img-1 :img-2 :img-3})
                                                  sort-fn)
                       }
            ]


        (let [src (name src)
              file-name (str base-dir src "_" (.format (new java.text.SimpleDateFormat "dd-MM-yyyy") (java.util.Date.))  ".csv")
              ids-file-name (str base-dir src "_ids.edn")]

          (when-not (empty? ids)
            (spit ids-file-name ids)

            (info ids-file-name " written!")
            (vswap! *log conj (str ids-file-name " written!")))


          (when-not (empty? listings)
            (with-open [out-file (io/writer file-name)]
              (->> listings
                   (sc/cast-with {
                                  ;;
                                  :img-1 (fnil #(str "=IMAGE(" (pr-str %) ")") "")
                                  :img-2 (fnil #(str "=IMAGE(" (pr-str %) ")") "")
                                  :img-3 (fnil #(str "=IMAGE(" (pr-str %) ")") "")
                                  })

                   (sc/vectorize VECT-OPTS)
                   (cd-csv/write-csv out-file))
              )

            (info file-name " written!")
            (vswap! *log conj (str file-name " written!"))
            )
          )
        )
      )
      @*log
    )
  )

