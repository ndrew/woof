(ns woof.ccs-api
  (:require
    [clojure.core.async :as async :refer [go go-loop]]

    [woof.data :as d]
    [woof.wf :as wf]
    [woof.wf-data :as wdata]

    [woof.utils :as u]
    [woof.test-data :as test-data]

    [clojure.java.io :as io]
    [clojure.java.shell :refer [sh]]

    [clojure.data.json :as json]
))


(comment

(def work-dir "/Users/ndrw/w/wltp/AT/")



(def CCS-API-URL "https://test.api.corpinter.net/ccs-edc-int2/api/v1")

(def CCS-X-API-KEY "aa72d31a-d458-4397-a56a-3bceff025d40")



;; curl -X GET "https://test.api.corpinter.net/ccs-edc-int2/api/v1/markets/de_AT/dataversion" -H "accept: application/json" -H "x-api-key: aa72d31a-d458-4397-a56a-3bceff025d40"

(defn dataversion-curl-params [ccs-market]
  [ "-X" "GET" (str  CCS-API-URL
                     "/markets/" ccs-market
                     "/dataversion")
    "-H" "accept: application/json" "-H" (str "x-api-key: " CCS-X-API-KEY )
    ]
  )


;; curl -X GET "https://test.api.corpinter.net/ccs-edc-int2/api/v1/models?country=AT" -H "accept: application/json" -H "x-api-key: aa72d31a-d458-4397-a56a-3bceff025d40"


(defn models-curl-params [ccs-country]
  [ "-X" "GET" (str  CCS-API-URL
                     "/models?country=" ccs-country)
    "-H" "accept: application/json" "-H" (str "x-api-key: " CCS-X-API-KEY )])


(defn cofiguration-curl-params [ccs-market-id dataversion ccs-model-id]
  [ "-X" "GET" (str  CCS-API-URL
                     "/markets/" ccs-market-id
                     "/dataversion/" dataversion
                     "/models/" ccs-model-id
                     "/configurations/initial")
    "-H" "accept: application/json" "-H" (str "x-api-key: " CCS-X-API-KEY )])





(defn response2json [resp]
  (let [{out :out} resp]
    (json/read-json out)
    ))


(def curl (memoize (partial sh "curl")))


(defn get-models [ccs-country]
  (let [r (apply curl (models-curl-params "AT"))
        j (response2json r)
       ]
    j))


(defn get-dataversion [ccs-market]
  (let [r (apply curl (dataversion-curl-params ccs-market))
        j (response2json r)
        {dv :dataversion} j]
    dv
))


;; curl -X GET "https://test.api.corpinter.net/ccs-edc-int2/api/v1/markets/de_AT/dataversion/53512f2b/models/23145712_100/configurations/initial" -H "accept: application/json" -H "x-api-key: aa72d31a-d458-4397-a56a-3bceff025d40" >> configuration_23145712_100.json

(defn get-model [market-id dataversion model-id]
  (let [r (apply curl (cofiguration-curl-params market-id dataversion model-id))
        j (response2json r)
        ]
    j
    )
)




;; get info for model - standalone

(let [market-id "de_AT"
      model-id "2132051_AT1"
      dataversion (get-dataversion market-id)]
  (get-model market-id dataversion model-id)
  )





(defn str-pad [s n]
  (str
    s
    (clojure.string/join "" (vec (repeat (- n (count s)) " ")))))


; (defn padding-right [s width pad]
;   (apply str (take width (concat s (repeat pad)))))




(let [{{market :market
        cars :collection} :de_AT} (get-models "AT")

      formatted-models-list
      (apply str
             (map (fn [{b :baumuster
                        nst :nationalSalesType
                        n :name}]
                    (str (str-pad n 50) "baumuster: " b "\tnst: " (d/pretty nst))
                    ) cars))

      ]


    formatted-models-list

    ;(spit "/Users/ndrw/w/wltp/AT/models.txt")

  )


;; todo: get emission for semi-integrated scenario



)

