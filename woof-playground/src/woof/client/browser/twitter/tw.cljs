(ns woof.client.browser.twitter.tw
  (:require

    [woof.base :as base]
    [woof.client.dom :as woof-dom]
    [woof.client.dbg :as dbg :refer [__log]]
    [woof.data :as d]
    [woof.utils :as u]

    [goog.dom :as dom]
    [goog.string :as gstr]
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


;; first idea - is to register in-view library and monitor when elements matching selector will get into view
;; but twitter does have dynamic css selectors - so the idea is to figure out dynamic selector for a tweet

;; we start from first tweets that are under
;; div[aria-label=\"Timeline: '(Andrew Sernyak)’s liked Tweets\"] > div > div

;; css-... classes seems to be too generic


;; second idea - all tweets are article elements, but inview does not capture them all, so we may try to emulate scroll
;; and to iterate through all article elements manually

(defn load-in-view-library [url]
  (if-not (. js/window -inView)
    ;; for now just inline the library
    ;; todo: why loading library via woof-dom/add-script is not working?
    (do
      (js* "!function(t,e){\"object\"==typeof exports&&\"object\"==typeof module?module.exports=e():\"function\"==typeof define&&define.amd?define([],e):\"object\"==typeof exports?exports.inView=e():t.inView=e()}(this,function(){return function(t){function e(r){if(n[r])return n[r].exports;var i=n[r]={exports:{},id:r,loaded:!1};return t[r].call(i.exports,i,i.exports,e),i.loaded=!0,i.exports}var n={};return e.m=t,e.c=n,e.p=\"\",e(0)}([function(t,e,n){\"use strict\";function r(t){return t&&t.__esModule?t:{\"default\":t}}var i=n(2),o=r(i);t.exports=o[\"default\"]},function(t,e){function n(t){var e=typeof t;return null!=t&&(\"object\"==e||\"function\"==e)}t.exports=n},function(t,e,n){\"use strict\";function r(t){return t&&t.__esModule?t:{\"default\":t}}Object.defineProperty(e,\"__esModule\",{value:!0});var i=n(9),o=r(i),u=n(3),f=r(u),s=n(4),c=function(){if(\"undefined\"!=typeof window){var t=100,e=[\"scroll\",\"resize\",\"load\"],n={history:[]},r={offset:{},threshold:0,test:s.inViewport},i=(0,o[\"default\"])(function(){n.history.forEach(function(t){n[t].check()})},t);e.forEach(function(t){return addEventListener(t,i)}),window.MutationObserver&&addEventListener(\"DOMContentLoaded\",function(){new MutationObserver(i).observe(document.body,{attributes:!0,childList:!0,subtree:!0})});var u=function(t){if(\"string\"==typeof t){var e=[].slice.call(document.querySelectorAll(t));return n.history.indexOf(t)>-1?n[t].elements=e:(n[t]=(0,f[\"default\"])(e,r),n.history.push(t)),n[t]}};return u.offset=function(t){if(void 0===t)return r.offset;var e=function(t){return\"number\"==typeof t};return[\"top\",\"right\",\"bottom\",\"left\"].forEach(e(t)?function(e){return r.offset[e]=t}:function(n){return e(t[n])?r.offset[n]=t[n]:null}),r.offset},u.threshold=function(t){return\"number\"==typeof t&&t>=0&&t<=1?r.threshold=t:r.threshold},u.test=function(t){return\"function\"==typeof t?r.test=t:r.test},u.is=function(t){return r.test(t,r)},u.offset(0),u}};e[\"default\"]=c()},function(t,e){\"use strict\";function n(t,e){if(!(t instanceof e))throw new TypeError(\"Cannot call a class as a function\")}Object.defineProperty(e,\"__esModule\",{value:!0});var r=function(){function t(t,e){for(var n=0;n<e.length;n++){var r=e[n];r.enumerable=r.enumerable||!1,r.configurable=!0,\"value\"in r&&(r.writable=!0),Object.defineProperty(t,r.key,r)}}return function(e,n,r){return n&&t(e.prototype,n),r&&t(e,r),e}}(),i=function(){function t(e,r){n(this,t),this.options=r,this.elements=e,this.current=[],this.handlers={enter:[],exit:[]},this.singles={enter:[],exit:[]}}return r(t,[{key:\"check\",value:function(){var t=this;return this.elements.forEach(function(e){var n=t.options.test(e,t.options),r=t.current.indexOf(e),i=r>-1,o=n&&!i,u=!n&&i;o&&(t.current.push(e),t.emit(\"enter\",e)),u&&(t.current.splice(r,1),t.emit(\"exit\",e))}),this}},{key:\"on\",value:function(t,e){return this.handlers[t].push(e),this}},{key:\"once\",value:function(t,e){return this.singles[t].unshift(e),this}},{key:\"emit\",value:function(t,e){for(;this.singles[t].length;)this.singles[t].pop()(e);for(var n=this.handlers[t].length;--n>-1;)this.handlers[t][n](e);return this}}]),t}();e[\"default\"]=function(t,e){return new i(t,e)}},function(t,e){\"use strict\";function n(t,e){var n=t.getBoundingClientRect(),r=n.top,i=n.right,o=n.bottom,u=n.left,f=n.width,s=n.height,c={t:o,r:window.innerWidth-u,b:window.innerHeight-r,l:i},a={x:e.threshold*f,y:e.threshold*s};return c.t>e.offset.top+a.y&&c.r>e.offset.right+a.x&&c.b>e.offset.bottom+a.y&&c.l>e.offset.left+a.x}Object.defineProperty(e,\"__esModule\",{value:!0}),e.inViewport=n},function(t,e){(function(e){var n=\"object\"==typeof e&&e&&e.Object===Object&&e;t.exports=n}).call(e,function(){return this}())},function(t,e,n){var r=n(5),i=\"object\"==typeof self&&self&&self.Object===Object&&self,o=r||i||Function(\"return this\")();t.exports=o},function(t,e,n){function r(t,e,n){function r(e){var n=x,r=m;return x=m=void 0,E=e,w=t.apply(r,n)}function a(t){return E=t,j=setTimeout(h,e),M?r(t):w}function l(t){var n=t-O,r=t-E,i=e-n;return _?c(i,g-r):i}function d(t){var n=t-O,r=t-E;return void 0===O||n>=e||n<0||_&&r>=g}function h(){var t=o();return d(t)?p(t):void(j=setTimeout(h,l(t)))}function p(t){return j=void 0,T&&x?r(t):(x=m=void 0,w)}function v(){void 0!==j&&clearTimeout(j),E=0,x=O=m=j=void 0}function y(){return void 0===j?w:p(o())}function b(){var t=o(),n=d(t);if(x=arguments,m=this,O=t,n){if(void 0===j)return a(O);if(_)return j=setTimeout(h,e),r(O)}return void 0===j&&(j=setTimeout(h,e)),w}var x,m,g,w,j,O,E=0,M=!1,_=!1,T=!0;if(\"function\"!=typeof t)throw new TypeError(f);return e=u(e)||0,i(n)&&(M=!!n.leading,_=\"maxWait\"in n,g=_?s(u(n.maxWait)||0,e):g,T=\"trailing\"in n?!!n.trailing:T),b.cancel=v,b.flush=y,b}var i=n(1),o=n(8),u=n(10),f=\"Expected a function\",s=Math.max,c=Math.min;t.exports=r},function(t,e,n){var r=n(6),i=function(){return r.Date.now()};t.exports=i},function(t,e,n){function r(t,e,n){var r=!0,f=!0;if(\"function\"!=typeof t)throw new TypeError(u);return o(n)&&(r=\"leading\"in n?!!n.leading:r,f=\"trailing\"in n?!!n.trailing:f),i(t,e,{leading:r,maxWait:e,trailing:f})}var i=n(7),o=n(1),u=\"Expected a function\";t.exports=r},function(t,e){function n(t){return t}t.exports=n}])});")
      (js* "window['INVIEW'] = inView;")
      )

    :loaded))


;; using external state with watcher is a bit overkill
(defn _guess-tweet-selector [*selectors el]
  (let [els (.-children el)]

    (if-let [c (aget els 0)]
      (let [all-classes (classes/get c)
            tweet-classes (filter #(not (str/starts-with? % "css-")) all-classes)]

        (swap! *selectors into
               (map #(str "." % " article") tweet-classes)))

      )
    )
  )


;;
;; parsing implementation


(defonce *SCRAPED-TWEETS (atom []))


;;
;; browser impl
(defn scrape-tweet [el]

  (.log js/console el)
  (.warn js/console (d/pretty! (woof-dom/dataset el)))

  (swap! *SCRAPED-TWEETS conj (. (. el -parentElement) -innerHTML))

  ;; todo: do parsing some day
  (classes/add el "parsed")


  (set! (-> el .-style .-outline) (str "1px dashed "
                                       (rand-nth ["red" "blue" "yellow" "orange"])
                                       ))

  ;(classes/swap el "parsed" "parsed-twice")


  #_(set! (. (. el -parentElement) -innerHTML)
          (rand-nth ["AAA" "BBB" "CCC" "DDD" "EEE"])
          )

  )





;;
;; inview


(defn _in-view [evt-loop [selector process-step]]
  ;; just to ensure that there are actually some elements with such selector
  (.log js/console "registering in-view"
        selector
        (.querySelectorAll js/document selector))

  (->
    (js/inView selector)

    (.on "enter" (fn [el]
                   ;; <?> is using dataset instead of attribute a good idea?
                   (if-let [id (dataset/get el "woof_id")]
                     (do
                       ;; skip already processed elements
                       )
                     (do
                       ;; set woof_id for element, so it won't be processed twice
                       (dataset/set el "woof_id" (base/rand-sid))
                       ;; emit scraping step
                       (async/put! evt-loop {
                                             ;; (base/rand-sid "scrape-") [:scrape el]
                                             (base/rand-sid "iv-") [process-step el]

                                             })
                       )
                     )

                   ))
    )
  )



;;
;; recurring brute force

(defn brute-force [[selector process-step]]

  ;; try to find elements to be processed
  (let [_els (array-seq (.querySelectorAll (.-body js/document) selector))
        ;; skip if they were alread processed
        els (filter (fn [el] (not (dataset/has el "woof_id"))) _els)]


    (if (empty? els) ;; all elements had been processed
      (let [
            k_selector (base/rand-sid)
            k_step (base/rand-sid)
            k_scroll-amount (base/rand-sid)
            k_scroll-wait-time (base/rand-sid)
            k_!selector (base/rand-sid)
            k4 (base/rand-sid)
            ]

        ;; no more els to scrape - scroll and brute force again
        {
         k_selector   [:v selector]
         k_step [:v process-step]

         k_scroll-amount   [:scroll (rand-nth [1 2 3])]
         k_scroll-wait-time   [:v (u/timeout 1000)]

         k_!selector   [:wait-rest [k_selector k_scroll-wait-time]]

         (base/rand-sid)   [:brute-force* [k_!selector k_step]]
         }
        )
      (do ;; there are elements to process further
        ;; (swap! *brute-force-counter inc)

        ;; parse each element
        (reduce (fn [a el]
                  ;; mark element as processed
                  (dataset/set el "woof_id" (base/rand-sid))

                  (assoc a (base/rand-sid "el-")
                           [process-step el])
                  ) {} els)
        )

      )

    )

  )


;;;;;;;;;;;;;;

(defn scrape-tweet-local [el]
  (let [[root] (woof-dom/query-selector* el "div > div > div > div > div:nth-of-type(2)")
          [user-block tweet-block] (array-seq (.-children root))]

      (.clear js/console)
      (.log js/console (. user-block -innerHTML))

      (dotimes [n 8]
        (let [selector (str "div" (apply str (repeat n " > div" )))
              $el (.querySelector user-block selector)
              ]
          (.log js/console
                selector
                (-> $el .-children .-length )
                $el
                )

          )


        )

      ;    (set! (. b -innerHTML) "BBB")
      ;   (.log js/console (. a -innerHTML))
      ;; copy to console
      ; (set! (-> a .-style .-backgroundColor) (rand-nth ["red" "coral" "yellow" "orange"]))
      {
       :HERE-WILL :BE-A-MODEL
       ;:user-block user-block
       })
  )


(defn parse-plan [nodes]
  (reduce
    (fn [s n]
      (let [$ (get n :$)
            curr-tag (:t (last $))
            selector (woof-dom/to-selector $)]
        (str s

             "<div class='selector-box'>"

             "<span class='selector'>"

             (gstr/htmlEscape selector)
             "</span>"

             (if (= "IMG" curr-tag)
               (str
                 "<img class='el-img' src='" (woof-dom/attr (:el n) "src") "'/>"
                 )
               "")

             (if (= "A" curr-tag)
               (str
                 "<div class='el-attr'>" (woof-dom/attr (:el n) "href") "</div>"
                 )
               "")


             (let [t (:text n)]
               (if (= "" t)
                 ""
                 (str
                   "<div class='el-value'>"
                   (gstr/htmlEscape t)
                   "</div>")
                 )
               )


             (if-let [data (woof-dom/dataset->clj (:el n))]
               (str "<div class='el-data'>" (pr-str data) "</div>")
               ""
               )


             "</div>"
             )
        )
      )
    "" nodes
    )
  )




(defn analyze-el [el]
  (.log js/console "parsing" el)

  (classes/add el "woof-el")

  (let [$details (dom/createDom "div" "woof-details" "")

        dom-map (woof-dom/el-map el (fn [$el $]
                                (#{"SVG" "G" "PATH"} (str/upper-case (.-tagName $el)))
                                ))

        ;;find-interesting (fn [x] true)
        find-interesting (fn [node]
                           (cond


                             ;; images
                             (= "IMG" (.-tagName (:el node))) true
                             ;; leafs
                             (= 0 (:child-count node)) true
                             ;; links
                             (= "A" (.-tagName (:el node))) true

                             (not= "" (:text node)) true

                             :else false
                             )
                           )

        interesting-nodes (filter find-interesting dom-map)

        dom-plan (parse-plan interesting-nodes)
        ]


   ;; (.log js/console dom-plan)

    (set! (. $details -innerHTML)

          (str
            dom-plan
            ;"<hr>"
            ;(d/pretty! interesting-nodes)
           )



            )
    ;; add
    (woof-dom/add-el! el ".woof-details" $details)


    interesting-nodes

    )






  #_(let [v (woof-dom/el-map el (fn [$el $]
                                  (#{"SVG" "G" "PATH"} (str/upper-case (.-tagName $el)))
                                  ))
          interesting-nodes (filter (fn [node]
                                      (let [no-text? (= "" (:text node))
                                            no-children? (< (:child-count node) 2)
                                            div? (= "DIV" (.-tagName (:el node)))
                                            ]
                                        #_(.log js/console no-text?
                                                no-children?
                                                div? node)
                                        (not
                                          (and
                                            no-text?
                                            no-children?
                                            div?
                                            ))
                                        )

                                      ) v)
          ]
      ;; (.log js/console v)

      (.log js/console  interesting-nodes)

      (doseq [{el :el
               $ :$
               cnt :child-count
               text :text} interesting-nodes]

        (when (>= cnt 2)
          (set! (-> el .-style .-margin) (str "15px !important"))
          (set! (-> el .-style .-backgroundColor) (str "rgba(128,0,128,.0333)"))
          )

        (when (not= "" text)
          (set! (-> el .-style .-padding) (str "25px !important"))
          (set! (-> el .-style .-backgroundColor) "yellow")
          (set! (-> el .-style .-color) "black")
          (set! (-> el .-style .-fontSize) "18pt"))


        (if (= "IMG" (last $))
          (.log js/console el)
          )

        (if (= "A" (last $))
          (.log js/console el)
          )



        ; (set! (-> el .-style .-outline) (str "1px solid " (rand-nth ["red" "blue" "yellow" "orange"])))

        )
      )

  ; (set! (-> el .-style .-padding) "20px")
  #_(set! (. el -innerHTML)
          (rand-nth ["AAA" "BBB" "CCC" "DDD" "EEE"]))


  )


;;
;; WF
;;

(defn css-cleaup-on-init [params]

  ;; remove previosly added styles sheets + added classes like woof-el
  (woof-dom/remove-added-css
    ["woof-el"]
    )


  ;; remove dynamically inserted debug nodes
  (doseq [$details (woof-dom/q* ".woof-details")]
    (dom/removeNode $details))


  {}
  )

(defn twitter-wf! [*wf-state meta-info]

  (let [*selectors (atom #{})
        WATCHER-ID :css

        *brute-force-counter (atom 0)
        ]
    {
     :init    [css-cleaup-on-init

               (fn [params] ;; watcher
                 (watcher/do-watcher-chan-init WATCHER-ID
                                               (base/make-chan (base/&chan-factory params) (base/rand-sid))
                                               *selectors params))]

     :ctx     [watcher/watcher-ctx

               (fn [params]
                 {
                  :tick       {:fn       (fn [[t max-num]]
                                           (let [chan-factory (base/&chan-factory params)
                                                 chan (base/make-chan chan-factory (base/rand-sid))]

                                             (async/go-loop [i 0]
                                               (async/>! chan (u/now))
                                               (async/<! (u/timeout t))

                                               (if (< i max-num)
                                                 (recur (inc i))))

                                             chan))
                               :infinite true
                               }

                  :rnd-scroll {:fn (fn [_]
                                     (rand-nth [1 2 3]))}

                  }
                 )

               (fn [params]
                 {
                  ;; load the in-view library
                  :load-in-view      {:fn load-in-view-library}

                  ;; as we can find by selector first tweets, we need to figure out their dynamically generated
                  ;; css classes, so we can register in-view listener for them
                  :find-tweet-css-id {:fn (partial _guess-tweet-selector *selectors)}

                  ;;
                  :scrape-tweet      {:fn scrape-tweet}


                  :analyze-dom       {:fn analyze-el}

                  :scrape-tweet!     {:fn scrape-tweet-local}

                  :brute-force*      {:fn       brute-force
                                      :expands? true
                                      :collect? true
                                      }
                  :in-view*          {:fn       (partial _in-view (evt-loop/&evt-loop params))
                                      :collect? true
                                      }
                  }
                 )

               ]
     :steps   [(fn [params]
                 (merge
                   {
                    ;; add temporary stylesheet - if localy, figwheel will re-load the styles automatically

                    :CSS/custom-styles [:css-file "http://localhost:9500/css/twitter.css"]
                    }

                   ;; local version, scrape just single tweet
                   {

                    ::selector         [:v "article"]
                    ;; query first tweet
                    ::el               [:query-selector ::selector]

                    ::log              [:log ::el]
                    ::scrape           [:analyze-dom ::el]

                    ::log-1            [:log ::scrape]

                    }

                   )

                 #_{

                  :CSS/custom-styles [:css-file "http://localhost:9500/css/twitter.css"]


                  ;; load in-view
                    :JS/load-in-view     [:load-in-view "http://localhost:9500/scraper/in-view.min.js"]

                    :JS/_scrape-selector [:v "div[aria-label=\"Timeline: '(Andrew Sernyak)’s liked Tweets\"] > div > div > div"]
                    :log/selector        [:log :JS/_scrape-selector]


                    :css/parsed          [:css-rule ".parsed { opacity: .6; }"]


                    :JS/scrape-selector  [:wait-rest [:JS/_scrape-selector :JS/load-in-view]]


                    ;;:JS/scrape [:in-view* [:JS/scrape-selector :PARSE/step]]
                    :PARSE/step          [:v :scrape-tweet]

                    ;; :parse/first-tweets [:v :find-tweet-css-id]
                    ;; :JS/register         [:in-view* [:JS/scrape-selector :parse/first-tweets]]


                    ;; todo: register in-view for scraping tweets here
                    :watch/-8            [:watch WATCHER-ID]
                    :log/log             [:prn :watch/-8]

                    }

                 ;; real twitter version
                 #_{

                    ;; for now limit the number of tweets to parse by having finite clock cycles
                    ::clock              [:tick [3000 1]]

                    ;;::scroller [:rnd-scroll ::clock]
                    ;; :infinite/scroll [:scroll ::scroller]

                    ::selector           [:v "article"]
                    ;; normal version
                    ;;::op-k [:v :scrape-tweet]
                    ;; debug version
                    ::op-k               [:v :scrape-tweet!]

                    ;; making a value recuring
                    ::_recuring-selector [:wait-rest [::selector
                                                      ::clock
                                                      ;;:infinite/scroll
                                                      ]]

                    ::_parse             [:brute-force* [::_recuring-selector ::op-k]]
                    }
                 )]
     :opts    [
               watcher/watcher-opts

               ]

     :api     (array-map


                "COPY TWEETS" (fn []
                                (let [tweets @*SCRAPED-TWEETS]
                                  (.log js/console (str/join tweets))
                                  )
                                )


                "in view scrape" (fn []
                                   (let [selector (js/prompt "Selector" "div[aria-label=\"Timeline: '(Andrew Sernyak)’s liked Tweets\"] > div > div > div")
                                         params (get @*wf-state :WF/params {})
                                         evt-loop (evt-loop/&evt-loop params)
                                         ]

                                     (async/put! evt-loop
                                                 {(base/rand-sid) [:in-view* [selector :scrape-tweet]]})
                                     ))

                "brute force" (fn []
                                (let [selector (js/prompt "Selector" "div[aria-label=\"Timeline: '(Andrew Sernyak)’s liked Tweets\"] > div > div > div")
                                      params (get @*wf-state :WF/params {})
                                      evt-loop (evt-loop/&evt-loop params)
                                      ]

                                  (async/put! evt-loop
                                              {(base/rand-sid) [:brute-force* [selector :scrape-tweet]]})
                                  )
                                )

                "reg scroll watcher"
                (fn []
                  (.log js/console (pr-str @*selectors))

                  (let [selectors @*selectors
                        params (get @*wf-state :WF/params {})
                        evt-loop (evt-loop/&evt-loop params)]

                    (async/put! evt-loop
                                (reduce (fn [a selector]
                                          (assoc a
                                            (base/rand-sid) [:in-view* [selector :scrape-tweet]])
                                          ) {} selectors))
                    )

                  )


                "scroll" (fn []
                           (let [params (get @*wf-state :WF/params {})
                                 evt-loop (evt-loop/&evt-loop params)]
                             (async/put! evt-loop {
                                                   (base/rand-sid) [:scroll 1]
                                                   })
                             )

                           )

                "request broad cast" (fn []
                                       #_(let [params (get @*wf-state :WF/params {})
                                               evt-loop (evt-loop/&evt-loop params)

                                               MSG (base/rand-sid)
                                               ]

                                           (async/put! evt-loop {
                                                                 MSG             [:v (ss/ask-for-update-msg)]
                                                                 (base/rand-sid) [:ws-send! [:ws/socket MSG]]

                                                                 })

                                           ;; (.log js/console @*wf-state)

                                           )
                                       )

                )

     :on-stop (fn [state]

                (__log "ON STOP")
                (.log js/console state)

                ;; can return channel
                )
     }
    )


  )