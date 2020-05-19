(ns blog.core.templates
  (:require
    [clojure.edn :as edn]

    [woof.data :as d]

    [hiccup.core :as hiccup]
    [rum.core :as rum])
  (:import
    (org.commonmark.parser Parser)
    (org.commonmark.node FencedCodeBlock)
    (org.commonmark.renderer.html HtmlRenderer))
  )


;; for now use single instance of the parser
(defonce parser (.build (Parser/builder)))
(defonce renderer (.build (HtmlRenderer/builder)))


;; https://spec.commonmark.org/dingus/


;; index page, converted from old sernyak.com template

(defn fake-index-template [data]

  [:html
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:name "author" :content "Andrew Sernyak"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]

    [:title "Andrew Sernyak Blog"]

    [:link {:rel "stylesheet" :href "/css.css"}]
    [:link {:rel "shortcut icon" :sizes "16x16 24x24 32x32 48x48 72x72" :href "/favicon.ico"}]

    [:link {:rel "alternate" :type "application/rss+xml" :title "RSS" :href "atom.xml"}]
    [:link {:rel "image_src" :href "../logo.png"}]

    [:script "var BLOG = 'index';"]
    [:link {:rel "stylesheet" :href "http://localhost:9500/css/base.css"}]
    [:script {:src "http://localhost:9500/cljs-out/blog-main.js"}]

    ]
   [:body
    "HELLO, I AM SERVER RENDERED PAGE."
    [:div.blog-dashboard

     ]
    ]
   ]
  )

(defn blog-index-template [posts]
  [:html
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:name "author" :content "Andrew Sernyak"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]

    [:title "Andrew Sernyak Blog"]

    [:link {:rel "stylesheet" :href "/css.css"}]
    [:link {:rel "shortcut icon" :sizes "16x16 24x24 32x32 48x48 72x72" :href "/favicon.ico"}]

    [:link {:rel "alternate" :type "application/rss+xml" :title "RSS" :href "atom.xml"}]
    [:link {:rel "image_src" :href "../logo.png"}]

    #_[:script "var _gaq = [['_setAccount', 'UA-18813210-3'], ['_trackPageview']];
     (function(d, t) {
      var g = d.createElement(t),
          s = d.getElementsByTagName(t)[0];
      g.async = true;
      g.src = '//www.google-analytics.com/ga.js';
      s.parentNode.insertBefore(g, s);
     })(document, 'script');"]
    ]
   [:body
    [:nav
     [:section
      [:ul
       [:li [:a {:href "atom.xml"} "RSS"]]]]]

    [:header [:pre "                   /\\\\\\\n                   \\/\\\\\\\n                    \\/\\\\\\\n /\\\\/\\\\\\\\\\\\          \\/\\\\\\   /\\\\/\\\\\\\\\\\\\\   /\\\\    /\\\\   /\\\\\n \\/\\\\\\////\\\\\\    /\\\\\\\\\\\\\\\\\\  \\/\\\\\\/////\\\\\\ \\/\\\\\\  /\\\\\\\\ /\\\\\\\n  \\/\\\\\\  \\//\\\\\\  /\\\\\\////\\\\\\  \\/\\\\\\   \\///  \\//\\\\\\/\\\\\\\\\\/\\\\\\\n   \\/\\\\\\   \\/\\\\\\ \\/\\\\\\  \\/\\\\\\  \\/\\\\\\          \\//\\\\\\\\\\/\\\\\\\\\\\n    \\/\\\\\\   \\/\\\\\\ \\//\\\\\\\\\\\\\\/\\\\ \\/\\\\\\           \\//\\\\\\\\//\\\\\\\n     \\///    \\///   \\///////\\//  \\///             \\///  \\///\n"]]


    (map (fn [post]
           [:pre
            (d/pretty (:meta post))]
           )
         posts
         )

    [:ul
     (map (fn [post]
            [:li [:article
                  [:header
                   [:a {:href (str "posts/" (get-in post [:index :name] ) "htm")} (get-in post [:meta :Title])]
                   ]
                  [:p.summary "— " (get-in post [:meta :Summary])]]]
            )

          posts)

     ]
    [:footer
     [:section "&copy;" [:b "A. Sernyak"] "— all rights reserved. 2012-2020"]
     [:section]]]]
  )





(defn post-md->html [md]

  (let [tree (.parse parser md)

        ;; unlinks first clojure
        extract-meta! (fn [doc]
          (let [node (.getFirstChild doc)]

            #_(when (instance? FencedCodeBlock node)
              (prn (.getInfo node)))


            (when (and (instance? FencedCodeBlock node)
                       (= (.getInfo node) "blog-post"))
              (.unlink node)
              (edn/read-string (.getLiteral node)))))

        meta (extract-meta! tree)
        ]
    ;; todo: does commonmark have lazy parsing?
    ;; todo: reference rum component to be rendered as markdown block

    ;; todo: comparing trees if md changes? sending these to js?

    ;; woof code block with workflow edn

    ;;
    ;; (prn "extracted" meta)

    (.render renderer tree)
    )

  )



(defn post-body [post]

  (let [md (:contents post)]
    [:article#backend-md {:dangerouslySetInnerHTML {:__html (post-md->html md)}}]
    )

  )


(defn post-head [post]
  [:head

   ;; figwheel
   ;;
   ;; use explicit global variable in order not to confuse figwheel reloading in playground/browser
   [:script "var BLOG = 'post';"]
   [:link {:rel "stylesheet" :href "http://localhost:9500/css/base.css"}]
   [:script {:src "http://localhost:9500/cljs-out/blog-main.js"}]


   ;;

   [:meta {:charset "utf-8"}]

   [:meta {:name "author" :content "Andrew Sernyak"}]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]

   [:title (str (get-in post [:meta :Title]) ":: ndrw ze destruktor")]


   [:link {:rel "alternate" :type "application/rss+xml" :title "RSS" :href "../atom.xml"}]
   [:link {:rel "image_src" :href "../../logo.png"}]


   ;; insert figwheel dependency in head (as it uses document write)



;;   [:script {:src "/cljs-out/blog-main.js"}]

   ;; ga
   #_[:script "var _gaq = [['_setAccount', 'UA-18813210-3'], ['_trackPageview']];
     (function(d, t) {
      var g = d.createElement(t),
          s = d.getElementsByTagName(t)[0];
      g.async = true;
      g.src = '//www.google-analytics.com/ga.js';
      s.parentNode.insertBefore(g, s);
     })(document, 'script');"]]
  )


;; templating stuff. post -> html, generating index -> html
(defn templating-ctx-fn [params]
  ;; should be configured somehow?
  {

   :fake-index-page {:fn (fn [data]
                           (hiccup/html
                             (fake-index-template data))
                           )}

   :blog-index-page {:fn (fn [posts]
                           (hiccup/html
                             (blog-index-template posts)))}


   :post-page       {:fn (fn [post]
                           (str
                             "<!DOCTYPE html><html>"
                             (hiccup/html
                               (post-head post))

                             "<body>"
                             ;; render single
                             (rum/render-static-markup
                               (post-body post))

                             "</body>"
                             )
                           )}
   }
  )


(defn templating-impl []
  {:ctx templating-ctx-fn}
  )