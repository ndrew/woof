(ns ^:figwheel-hooks
    ^:figwheel-always
  woof.playground
  (:require
    [rum.core :as rum]

    ;; testing alpha
    [woof.alpha.playground :as playground]

    [woof.lib :as lib]
    ))


(enable-console-print!)


;;
;; root ui component

(rum/defcs <app-ui>
  < rum/reactive [local <ui>]

  [:div#app
   (<ui>)

   ;; test that woof lib is working
   ; [:hr]
   ; (lib/foo)
   ]
)



;; for now use js variables on pages

(when (goog.object/get js/window "PLAYGROUND")
  ;; try to init playground
  (let [el (. js/document (getElementById "app"))

        <app> playground/<app> ;; (fn [] .. a rum component .. )
        init! playground/init! ;; (fn [mount-fn] .. initializer - call mount-fn to rebuild ui.. )
        reload! playground/reload!

        mount-app #(rum/mount (<app-ui> <app>) el)]

    (init! mount-app)

    ;; will this be working?
    (defn ^:after-load on-js-reload []
      (reload!)
      (mount-app)
      ) ;; re-mount app on js reload
    )

  )






