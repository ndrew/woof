(ns ^:figwheel-hooks
    ^:figwheel-always
  woof.playground
  (:require
    [rum.core :as rum]

    ;; testing alpha
    [woof.client.playground.core :as playground]
    ;; [woof.client.playground.minimal :as playground]
    ))


(enable-console-print!)


;; root ui component

(rum/defcs <app-ui>
  < rum/reactive [local <ui>]

  [:div#app (<ui>)]
)



;; for now use js variables on pages

(when (goog.object/get js/window "PLAYGROUND")
  ;; try to init playground
  (let [el (. js/document (getElementById "app"))

        <app> playground/<app> ;; (fn [] .. a rum component .. )
        init! playground/init! ;; (fn [mount-fn] .. initializer - call mount-fn to rebuild ui.. )
        ;; reload! playground/reload!

        mount-app #(rum/mount (<app-ui> <app>) el)]

    (init! mount-app)

    )
  )

;; will this be working?
(defn ^:after-load on-js-reload []
  (when (goog.object/get js/window "PLAYGROUND")
    (playground/reload!)
    (rum/mount (<app-ui> playground/<app>) (. js/document (getElementById "app")))
    )
  )






