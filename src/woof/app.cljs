(ns ^:figwheel-hooks
    ^:figwheel-always
  woof.app
  (:require
    [rum.core :as rum]

    [woof.ui.playground.core :as playground]))


(enable-console-print!)


;;
;; root ui component

(rum/defcs <app-ui>
  < rum/reactive [local <ui>]

  [:div#app (<ui>)]
)



;;
;; mount the application

(let [el (. js/document (getElementById "app"))

      <app> playground/<app> ;; (fn [] .. a rum component .. )
      init! playground/init! ;; (fn [mount-fn] .. initializer - call mount-fn to rebuild ui.. )

      mount-app #(rum/mount (<app-ui> <app>) el)]

  (init! mount-app)

  (defn ^:after-load on-js-reload []
    (playground/reload!)
    (mount-app)) ;; re-mount app on js reload

)


