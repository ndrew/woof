(ns woof.app
  (:require
    [rum.core :as rum]

    [woof.ui.wf-runner :as runner]))


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

      <app> runner/<app> ;; (fn [] .. a rum component .. )
      init! runner/init! ;; (fn [mount-fn] .. initializer - call mount-fn to rebuild ui.. )

      mount-app #(rum/mount (<app-ui> <app>) el)]

  (init! mount-app)

  (defn on-js-reload []
    (mount-app)) ;; re-mount app on js reload

)


