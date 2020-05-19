(ns blog.backend
  (:require
    [clojure.core.async :as async :refer [go go-loop]]
    [woof.base :as base]

    [blog.backend.logging :refer [init-logging!]]
    [taoensso.timbre :as timbre :refer [info]]

    [blog.backend.wf :as wf]
    [woof.utils :as u]))


;; Workflow that powers blog backend.
;;
;; Reloads automatically by figwheel if there were changes.

;; stateful workflow


;; hardcoded configuration of features

;; posts stored as markdown files with json front-matter on the filesystem
;; -> convert to common markdown with EDN front-matter
  ;; additional parse, but keeping metadata with data is better
;; <?> post assets?
;; templates are stored as .template files
;; -> convert to hiccup
  ;; <?> test how live reloading will work

;; http/ws server
;;   serving preview/editor
;;   exposing data to frontend
;;   watch for post changes
    ;; see woof.infinite-collect-test for watcher stuff
;;   assets management: storing images, scaffolding new post, etc.

;; ssr
  ;; html rendering via parametrized head block and markdown based rum component renderer

;; Blog editor (frontend) workflow
;; <?> initialized via global js vars?
;; * md based rum renderer via common mark - for editing
;; * real preview of generated file


;; todo: bi-directional editing


;; IMPL
;;

;;
;; state mgmt - how to reload properly a running workflow
;;


;; use 'global' state atom to survive REPL reloads
(defonce *STATE (atom {}))

(defonce *blog-wf (volatile! nil))
(defonce *updated (volatile! 0))

;;
;; repl helpers

;; start
(defn start-blog-wf []
  ((:start-wf! @*blog-wf)))

(defn stop-blog-wf []
  ((:stop-wf! @*blog-wf)))

;;
(defn run-blog-wf! []
  (vreset! *updated (u/now))
  (when-let [old-instance @*blog-wf]
    (info "[Backend]  Stopping WF")
    ((:stop-wf! old-instance)))

  (info "[Backend]  Initializing WF")
  (vreset! *blog-wf (wf/init-blog-backend-wf *STATE))

  (info "[Backend]  Starting WF")
  ((:start-wf! @*blog-wf))
  )






;; reloadable version
;;

(defonce INITIALIZATION-BLOCK
         (do
           (init-logging!)

           (info ::blog-backend-first-start)
           ))


(run-blog-wf!)


(defn -post-build-hook [& args]
  (let [last-upd (- (u/now) @*updated)]
    (info "RELOAD happened " last-upd (> last-upd 1000))
    (when (> last-upd 1000)
      (run-blog-wf!)
      )
    )

  )

