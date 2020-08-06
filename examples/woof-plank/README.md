### `woof-plank`

Example of running woof worklow via [planck REPL](https://planck-repl.org/).

* one way is using `plk` with deps.edn file
or 
* specifying woof dependencies implicitely (as for lumo) via

    `planck -c "src:<your-path-to-woof>/woof-core/src:<your-path-to-.m2>/repository/andare/andare/1.1.587/andare-1.1.587.jar"`

### Example you can type in REPL

    cljs.user=> (require '[woof.base :as base])
                (base/rand-sid) ;; :cljs.user/7abfdb75-375c-4799-9ec4-77673e7e57b3