## `WOOF Playground`

Sandbox for exploring and working on potential usages of [woof](https://github.com/ndrew/woof).
 
Currently focusing on certain topics:
* workflow tooling
    * defining/running/maintaining workflows 
    * re-loadable workflows - having similar experience to figwheel, but with workflows
    * UI visual workflow representation
* using workflows for certain domain problems
    * in-browser scraping
    * web & publishing
 
---

#### Running via Clojure CLI tools and Figwheel

Note: use [Clojure CLI tools version 1.10.1.697](https://clojure.org/guides/getting_started) or above


### WIP: Playground

* idea: some kind of IDE can be used for creating/executing/maintaining workflows. 

Allows to run workflows with react UI and server backend (someday)

    clojure -M:fig:playground


### WIP: scraper & playground 

* idea: in-browser scraping via workflows 

    clojure -M:fig:scraper 


##### In page running: 

build the js for browser

    clojure -M:fig:browser-min

inject the script on the web page

    (function() { var $script = document.createElement('script'); $script.setAttribute("type","text/javascript"); $script.setAttribute("src", "http://localhost:9500/cljs-out/browser-main.js"); document.body.appendChild($script); })()

run in dev tools 

    woof.browser.run_workflow()

or use [chrome extension](https://github.com/ndrew/woof/tree/master/woof-chrome) for loading workflow automatically


##### Auto building the js

see [build-scraper-wf.sh](https://github.com/ndrew/woof/tree/master/woof-playground/scripts/build-scraper-wf.sh)


### WIP: server workflow

start a server as. For now — use REPL

    clojure -A:server 


#### REPL

launch server workflows from REPL

    Loading src/woof/server/scraper.clj... done

    (in-ns 'woof.server.scraper)
    
    (def wf-instance (scraper-wf!))

    ; start wf 
    ((:start-wf! wf-instance))

    ; stop wf
    ((:start-wf! wf-instance))

### Figwheel Automatic tests

http://localhost:9500/figwheel-extra-main/auto-testing


## Current status

Very alpha, use with caution.



