## `WOOF Playground`

# playground for woof

sandbox for running woof workflows

 

## playground

Allows run workflows with react UI and server backend

    clojure -A:fig:playground

## Browser playground

copy the contents of the webpage to resources/public/browser.html

    clojure -A:fig:browser 

### In page running: 

build the js for browser

    clojure -A:fig:browser-min

inject the script on the web page

    (function() { var $script = document.createElement('script'); $script.setAttribute("type","text/javascript"); $script.setAttribute("src", "http://localhost:9500/cljs-out/browser-main.js"); document.body.appendChild($script); })()

run in dev tools 

    woof.browser.run_workflow()

### server workflow

WIP: start a server as. For now — use REPL

    clojure -A:server 


### REPL

launch server workflows from REPL

    Loading src/woof/server/scraper.clj... done

    (in-ns 'woof.server.scraper)
    
    (def wf-instance (scraper-wf!))

    ; start wf 
    ((:start-wf! wf-instance))

    ; stop wf
    ((:start-wf! wf-instance))

## tests

http://localhost:9500/figwheel-extra-main/auto-testing

## devcards 

http://localhost:9500/figwheel-extra-main/devcards


