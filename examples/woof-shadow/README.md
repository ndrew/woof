### `woof-shadow`

Example of running woof workflow with [shadow-cljs](https://shadow-cljs.org/)

* for some reason latest shadow-cljs version 2.8.95 causes error, so using 2.8.94 instead

Workflow is defined in `woof.app` namespace

### Running

perform JS voodo crap

    npm install
    yarn

compile frontend/node env via

    yarn watch    

run node server (in separate shell) 

    node target/node.js

* check `http://localhost:3000/` for workflow running in node js environment
* check `http://localhost:8020/` for workflow running in browser (open dev tools)
