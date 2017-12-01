# woof

reusable workflow implementation in clojure/clojurescript.

## Overview

The idea is to use have a 'unit of work' implemented as a very specific, yet generalizable implementation of producer/consumer problem.

The goal of woof is to test the hypothesis that finite computations ('work', you know) can be organized via *flat workflows*.

## Flat workflows

### Theory

Flat workflow is a flat (no nested keys) map (possibly ordered) with finite numbers of steps, where each step has a unique id (step-id) and a certain function associated with it - step handler.

Step handler is a 

    (fn [param]), where param can an step-id or any other data

There can be certain kinds of step handlers

* compute: 

does some work and return value (sync/async) based upon param passed. These will be stored in workflow under step-id. 

'Compute' steps can be run parallel and in any particular order.

* expand: 
 
adds new steps to the workflow. 

It's the way to 'flatten' the collection/seq passed as param, so the processing will done always on colleaction items, not collection itself — think of this as loop

* compose: 

If param is a step-id, then workflow will wait until that step is processed and pass the result as a param.

This is similar to function composition, the types should match. Or the adapter step should be added.

'Compose' steps will run in certain order.

Flat workflow can be executed and will produce the flattened map with step-handler results under corresponding step-id.

### Hammock stuff

Flat workflows may be described in a declarative way via hiccup-like DSL. 

Also flat workflows may be configured via generic ui. 

Step handler type check may be done via spec. 



### Implementation

the workflow is represented as 

    {
        ::step-id [:handler-id param]
        ..
        ::step-id [:handler-id-N param]
    }

where step handlers live in execution context, like 

    {
        :handler-id {
            ::fn (fn [param] 42)
            ; and options here like if it's expands, caches etc.
        }

    }

These can be executed via executor protocol. 

Details later.


## Setup

WIP: use at your own risk. 

For now use 

    lein test

or

    lein figwheel

## License

Copyright © 2017 Andrij Sernyak.

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
