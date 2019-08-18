# woof dev log

--- 
## 2019

## August, 2019

prototyping workflow powered ui 

## June, 2019

working on ui prototype

Implementing workflow mgmt on the server side 

## May, 2019

simplyfing workflow definition and implementation.
IN-OUT wfs.

Composing init-fns, steps-fn, context-fn, etc.

## March, 2019
Split woof into core lib + playground


## 2018
--- 



## 2017
---



## May, 2017

codename: gendarme (as ordnung muss sein)

Initial idea, generic static blog engine (Setup-Content-Render-Deploy-Promote)

*edn editor*
a way to enter data into system, as edn

*rules*
idea was to use hiccup-style dsl for describing workflows, like

[:action1 {:a 1}
    [:sub-action1 {:sa 1}]
    [:sub-action2 {:sa 2}
        [:sub-sub-action1 ]
    ]
]

such tree can be traversed in depth/breadth so we can merge attrs top-down (from parent action into sub-actions) and merge results bottom-down (bubbling results up)

_idea:_ is to represent a computation as a data structure
so data can manipulated/updated via some kind of ui

so instead of having a parametrizable algorithm, you can build algorithm from blocks

*execution plan*
prior to actual execution of the actions, we'd build execution plan, which will be easier to execute.
Also, this a way to avoid side-effects, like the compile/run stages


*substitutions in hash-map*


Idea is to have a pipeline as a 2 way process: 
first stage is compilation, next - execution.


## June, 2017

using fipp for traversing tree like data, so the ui editor for map can be implemented


## August, 2017

*pipeline* 
as an data-structure for representing computation
pipeline of actions ([:action attrs ...])

*substitutions*
way of having placeholder in the hash-map which will be substituted with data later - again compile/runtime stuff

idea was to use placeholders for action attributes.


## Dec, 2017 

renamed project to woof.
The idea converged to "reusable workflow implementation in clojure/clojurescript."

>> The idea is to use have a 'unit of work' implemented as a very specific, yet generalizable implementation of producer/consumer problem.

*flat workwlow*

>> Flat workflow is a flat (no nested keys) map (possibly ordered) with finite numbers of steps, where each step has a unique id (step-id) and a certain function associated with it - step handler.


## Dec, 2017

Infinite workflows

...


UI prototypes.
Idea to work with maps with placeholders as main data type. 



