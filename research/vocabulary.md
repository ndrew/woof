# common vocabulary (WIP)

terminology used in woof


## context
### step handler
## steps
### steps map
### steps tree
### steps model

## workflow
### executor
#### executor model

### results processor












* placeholder
* selector
* pipeline
* action
* execution plan


## Data

`selector` ...
`placeholder` substitution


## Actions (plugins?)

`Actions` are like plugins, they are executed as ((f cfg) params)
`Actions` are represented as hiccup-style tripple (like [:action-id cfg params])
Action config is a "compilation" step data needed to initialize action.
Action params are the data passed into action during execution ("runtime").

Example:
    ;; action-id       cfg           params
       [:debug     {:to :console}  "DEBUG ME"]


## Pipeline (workflow?)
`Pipeline` represents an absraction for computation.
`Pipeline` is an tree (or a list) of actions.

Example:
  ;; TODO

Pipelines can be executed both asynchronously and synchronously.
Pipelines are designed to allow four-step execution (which can be parametrized), so

1) the tree is traversed from the root to the leafs (to pass configs from the topmost action to the inner ones)

2) actions are stored in resulting object (so-called `execution plan`) in order of the tree traversal.
It should be a side-effect free list of actions in order they should be executed.
<?> can it be async?

3) actions are executed from the plan with the necessary atts and params

4) after all actions had been sucessfully executed — the results are merged.

TODO: selectors

