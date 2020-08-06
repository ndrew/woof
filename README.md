## `WOOF`

Woof is a software experiment exploring different ideas about building software from re-usable blocks.

Woof tries to bring the application programming back and allow people to solve their specific problems by creating specialized applications for themselves powered by woof workflows.

### Rationale

Software/computation is built from existing blocks — functions/objects/libraries, etc., but only software developers can combine these blocks into a working piece of software.

What if there could be a better building blocks for making software? 

My research hypothesis is — if we organize computation into simple, yet compossible blocks, will it still be possible to write decent programs by combining these?

The idea is that finite/infinite computations can be organized via **flat workflows**

TODO: *write about woof workflows in detail* .

### Implementation

Woof is a working prototype that implements flat workflow in clojure/clojurescript. 

Woof consists of **[core library](woof-core/)** for defining *flat workflows* and **[prototype projects](#protypes)** that explore how workflows can be used .

Check more details about development progress in [dev-log.md](dev-log.md).

#### Prototypes

##### [woof-playground](woof-playground/)

Sandbox for running/defying woof workflows. 

Idea is to explore possibilities of using a generic UI for presenting workflow results/definining workflows/etc. 


##### [woof-blog](woof-blog/)

static site generator example


### License

Copyright © 2017-2020 Andrij Sernyak.

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.