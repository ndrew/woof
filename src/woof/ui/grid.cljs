(ns woof.ui.grid
  (:require
    [cljs.core.async :as async]
    [cljs.test :as test :refer-macros [deftest is testing run-tests async]]

    [goog.string :as gstring]
    [goog.string.format]

    [rum.core :as rum]

    [woof.data :as d]
    [woof.graph :as g]
    [woof.wf :as wf]
    [woof.ws :as ws]

    [woof.ui :as ui]
    [woof.wf-ui :as wf-ui]
    [woof.utils :as u]

    [woof.test-data :as test-data]

    ; [woof.wf-tester-ui :as tester-ui]
    )


  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))


(defonce GRID-SIZE 20)


(rum/defc <node> [node]
  (comment
  ;       <g class='node ${node.is_mesh ? 'mesh' : ''}' id='node_${node.id}'>
  ;      <rect rx='2' ry='2' x=${rect.x} y=${rect.y-(GRID_SIZE/2)} width="${rect.w}" height="${rect.h}" class='${node.children.length == 0 ? "fill" : ""}'/>
  ;      <text x="${rect.x+(rect.w/2)}" y="${rect.y+rect.h+(GRID_SIZE/2)}">${node.label}</text>
  ;      ${draw_ports(node)}
  ;      ${draw_glyph(node)}
  ;    </g>

    )
  (let [{_x :x
         _y :y
         _h :h
         _w :w } (:rect node)

        hg (/ GRID-SIZE 2)
        {x :x
         y :y
         h :h
         w :w } {:x (* GRID-SIZE _x)
              :y (* GRID-SIZE _y)
              :h (* GRID-SIZE _h)
              :w (* GRID-SIZE _w)
              }

        ]

    [:g {:class "node"
         :id (str "node_" (:id node))}

     [:rect {:rx 2 :ry 2
             :x x
             :y (- y hg)
             :width w
             :height h

             :class "fill" ;;
             }

      [:text {:x (+ x
                    (/ w 2)) ; rect.x+(rect.w/2)
              :y (- y hg)
              } (:label node)]

      ;; draw_ports
      ;; draw_glyph
      ]

     ]

    )

  )

(rum/defcs <graph> < rum/reactive
  [local]

  (let [
        ;v 1234
        network {:node {:id "node"
                        :label "Node"
                        :x 2 :y 2
                        :rect {
                                :x 2
                                :y 4
                                :h 2
                                :w 2
                                }
                        }
                  }
        ]
  [:svg
   {:xmlns "http://www.w3.org/2000/svg" :baseProfile "full" :version="1.1"}
     ;[:circle {:cx 10 :cy 10 :r 5 :fill "#ccc"}]
     (<node> (:node network))

   ]
    )

  )
