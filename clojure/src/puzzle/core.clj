;; core.clj -- puzzle.core
;; Solves a scramble-squares puzzle using ~idiomatic~ hacked together Clojure
;; Copyright (c) 2020, Kevin Kredit
;; License MIT

(ns puzzle.core
  (:gen-class))

(defn relativePiece
  [pred offset b index]
  (cond
    (pred index) nil
    (> index offset) nil
    :else (nth b (- index offset))
    )
  )

(defn boardIsLegal
  [board]
  (cond
    (> 2 (count board)) true
    :else
      (let [pos (- (count board) 1)
            topRow (< pos 3)
            leftCol (= 0 (mod pos 3))
            this (last board)
            above (fn [piece] (relativePiece #(> 3 %) 3 board piece))
            leftTo (fn [piece] (relativePiece #((= 0 (mod % 3)) 1 board piece)))
            edgesMatch #(and (= (:insect %1) (:insect %2)) (not= (:end %1) (:end %2)))
            matchesAbove #(edgesMatch (:top this) (:bottom (above this)))
            matchesLeft #(edgesMatch (:left this) (:right (leftTo this)))
          ]
        (and (or topRow matchesAbove) (or leftCol matchesLeft))
        )
  ))

(defn nextStates
  [state]
  state
  )

(defn solutions
  [state]
  (case state
    (_, []) (:placed state)
    (do
      (let [nextLegalStates (filter #(boardIsLegal (:placed %)) (nextStates state))]
        (reduce conj (map solutions nextLegalStates))
        ))
  ))

(defn makePiece
  [n i1 e1 i2 e2 i3 e3 i4 e4]
  (let [e #(do {:insect %1 :end %2})]
    {:name n, :rot 0, :top (e i1 e1), :right (e i2 e2), :bottom (e i3 e3), :left (e i4 e4)}
    )
  )

(defn -main
  "Solve a scramble squares puzzle."
  []
  (println "Working on it!")
  (let [initState {:placed [], :unplaced [
    (makePiece 0 'dragonfly 'tail, 'ant 'head, 'beetle 'tail, 'mantis 'head)
    (makePiece 1 'dragonfly 'tail, 'ant 'tail, 'beetle 'head, 'mantis 'tail)
    (makePiece 2 'dragonfly 'tail, 'mantis 'head, 'beetle 'tail, 'ant 'head)
    (makePiece 3 'dragonfly 'tail, 'ant 'head, 'mantis 'head, 'ant 'tail)
    (makePiece 4 'dragonfly 'tail, 'ant 'head, 'beetle 'head, 'mantis 'head)
    (makePiece 5 'dragonfly 'head, 'beetle 'tail, 'mantis 'head, 'ant 'tail)
    (makePiece 6 'dragonfly 'head, 'mantis 'tail, 'beetle 'head, 'ant 'tail)
    (makePiece 7 'dragonfly 'head, 'mantis 'head, 'beetle 'head, 'dragonfly 'tail)
    (makePiece 8 'beetle 'tail, 'mantis 'tail, 'ant 'head, 'beetle 'head)
    ]}]
    (println (solutions initState))))
