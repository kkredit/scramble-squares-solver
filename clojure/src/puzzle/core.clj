;; core.clj -- puzzle.core
;; Solves a scramble-squares puzzle using ~idiomatic~ hacked together Clojure
;; Copyright (c) 2020, Kevin Kredit
;; License MIT

(ns puzzle.core
  (:gen-class))

; (use 'clojure.tools.trace)

(defn relativePiece [pred offset b index]
  (cond
    (pred index) nil
    (< index offset) nil
    :else (nth b (- index offset))
    )
  )

(defn boardIsLegal [board]
  (cond
    (> 2 (count board)) true
    :else
      (let [pos (- (count board) 1)
            topRow (< pos 3)
            leftCol (= 0 (mod pos 3))
            this (last board)
            above (fn [pos] (relativePiece #(> 3 %) 3 board pos))
            leftTo (fn [pos] (relativePiece #(= 0 (mod % 3)) 1 board pos))
            edgesMatch #(and (= (:insect %1) (:insect %2)) (not= (:end %1) (:end %2)))
            matchesAbove #(edgesMatch (:top this) (:bottom (above pos)))
            matchesLeft #(edgesMatch (:left this) (:right (leftTo pos)))
          ]
        (and (or topRow (matchesAbove)) (or leftCol (matchesLeft)))
        ))
  )

(defn rotatePiece [p]
  {:name (:name p), :rot (mod (inc (:rot p)) 4),
   :top (:left p), :right (:top p), :bottom (:right p), :left (:bottom p) }
  )

(defn addWithEachRotation [state p]
  (let [spunPiece (fn [n] (last (take n (iterate rotatePiece p))))
        newStateWithRotation (fn [n] {:placed (conj (:placed state) (spunPiece n)),
                                      :unplaced (filter #(not= p %) (:unplaced state))})]
    (map newStateWithRotation (into [] (range 1 5))))
  )

(defn solutions [state]
  (cond
    (= 0 (count (:unplaced state))) [(:placed state)]
    :else
      (let [
            nextStates (fn [] (into [] (flatten (map #(addWithEachRotation state %) (:unplaced state)))))
            nextLegalStates (fn [] (into [] (filter #(boardIsLegal (:placed %)) (nextStates))))]
        (->> (nextLegalStates)
          (mapcat solutions)
        )))
  )

(defn makePiece [n i1 e1 i2 e2 i3 e3 i4 e4]
  (let [e #(do {:insect %1 :end %2})]
    {:name n, :rot 0, :top (e i1 e1), :right (e i2 e2), :bottom (e i3 e3), :left (e i4 e4)}
    )
  )

(defn printBoard [board]
  (println (str " --> " (pr-str (map #(str (:name %) ":" (:rot %)) board))))
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
    (doseq [sol (solutions initState)] (printBoard sol))
))

;; Tests

; (defn helperSpunPiece [n p]
;   (last (take n (iterate rotatePiece p))))

; (defn testBoardIsLegal []
;   (let [goodboard [(makePiece 0 'dragonfly 'tail, 'ant 'head, 'beetle 'tail, 'mantis 'head)
;                    (helperSpunPiece 3 (makePiece 1 'dragonfly 'tail, 'ant 'tail, 'beetle 'head, 'mantis 'tail))]
;         badboard [(makePiece 0 'dragonfly 'tail, 'ant 'head, 'beetle 'tail, 'mantis 'head)
;                   (helperSpunPiece 1 (makePiece 1 'dragonfly 'tail, 'ant 'tail, 'beetle 'head, 'mantis 'tail))]
;         ]
;     (assert (boardIsLegal goodboard))
;     (assert (not (boardIsLegal badboard)))
;     )
;   )

; (defn testNextStatesAndNextLegalStates []
;   (let [nextLegalStates (fn [ns] (into [] (filter #(boardIsLegal (:placed %)) ns)))
;         startState {:placed [(makePiece 0 'dragonfly 'tail, 'ant 'head, 'beetle 'tail, 'mantis 'head)],
;                     :unplaced [(makePiece 1 'dragonfly 'tail, 'ant 'tail, 'beetle 'head, 'mantis 'tail)]}
;         next (nextStates startState)
;         ; next (dotrace [nextStates] (nextStates startState))
;         nextLegal (nextLegalStates next)
;         ]
;     (println (count next))
;     (println next)
;     (println (count nextLegal))
;     (println nextLegal)
;     (assert (= 4 (count next)))
;     (assert (= 1 (count nextLegal)))
;   ))
