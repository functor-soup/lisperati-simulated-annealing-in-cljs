(ns lisperati-simulated-annealing-in-cljs.utils)

;; specific use for this application
;; [points] -> [lines]
(defn make-lines [x]
  (let [fst (first x)
        x-with-head-at-end (conj (vec (rest x)) fst)]
    (map (fn [a b] {:x1 (:x a) :y1 (:y a)
                    :x2 (:x b) :y2 (:y b)})
         x x-with-head-at-end)))

(defn line-color [line r g b]
  (assoc line :color [r g b]))

;; Polygon -> [Polygon]
;; todo- make this non recursive
(defn triangulate [polygon]
  (condp <= (count polygon)
    3 (let [[a b c & remaining] polygon
            true-remaining (concat [a c] remaining)]
        (conj (triangulate true-remaining) [a b c]))
    []))

;; clips triangle into two triangles
(defn clip-triangle [i list1 list2]
  {:pre [(and (<= (count list1) 3)
              (<= (count list2) 3))]}
  (condp = (count list1)
    0 []
    1 (let [a (first list1)
            b (first list2)
            c (second list2)]
        [[a, (i a b), (i a c) ]])
    2 (let [a (first list1)
            b (second list1)
            c (first list2)]
        [[a, (i b c), b],[a, (i a c), (i b c)]])
    3 [list1]))

(defn interpolate-y-via-x [x point1 point2]
  (let [{x1 :x y1 :y} point1
        {x2 :x y2 :y} point2
        y (+ (/ (* (- y2 y1) (- x x1))
                (- x2 x1))
             y1)]
    {:x x :y y}))

(defn interpolate-x-via-y [y point1 point2]
  (let [{x1 :x y1 :y} point1
        {x2 :x y2 :y} point2
        x (+ (/ (* (- x2 x1) (- y y1))
                (- y2 y1))
             y1)]
    {:x x :y y}))

(defn slice [f i triangle-list]
  (letfn [(p [g] (comp
                  (partial apply clip-triangle i)
                  #(map % [true false])
                  (partial group-by g)))
          (clip [f] (mapcat (p f) triangle-list))]
    [(clip f), (clip (comp not f))]))

(defn slice-x [num triangle-list]
  (slice (comp (partial < num) (partial :x))
         (partial interpolate-y-via-x num) triangle-list))

(defn slice-y [num triangle-list]
  (slice (comp (partial < num) (partial :y))
         (partial interpolate-x-via-y) triangle-list))
