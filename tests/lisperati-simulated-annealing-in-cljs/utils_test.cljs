(ns lisperati-simulated-annealing-in-cljs.utils-test
  (:require [lisperati-simulated-annealing-in-cljs.utils :as u]
            [cljs.test :refer-macros [deftest is]]))

(deftest make-lines-test
  (let [a {:x 2 :y 7}
        b {:x 3 :y 8}
        l '({:x1 2 :y1 7 :x2 3 :y2 8}
            {:x1 3 :y1 8 :x2 2 :y2 7})]
   (is (= (u/make-lines [a b]) l))))

(deftest triangulate-test
  (let [list-mock [:a :b :c :d]
        result '([:a :c :d], [:a :b :c])]
    (is (= result (u/triangulate list-mock)))))

(deftest interpolate-y-via-x-test
  (let [point1 {:x 100 :y 100}
        point2 {:x 500 :y 500}
        result1 {:x 200 :y 200}]
    (is (= result1 (u/interpolate-y-via-x 200 point1 point2)))))

(deftest interpolate-x-via-y-test
  (let [point1 {:x 100 :y 100}
        point2 {:x 500 :y 500}
        result1 {:x 200 :y 200}]
    (is (= result1 (u/interpolate-x-via-y 200 point1 point2)))))

(deftest clip-triangle-test
  (let [f       (fn [x y] y)
        input1  [[:a] [:b :c]]
        result1 [[:a :b :c]]
        input2  [[:a :b] [:c]]
        result2 [[:a :c :b] [:a :c :c]]]
    (is (and (= (apply
                 (partial u/clip-triangle f) input1) result1)
             (= (apply
                 (partial u/clip-triangle f) input2) result2)
              ))))

;; 
(deftest slice-test
  (let [f (partial contains? #{:a :b})
        i (fn [x y] y)
        input [[:a :b :f]]
        result [ '([:a :f :b] [:a :f :f]) '([:f :a :b])]]
    (is (= (u/slice f i input) result))))
