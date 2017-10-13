(ns lisperati-simulated-annealing-in-cljs.core
    (:require [quil.core :as q :include-macros true]
              [lisperati-simulated-annealing-in-cljs.data :as d]
              [lisperati-simulated-annealing-in-cljs.utils :as u]
              [dommy.core :as dom :refer-macros [sel1]]))


(enable-console-print!)

(defonce app-state (atom {:state :clipped}))

(defn setup
  []
  (q/frame-rate 1))

(defn triangulate-polyon-list [polygon-list]
  (->> (map u/triangulate polygon-list) ;; [[Polygon]]
       (apply concat)  ;; [Polygon]
       (map u/make-lines) ;; [[lines]]
       (flatten)));; [lines]

(defn draw-lines [lines-seq]
  (doseq [{x1 :x1 y1 :y1
           x2 :x2 y2 :y2
           color :color} lines-seq]
    (apply q/stroke color)
    (q/line x1 y1 x2 y2)))

(defn draw-skeleton []
  (do
    (q/background 255)
    (let [line-list
          (->> (map u/make-lines d/polygon-list)
               (flatten))
          colored-line-list
          (map #(u/line-color % 0 255 0) line-list)]
      (draw-lines colored-line-list))))

(defn draw-triangulated-state []
  (do
    (q/background 255)
    (q/fill 0)
    (draw-lines (->> (triangulate-polyon-list d/polygon-list)
                     (map #(u/line-color % 0 0 0))))))

(defn draw-clipped-state []
  (do
    (q/background 255)
    (let [[left right] (->> (map u/triangulate d/polygon-list) ;; [[Polygon]]
                            (apply concat)  ;; [Polygon])
                            (u/slice-x 300))
          left_ (flatten (map u/make-lines left))
          right_ (flatten (map u/make-lines right))
          red (map #(u/line-color % 255 0 0) left_)
          blue (map #(u/line-color % 0 0 255) right_)]
      (doseq [color-list [red blue]]
        (draw-lines color-list)))))

(defonce state-draw
  {:skeleton draw-skeleton
   :triangulate draw-triangulated-state
   :clipped draw-clipped-state})

(defn draw []
  (apply (get state-draw (:state @app-state)) '()))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

(q/defsketch hello
      :setup setup
      :draw draw
      :host "chicken"
      :size [700 500])

(defn click-handler [state]
  (fn [] (swap! app-state assoc :state state)))

(doseq [state (keys state-draw)]
  (let [id (->> (name state)
                (str "#")
                (keyword))]
    (dom/listen! (sel1 id) :click (click-handler state))))

