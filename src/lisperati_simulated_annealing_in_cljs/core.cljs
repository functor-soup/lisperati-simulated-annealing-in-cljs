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

(defn draw-skeleton []
  (do
    (q/background 255)
    (q/stroke 0 255 0)
    (let [line-list (->> (map u/make-lines d/polygon-list)
                         (flatten))]
      (doseq [{x1 :x1 y1 :y1 x2 :x2 y2 :y2} line-list]
        (q/line x1 y1 x2 y2)))))

(defn draw-triangulated-state []
  (do
    (q/background 255)
    (q/fill 0)
    (doseq [ {x1 :x1 y1 :y1 x2 :x2 y2 :y2}
            (triangulate-polyon-list d/polygon-list)]
      (q/line x1 y1 x2 y2))))

(defn draw-clipped-state []
  (do
    (q/background 255)
    (let [[left right] (->> (map u/triangulate d/polygon-list) ;; [[Polygon]]
                            (apply concat)  ;; [Polygon])
                            (u/slice-x 300))
          left_ (flatten (map u/make-lines left))
          right_ (flatten (map u/make-lines right))]
      (do
        (q/stroke 255 0 0)
        (doseq [{x1 :x1 y1 :y1 x2 :x2 y2 :y2} left_]
          (q/line x1 y1 x2 y2))
        (q/stroke 0 0 255)
        (doseq [{x1 :x1 y1 :y1 x2 :x2 y2 :y2} right_]
          (q/line x1 y1 x2 y2))))))

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

