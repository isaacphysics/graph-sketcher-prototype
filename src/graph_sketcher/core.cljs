(ns graph-sketcher.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [reagent.core :as reagent]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

(defn smooth-curve [points]
  (take-nth (/ (count points) 20) points))

(defn distance-squared [p1 p2]
  (reduce + (map #(* % %) (map - p1 p2))))

(defn nearest-point-index [curve p]
  (first (apply min-key (fn [[i cp]] (distance-squared cp p)) (map-indexed vector curve))))

(defn update-current-curve [state]
  (let [start-nearest (nearest-point-index (:current-curve state) (first (:current-stroke state)))
        end-nearest (nearest-point-index (:current-curve state) (last (:current-stroke state)))
        start-cut (min start-nearest end-nearest)
        end-cut (max start-nearest end-nearest)

        stroke (if (= start-cut start-nearest)
                 (:current-stroke state)
                 (reverse (:current-stroke state)))

        new-curve (concat (take start-cut (:current-curve state))
                          stroke
                          (drop end-cut (:current-curve state)))]
    (println start-nearest)

    (-> state
        (assoc :current-curve new-curve)
        (dissoc state :current-stroke))))

(defn repeat-first-and-last [coll]
  (conj (vec (cons (first coll) coll)) (last coll)))

(defn draw-curve [curve]
  (q/begin-shape)
  (doseq [[x y] (repeat-first-and-last curve)]
    (q/curve-vertex x y))
  (q/end-shape))

(defn setup []
  {:frame 0
   :curves []})

(defn update-state [state]
  (-> state
      (update :frame inc)
      ;(update :curves #(take 3 %))
      ))

(defn draw [state]
  (q/background 240)
  (q/fill 0 0 0 0)
  (q/stroke-weight 2)

  ;(println state)
  (doseq [c (:curves state)]
    (q/stroke 0 0 255 128)
    (draw-curve (smooth-curve c)))

  (when-let [c (:current-curve state)]
    (q/stroke 0 128 0)
    (draw-curve c))
  (when-let [c (:current-stroke state)]
    (q/stroke 255 0 0)
    (draw-curve c)))



(defn mouse-pressed [state]
  (assoc state :current-stroke []))

(defn mouse-released [state]
  (if (:current-curve state)
    (update-current-curve state)
    (-> state
        (assoc :current-curve (:current-stroke state))
        (dissoc :current-stroke))))

(defn mouse-dragged [state {:keys [x y p-x p-y]}]
  (update state :current-stroke conj [x y]))

(println (repeat-first-and-last [1 2 3 4 5]))

(q/defsketch my-sketch-definition
             :host "canvas"
             :setup setup
             :update update-state
             :draw draw
             :mouse-pressed mouse-pressed
             :mouse-released mouse-released
             :mouse-dragged mouse-dragged
             :size [800 600]
             :middleware [m/fun-mode])


(defn page []
  [:div [:button {:on-click (fn [] (q/with-sketch (q/get-sketch-by-id "canvas")
                                                  (swap! (q/state-atom) #(-> %
                                                                             (update :curves conj (:current-curve %))
                                                                             (update :curves (partial take 3))
                                                                             (dissoc :current-curve)))))} "Hello"]])



(.setTimeout js/window (fn [] (reagent/render [page] (.getElementById js/document "app"))))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  (reagent/force-update-all)
)
