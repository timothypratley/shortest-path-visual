(ns shortest-path-visual.visualize
  (:require [reagent.core :as reagent]))

(def vis
  (reagent/atom nil))

;; TODO: add a class to nodes?
(def colors
  {:red "#c82829"
   :orange "#f5871f"
   :yellow "#eab700"
   :green "#718c00"
   :aqua "#3e999f"
   :blue "#4271ae"
   :purple "#8959a8"})

(defn visualize-start [g start-node target-node]
  (reset! vis {:distance 0
               :status :searching
               :edge nil
               :visited {}
               :candidates {}})
  (swap! g assoc-in [:nodes start-node :node/color] (:green colors))
  (swap! g assoc-in [:nodes target-node :node/color] (:blue colors)))

(defn visualize-expand [g visited candidates]
  (swap! vis assoc
         :visited visited
         :candidates candidates)
  (swap! g update :nodes #(merge-with merge %1 %2)
         (zipmap (keys candidates)
                 (repeat {:node/color (:aqua colors)})))
  (doseq [{:keys [edge/from edge/to]} (vals candidates)]
    (swap! g assoc-in [:edges from to :edge/color] (:aqua colors))))

(defn visualize-visit [g edge visited candidates distance]
  (swap! vis assoc
         :edge edge
         :visited visited
         :candidates candidates
         :distance distance)
  (swap! g update :nodes #(merge-with merge %1 %2)
         (zipmap (keys visited)
                 (repeat {:node/color "yellow"}))))

(defn visualize-solution [g visited target-node distance]
  (swap! vis assoc
         :visited visited
         :distance distance
         :status :success)
  (swap! g assoc-in [:nodes target-node :node/color] (:blue colors))
  (loop [n target-node]
    (when-let [v (visited n)]
      (swap! g assoc-in [:nodes v :node/color] (:blue colors))
      (swap! g assoc-in [:edges v n :edge/color] (:blue colors))
      (recur v))))

(defn visualize-fail [g]
  (swap! vis assoc
         :status :failed))

(defn slow-trampoline
  ([t f]
   (let [ret (f)]
     (if (fn? ret)
       (js/setTimeout #(slow-trampoline t ret) t)
       ret)))
  ([t f & args]
   (slow-trampoline t #(apply f args))))
