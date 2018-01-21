(ns shortest-path-visual.visualize
  (:require [reagent.core :as reagent]
            [clojure.set :as set]
            [clojure.string :as string]))

(def vis
  (reagent/atom nil))

;; TODO: add a class to nodes?
(def colors
  {:red "#c82829"
   :orange "#f5871f"
   :yellow "#eab700"
   :green "#718c00"
   :aqua "#3e999f"
   :aqua2 "#6ec9cf"
   :blue "#4271ae"
   :purple "#8959a8"})

(defn visualize-start [g start-node target-node]
  (reset! vis {:distance 0
               :status (str "Initialize search from " start-node " to " target-node)
               :edge nil
               :visited {}
               :candidates {}})
  (doseq [node (keys (:nodes @g))]
    (swap! g assoc-in [:nodes node :node/color]
           (cond
             (= node start-node) (:green colors)
             (= node target-node) (:blue colors)
             :else nil)))
  (doseq [[from es] (:edges @g)
          [to edge] es]
    (swap! g assoc-in [:edges from to :edge/color] nil)))

(defn visualize-expand [g visited candidates expanded-candidates]
  (let [new-candidates (set/difference
                         (set (keys expanded-candidates))
                         (set (keys candidates)))]
    (swap! vis assoc
           :status (str "Expand to new candidates: "
                        (if (seq new-candidates)
                          (string/join ", " (sort new-candidates))
                          "none"))
           :visited visited
           :candidates expanded-candidates)
    (doseq [node-id (keys expanded-candidates)]
      (swap! g assoc-in [:nodes node-id :node/color]
             (if (new-candidates node-id)
               (:aqua2 colors)
               (:aqua colors))))
    (doseq [{:keys [edge/from edge/to]} (vals expanded-candidates)]
      (swap! g assoc-in [:edges from to :edge/color]
             (if (new-candidates to)
               (:aqua2 colors)
               (:aqua colors))))))

(defn visualize-visit [g [closest-node from :as edge] visited candidates distance]
  (swap! vis assoc
         :status (str "Visit closest candidate: " closest-node " via " from)
         :edge edge
         :visited visited
         :candidates candidates
         :distance distance)
  (doseq [[to from] visited :when from]
    (swap! g assoc-in [:nodes to :node/color]
           (if (= to closest-node)
             (:orange colors)
             (:yellow colors)))
    (swap! g assoc-in [:edges from to :edge/color]
           (if (= to closest-node)
             (:orange colors)
             (:yellow colors)))))

(defn backtrack [g visited [to & more :as path]]
  (if-let [from (visited to)]
    (do
      (swap! vis assoc
             :status (str "Backtrack from " from " to " to ". Path is: "
                          (string/join ", " path)))
      (swap! g assoc-in [:nodes from :node/color] (:blue colors))
      (swap! g assoc-in [:edges from to :edge/color] (:blue colors))
      #(backtrack g visited (cons from path)))
    (swap! vis assoc
           :status (str "Found path: "
                        (string/join ", " path)))))

(defn visualize-solution [g visited target-node distance]
  (swap! vis assoc
         :visited visited
         :distance distance
         :status (str "Found target node " target-node))
  (swap! g assoc-in [:nodes target-node :node/color] (:blue colors))
  #(backtrack g visited (list target-node)))

(defn visualize-fail [g]
  (swap! vis assoc
         :status "No path found"))

(defn slow-trampoline
  ([t searching? f]
   (let [result (f)]
     (if (fn? result)
       (js/setTimeout #(slow-trampoline t searching? result) t)
       (do (reset! searching? false)
           result))))
  ([t searching? f & args]
   (slow-trampoline t searching? #(apply f args))))
