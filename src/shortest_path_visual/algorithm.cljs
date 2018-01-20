(ns shortest-path-visual.algorithm
  (:require
    [shortest-path-visual.visualize :as visualize]))

(defn min-edge [candidates]
  (let [{:keys [edge/to edge/from distance]} (first (sort-by :distance (vals candidates)))]
    [to from distance]))

(defn add-candidates [g distance current-node visited candidates]
  (apply dissoc
         (merge candidates
                (for [[k v] (get-in @g [:edges current-node])
                      :when
                      (or (not (candidates (:edge/to v)))
                          (< (+ distance (:edge/weight v 1)) (:distance (candidates (:edge/to v)))))]
                  [k (assoc
                       (select-keys v [:edge/weight :edge/from :edge/to :distance])
                       :distance (+ distance (:edge/weight v 1)))]))
         (keys visited)))

(declare expand)

(defn visit [g target-node visited candidates]
  (let [[to from distance :as edge] (min-edge candidates)
        visited-result (conj visited edge)
        candidates-result (dissoc candidates to)]
    (visualize/visualize-visit g edge visited-result candidates-result distance)
    (cond
      (= to target-node) #(visualize/visualize-solution g visited-result target-node distance)
      (empty? candidates) #(visualize/visualize-fail g)
      :else #(expand g distance to target-node visited-result candidates-result))))

(defn expand [g distance current-node target-node visited candidates]
  (let [expanded-candidates (add-candidates g distance current-node visited candidates)]
    (visualize/visualize-expand g visited expanded-candidates)
    #(visit g target-node visited expanded-candidates)))

(defn shortest-path-step [g distance current-node target-node visited candidates]
  (let [candidates (add-candidates g distance current-node visited candidates)]
    (let [[to from distance-result :as me] (min-edge candidates)
          v (conj visited me)]
      (cond
        ;;TODO: unroll
        (= to target-node) v
        (empty? candidates) "FAIL"
        ;; timeout
        :else (recur g distance-result to target-node v candidates)))))

(defn shortest-path [g]
  ;;(prn "SP" (shortest-path-step g 0 "0" "5" {} {}))
  (visualize/visualize-start g "0" "5")
  (visualize/slow-trampoline 2000 expand g 0 "0" "5" {} {}))



;; TODO: edge/distance and distance is confusing


