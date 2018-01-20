(ns shortest-path-visual.main
    (:require
      [reagent.core :as reagent]
      [shortest-path-visual.view :as view]))

(enable-console-print!)

(println "This text is printed from src/shortest-path-visual/core.cljs. Go ahead and edit it and see reloading in action.")

(defn main []
  [:div
   [:h2 "Shortest path visual"]
   [view/graph-loader]])

(reagent/render-component
  [main]
  (js/document.getElementById "app"))

(defn on-js-reload [])
