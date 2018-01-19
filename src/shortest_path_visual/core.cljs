(ns shortest-path-visual.core
    (:require
      [reagent.core :as reagent]
      [shortest-path-visual.util :as util]))

(enable-console-print!)

(println "This text is printed from src/shortest-path-visual/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state
  (reagent/atom {:text "Hello world!"}))


(defn main []
  [:div
   [:h2 "Shortest path visual"]
   [util/graph-loader app-state]])

(reagent/render-component
  [main]
  (js/document.getElementById "app"))

(defn on-js-reload [])
