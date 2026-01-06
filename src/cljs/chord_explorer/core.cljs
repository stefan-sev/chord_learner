(ns chord-explorer.core
  "Application entry point."
  (:require [reagent.dom :as rdom]
            [re-frame.core :as rf]
            [chord-explorer.events]
            [chord-explorer.subs]
            [chord-explorer.views.main :as main]))

(defn ^:dev/after-load mount-root
  "Mount the root component. Called after hot reload."
  []
  (rf/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [main/app] root-el)))

(defn ^:export init
  "Initialize the application."
  []
  (rf/dispatch-sync [:initialize-db])
  (rf/dispatch [:initialize-app])
  (mount-root))
