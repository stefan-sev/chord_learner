(ns chord-explorer.views.voicing-selector
  "Voicing picker UI component."
  (:require [re-frame.core :as rf]
            [chord-explorer.views.keyboard :as keyboard]
            [chord-explorer.views.guitar :as guitar-view]
            [chord-explorer.theory.voicings :as voicings]))

(defn piano-voicing-card
  "Card displaying a piano voicing option."
  [voicing selected?]
  (let [display (voicings/voicing->display voicing)]
    [:div.voicing-card
     {:class (when selected? "selected")
      :on-click #(rf/dispatch [:select-voicing voicing])}

     [:div.voicing-preview
      [keyboard/mini-keyboard (mapv :note (:notes voicing))]]

     [:div.voicing-info
      [:span.voicing-name (:name display)]
      [:div.complexity
       (for [i (range 5)]
         ^{:key i}
         [:span.dot {:class (when (< i (:complexity display)) "filled")}])]]]))

(defn guitar-voicing-card
  "Card displaying a guitar voicing option."
  [voicing selected?]
  [:div.voicing-card
   {:class (when selected? "selected")
    :on-click #(rf/dispatch [:select-voicing voicing])}

   [:div.voicing-preview
    [guitar-view/chord-diagram-mini voicing]]

   [:div.voicing-info
    [:span.voicing-name (:name voicing)]
    [:span.difficulty
     {:class (name (:difficulty voicing))}
     (name (:difficulty voicing))]]])

(defn voicing-selector
  "Main voicing selector panel."
  []
  (let [voicing-mode @(rf/subscribe [:voicing-mode])
        available-voicings @(rf/subscribe [:available-voicings])
        selected-voicing @(rf/subscribe [:selected-voicing])
        selected-chord @(rf/subscribe [:selected-chord])]

    (when selected-chord
      [:div.card
       [:div.card-header
        [:h3 "Voicings"]
        [:span.voicing-count (str (count available-voicings) " available")]]

       [:div.card-body
        (if (empty? available-voicings)
          [:div.empty-state
           [:p "No voicings available for this chord"]]

          [:div.voicing-grid
           (for [[idx voicing] (map-indexed vector available-voicings)]
             ^{:key idx}
             (case voicing-mode
               :piano [piano-voicing-card voicing (= voicing selected-voicing)]
               :guitar [guitar-voicing-card voicing (= voicing selected-voicing)]))])]])))
