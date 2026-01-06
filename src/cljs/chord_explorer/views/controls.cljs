(ns chord-explorer.views.controls
  "Key/scale selectors and transpose controls."
  (:require [re-frame.core :as rf]
            [chord-explorer.db :as db]
            [chord-explorer.theory.progression :as progression]))

(defn key-selector
  "Dropdown for selecting root note."
  []
  (let [current-key @(rf/subscribe [:current-key])]
    [:div.control-group
     [:label.control-label "Root"]
     [:select.select
      {:value (name current-key)
       :on-change #(rf/dispatch [:set-key (keyword (.-value (.-target %)))])}
      (for [note db/note-order]
        ^{:key (name note)}
        [:option {:value (name note)} (name note)])]]))

(defn scale-selector
  "Dropdown for selecting scale type with category grouping."
  []
  (let [scale-type @(rf/subscribe [:scale-type])
        grouped-scales (group-by :category db/scale-types)]
    [:div.control-group
     [:label.control-label "Scale"]
     [:select.select
      {:value (name scale-type)
       :on-change #(rf/dispatch [:set-scale-type (keyword (.-value (.-target %)))])}
      (for [[category scales] grouped-scales]
        ^{:key category}
        [:optgroup {:label category}
         (for [{:keys [key name]} scales]
           ^{:key (clojure.core/name key)}
           [:option {:value (clojure.core/name key)} name])])]]))

(defn transpose-controls
  "Buttons for transposing up/down."
  []
  [:div.control-group
   [:label.control-label "Transpose"]
   [:div.button-group
    [:button.btn.btn-icon
     {:on-click #(rf/dispatch [:transpose-progression -1])
      :title "Down half step"}
     "-"]
    [:button.btn.btn-icon
     {:on-click #(rf/dispatch [:transpose-progression 1])
      :title "Up half step"}
     "+"]]])

(defn template-selector
  "Dropdown for loading progression templates."
  []
  [:div.control-group
   [:label.control-label "Templates"]
   [:select.select
    {:value ""
     :on-change #(let [value (.-value (.-target %))]
                   (when (not= value "")
                     (rf/dispatch [:load-template (keyword value)])))}
    [:option {:value ""} "Load template..."]
    (for [[template-key _] progression/common-progression-templates]
      ^{:key (name template-key)}
      [:option {:value (name template-key)}
       (clojure.string/replace (name template-key) "-" " ")])]])

(defn control-bar
  "Main control bar with key/scale selectors."
  []
  [:div.control-bar
   [:div.controls-left
    [key-selector]
    [scale-selector]]
   [:div.controls-center
    [transpose-controls]]
   [:div.controls-right
    [template-selector]]])
