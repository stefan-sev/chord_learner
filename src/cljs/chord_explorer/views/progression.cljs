(ns chord-explorer.views.progression
  "Progression editor component."
  (:require [re-frame.core :as rf]
            [chord-explorer.theory.chords :as chords]
            [chord-explorer.theory.analysis :as analysis]))

(defn chord-slot
  "Individual chord slot in the progression."
  [index chord]
  (let [selected-index @(rf/subscribe [:selected-chord-index])
        is-selected (= index selected-index)
        chord-def (chords/get-chord-def (:type chord))
        analysis (:analysis chord)]
    [:div.chord-slot
     {:class (when is-selected "selected")
      :on-click #(rf/dispatch [:select-chord index])}

     ;; Chord name
     [:div.chord-name
      (str (name (:root chord))
           (:symbol chord-def))]

     ;; Roman numeral
     [:div.numeral
      (if analysis
        (analysis/get-analysis-label analysis)
        "")]

     ;; Function badge (if diatonic)
     (when (and analysis (:function analysis))
       [:div.function-badge
        {:class (name (:function analysis))}
        (case (:function analysis)
          :tonic "T"
          :subdominant "SD"
          :dominant "D"
          "")])

     ;; Remove button
     [:button.remove-btn
      {:on-click (fn [e]
                   (.stopPropagation e)
                   (rf/dispatch [:remove-chord-from-progression index]))
       :title "Remove chord"}
      "\u00D7"]]))

(defn add-chord-button
  "Button to add a new chord."
  []
  [:button.add-chord-btn
   {:on-click #(js/alert "Click a chord from the palette below to add it")}
   [:span.plus-icon "+"]
   [:span.label "Add Chord"]])

(defn progression-builder
  "Main progression builder component."
  []
  (let [chords @(rf/subscribe [:progression-with-analysis])
        progression-name @(rf/subscribe [:progression-name])
        empty? @(rf/subscribe [:progression-empty?])]
    [:div.card.progression-card
     [:div.card-header
      [:div.progression-header
       [:input.progression-name-input
        {:type "text"
         :value progression-name
         :placeholder "Progression Name"
         :on-change #(rf/dispatch [:set-progression-name (.-value (.-target %))])}]
       [:div.progression-actions
        [:button.btn.btn-small
         {:on-click #(rf/dispatch [:clear-progression])
          :disabled empty?}
         "Clear"]]]]

     [:div.card-body
      [:div.progression-slots
       (if empty?
         ;; Empty state
         [:div.empty-progression
          [:p "Your progression is empty"]
          [:p.hint "Click a chord from the palette below to add it"]]

         ;; Chord slots
         [:<>
          (for [[index chord] (map-indexed vector chords)]
            ^{:key (:id chord)}
            [chord-slot index chord])

          ;; Add button at the end
          [add-chord-button]])]]]))
