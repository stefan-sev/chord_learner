(ns chord-explorer.views.main
  "Main layout component."
  (:require [re-frame.core :as rf]
            [reagent.core]
            [chord-explorer.views.controls :as controls]
            [chord-explorer.views.progression :as progression]
            [chord-explorer.views.analysis :as analysis]
            [chord-explorer.views.keyboard :as keyboard]
            [chord-explorer.views.sheet-music :as sheet-music]
            [chord-explorer.views.guitar :as guitar-view]
            [chord-explorer.views.voicing-selector :as voicing-selector]))

(defn save-modal
  "Modal for saving progression."
  []
  (let [show? @(rf/subscribe [:show-save-modal?])
        prog-name @(rf/subscribe [:progression-name])
        name-atom (reagent.core/atom prog-name)]
    (when show?
      [:div.modal-overlay
       {:on-click #(rf/dispatch [:toggle-save-modal])}
       [:div.modal
        {:on-click #(.stopPropagation %)}
        [:div.modal-header
         [:h3 "Save Progression"]
         [:button.close-btn
          {:on-click #(rf/dispatch [:toggle-save-modal])}
          "\u00D7"]]
        [:div.modal-body
         [:div.form-group
          [:label "Progression Name"]
          [:input.input
           {:type "text"
            :value @name-atom
            :on-change #(reset! name-atom (.-value (.-target %)))
            :placeholder "Enter progression name"}]]]
        [:div.modal-footer
         [:button.btn.btn-secondary
          {:on-click #(rf/dispatch [:toggle-save-modal])}
          "Cancel"]
         [:button.btn.btn-primary
          {:on-click #(do (rf/dispatch [:save-progression @name-atom])
                         (rf/dispatch [:toggle-save-modal]))}
          "Save"]]]])))

(defn load-modal
  "Modal for loading saved progressions."
  []
  (let [show? @(rf/subscribe [:show-load-modal?])
        saved @(rf/subscribe [:saved-progressions])]
    (when show?
      [:div.modal-overlay
       {:on-click #(rf/dispatch [:toggle-load-modal])}
       [:div.modal
        {:on-click #(.stopPropagation %)}
        [:div.modal-header
         [:h3 "Load Progression"]
         [:button.close-btn
          {:on-click #(rf/dispatch [:toggle-load-modal])}
          "\u00D7"]]
        [:div.modal-body
         (if (seq saved)
           [:div.saved-list
            (for [prog saved]
              ^{:key (:id prog)}
              [:div.saved-item
               [:div.saved-info
                [:span.saved-name (:name prog)]
                [:span.saved-key (str (name (keyword (:key prog))) " " (name (keyword (:scale-type prog))))]
                [:span.saved-date (when (:saved-at prog)
                                    (subs (:saved-at prog) 0 10))]]
               [:div.saved-actions
                [:button.btn.btn-small.btn-primary
                 {:on-click #(do (rf/dispatch [:load-saved-progression (:id prog)])
                                (rf/dispatch [:toggle-load-modal]))}
                 "Load"]
                [:button.btn.btn-small.btn-danger
                 {:on-click #(rf/dispatch [:delete-saved-progression (:id prog)])}
                 "Delete"]]])]
           [:p.empty-state "No saved progressions yet."])]
        [:div.modal-footer
         [:button.btn.btn-secondary
          {:on-click #(rf/dispatch [:toggle-load-modal])}
          "Close"]]]])))

(defn header
  "Application header with key display and actions."
  []
  (let [key-display @(rf/subscribe [:key-display])
        has-chords? (not @(rf/subscribe [:progression-empty?]))]
    [:header.header
     [:div.header-left
      [:h1.logo "Chord Explorer"]]
     [:div.header-center
      [:div.key-display
       [:span.key-label "Key:"]
       [:span.key-value (:name key-display)]]]
     [:div.header-right
      [:button.btn.btn-secondary
       {:on-click #(rf/dispatch [:toggle-load-modal])}
       "Load"]
      [:button.btn.btn-secondary
       {:on-click #(rf/dispatch [:toggle-save-modal])
        :disabled (not has-chords?)}
       "Save"]
      [:button.btn.btn-secondary
       {:on-click #(rf/dispatch [:clear-progression])}
       "Clear"]
      [:button.btn.btn-primary
       {:on-click #(js/alert "Export feature coming soon!")}
       "Export"]]]))

(defn diatonic-chord-palette
  "Display diatonic chords for the current key."
  []
  (let [diatonic-chords @(rf/subscribe [:diatonic-chords])
        show-seventh? @(rf/subscribe [:show-seventh?])]
    [:div.card
     [:div.card-header
      [:h3 "Diatonic Chords"]
      [:label.checkbox
       [:input {:type "checkbox"
                :checked show-seventh?
                :on-change #(rf/dispatch [:toggle-seventh])}]
       [:span "7th chords"]]]
     [:div.card-body
      [:div.chord-grid
       (for [chord diatonic-chords]
         ^{:key (:degree chord)}
         [:button.chord-chip
          {:class (name (:function chord))
           :on-click #(rf/dispatch [:add-chord-to-progression
                                    {:root (:root chord)
                                     :type (:type chord)}])}
          [:span.chord-name (str (name (:root chord))
                                 (:symbol (chord-explorer.theory.chords/get-chord-def (:type chord))))]
          [:span.numeral (:numeral chord)]])]]]))

(defn secondary-dominants-palette
  "Display secondary dominants."
  []
  (let [sec-doms @(rf/subscribe [:secondary-dominants])]
    (when (seq sec-doms)
      [:div.card
       [:div.card-header
        [:h3 "Secondary Dominants"]]
       [:div.card-body
        [:div.chip-row
         (for [sd sec-doms]
           ^{:key (str (:root sd) "-" (get-in sd [:analysis :target-degree]))}
           [:button.borrowed-chip.secondary-dominant
            {:on-click #(rf/dispatch [:add-chord-to-progression
                                      {:root (:root sd)
                                       :type (:type sd)}])}
            [:span (str (get-in sd [:analysis :notation])
                        " (" (name (:root sd)) "7)")]])]]])))

(defn modal-interchange-palette
  "Display borrowed chords."
  []
  (let [borrowed @(rf/subscribe [:borrowed-chords])]
    (when (seq borrowed)
      [:div.card
       [:div.card-header
        [:h3 "Modal Interchange"]]
       [:div.card-body
        [:div.chip-row
         (for [b borrowed]
           ^{:key (str (:root b) "-" (:type b))}
           [:button.borrowed-chip.modal-interchange
            {:on-click #(rf/dispatch [:add-chord-to-progression
                                      {:root (:root b)
                                       :type (:type b)}])}
            [:span (str (get-in b [:analysis :notation])
                        " (" (name (:root b))
                        (:symbol (chord-explorer.theory.chords/get-chord-def (:type b)))
                        ")")]])]]])))

(defn chord-detail-panel
  "Side panel showing selected chord details."
  []
  (let [chord-info @(rf/subscribe [:selected-chord-info])
        voicing-mode @(rf/subscribe [:voicing-mode])]
    [:div.side-panel
     (if chord-info
       [:<>
        [:div.card
         [:div.card-header
          [:h3 "Chord Details"]]
         [:div.card-body
          [:div.chord-display
           [:span.chord-symbol (:symbol chord-info)]
           [:span.chord-full-name (:name chord-info)]]
          [:div.chord-notes
           (for [note (:notes chord-info)]
             ^{:key (str note)}
             [:span.note-badge (name note)])]
          (when (:numeral chord-info)
            [:div.analysis-info
             [:span.numeral (:numeral chord-info)]
             (when (:function chord-info)
               [:span.function-badge
                {:class (name (:function chord-info))}
                (case (:function chord-info)
                  :tonic "T"
                  :subdominant "SD"
                  :dominant "D"
                  "?")])])]]

        ;; Voicing mode toggle
        [:div.card
         [:div.card-header
          [:h3 "Voicing"]
          [:div.toggle-group
           [:button {:class (str "toggle-btn " (when (= voicing-mode :piano) "active"))
                     :on-click #(rf/dispatch [:set-voicing-mode :piano])}
            "Piano"]
           [:button {:class (str "toggle-btn " (when (= voicing-mode :guitar) "active"))
                     :on-click #(rf/dispatch [:set-voicing-mode :guitar])}
            "Guitar"]]]
         [:div.card-body
          (case voicing-mode
            :piano [keyboard/keyboard]
            :guitar [guitar-view/chord-diagram])]]

        [voicing-selector/voicing-selector]]

       ;; No chord selected
       [:div.card
        [:div.card-body.empty-state
         [:p "Select a chord from the progression to see details"]]])]))

(defn main-content
  "Main content area with progression builder."
  []
  [:div.main-content
   ;; Progression Builder
   [progression/progression-builder]

   ;; Sheet Music Display
   [sheet-music/staff-display]

   ;; Scale Fretboard
   [guitar-view/scale-fretboard]

   ;; Chord Palettes
   [diatonic-chord-palette]
   [secondary-dominants-palette]
   [modal-interchange-palette]])

(defn app
  "Root application component."
  []
  [:div.app
   [header]
   [:div.content-wrapper
    ;; Controls bar
    [controls/control-bar]

    [:div.content-grid
     ;; Main content
     [main-content]

     ;; Side panel
     [chord-detail-panel]]]

   ;; Modals
   [save-modal]
   [load-modal]])
