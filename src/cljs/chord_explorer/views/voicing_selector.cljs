(ns chord-explorer.views.voicing-selector
  "Voicing picker UI component."
  (:require [re-frame.core :as rf]
            [chord-explorer.views.keyboard :as keyboard]
            [chord-explorer.views.guitar :as guitar-view]
            [chord-explorer.theory.voicings :as voicings]
            [chord-explorer.theory.guitar :as guitar-theory]))

(defn piano-voicing-card
  "Card displaying a piano voicing option."
  [voicing selected?]
  (let [display (voicings/voicing->display voicing)]
    [:div.voicing-card
     {:class (when selected? "selected")
      :on-click #(rf/dispatch [:select-voicing voicing])}

     [:div.voicing-preview
      [keyboard/mini-keyboard
       (mapv :note (:notes voicing))
       (:root voicing)
       (:chord-type voicing)]]

     [:div.voicing-info
      [:span.voicing-name (:name display)]
      (when (:inversion-short display)
        [:span.inversion-badge (:inversion-short display)])
      [:div.complexity
       (for [i (range 5)]
         ^{:key i}
         [:span.dot {:class (when (< i (:complexity display)) "filled")}])]]]))

(defn guitar-voicing-card
  "Card displaying a guitar voicing option."
  [voicing selected?]
  (let [diagram-data (guitar-theory/voicing->diagram-data voicing)]
    [:div.voicing-card
     {:class (when selected? "selected")
      :on-click #(rf/dispatch [:select-voicing voicing])}

     [:div.voicing-preview
      [guitar-view/chord-diagram-mini voicing]]

     [:div.voicing-info
      [:span.voicing-name (:name voicing)]
      [:div.voicing-meta
       (when (:inversion-short diagram-data)
         [:span.inversion-badge (:inversion-short diagram-data)])
       [:span.difficulty
        {:class (name (:difficulty voicing))}
        (name (:difficulty voicing))]]]]))

;; =============================================================================
;; Grouping by Inversion and Position
;; =============================================================================

(def inversion-order
  "Order for displaying inversion groups."
  [:root :first :second :third])

(def inversion-labels
  "Display labels for inversion groups."
  {:root "Root Position"
   :first "1st Inversion"
   :second "2nd Inversion"
   :third "3rd Inversion"})

(defn- get-voicing-inversion
  "Get the inversion of a voicing based on mode."
  [voicing mode]
  (case mode
    :guitar (guitar-theory/detect-guitar-inversion voicing)
    :piano (voicings/detect-inversion voicing)
    :root))

(defn- get-position-label
  "Get a display label for a fret position range."
  [position]
  (cond
    (= position 0) "Open"
    (<= position 4) (str "Pos " position "-" (+ position 3))
    :else (str "Pos " position "-" (+ position 3))))

(defn- group-by-position
  "Group voicings by fret position ranges."
  [voicings]
  (let [;; Group into position ranges: Open (0), Low (1-4), Mid (5-8), High (9-12)
        position-ranges [[0 0 "Open"]
                         [1 4 "Low (1-4)"]
                         [5 8 "Mid (5-8)"]
                         [9 12 "High (9-12)"]]
        grouped (group-by
                 (fn [v]
                   (let [pos (or (:position v) 0)]
                     (cond
                       (= pos 0) "Open"
                       (<= pos 4) "Low (1-4)"
                       (<= pos 8) "Mid (5-8)"
                       :else "High (9-12)")))
                 voicings)]
    ;; Return in order
    (for [[_ _ label] position-ranges
          :let [group (get grouped label)]
          :when (seq group)]
      {:label label
       :voicings (sort-by :position group)})))

(defn- group-voicings-by-inversion
  "Group voicings by their inversion, maintaining order."
  [voicings mode]
  (let [grouped (group-by #(get-voicing-inversion % mode) voicings)]
    ;; Return groups in defined order, filtering out empty groups
    (for [inv inversion-order
          :let [group (get grouped inv)]
          :when (seq group)]
      {:inversion inv
       :label (get inversion-labels inv)
       :voicings group
       :by-position (group-by-position group)})))

(defn position-subgroup
  "Render voicings for a position subgroup."
  [pos-data voicing-mode selected-voicing]
  [:div.position-subgroup
   [:span.position-label (:label pos-data)]
   [:div.voicing-row
    (for [[idx voicing] (map-indexed vector (:voicings pos-data))]
      ^{:key idx}
      (case voicing-mode
        :piano [piano-voicing-card voicing (= voicing selected-voicing)]
        :guitar [guitar-voicing-card voicing (= voicing selected-voicing)]))]])

(defn voicing-group
  "Render a group of voicings with a header, organized by position."
  [group-data voicing-mode selected-voicing]
  [:div.voicing-group
   [:div.voicing-group-header
    [:h4 (:label group-data)]
    [:span.group-count (str (count (:voicings group-data)) " voicings")]]
   [:div.voicing-group-content
    (for [pos-group (:by-position group-data)]
      ^{:key (:label pos-group)}
      [position-subgroup pos-group voicing-mode selected-voicing])]])

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

          (let [grouped (group-voicings-by-inversion available-voicings voicing-mode)]
            [:div.voicing-groups
             (for [group grouped]
               ^{:key (:inversion group)}
               [voicing-group group voicing-mode selected-voicing])]))]])))
