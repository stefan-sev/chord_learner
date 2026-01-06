(ns chord-explorer.views.analysis
  "Analysis display component."
  (:require [re-frame.core :as rf]))

(defn analysis-badge
  "Badge showing analysis type."
  [analysis-item]
  (let [atype (:type analysis-item)]
    [:span.analysis-badge
     {:class (name atype)}
     (case atype
       :diatonic "Diatonic"
       :secondary-dominant "Sec. Dom."
       :secondary-leading-tone "Sec. LT"
       :modal-interchange "Borrowed"
       :tritone-substitution "Tritone Sub"
       (name atype))]))

(defn chord-analysis-detail
  "Detailed analysis for a single chord."
  [chord-with-analysis]
  (let [analysis (:analysis chord-with-analysis)
        analyses (:analyses analysis)]
    [:div.analysis-detail
     [:div.chord-header
      [:span.chord-symbol
       (str (name (:root chord-with-analysis))
            (:symbol (chord-explorer.theory.chords/get-chord-def (:type chord-with-analysis))))]

      ;; Primary analysis
      (when (:numeral analysis)
        [:span.numeral (:numeral analysis)])]

     ;; Function
     (when (:function analysis)
       [:div.function-info
        [:span.function-badge
         {:class (name (:function analysis))}
         (case (:function analysis)
           :tonic "Tonic"
           :subdominant "Subdominant"
           :dominant "Dominant"
           "Unknown")]])

     ;; Additional analyses
     (when (seq analyses)
       [:div.analyses-list
        (for [a analyses]
          ^{:key (str (:type a) "-" (:notation a))}
          [analysis-badge a])])]))

(defn progression-analysis
  "Full progression analysis view."
  []
  (let [chords @(rf/subscribe [:progression-with-analysis])
        numeral-sequence @(rf/subscribe [:numeral-sequence])
        show-analysis? @(rf/subscribe [:show-analysis?])]
    (when (and show-analysis? (seq chords))
      [:div.card
       [:div.card-header
        [:h3 "Analysis"]
        [:button.btn.btn-small
         {:on-click #(rf/dispatch [:toggle-analysis])}
         (if show-analysis? "Hide" "Show")]]
       [:div.card-body
        ;; Numeral sequence
        [:div.numeral-sequence
         (interpose " - "
                    (for [[idx numeral] (map-indexed vector numeral-sequence)]
                      ^{:key idx}
                      [:span.numeral numeral]))]

        ;; Detailed analysis
        [:div.analysis-details
         (for [[idx chord] (map-indexed vector chords)]
           ^{:key (:id chord)}
           [chord-analysis-detail chord])]]])))
