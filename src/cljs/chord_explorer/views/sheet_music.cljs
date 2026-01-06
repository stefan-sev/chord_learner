(ns chord-explorer.views.sheet-music
  "SVG-based staff notation renderer."
  (:require [re-frame.core :as rf]
            [chord-explorer.theory.core :as core]))

;; =============================================================================
;; Constants
;; =============================================================================

(def staff-config
  {:line-spacing 10
   :staff-width 800
   :staff-height 80
   :margin-top 30
   :margin-left 60
   :chord-spacing 120
   :note-radius 6})

(def note-positions
  "Y positions for notes on treble clef. Middle C = ledger line below staff."
  {:C4 50  ; Middle C - ledger line
   :D4 45
   :E4 40  ; Bottom line of staff
   :F4 35
   :G4 30  ; Second line
   :A4 25
   :B4 20  ; Third line
   :C5 15
   :D5 10  ; Fourth line
   :E5 5
   :F5 0   ; Top line
   :G5 -5
   :A5 -10})

;; =============================================================================
;; SVG Helpers
;; =============================================================================

(defn staff-lines
  "Draw the 5 staff lines."
  []
  (let [{:keys [margin-left staff-width line-spacing margin-top]} staff-config]
    [:g.staff-lines
     (for [i (range 5)]
       ^{:key i}
       [:line {:x1 margin-left
               :y1 (+ margin-top (* i line-spacing))
               :x2 (+ margin-left staff-width)
               :y2 (+ margin-top (* i line-spacing))
               :stroke "#2d2a26"
               :stroke-width 1}])]))

(defn treble-clef
  "Draw a simple treble clef symbol."
  []
  (let [{:keys [margin-left margin-top]} staff-config]
    [:text.treble-clef
     {:x (- margin-left 40)
      :y (+ margin-top 35)
      :font-size "60px"
      :font-family "serif"
      :fill "#2d2a26"}
     "\uD834\uDD1E"])) ; Unicode treble clef

(defn note->y-position
  "Calculate Y position for a note."
  [note octave]
  (let [{:keys [margin-top line-spacing]} staff-config
        ;; Reference: E4 is bottom line (y = 40 from note-positions)
        ;; Each step = line-spacing / 2
        note-values {:C 0 :D 1 :E 2 :F 3 :G 4 :A 5 :B 6}
        base-note (get note-values (if (keyword? note)
                                     (keyword (first (name note)))
                                     :C) 0)
        ;; Calculate steps from E4 (the bottom staff line)
        e4-steps (+ (* 7 (- octave 4)) base-note)
        ;; E4 is step 2, so adjust
        steps-from-e4 (- e4-steps 2)
        ;; Each step moves 5 units up
        y-offset (* steps-from-e4 -5)]
    (+ margin-top 40 y-offset)))

(defn needs-ledger-lines?
  "Check if a note needs ledger lines."
  [note octave]
  (let [y (note->y-position note octave)
        {:keys [margin-top line-spacing]} staff-config
        top-line margin-top
        bottom-line (+ margin-top (* 4 line-spacing))]
    (or (< y top-line) (> y bottom-line))))

(defn ledger-lines
  "Draw ledger lines for notes outside the staff."
  [x note octave]
  (let [y (note->y-position note octave)
        {:keys [margin-top line-spacing note-radius]} staff-config
        top-line margin-top
        bottom-line (+ margin-top (* 4 line-spacing))]
    [:g.ledger-lines
     (cond
       ;; Above staff
       (< y top-line)
       (for [ly (range (- top-line line-spacing) y (- line-spacing))]
         ^{:key ly}
         [:line {:x1 (- x 12)
                 :y1 ly
                 :x2 (+ x 12)
                 :y2 ly
                 :stroke "#2d2a26"
                 :stroke-width 1}])

       ;; Below staff (like middle C)
       (> y bottom-line)
       (for [ly (range (+ bottom-line line-spacing) (+ y 1) line-spacing)]
         ^{:key ly}
         [:line {:x1 (- x 12)
                 :y1 ly
                 :x2 (+ x 12)
                 :y2 ly
                 :stroke "#2d2a26"
                 :stroke-width 1}]))]))

(defn note-head
  "Draw a note head (whole note)."
  [x y & [highlighted?]]
  (let [{:keys [note-radius]} staff-config]
    [:ellipse {:cx x
               :cy y
               :rx note-radius
               :ry (- note-radius 1)
               :fill (if highlighted? "#4f46e5" "#2d2a26")
               :stroke (if highlighted? "#4f46e5" "#2d2a26")
               :stroke-width 1.5}]))

(defn accidental
  "Draw an accidental (sharp or flat)."
  [x y acc-type]
  [:text {:x (- x 15)
          :y (+ y 5)
          :font-size "16px"
          :font-family "serif"
          :fill "#2d2a26"}
   (case acc-type
     :sharp "\u266F"
     :flat "\u266D"
     :natural "\u266E"
     "")])

(defn chord-stack
  "Draw a chord as stacked notes."
  [x notes label]
  (let [sorted-notes (sort-by (fn [{:keys [note octave]}]
                                (core/note->midi {:note note :octave octave}))
                              notes)]
    [:g.chord-stack
     ;; Notes
     (for [{:keys [note octave]} sorted-notes]
       (let [y (note->y-position note octave)
             is-sharp (clojure.string/includes? (name note) "#")
             is-flat (clojure.string/includes? (name note) "b")]
         ^{:key (str note octave)}
         [:g
          [ledger-lines x note octave]
          (when is-sharp [accidental x y :sharp])
          (when is-flat [accidental x y :flat])
          [note-head x y]]))

     ;; Chord label below
     [:text {:x x
             :y 100
             :text-anchor "middle"
             :font-family "'Libre Baskerville', serif"
             :font-size "14px"
             :fill "#2d2a26"}
      label]]))

;; =============================================================================
;; Main Component
;; =============================================================================

(defn staff-display
  "Main staff notation display."
  []
  (let [chords @(rf/subscribe [:progression-with-analysis])
        {:keys [staff-width margin-left chord-spacing margin-top]} staff-config]
    [:div.card
     [:div.card-header
      [:h3 "Sheet Music"]]
     [:div.card-body
      (if (empty? chords)
        [:div.empty-state
         [:p "Add chords to see notation"]]

        [:svg.staff
         {:viewBox (str "0 0 " (+ margin-left staff-width 20) " 120")
          :preserveAspectRatio "xMidYMid meet"}

         ;; Treble clef (simplified)
         [:text {:x 10 :y 55 :font-size "50" :font-family "serif"} "\uD834\uDD1E"]

         ;; Staff lines
         [staff-lines]

         ;; Chords
         (for [[idx chord] (map-indexed vector chords)]
           (let [x (+ margin-left 40 (* idx chord-spacing))
                 notes (mapv (fn [n] {:note n :octave 4}) (:notes chord))
                 label (str (name (:root chord))
                            (:symbol (chord-explorer.theory.chords/get-chord-def (:type chord))))]
             ^{:key (:id chord)}
             [chord-stack x notes label]))])]]))
