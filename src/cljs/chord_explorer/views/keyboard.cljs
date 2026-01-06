(ns chord-explorer.views.keyboard
  "Piano keyboard visualization component."
  (:require [re-frame.core :as rf]
            [chord-explorer.theory.core :as core]))

;; =============================================================================
;; Constants
;; =============================================================================

(def keyboard-config
  {:white-key-width 28
   :white-key-height 100
   :black-key-width 18
   :black-key-height 60
   :num-octaves 2
   :start-octave 4})

(def white-notes [:C :D :E :F :G :A :B])
(def black-note-offsets
  "X offsets for black keys relative to their white key."
  {:C# 20, :D# 48, :F# 92, :G# 120, :A# 148})

;; =============================================================================
;; Key Components
;; =============================================================================

(defn white-key
  "Render a white piano key."
  [x note octave highlighted? scale-note?]
  (let [{:keys [white-key-width white-key-height]} keyboard-config]
    [:rect.white-key
     {:x x
      :y 0
      :width (dec white-key-width)
      :height white-key-height
      :rx 3
      :fill (cond
              highlighted? "#4f46e5"
              scale-note? "#e8f4ea"
              :else "#ffffff")
      :stroke "#d0c8bc"
      :stroke-width 1}]))

(defn black-key
  "Render a black piano key."
  [x note octave highlighted?]
  (let [{:keys [black-key-width black-key-height]} keyboard-config]
    [:rect.black-key
     {:x x
      :y 0
      :width black-key-width
      :height black-key-height
      :rx 2
      :fill (if highlighted? "#4f46e5" "#2d2a26")}]))

(defn note-label
  "Label showing note name under key."
  [x note highlighted?]
  (let [{:keys [white-key-width white-key-height]} keyboard-config]
    [:text {:x (+ x (/ white-key-width 2))
            :y (+ white-key-height 15)
            :text-anchor "middle"
            :font-size "11px"
            :font-family "sans-serif"
            :fill (if highlighted? "#4f46e5" "#5c5650")}
     (name note)]))

;; =============================================================================
;; Keyboard Component
;; =============================================================================

(defn keyboard
  "Piano keyboard visualization."
  []
  (let [selected-chord @(rf/subscribe [:selected-chord])
        scale-notes @(rf/subscribe [:current-scale-notes])
        chord-notes (when selected-chord
                      (set (map core/normalize-note (:notes selected-chord))))
        scale-set (when scale-notes
                    (set (map core/normalize-note scale-notes)))
        {:keys [white-key-width num-octaves start-octave]} keyboard-config
        total-width (* white-key-width 7 num-octaves)]

    [:svg.piano-keyboard
     {:viewBox (str "0 0 " total-width " 120")
      :preserveAspectRatio "xMidYMid meet"}

     ;; White keys
     (for [octave (range start-octave (+ start-octave num-octaves))
           [idx note] (map-indexed vector white-notes)]
       (let [octave-offset (* (- octave start-octave) 7 white-key-width)
             x (+ octave-offset (* idx white-key-width))
             semitone (core/normalize-note note)
             highlighted? (and chord-notes (contains? chord-notes semitone))
             scale-note? (and scale-set (contains? scale-set semitone))]
         ^{:key (str note octave)}
         [:g
          [white-key x note octave highlighted? scale-note?]
          [note-label x note highlighted?]]))

     ;; Black keys (drawn on top)
     (for [octave (range start-octave (+ start-octave num-octaves))
           [note offset] black-note-offsets]
       (let [octave-offset (* (- octave start-octave) 7 white-key-width)
             x (+ octave-offset offset)
             semitone (core/normalize-note note)
             highlighted? (and chord-notes (contains? chord-notes semitone))]
         ^{:key (str note octave)}
         [black-key x note octave highlighted?]))]))

(defn mini-keyboard
  "Smaller keyboard for voicing display."
  [notes]
  (let [note-set (set (map core/normalize-note notes))
        {:keys [white-key-width]} keyboard-config
        total-width (* white-key-width 7)]

    [:svg.mini-keyboard
     {:viewBox (str "0 0 " total-width " 50")
      :width (* total-width 0.5)
      :height 50}

     ;; White keys
     (for [[idx note] (map-indexed vector white-notes)]
       (let [x (* idx white-key-width)
             semitone (core/normalize-note note)
             highlighted? (contains? note-set semitone)]
         ^{:key (str note)}
         [:rect {:x x
                 :y 0
                 :width (dec white-key-width)
                 :height 40
                 :rx 2
                 :fill (if highlighted? "#4f46e5" "#ffffff")
                 :stroke "#d0c8bc"
                 :stroke-width 0.5}]))

     ;; Black keys
     (for [[note offset] black-note-offsets]
       (let [semitone (core/normalize-note note)
             highlighted? (contains? note-set semitone)]
         ^{:key (str note)}
         [:rect {:x (* offset 0.5)
                 :y 0
                 :width 9
                 :height 24
                 :rx 1
                 :fill (if highlighted? "#4f46e5" "#2d2a26")}]))]))
