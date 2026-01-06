(ns chord-explorer.views.keyboard
  "Piano keyboard visualization component."
  (:require [re-frame.core :as rf]
            [chord-explorer.theory.core :as core]
            [chord-explorer.theory.chords :as chords]))

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
;; Chord Tone Colors (matching guitar fretboard)
;; =============================================================================

(def chord-tone-colors
  "Colors for different chord tones."
  {:root "#4f46e5"       ; Indigo - chord root
   :third "#e11d48"      ; Rose - third
   :second "#e11d48"     ; Rose - sus2/sus4
   :fifth "#059669"      ; Emerald - fifth
   :seventh "#d97706"    ; Amber - seventh
   :ninth "#8b5cf6"      ; Violet - ninth
   :eleventh "#06b6d4"   ; Cyan - eleventh
   :thirteenth "#ec4899" ; Pink - thirteenth
   :extension "#8b5cf6"  ; Violet - other extensions
   :scale "#e8f4ea"      ; Light green - scale tone
   :default "#ffffff"})  ; White - default

(defn interval->chord-function
  "Map a semitone interval to its chord function."
  [interval]
  (cond
    (= interval 0) :root
    (#{3 4} interval) :third        ; minor 3rd (3) or major 3rd (4)
    (#{2 5} interval) :second       ; sus2 (2) or sus4 (5)
    (#{6 7 8} interval) :fifth      ; dim5 (6), perfect 5th (7), aug5 (8)
    (#{9 10 11} interval) :seventh  ; dim7 (9), dom7 (10), maj7 (11)
    (#{13 14} interval) :ninth      ; b9 (13), 9 (14)
    (#{15 16 17 18} interval) :eleventh ; 11 (17), #11 (18)
    (#{20 21} interval) :thirteenth ; b13 (20), 13 (21)
    :else :extension))

(defn build-chord-tones-map
  "Build a map of semitone -> chord function for a chord."
  [root chord-type]
  (when-let [chord-def (chords/get-chord-def chord-type)]
    (let [intervals (:intervals chord-def)
          root-semitone (core/normalize-note root)]
      (into {}
            (map (fn [interval]
                   (let [note-semitone (mod (+ root-semitone interval) 12)]
                     [note-semitone (interval->chord-function interval)]))
                 intervals)))))

;; =============================================================================
;; Key Components
;; =============================================================================

(defn white-key
  "Render a white piano key."
  [x note octave chord-tone-type scale-note?]
  (let [{:keys [white-key-width white-key-height]} keyboard-config
        fill-color (cond
                     chord-tone-type (get chord-tone-colors chord-tone-type (:default chord-tone-colors))
                     scale-note? (:scale chord-tone-colors)
                     :else (:default chord-tone-colors))]
    [:rect.white-key
     {:x x
      :y 0
      :width (dec white-key-width)
      :height white-key-height
      :rx 3
      :fill fill-color
      :stroke "#d0c8bc"
      :stroke-width 1}]))

(defn black-key
  "Render a black piano key."
  [x note octave chord-tone-type]
  (let [{:keys [black-key-width black-key-height]} keyboard-config
        fill-color (if chord-tone-type
                     (get chord-tone-colors chord-tone-type "#2d2a26")
                     "#2d2a26")]
    [:rect.black-key
     {:x x
      :y 0
      :width black-key-width
      :height black-key-height
      :rx 2
      :fill fill-color}]))

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
        chord-tones-map (when selected-chord
                          (build-chord-tones-map (:root selected-chord) (:type selected-chord)))
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
             chord-tone-type (when chord-tones-map (get chord-tones-map semitone))
             scale-note? (and scale-set (contains? scale-set semitone))]
         ^{:key (str note octave)}
         [:g
          [white-key x note octave chord-tone-type scale-note?]
          [note-label x note (boolean chord-tone-type)]]))

     ;; Black keys (drawn on top)
     (for [octave (range start-octave (+ start-octave num-octaves))
           [note offset] black-note-offsets]
       (let [octave-offset (* (- octave start-octave) 7 white-key-width)
             x (+ octave-offset offset)
             semitone (core/normalize-note note)
             chord-tone-type (when chord-tones-map (get chord-tones-map semitone))]
         ^{:key (str note octave)}
         [black-key x note octave chord-tone-type]))]))

(defn keyboard-legend
  "Legend for piano keyboard chord tone colors."
  [show-chord-tones?]
  [:div.keyboard-legend
   (if show-chord-tones?
     [:<>
      [:span.legend-item
       [:span.dot {:style {:background-color (:root chord-tone-colors)}}] "Root"]
      [:span.legend-item
       [:span.dot {:style {:background-color (:third chord-tone-colors)}}] "3rd"]
      [:span.legend-item
       [:span.dot {:style {:background-color (:fifth chord-tone-colors)}}] "5th"]
      [:span.legend-item
       [:span.dot {:style {:background-color (:seventh chord-tone-colors)}}] "7th"]
      [:span.legend-item
       [:span.dot {:style {:background-color (:scale chord-tone-colors)}}] "Scale"]]
     [:<>
      [:span.legend-item
       [:span.dot {:style {:background-color (:scale chord-tone-colors)}}] "Scale tone"]])])

(defn keyboard-with-legend
  "Piano keyboard with legend."
  []
  (let [selected-chord @(rf/subscribe [:selected-chord])]
    [:div.keyboard-container
     [keyboard]
     [keyboard-legend (boolean selected-chord)]]))

(defn mini-keyboard
  "Smaller keyboard for voicing display."
  ([notes]
   (mini-keyboard notes nil nil))
  ([notes chord-root chord-type]
   (let [note-set (set (map core/normalize-note notes))
         chord-tones-map (when (and chord-root chord-type)
                           (build-chord-tones-map chord-root chord-type))
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
              in-voicing? (contains? note-set semitone)
              chord-tone-type (when (and in-voicing? chord-tones-map)
                                (get chord-tones-map semitone))
              fill-color (cond
                           chord-tone-type (get chord-tone-colors chord-tone-type "#4f46e5")
                           in-voicing? "#4f46e5"
                           :else "#ffffff")]
          ^{:key (str note)}
          [:rect {:x x
                  :y 0
                  :width (dec white-key-width)
                  :height 40
                  :rx 2
                  :fill fill-color
                  :stroke "#d0c8bc"
                  :stroke-width 0.5}]))

      ;; Black keys
      (for [[note offset] black-note-offsets]
        (let [semitone (core/normalize-note note)
              in-voicing? (contains? note-set semitone)
              chord-tone-type (when (and in-voicing? chord-tones-map)
                                (get chord-tones-map semitone))
              fill-color (cond
                           chord-tone-type (get chord-tone-colors chord-tone-type "#4f46e5")
                           in-voicing? "#4f46e5"
                           :else "#2d2a26")]
          ^{:key (str note)}
          [:rect {:x (* offset 0.5)
                  :y 0
                  :width 9
                  :height 24
                  :rx 1
                  :fill fill-color}]))])))
