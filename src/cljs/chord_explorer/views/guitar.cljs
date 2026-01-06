(ns chord-explorer.views.guitar
  "SVG-based guitar chord diagram renderer."
  (:require [re-frame.core :as rf]))

;; =============================================================================
;; Constants
;; =============================================================================

(def diagram-config
  {:width 120
   :height 140
   :string-spacing 18
   :fret-spacing 22
   :margin-top 25
   :margin-left 25
   :num-frets 5
   :num-strings 6
   :dot-radius 6})

;; =============================================================================
;; SVG Components
;; =============================================================================

(defn fret-lines
  "Draw the horizontal fret lines."
  []
  (let [{:keys [margin-left margin-top string-spacing fret-spacing num-frets num-strings]} diagram-config
        width (* (dec num-strings) string-spacing)]
    [:g.fret-lines
     ;; Nut (thick line at top)
     [:line {:x1 margin-left
             :y1 margin-top
             :x2 (+ margin-left width)
             :y2 margin-top
             :stroke "#2d2a26"
             :stroke-width 4}]
     ;; Fret lines
     (for [i (range 1 (inc num-frets))]
       ^{:key i}
       [:line {:x1 margin-left
               :y1 (+ margin-top (* i fret-spacing))
               :x2 (+ margin-left width)
               :y2 (+ margin-top (* i fret-spacing))
               :stroke "#8c8478"
               :stroke-width 1}])]))

(defn string-lines
  "Draw the vertical string lines."
  []
  (let [{:keys [margin-left margin-top string-spacing fret-spacing num-frets num-strings]} diagram-config
        height (* num-frets fret-spacing)]
    [:g.string-lines
     (for [i (range num-strings)]
       ^{:key i}
       [:line {:x1 (+ margin-left (* i string-spacing))
               :y1 margin-top
               :x2 (+ margin-left (* i string-spacing))
               :y2 (+ margin-top height)
               :stroke "#5c5650"
               :stroke-width (+ 1 (* 0.3 i))}])]))

(defn finger-dot
  "Draw a finger position dot."
  [string fret finger is-root?]
  (let [{:keys [margin-left margin-top string-spacing fret-spacing dot-radius]} diagram-config
        ;; String 1 is rightmost (high E), string 6 is leftmost (low E)
        x (+ margin-left (* (- 6 string) string-spacing))
        ;; Position in middle of fret
        y (+ margin-top (* fret fret-spacing) (- (/ fret-spacing 2)))]
    [:g.finger-dot
     ;; Dot
     [:circle {:cx x
               :cy y
               :r dot-radius
               :fill (if is-root? "#4f46e5" "#2d2a26")}]
     ;; Finger number
     (when finger
       [:text {:x x
               :y (+ y 4)
               :text-anchor "middle"
               :font-size "10px"
               :font-family "sans-serif"
               :fill "white"}
        (str finger)])]))

(defn barre-indicator
  "Draw a barre bar."
  [fret from-string to-string]
  (let [{:keys [margin-left margin-top string-spacing fret-spacing dot-radius]} diagram-config
        x1 (+ margin-left (* (- 6 to-string) string-spacing))
        x2 (+ margin-left (* (- 6 from-string) string-spacing))
        y (+ margin-top (* fret fret-spacing) (- (/ fret-spacing 2)))]
    [:rect {:x (- x1 dot-radius)
            :y (- y dot-radius)
            :width (+ (- x2 x1) (* 2 dot-radius))
            :height (* 2 dot-radius)
            :rx dot-radius
            :fill "#2d2a26"}]))

(defn open-string-marker
  "Draw an open string circle."
  [string]
  (let [{:keys [margin-left margin-top string-spacing]} diagram-config
        x (+ margin-left (* (- 6 string) string-spacing))]
    [:circle {:cx x
              :cy (- margin-top 10)
              :r 5
              :fill "none"
              :stroke "#2d2a26"
              :stroke-width 1.5}]))

(defn muted-string-marker
  "Draw a muted string X."
  [string]
  (let [{:keys [margin-left margin-top string-spacing]} diagram-config
        x (+ margin-left (* (- 6 string) string-spacing))
        y (- margin-top 10)]
    [:g
     [:line {:x1 (- x 4) :y1 (- y 4) :x2 (+ x 4) :y2 (+ y 4)
             :stroke "#2d2a26" :stroke-width 2}]
     [:line {:x1 (- x 4) :y1 (+ y 4) :x2 (+ x 4) :y2 (- y 4)
             :stroke "#2d2a26" :stroke-width 2}]]))

(defn position-indicator
  "Show fret position number."
  [position]
  (let [{:keys [margin-left margin-top fret-spacing]} diagram-config]
    [:text {:x (- margin-left 15)
            :y (+ margin-top (/ fret-spacing 2) 4)
            :font-size "12px"
            :font-family "sans-serif"
            :fill "#5c5650"}
     (str position "fr")]))

(defn fingering-row
  "Show fingering numbers below diagram."
  [fingers]
  (let [{:keys [margin-left margin-top string-spacing fret-spacing num-frets]} diagram-config
        y (+ margin-top (* num-frets fret-spacing) 15)]
    [:g.fingering
     (for [[idx finger] (map-indexed vector (reverse fingers))]
       ^{:key idx}
       [:text {:x (+ margin-left (* idx string-spacing))
               :y y
               :text-anchor "middle"
               :font-size "11px"
               :font-family "sans-serif"
               :fill "#5c5650"}
        (if finger (str finger) "")])]))

;; =============================================================================
;; Main Component
;; =============================================================================

(defn chord-diagram
  "Render a guitar chord diagram."
  []
  (let [selected-chord @(rf/subscribe [:selected-chord])
        guitar-voicings @(rf/subscribe [:available-guitar-voicings])
        voicing (first guitar-voicings)]

    (if (and selected-chord voicing)
      (let [{:keys [frets fingers barre position]} voicing
            {:keys [width height margin-top string-spacing fret-spacing num-frets]} diagram-config]

        [:svg.guitar-diagram
         {:viewBox (str "0 0 " width height)
          :preserveAspectRatio "xMidYMid meet"}

         ;; Frets and strings
         [fret-lines]
         [string-lines]

         ;; Position indicator if not at nut
         (when (and position (> position 0))
           [position-indicator position])

         ;; Barre
         (when barre
           [barre-indicator (:fret barre) (:from-string barre) (:to-string barre)])

         ;; Open and muted strings, finger dots
         (for [string (range 1 7)]
           (let [fret (nth frets (- 6 string) -1)
                 finger (when fingers (nth fingers (- 6 string) nil))]
             ^{:key string}
             [:g
              (cond
                (= fret -1) [muted-string-marker string]
                (= fret 0) [open-string-marker string]
                :else [finger-dot string fret finger (= (- 6 string) 0)])]))

         ;; Fingering row
         (when fingers
           [fingering-row fingers])])

      ;; No voicing available
      [:div.empty-state
       [:p "No guitar voicing available"]])))

(defn chord-diagram-mini
  "Smaller version for voicing selector."
  [voicing]
  (let [{:keys [frets fingers barre position]} voicing
        {:keys [string-spacing fret-spacing num-frets]} diagram-config
        scale 0.6
        width (* 120 scale)
        height (* 140 scale)]

    [:svg.guitar-diagram-mini
     {:viewBox "0 0 120 140"
      :width width
      :height height}

     [fret-lines]
     [string-lines]

     (when (and position (> position 0))
       [position-indicator position])

     (when barre
       [barre-indicator (:fret barre) (:from-string barre) (:to-string barre)])

     (for [string (range 1 7)]
       (let [fret (nth frets (- 6 string) -1)]
         ^{:key string}
         [:g
          (cond
            (= fret -1) [muted-string-marker string]
            (= fret 0) [open-string-marker string]
            :else [finger-dot string fret nil false])]))]))
