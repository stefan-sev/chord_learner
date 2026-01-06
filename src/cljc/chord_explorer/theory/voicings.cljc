(ns chord-explorer.theory.voicings
  "Piano/keyboard voicing generation and management."
  (:require [chord-explorer.theory.core :as core]
            [chord-explorer.theory.chords :as chords]))

;; =============================================================================
;; Voicing Types
;; =============================================================================

(def voicing-types
  "Available voicing styles."
  {:close       {:name "Close Position"
                 :description "Notes as close together as possible"
                 :complexity 1}
   :open        {:name "Open Position"
                 :description "Spread across octaves"
                 :complexity 2}
   :drop2       {:name "Drop 2"
                 :description "2nd voice from top dropped an octave"
                 :complexity 3}
   :drop3       {:name "Drop 3"
                 :description "3rd voice from top dropped an octave"
                 :complexity 3}
   :drop2-4     {:name "Drop 2 and 4"
                 :description "2nd and 4th voices dropped an octave"
                 :complexity 4}
   :spread      {:name "Spread"
                 :description "Wide spread voicing"
                 :complexity 3}
   :rootless-a  {:name "Rootless Type A"
                 :description "3-5-7-9 voicing (no root)"
                 :complexity 4}
   :rootless-b  {:name "Rootless Type B"
                 :description "7-9-3-5 voicing (no root)"
                 :complexity 4}
   :shell       {:name "Shell"
                 :description "Root, 3rd, 7th only"
                 :complexity 2}
   :quartal     {:name "Quartal"
                 :description "Stacked 4ths"
                 :complexity 4}})

;; =============================================================================
;; Note with Octave Helpers
;; =============================================================================

(defn- note-with-octave
  "Create a note with octave."
  [note octave]
  {:note note :octave octave})

(defn- notes-to-voiced
  "Convert chord notes to notes with octaves starting from base octave."
  [notes base-octave]
  (loop [result []
         remaining notes
         current-octave base-octave
         prev-semitone nil]
    (if (empty? remaining)
      result
      (let [note (first remaining)
            semitone (core/normalize-note note)
            ;; If semitone is lower than previous, bump up octave
            octave (if (and prev-semitone (< semitone prev-semitone))
                     (inc current-octave)
                     current-octave)]
        (recur (conj result (note-with-octave note octave))
               (rest remaining)
               octave
               semitone)))))

;; =============================================================================
;; Close Position Voicing
;; =============================================================================

(defn close-voicing
  "Generate a close position voicing.
   All notes as close together as possible."
  ([root chord-type]
   (close-voicing root chord-type {:base-octave 4}))
  ([root chord-type {:keys [base-octave] :or {base-octave 4}}]
   (let [notes (chords/build-chord root chord-type)]
     {:root root
      :chord-type chord-type
      :voicing-type :close
      :notes (notes-to-voiced notes base-octave)
      :hand :both})))

;; =============================================================================
;; Drop Voicings
;; =============================================================================

(defn- drop-voice
  "Drop a specific voice (by index from top) down an octave."
  [voiced-notes voice-from-top]
  (let [n (count voiced-notes)
        idx (- n voice-from-top)]
    (when (and (>= idx 0) (< idx n))
      (update-in voiced-notes [idx :octave] dec))))

(defn drop2-voicing
  "Generate a drop 2 voicing.
   Second voice from the top is dropped down an octave."
  ([root chord-type]
   (drop2-voicing root chord-type {:base-octave 4}))
  ([root chord-type {:keys [base-octave] :or {base-octave 4}}]
   (let [notes (chords/build-chord root chord-type)]
     (when (>= (count notes) 4)
       (let [close (notes-to-voiced notes base-octave)
             dropped (drop-voice close 2)]
         {:root root
          :chord-type chord-type
          :voicing-type :drop2
          :notes (vec (sort-by #(core/note->midi %) dropped))
          :hand :both})))))

(defn drop3-voicing
  "Generate a drop 3 voicing.
   Third voice from the top is dropped down an octave."
  ([root chord-type]
   (drop3-voicing root chord-type {:base-octave 4}))
  ([root chord-type {:keys [base-octave] :or {base-octave 4}}]
   (let [notes (chords/build-chord root chord-type)]
     (when (>= (count notes) 4)
       (let [close (notes-to-voiced notes base-octave)
             dropped (drop-voice close 3)]
         {:root root
          :chord-type chord-type
          :voicing-type :drop3
          :notes (vec (sort-by #(core/note->midi %) dropped))
          :hand :both})))))

(defn drop2-4-voicing
  "Generate a drop 2 and 4 voicing."
  ([root chord-type]
   (drop2-4-voicing root chord-type {:base-octave 4}))
  ([root chord-type {:keys [base-octave] :or {base-octave 4}}]
   (let [notes (chords/build-chord root chord-type)]
     (when (>= (count notes) 4)
       (let [close (notes-to-voiced notes base-octave)
             dropped (-> close
                         (drop-voice 2)
                         (drop-voice 4))]
         {:root root
          :chord-type chord-type
          :voicing-type :drop2-4
          :notes (vec (sort-by #(core/note->midi %) dropped))
          :hand :both})))))

;; =============================================================================
;; Shell Voicings
;; =============================================================================

(defn shell-voicing
  "Generate a shell voicing (root, 3rd, 7th only)."
  ([root chord-type]
   (shell-voicing root chord-type {:base-octave 3}))
  ([root chord-type {:keys [base-octave] :or {base-octave 3}}]
   (let [chord-def (chords/get-chord-def chord-type)
         intervals (:intervals chord-def)]
     (when (>= (count intervals) 4)
       (let [;; Root, 3rd, 7th = intervals at positions 0, 1, 3
             shell-intervals [0
                              (nth intervals 1)  ; 3rd
                              (nth intervals 3)] ; 7th
             shell-notes (mapv #(core/transpose root %) shell-intervals)]
         {:root root
          :chord-type chord-type
          :voicing-type :shell
          :notes (notes-to-voiced shell-notes base-octave)
          :hand :left})))))

;; =============================================================================
;; Rootless Voicings
;; =============================================================================

(defn rootless-a-voicing
  "Generate a Type A rootless voicing (3-5-7-9)."
  ([root chord-type]
   (rootless-a-voicing root chord-type {:base-octave 4}))
  ([root chord-type {:keys [base-octave] :or {base-octave 4}}]
   (let [chord-def (chords/get-chord-def chord-type)
         intervals (:intervals chord-def)]
     (when (>= (count intervals) 4)
       ;; 3-5-7-9 = intervals[1], intervals[2], intervals[3], +14 (9th)
       (let [voicing-intervals [(nth intervals 1)      ; 3rd
                                (nth intervals 2)      ; 5th
                                (nth intervals 3)      ; 7th
                                14]                    ; 9th
             voicing-notes (mapv #(core/transpose root %) voicing-intervals)]
         {:root root
          :chord-type chord-type
          :voicing-type :rootless-a
          :notes (notes-to-voiced voicing-notes base-octave)
          :hand :right})))))

(defn rootless-b-voicing
  "Generate a Type B rootless voicing (7-9-3-5)."
  ([root chord-type]
   (rootless-b-voicing root chord-type {:base-octave 3}))
  ([root chord-type {:keys [base-octave] :or {base-octave 3}}]
   (let [chord-def (chords/get-chord-def chord-type)
         intervals (:intervals chord-def)]
     (when (>= (count intervals) 4)
       (let [voicing-intervals [(nth intervals 3)      ; 7th
                                14                     ; 9th
                                (+ 12 (nth intervals 1)) ; 3rd (octave up)
                                (+ 12 (nth intervals 2))] ; 5th (octave up)
             voicing-notes (mapv #(core/transpose root %) voicing-intervals)]
         {:root root
          :chord-type chord-type
          :voicing-type :rootless-b
          :notes (notes-to-voiced voicing-notes base-octave)
          :hand :left})))))

;; =============================================================================
;; Spread Voicing
;; =============================================================================

(defn spread-voicing
  "Generate a spread voicing across a wide range."
  ([root chord-type]
   (spread-voicing root chord-type {:base-octave 2}))
  ([root chord-type {:keys [base-octave] :or {base-octave 2}}]
   (let [notes (chords/build-chord root chord-type)]
     (when (>= (count notes) 3)
       (let [;; Root in bass, distribute others
             voiced (mapv (fn [note idx]
                            {:note note
                             :octave (+ base-octave (quot (* idx 2) (count notes)))})
                          notes
                          (range))]
         {:root root
          :chord-type chord-type
          :voicing-type :spread
          :notes voiced
          :hand :both})))))

;; =============================================================================
;; Quartal Voicing
;; =============================================================================

(defn quartal-voicing
  "Generate a quartal voicing (stacked 4ths)."
  ([root chord-type]
   (quartal-voicing root chord-type {:base-octave 4}))
  ([root chord-type {:keys [base-octave] :or {base-octave 4}}]
   (let [chord-def (chords/get-chord-def chord-type)]
     (when chord-def
       ;; Build stacked 4ths from the 3rd of the chord
       (let [third (core/transpose root 4)  ; Major 3rd
             fourth-intervals [0 5 10 15]   ; Stacked P4s
             quartal-notes (mapv #(core/transpose third %) fourth-intervals)]
         {:root root
          :chord-type chord-type
          :voicing-type :quartal
          :notes (notes-to-voiced quartal-notes base-octave)
          :hand :right})))))

;; =============================================================================
;; Voicing Generation
;; =============================================================================

(defn generate-voicing
  "Generate a voicing of a specific type."
  [root chord-type voicing-type & [opts]]
  (let [opts (or opts {})]
    (case voicing-type
      :close (close-voicing root chord-type opts)
      :drop2 (drop2-voicing root chord-type opts)
      :drop3 (drop3-voicing root chord-type opts)
      :drop2-4 (drop2-4-voicing root chord-type opts)
      :shell (shell-voicing root chord-type opts)
      :rootless-a (rootless-a-voicing root chord-type opts)
      :rootless-b (rootless-b-voicing root chord-type opts)
      :spread (spread-voicing root chord-type opts)
      :quartal (quartal-voicing root chord-type opts)
      nil)))

(defn get-all-voicings
  "Get all available voicings for a chord."
  [root chord-type]
  (filterv some?
           (map #(generate-voicing root chord-type %)
                (keys voicing-types))))

(defn get-voicings-by-complexity
  "Get voicings filtered by complexity level (1-4)."
  [root chord-type max-complexity]
  (let [all-voicings (get-all-voicings root chord-type)]
    (filterv (fn [v]
               (let [vt (:voicing-type v)
                     complexity (get-in voicing-types [vt :complexity] 1)]
                 (<= complexity max-complexity)))
             all-voicings)))

;; =============================================================================
;; Voice Leading
;; =============================================================================

(defn voice-leading-distance
  "Calculate total voice leading distance between two voicings."
  [voicing1 voicing2]
  (let [midi1 (sort (map core/note->midi (:notes voicing1)))
        midi2 (sort (map core/note->midi (:notes voicing2)))
        n (min (count midi1) (count midi2))]
    (reduce + (map (fn [m1 m2]
                     (Math/abs (- m1 m2)))
                   (take n midi1)
                   (take n midi2)))))

(defn find-closest-voicing
  "Find the voicing of chord2 that has minimal voice leading from voicing1."
  [voicing1 root2 chord-type2]
  (let [all-voicings (get-all-voicings root2 chord-type2)]
    (when (seq all-voicings)
      (apply min-key
             #(voice-leading-distance voicing1 %)
             all-voicings))))

(defn voice-lead-progression
  "Generate voice-led voicings for a chord progression.
   Takes a vector of {:root :type} maps."
  [chords]
  (when (seq chords)
    (loop [result [(close-voicing (:root (first chords)) (:type (first chords)))]
           remaining (rest chords)]
      (if (empty? remaining)
        result
        (let [prev-voicing (last result)
              next-chord (first remaining)
              next-voicing (find-closest-voicing
                            prev-voicing
                            (:root next-chord)
                            (:type next-chord))]
          (recur (conj result (or next-voicing
                                  (close-voicing (:root next-chord)
                                                 (:type next-chord))))
                 (rest remaining)))))))

;; =============================================================================
;; Inversion Detection
;; =============================================================================

(defn detect-inversion
  "Detect the inversion of a voicing based on the bass note.
   Returns :root, :first, :second, :third, or nil."
  [voicing]
  (when voicing
    (let [root (:root voicing)
          chord-type (:chord-type voicing)
          voiced-notes (:notes voicing)
          chord-def (chords/get-chord-def chord-type)
          intervals (:intervals chord-def)]
      (when (and (seq voiced-notes) (seq intervals))
        (let [;; Get the bass note (lowest in the voicing)
              bass-note (:note (first (sort-by #(core/note->midi %) voiced-notes)))
              bass-semitone (core/normalize-note bass-note)
              root-semitone (core/normalize-note root)
              ;; Calculate interval from root to bass
              bass-interval (mod (- bass-semitone root-semitone) 12)
              ;; Map intervals to inversion names
              third-intervals #{3 4}       ; minor 3rd, major 3rd
              fifth-intervals #{6 7 8}     ; dim5, P5, aug5
              seventh-intervals #{9 10 11}] ; dim7, dom7, maj7
          (cond
            (= bass-interval 0) :root
            (third-intervals bass-interval) :first
            (fifth-intervals bass-interval) :second
            (and (>= (count intervals) 4)
                 (seventh-intervals bass-interval)) :third
            :else :root))))))

(def inversion-names
  "Display names for inversions."
  {:root "Root Position"
   :first "1st Inversion"
   :second "2nd Inversion"
   :third "3rd Inversion"})

(def inversion-short-names
  "Short display names for inversions."
  {:root "Root"
   :first "1st Inv"
   :second "2nd Inv"
   :third "3rd Inv"})

;; =============================================================================
;; Inversion Generation
;; =============================================================================

(defn- rotate-notes
  "Rotate chord notes to create an inversion."
  [notes n]
  (let [notes-vec (vec notes)
        count-notes (count notes-vec)]
    (vec (concat (drop n notes-vec) (take n notes-vec)))))

(defn generate-inversions
  "Generate all inversions for a chord.
   Returns a vector of voicings, one for each inversion."
  ([root chord-type]
   (generate-inversions root chord-type {:base-octave 4}))
  ([root chord-type {:keys [base-octave] :or {base-octave 4}}]
   (let [notes (chords/build-chord root chord-type)
         num-notes (count notes)]
     (when (>= num-notes 3)
       (vec
        (for [inv-num (range num-notes)]
          (let [rotated (rotate-notes notes inv-num)
                voiced (notes-to-voiced rotated base-octave)
                inversion (case inv-num
                            0 :root
                            1 :first
                            2 :second
                            3 :third
                            :root)]
            {:root root
             :chord-type chord-type
             :voicing-type :close
             :notes voiced
             :inversion inversion
             :inversion-num inv-num
             :hand :both})))))))

;; =============================================================================
;; Voicing Display
;; =============================================================================

(defn voicing->display
  "Convert a voicing to display information."
  [voicing]
  (when voicing
    (let [inversion (or (:inversion voicing) (detect-inversion voicing))]
      {:name (get-in voicing-types [(:voicing-type voicing) :name])
       :description (get-in voicing-types [(:voicing-type voicing) :description])
       :complexity (get-in voicing-types [(:voicing-type voicing) :complexity])
       :notes (mapv (fn [{:keys [note octave]}]
                      (str (name note) octave))
                    (:notes voicing))
       :hand (:hand voicing)
       :inversion inversion
       :inversion-name (get inversion-names inversion)
       :inversion-short (get inversion-short-names inversion)})))
