(ns chord-explorer.theory.guitar
  "Guitar-specific chord voicings and fretboard logic."
  (:require [chord-explorer.theory.core :as core]
            [chord-explorer.theory.chords :as chords]))

;; =============================================================================
;; Tuning Definitions
;; =============================================================================

(def tunings
  "Guitar tuning definitions (low to high string)."
  {:standard     [:E :A :D :G :B :E]
   :drop-d       [:D :A :D :G :B :E]
   :dadgad       [:D :A :D :G :A :D]
   :open-g       [:D :G :D :G :B :D]
   :open-d       [:D :A :D :F# :A :D]
   :open-e       [:E :B :E :G# :B :E]
   :open-a       [:E :A :E :A :C# :E]
   :half-step-down [:D# :G# :C# :F# :A# :D#]})

(def standard-tuning (:standard tunings))

;; =============================================================================
;; Fretboard Logic
;; =============================================================================

(defn string-note-at-fret
  "Get the note at a specific fret on a string."
  [open-note fret]
  (when (>= fret 0)
    (core/transpose open-note fret)))

(defn fret-to-note
  "Get the note at a fret position.
   String is 1-indexed (1 = high E, 6 = low E in standard)."
  ([string fret]
   (fret-to-note string fret standard-tuning))
  ([string fret tuning]
   (let [;; Reverse tuning so string 1 = high E
         open-notes (vec (reverse tuning))
         open-note (nth open-notes (dec string) nil)]
     (when open-note
       (string-note-at-fret open-note fret)))))

(defn note-to-frets
  "Find all fret positions for a note on the fretboard.
   Returns a vector of {:string :fret} maps."
  ([note]
   (note-to-frets note standard-tuning 12))
  ([note tuning]
   (note-to-frets note tuning 12))
  ([note tuning max-fret]
   (let [target-semitone (core/normalize-note note)
         open-notes (vec (reverse tuning))]  ; String 1 = highest
     (for [string (range 1 (inc (count open-notes)))
           fret (range 0 (inc max-fret))
           :let [open-note (nth open-notes (dec string))
                 fret-note (string-note-at-fret open-note fret)]
           :when (= (core/normalize-note fret-note) target-semitone)]
       {:string string :fret fret}))))

;; =============================================================================
;; Guitar Voicing Data Structure
;; =============================================================================

(defn create-voicing
  "Create a guitar voicing.
   - frets: vector of fret numbers (1-6, low to high). Use -1 for muted, 0 for open.
   - fingers: optional fingering (1-4 for fingers, nil for open/muted)
   - barre: optional {:fret :from-string :to-string}
   - category: :open, :barre, :jazz, :partial"
  [{:keys [root chord-type frets fingers barre category position difficulty name]
    :or {position 0 difficulty :medium category :open}}]
  {:root root
   :chord-type chord-type
   :name (or name (str (clojure.core/name root)
                       (:symbol (chords/get-chord-def chord-type))))
   :frets (vec frets)
   :fingers fingers
   :barre barre
   :position position
   :difficulty difficulty
   :category category})

;; =============================================================================
;; Built-in Chord Voicing Database
;; =============================================================================

(def ^:private chord-shapes
  "Core chord shape templates. Positions are relative to root."
  {;; CAGED Major Shapes (for transposable shapes)
   :e-shape-major   {:frets [0 2 2 1 0 0] :root-string 6 :fingers [nil 2 3 1 nil nil]}
   :a-shape-major   {:frets [-1 0 2 2 2 0] :root-string 5 :fingers [nil nil 1 2 3 nil]}
   :d-shape-major   {:frets [-1 -1 0 2 3 2] :root-string 4 :fingers [nil nil nil 1 3 2]}
   :c-shape-major   {:frets [-1 3 2 0 1 0] :root-string 5 :fingers [nil 3 2 nil 1 nil]}
   :g-shape-major   {:frets [3 2 0 0 0 3] :root-string 6 :fingers [2 1 nil nil nil 3]}

   ;; E-shape Minor
   :e-shape-minor   {:frets [0 2 2 0 0 0] :root-string 6 :fingers [nil 2 3 nil nil nil]}
   :a-shape-minor   {:frets [-1 0 2 2 1 0] :root-string 5 :fingers [nil nil 2 3 1 nil]}

   ;; Seventh chord shapes
   :e-shape-7       {:frets [0 2 0 1 0 0] :root-string 6}
   :a-shape-7       {:frets [-1 0 2 0 2 0] :root-string 5}
   :e-shape-maj7    {:frets [0 2 1 1 0 0] :root-string 6}
   :a-shape-maj7    {:frets [-1 0 2 1 2 0] :root-string 5}
   :e-shape-min7    {:frets [0 2 0 0 0 0] :root-string 6}
   :a-shape-min7    {:frets [-1 0 2 0 1 0] :root-string 5}})

;; Database of guitar chord voicings.
(defonce guitar-voicing-db (atom {}))

(defn- init-voicing-db!
  "Initialize the voicing database with common chords."
  []
  (reset! guitar-voicing-db
          {;; C Major voicings
           [:C :major]
           [(create-voicing {:root :C :chord-type :major
                             :frets [-1 3 2 0 1 0]
                             :fingers [nil 3 2 nil 1 nil]
                             :category :open :difficulty :easy
                             :name "C Major (Open)"})
            (create-voicing {:root :C :chord-type :major
                             :frets [8 10 10 9 8 8]
                             :fingers [1 3 4 2 1 1]
                             :barre {:fret 8 :from-string 1 :to-string 6}
                             :category :barre :position 8 :difficulty :medium
                             :name "C Major (Barre - E shape)"})]

           ;; D Major voicings
           [:D :major]
           [(create-voicing {:root :D :chord-type :major
                             :frets [-1 -1 0 2 3 2]
                             :fingers [nil nil nil 1 3 2]
                             :category :open :difficulty :easy
                             :name "D Major (Open)"})
            (create-voicing {:root :D :chord-type :major
                             :frets [-1 5 7 7 7 5]
                             :fingers [nil 1 3 3 3 1]
                             :barre {:fret 5 :from-string 1 :to-string 5}
                             :category :barre :position 5 :difficulty :medium
                             :name "D Major (Barre - A shape)"})]

           ;; E Major voicings
           [:E :major]
           [(create-voicing {:root :E :chord-type :major
                             :frets [0 2 2 1 0 0]
                             :fingers [nil 2 3 1 nil nil]
                             :category :open :difficulty :easy
                             :name "E Major (Open)"})]

           ;; G Major voicings
           [:G :major]
           [(create-voicing {:root :G :chord-type :major
                             :frets [3 2 0 0 0 3]
                             :fingers [2 1 nil nil nil 3]
                             :category :open :difficulty :easy
                             :name "G Major (Open)"})
            (create-voicing {:root :G :chord-type :major
                             :frets [3 2 0 0 3 3]
                             :fingers [2 1 nil nil 3 4]
                             :category :open :difficulty :easy
                             :name "G Major (Open - Alt)"})]

           ;; A Major voicings
           [:A :major]
           [(create-voicing {:root :A :chord-type :major
                             :frets [-1 0 2 2 2 0]
                             :fingers [nil nil 1 2 3 nil]
                             :category :open :difficulty :easy
                             :name "A Major (Open)"})
            (create-voicing {:root :A :chord-type :major
                             :frets [5 7 7 6 5 5]
                             :fingers [1 3 4 2 1 1]
                             :barre {:fret 5 :from-string 1 :to-string 6}
                             :category :barre :position 5 :difficulty :medium
                             :name "A Major (Barre - E shape)"})]

           ;; F Major voicings
           [:F :major]
           [(create-voicing {:root :F :chord-type :major
                             :frets [1 3 3 2 1 1]
                             :fingers [1 3 4 2 1 1]
                             :barre {:fret 1 :from-string 1 :to-string 6}
                             :category :barre :position 1 :difficulty :medium
                             :name "F Major (Barre)"})
            (create-voicing {:root :F :chord-type :major
                             :frets [-1 -1 3 2 1 1]
                             :fingers [nil nil 3 2 1 1]
                             :barre {:fret 1 :from-string 1 :to-string 2}
                             :category :partial :position 1 :difficulty :easy
                             :name "F Major (Partial)"})]

           ;; A minor voicings
           [:A :minor]
           [(create-voicing {:root :A :chord-type :minor
                             :frets [-1 0 2 2 1 0]
                             :fingers [nil nil 2 3 1 nil]
                             :category :open :difficulty :easy
                             :name "Am (Open)"})]

           ;; E minor voicings
           [:E :minor]
           [(create-voicing {:root :E :chord-type :minor
                             :frets [0 2 2 0 0 0]
                             :fingers [nil 2 3 nil nil nil]
                             :category :open :difficulty :easy
                             :name "Em (Open)"})]

           ;; D minor voicings
           [:D :minor]
           [(create-voicing {:root :D :chord-type :minor
                             :frets [-1 -1 0 2 3 1]
                             :fingers [nil nil nil 2 3 1]
                             :category :open :difficulty :easy
                             :name "Dm (Open)"})]

           ;; G7 voicings
           [:G :7]
           [(create-voicing {:root :G :chord-type :7
                             :frets [3 2 0 0 0 1]
                             :fingers [3 2 nil nil nil 1]
                             :category :open :difficulty :easy
                             :name "G7 (Open)"})]

           ;; C7 voicings
           [:C :7]
           [(create-voicing {:root :C :chord-type :7
                             :frets [-1 3 2 3 1 0]
                             :fingers [nil 3 2 4 1 nil]
                             :category :open :difficulty :medium
                             :name "C7 (Open)"})]

           ;; Cmaj7 voicings
           [:C :maj7]
           [(create-voicing {:root :C :chord-type :maj7
                             :frets [-1 3 2 0 0 0]
                             :fingers [nil 3 2 nil nil nil]
                             :category :open :difficulty :easy
                             :name "Cmaj7 (Open)"})]

           ;; Dm7 voicings
           [:D :min7]
           [(create-voicing {:root :D :chord-type :min7
                             :frets [-1 -1 0 2 1 1]
                             :fingers [nil nil nil 2 1 1]
                             :category :open :difficulty :easy
                             :name "Dm7 (Open)"})]

           ;; Am7 voicings
           [:A :min7]
           [(create-voicing {:root :A :chord-type :min7
                             :frets [-1 0 2 0 1 0]
                             :fingers [nil nil 2 nil 1 nil]
                             :category :open :difficulty :easy
                             :name "Am7 (Open)"})]}))

;; Initialize on load
(init-voicing-db!)

;; =============================================================================
;; Voicing Lookup Functions
;; =============================================================================

(defn get-guitar-voicings
  "Get all guitar voicings for a chord."
  [root chord-type]
  (get @guitar-voicing-db [root chord-type] []))

(defn get-voicings-by-difficulty
  "Get voicings filtered by difficulty."
  [root chord-type difficulty]
  (filterv #(= difficulty (:difficulty %))
           (get-guitar-voicings root chord-type)))

(defn get-voicings-by-position
  "Get voicings near a fret position."
  [root chord-type position tolerance]
  (filterv #(<= (Math/abs (- (:position %) position)) tolerance)
           (get-guitar-voicings root chord-type)))

(defn get-voicings-by-category
  "Get voicings by category (:open, :barre, :jazz, :partial)."
  [root chord-type category]
  (filterv #(= category (:category %))
           (get-guitar-voicings root chord-type)))

(defn get-open-voicings
  "Get only open position voicings."
  [root chord-type]
  (get-voicings-by-category root chord-type :open))

;; =============================================================================
;; Shape Transposition
;; =============================================================================

(defn transpose-shape
  "Transpose a chord shape up the neck by semitones.
   Only works for barre/moveable shapes."
  [voicing semitones]
  (when (= :barre (:category voicing))
    (let [new-position (+ (:position voicing) semitones)
          new-root (core/transpose (:root voicing) semitones)]
      (-> voicing
          (assoc :position new-position)
          (assoc :root new-root)
          (update :frets (fn [frets]
                           (mapv #(if (>= % 0) (+ % semitones) %) frets)))
          (update :barre (fn [b]
                           (when b
                             (update b :fret + semitones))))
          (assoc :name (str (name new-root)
                            (:symbol (chords/get-chord-def (:chord-type voicing)))
                            " (Barre)"))))))

(defn generate-barre-voicing
  "Generate a barre chord voicing from a shape template."
  [root chord-type shape-key]
  (when-let [shape (get chord-shapes shape-key)]
    (let [;; Find how many semitones to transpose
          root-semitone (core/normalize-note root)
          shape-root-string (:root-string shape)
          ;; For E-shape, open = E (semitone 4)
          ;; For A-shape, open = A (semitone 9)
          open-root-semitone (case shape-root-string
                               6 4   ; E
                               5 9   ; A
                               4 2   ; D
                               0)
          semitones (mod (- root-semitone open-root-semitone) 12)
          frets (mapv #(if (>= % 0) (+ % semitones) %) (:frets shape))]
      (create-voicing
       {:root root
        :chord-type chord-type
        :frets frets
        :fingers (:fingers shape)
        :barre (when (pos? semitones)
                 {:fret semitones :from-string 1 :to-string 6})
        :category (if (pos? semitones) :barre :open)
        :position semitones
        :difficulty (if (pos? semitones) :medium :easy)}))))

;; =============================================================================
;; Voicing Analysis
;; =============================================================================

(defn voicing-notes
  "Get the actual notes played in a voicing."
  ([voicing]
   (voicing-notes voicing standard-tuning))
  ([voicing tuning]
   (let [open-notes (vec tuning)]  ; Low to high: E A D G B E
     (keep-indexed
      (fn [string-idx fret]
        (when (>= fret 0)
          (let [open-note (nth open-notes string-idx)]
            {:string (inc string-idx)
             :fret fret
             :note (string-note-at-fret open-note fret)})))
      (:frets voicing)))))

(defn calculate-stretch
  "Calculate the finger stretch required for a voicing."
  [voicing]
  (let [frets (filter pos? (:frets voicing))]
    (if (seq frets)
      (- (apply max frets) (apply min frets))
      0)))

(defn playability-score
  "Calculate a playability score (lower is easier).
   Considers stretch, barre, position."
  [voicing]
  (let [stretch (calculate-stretch voicing)
        barre-penalty (if (:barre voicing) 2 0)
        position-penalty (/ (:position voicing) 4)]
    (+ stretch barre-penalty position-penalty)))

;; =============================================================================
;; Voicing Suggestions
;; =============================================================================

(defn suggest-voicing-sequence
  "Suggest voicings for a chord progression that minimize position changes."
  [chords]
  (when (seq chords)
    (loop [result []
           remaining chords
           last-position 0]
      (if (empty? remaining)
        result
        (let [{:keys [root type]} (first remaining)
              voicings (get-guitar-voicings root type)
              ;; Prefer voicings close to last position
              sorted (sort-by #(Math/abs (- (:position %) last-position))
                              voicings)
              best (or (first sorted)
                       (create-voicing {:root root
                                        :chord-type type
                                        :frets [-1 -1 -1 -1 -1 -1]
                                        :category :unknown}))]
          (recur (conj result best)
                 (rest remaining)
                 (:position best)))))))

;; =============================================================================
;; Voicing Registration
;; =============================================================================

(defn register-voicing!
  "Add a custom voicing to the database."
  [root chord-type voicing-data]
  (let [voicing (create-voicing (assoc voicing-data :root root :chord-type chord-type))]
    (swap! guitar-voicing-db update [root chord-type]
           (fn [existing]
             (conj (or existing []) voicing)))))

(defn clear-voicings!
  "Clear all voicings for a chord."
  [root chord-type]
  (swap! guitar-voicing-db dissoc [root chord-type]))

;; =============================================================================
;; Display Helpers
;; =============================================================================

(defn voicing->diagram-data
  "Convert a voicing to data suitable for SVG rendering."
  [voicing]
  {:name (:name voicing)
   :frets (:frets voicing)
   :fingers (:fingers voicing)
   :barre (:barre voicing)
   :position (:position voicing)
   :difficulty (:difficulty voicing)
   :muted (mapv #(= % -1) (:frets voicing))
   :open (mapv #(= % 0) (:frets voicing))})
