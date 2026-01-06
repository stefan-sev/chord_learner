(ns chord-explorer.theory.guitar
  "Guitar-specific chord voicings and fretboard logic."
  (:require [chord-explorer.theory.core :as core]
            [chord-explorer.theory.chords :as chords]))

;; Forward declaration for functions used before definition
(declare detect-guitar-inversion)

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
;; Combinatorics Helpers for Voicing Generation
;; =============================================================================

(defn- combinations
  "Generate all combinations of n items from coll."
  [coll n]
  (cond
    (zero? n) [[]]
    (empty? coll) []
    :else (concat
           (map #(cons (first coll) %)
                (combinations (rest coll) (dec n)))
           (combinations (rest coll) n))))

(defn- permutations
  "Generate all permutations of coll."
  [coll]
  (if (empty? coll)
    [[]]
    (for [x coll
          p (permutations (remove #{x} coll))]
      (cons x p))))

(defn- cartesian-product
  "Generate cartesian product of multiple collections."
  [& colls]
  (if (empty? colls)
    [[]]
    (for [x (first colls)
          xs (apply cartesian-product (rest colls))]
      (cons x xs))))

;; =============================================================================
;; String Combination Generator
;; =============================================================================

(def ^:private all-strings [1 2 3 4 5 6])

(defn- valid-string-combinations
  "Generate all valid 3-4 string combinations for voicings.
   Returns list of sets like #{1 2 3}, #{1 2 4}, etc."
  []
  (concat
   (map set (combinations all-strings 3))
   (map set (combinations all-strings 4))))

;; Pre-compute string combinations for performance
(def ^:private string-combos (valid-string-combinations))

;; =============================================================================
;; Chord Tone Position Finder
;; =============================================================================

(defn- chord-tone-positions
  "Find all fret positions for each chord tone.
   Returns map: {interval -> [{:string :fret :interval} ...]}"
  [root intervals tuning max-fret]
  (into {}
        (for [interval intervals]
          (let [note (core/transpose root interval)
                positions (note-to-frets note tuning max-fret)]
            [interval (mapv #(assoc % :interval interval) positions)]))))

;; =============================================================================
;; Voicing Generation Algorithm
;; =============================================================================

(def ^:private max-stretch 5)
(def ^:private max-fret 12)

(defn- playable?
  "Check if a voicing meets playability constraints."
  [fret-positions]
  (let [frets (map :fret fret-positions)
        non-open (filter pos? frets)]
    (and
     ;; Must have positions
     (seq fret-positions)
     ;; All frets within range
     (every? #(<= 0 % max-fret) frets)
     ;; Max stretch constraint (only for fretted notes)
     (or (empty? non-open)
         (<= (- (apply max non-open) (apply min non-open)) max-stretch))
     ;; Max 4 fretted notes
     (<= (count non-open) 4))))

(defn- calculate-voicing-difficulty
  "Assign difficulty based on voicing characteristics."
  [fret-positions]
  (let [frets (map :fret fret-positions)
        non-open (filter pos? frets)
        has-open? (some zero? frets)
        stretch (if (seq non-open)
                  (- (apply max non-open) (apply min non-open))
                  0)
        min-fret (if (seq non-open) (apply min non-open) 0)]
    (cond
      (and (< stretch 2) has-open?) :easy
      (and (<= stretch 3) (<= min-fret 5)) :medium
      :else :hard)))

(defn- determine-voicing-category
  "Categorize voicing as :open, :barre, :partial, or :jazz"
  [frets-vec position]
  (let [played (filter #(>= % 0) frets-vec)
        has-open? (some zero? played)
        num-muted (count (filter #(= % -1) frets-vec))]
    (cond
      (and has-open? (= position 0)) :open
      (>= num-muted 3) :partial
      (> position 0) :barre
      :else :jazz)))

(defn- generate-voicing-name
  "Generate a descriptive name for a voicing."
  [root chord-type position inversion]
  (let [chord-def (chords/get-chord-def chord-type)
        symbol-str (:symbol chord-def)
        inv-suffix (case inversion
                     :first "/3"
                     :second "/5"
                     :third "/7"
                     "")]
    (str (name root) symbol-str inv-suffix
         (when (pos? position) (str " (Pos " position ")")))))

(defn- fret-positions->voicing-struct
  "Convert fret positions to the voicing format expected by the UI."
  [root chord-type fret-positions tuning]
  (let [;; Build 6-element frets vector: -1 for muted, fret number for played
        ;; fret-positions have :string (1-6, 1=high E) and we need index (0=low E)
        ;; String 1 = high E = index 5, String 6 = low E = index 0
        frets-vec (reduce
                   (fn [acc {:keys [string fret]}]
                     (assoc acc (- 6 string) fret))
                   [-1 -1 -1 -1 -1 -1]
                   fret-positions)
        played-frets (filter pos? (map :fret fret-positions))
        position (if (seq played-frets) (apply min played-frets) 0)
        difficulty (calculate-voicing-difficulty fret-positions)
        category (determine-voicing-category frets-vec position)
        ;; Create a temporary voicing to detect inversion
        temp-voicing {:root root :chord-type chord-type :frets frets-vec}
        inversion (detect-guitar-inversion temp-voicing tuning)]
    {:root root
     :chord-type chord-type
     :name (generate-voicing-name root chord-type position inversion)
     :frets frets-vec
     :fingers nil
     :barre nil
     :position position
     :difficulty difficulty
     :category category}))

(defn- generate-voicings-for-string-set
  "Generate all valid voicings for a specific string combination."
  [root chord-type intervals string-set tone-positions tuning]
  (let [strings (vec (sort string-set))
        num-strings (count strings)
        num-tones (count intervals)]
    (when (= num-strings num-tones)
      ;; Generate all permutations of intervals to strings
      (for [perm (permutations intervals)
            :let [;; For each (string, interval) assignment, get valid fret positions
                  fret-options (map (fn [string interval]
                                      (filter #(= (:string %) string)
                                              (get tone-positions interval)))
                                    strings perm)]
            ;; Only proceed if all strings have at least one option
            :when (every? seq fret-options)
            ;; Generate cartesian product of all valid positions
            voicing-positions (apply cartesian-product fret-options)
            :when (playable? voicing-positions)]
        (fret-positions->voicing-struct root chord-type voicing-positions tuning)))))

(defn- select-essential-tones
  "For chords with >4 notes, select the most important tones.
   Priority: root > 3rd > 7th > 5th > extensions"
  [intervals num-tones]
  (let [prioritized (sort-by
                     (fn [i]
                       (cond
                         (= i 0) 0       ; root - highest priority
                         (#{3 4} i) 1    ; 3rd
                         (#{10 11} i) 2  ; 7th
                         (#{6 7 8} i) 3  ; 5th
                         :else 4))       ; extensions
                     intervals)]
    (take num-tones prioritized)))

(defn- generate-all-voicings
  "Generate all valid voicings for a chord."
  ([root chord-type]
   (generate-all-voicings root chord-type standard-tuning max-fret))
  ([root chord-type tuning search-max-fret]
   (let [chord-def (chords/get-chord-def chord-type)]
     (when chord-def
       (let [intervals (:intervals chord-def)
             num-tones (count intervals)]
         (cond
           ;; Not enough notes
           (< num-tones 2) []

           ;; Dyads (2 notes) - use 2-string combinations
           (= num-tones 2)
           (let [tone-positions (chord-tone-positions root intervals tuning search-max-fret)
                 string-combos-2 (map set (combinations all-strings 2))]
             (->> string-combos-2
                  (mapcat #(generate-voicings-for-string-set root chord-type intervals % tone-positions tuning))
                  (remove nil?)
                  vec))

           ;; Standard triads and 7th chords (3-4 notes)
           (<= num-tones 4)
           (let [tone-positions (chord-tone-positions root intervals tuning search-max-fret)
                 ;; Use string combinations that match the number of tones
                 matching-combos (filter #(= (count %) num-tones) string-combos)]
             (->> matching-combos
                  (mapcat #(generate-voicings-for-string-set root chord-type intervals % tone-positions tuning))
                  (remove nil?)
                  vec))

           ;; Extended chords (>4 notes) - select essential tones
           :else
           (let [;; Generate voicings for both 3 and 4 essential tones
                 results-3 (let [essential (select-essential-tones intervals 3)
                                 tone-positions (chord-tone-positions root essential tuning search-max-fret)
                                 matching-combos (filter #(= (count %) 3) string-combos)]
                             (->> matching-combos
                                  (mapcat #(generate-voicings-for-string-set root chord-type essential % tone-positions tuning))
                                  (remove nil?)))
                 results-4 (let [essential (select-essential-tones intervals 4)
                                 tone-positions (chord-tone-positions root essential tuning search-max-fret)
                                 matching-combos (filter #(= (count %) 4) string-combos)]
                             (->> matching-combos
                                  (mapcat #(generate-voicings-for-string-set root chord-type essential % tone-positions tuning))
                                  (remove nil?)))]
             (vec (concat results-3 results-4)))))))))

(defn- sort-voicings
  "Sort voicings by position, difficulty, then inversion."
  [voicings]
  (sort-by (juxt :position
                 #(case (:difficulty %) :easy 0 :medium 1 :hard 2)
                 #(case (detect-guitar-inversion %) :root 0 :first 1 :second 2 :third 3))
           voicings))

(defn- dedupe-voicings
  "Remove duplicate voicings based on fret positions."
  [voicings]
  (vals (reduce (fn [acc v]
                  (let [key (:frets v)]
                    (if (contains? acc key)
                      acc
                      (assoc acc key v))))
                {}
                voicings)))

;; Memoize the voicing generation for performance
(def ^:private generate-voicings-memo
  (memoize
   (fn [root chord-type]
     (->> (generate-all-voicings root chord-type)
          dedupe-voicings
          sort-voicings
          (take 25)
          vec))))

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
;; Voicing Lookup Functions
;; =============================================================================

(defn get-guitar-voicings
  "Get all guitar voicings for a chord - algorithmically generated."
  [root chord-type]
  (generate-voicings-memo root chord-type))

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
;; Inversion Detection
;; =============================================================================

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

(defn detect-guitar-inversion
  "Detect the inversion of a guitar voicing based on the bass note.
   Returns :root, :first, :second, :third, or nil."
  ([voicing]
   (detect-guitar-inversion voicing standard-tuning))
  ([voicing tuning]
   (when voicing
     (let [root (:root voicing)
           chord-type (:chord-type voicing)
           frets (:frets voicing)
           chord-def (chords/get-chord-def chord-type)
           intervals (:intervals chord-def)]
       (when (and (seq frets) (seq intervals))
         (let [;; Find the bass note (lowest string with a fret >= 0)
               ;; Strings are stored low to high (E A D G B E)
               bass-string-idx (first (keep-indexed
                                        (fn [idx fret]
                                          (when (>= fret 0) idx))
                                        frets))
               bass-fret (when bass-string-idx (nth frets bass-string-idx))
               ;; Get the open note for that string
               open-note (when bass-string-idx (nth tuning bass-string-idx))
               bass-note (when open-note (string-note-at-fret open-note bass-fret))
               ;; Calculate interval from root to bass
               bass-semitone (when bass-note (core/normalize-note bass-note))
               root-semitone (core/normalize-note root)
               bass-interval (when bass-semitone (mod (- bass-semitone root-semitone) 12))
               ;; Map intervals to inversion names
               third-intervals #{3 4}       ; minor 3rd, major 3rd
               fifth-intervals #{6 7 8}     ; dim5, P5, aug5
               seventh-intervals #{9 10 11}] ; dim7, dom7, maj7
           (cond
             (nil? bass-interval) :root
             (= bass-interval 0) :root
             (third-intervals bass-interval) :first
             (fifth-intervals bass-interval) :second
             (and (>= (count intervals) 4)
                  (seventh-intervals bass-interval)) :third
             :else :root)))))))

;; =============================================================================
;; Display Helpers
;; =============================================================================

(defn voicing->diagram-data
  "Convert a voicing to data suitable for SVG rendering."
  [voicing]
  (let [inversion (detect-guitar-inversion voicing)]
    {:name (:name voicing)
     :frets (:frets voicing)
     :fingers (:fingers voicing)
     :barre (:barre voicing)
     :position (:position voicing)
     :difficulty (:difficulty voicing)
     :muted (mapv #(= % -1) (:frets voicing))
     :open (mapv #(= % 0) (:frets voicing))
     :inversion inversion
     :inversion-name (get inversion-names inversion)
     :inversion-short (get inversion-short-names inversion)}))

;; =============================================================================
;; Scale Fretboard Functions
;; =============================================================================

(defn scale-notes-on-fretboard
  "Get all positions of scale notes on the fretboard.
   Returns a sequence of {:string :fret :note :degree :is-root?}."
  ([scale-notes]
   (scale-notes-on-fretboard scale-notes standard-tuning 15))
  ([scale-notes tuning]
   (scale-notes-on-fretboard scale-notes tuning 15))
  ([scale-notes tuning max-fret]
   (let [root-note (first scale-notes)
         root-semitone (core/normalize-note root-note)
         ;; Create a map of normalized semitone -> degree for lookup
         semitone->degree (into {}
                                (map-indexed
                                 (fn [idx note]
                                   [(core/normalize-note note) (inc idx)])
                                 scale-notes))
         ;; Get all scale note semitones for matching
         scale-semitones (set (map core/normalize-note scale-notes))]
     (for [string (range 1 (inc (count tuning)))
           fret (range 0 (inc max-fret))
           :let [note (fret-to-note string fret tuning)
                 note-semitone (core/normalize-note note)]
           :when (contains? scale-semitones note-semitone)]
       {:string string
        :fret fret
        :note note
        :degree (get semitone->degree note-semitone)
        :is-root? (= note-semitone root-semitone)}))))

(defn scale-positions
  "Get scale notes organized by position (CAGED-style boxes).
   Returns map of {:position-name {:start-fret :end-fret :notes [...]}}"
  ([scale-notes]
   (scale-positions scale-notes standard-tuning))
  ([scale-notes tuning]
   (let [all-notes (scale-notes-on-fretboard scale-notes tuning 15)
         ;; Define position ranges (roughly based on CAGED)
         position-ranges [{:name "Open" :start 0 :end 3}
                          {:name "Position 2" :start 2 :end 5}
                          {:name "Position 3" :start 4 :end 7}
                          {:name "Position 4" :start 7 :end 10}
                          {:name "Position 5" :start 9 :end 12}
                          {:name "Position 6" :start 12 :end 15}]]
     (into {}
           (for [{:keys [name start end]} position-ranges]
             [name {:start-fret start
                    :end-fret end
                    :notes (filterv #(<= start (:fret %) end) all-notes)}])))))

(defn scale-in-position
  "Get scale notes within a specific fret range."
  ([scale-notes start-fret end-fret]
   (scale-in-position scale-notes start-fret end-fret standard-tuning))
  ([scale-notes start-fret end-fret tuning]
   (let [all-notes (scale-notes-on-fretboard scale-notes tuning (+ end-fret 2))]
     (filterv #(<= start-fret (:fret %) end-fret) all-notes))))

(defn three-notes-per-string
  "Generate a 3-notes-per-string fingering pattern.
   Returns notes organized by string."
  ([scale-notes start-fret]
   (three-notes-per-string scale-notes start-fret standard-tuning))
  ([scale-notes start-fret tuning]
   (let [all-notes (scale-notes-on-fretboard scale-notes tuning (+ start-fret 6))
         ;; Group by string
         by-string (group-by :string all-notes)]
     (into {}
           (for [string (range 1 7)]
             [string (->> (get by-string string [])
                          (filter #(<= start-fret (:fret %)))
                          (sort-by :fret)
                          (take 3)
                          vec)])))))

(def position-names
  "Named positions on the fretboard."
  ["Open" "Position 2" "Position 3" "Position 4" "Position 5" "Position 6"])
