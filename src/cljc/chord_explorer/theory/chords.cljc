(ns chord-explorer.theory.chords
  "Chord construction and identification with extensible registry.

   Adding new chord types is simple:
   (register-chord! :my-chord [0 4 7 10] {:symbol \"my\" :name \"My Chord\"})"
  (:require [chord-explorer.theory.core :as core]))

;; =============================================================================
;; Chord Registry (Extensible)
;; =============================================================================

(def ^:private initial-chords
  "Built-in chord type definitions.
   Each chord is defined by:
   - :intervals - semitone distances from root
   - :symbol - chord symbol suffix (e.g., \"m7\" for minor 7th)
   - :name - full descriptive name
   - :category - grouping for UI
   - :quality - :major, :minor, :diminished, :augmented, :dominant, :suspended"

  {;; Triads
   :major
   {:intervals [0 4 7]
    :symbol ""
    :name "Major"
    :category :triads
    :quality :major}

   :minor
   {:intervals [0 3 7]
    :symbol "m"
    :name "Minor"
    :category :triads
    :quality :minor}

   :diminished
   {:intervals [0 3 6]
    :symbol "dim"
    :name "Diminished"
    :category :triads
    :quality :diminished
    :aliases [:dim :o]}

   :augmented
   {:intervals [0 4 8]
    :symbol "aug"
    :name "Augmented"
    :category :triads
    :quality :augmented
    :aliases [:aug :+]}

   :sus2
   {:intervals [0 2 7]
    :symbol "sus2"
    :name "Suspended 2nd"
    :category :triads
    :quality :suspended}

   :sus4
   {:intervals [0 5 7]
    :symbol "sus4"
    :name "Suspended 4th"
    :category :triads
    :quality :suspended}

   ;; Power Chord
   :power
   {:intervals [0 7]
    :symbol "5"
    :name "Power Chord"
    :category :triads
    :quality :power}

   ;; Seventh Chords
   :maj7
   {:intervals [0 4 7 11]
    :symbol "maj7"
    :name "Major 7th"
    :category :sevenths
    :quality :major
    :aliases [:M7 :major7]}

   :7
   {:intervals [0 4 7 10]
    :symbol "7"
    :name "Dominant 7th"
    :category :sevenths
    :quality :dominant
    :aliases [:dom7]}

   :min7
   {:intervals [0 3 7 10]
    :symbol "m7"
    :name "Minor 7th"
    :category :sevenths
    :quality :minor
    :aliases [:m7 :minor7 :-7]}

   :min-maj7
   {:intervals [0 3 7 11]
    :symbol "m(maj7)"
    :name "Minor Major 7th"
    :category :sevenths
    :quality :minor
    :aliases [:mM7 :minmaj7]}

   :dim7
   {:intervals [0 3 6 9]
    :symbol "dim7"
    :name "Diminished 7th"
    :category :sevenths
    :quality :diminished
    :aliases [:o7]}

   :half-dim7
   {:intervals [0 3 6 10]
    :symbol "m7b5"
    :name "Half Diminished 7th"
    :category :sevenths
    :quality :diminished
    :aliases [:m7b5 :ø7]}

   :aug7
   {:intervals [0 4 8 10]
    :symbol "7#5"
    :name "Augmented 7th"
    :category :sevenths
    :quality :augmented
    :aliases [:+7 :7#5]}

   :aug-maj7
   {:intervals [0 4 8 11]
    :symbol "maj7#5"
    :name "Augmented Major 7th"
    :category :sevenths
    :quality :augmented
    :aliases [:+M7 :maj7#5]}

   :7sus4
   {:intervals [0 5 7 10]
    :symbol "7sus4"
    :name "Dominant 7th Suspended 4th"
    :category :sevenths
    :quality :dominant}

   :7sus2
   {:intervals [0 2 7 10]
    :symbol "7sus2"
    :name "Dominant 7th Suspended 2nd"
    :category :sevenths
    :quality :dominant}

   ;; Sixth Chords
   :6
   {:intervals [0 4 7 9]
    :symbol "6"
    :name "Major 6th"
    :category :sixths
    :quality :major}

   :min6
   {:intervals [0 3 7 9]
    :symbol "m6"
    :name "Minor 6th"
    :category :sixths
    :quality :minor}

   ;; Extended Chords (9ths)
   :9
   {:intervals [0 4 7 10 14]
    :symbol "9"
    :name "Dominant 9th"
    :category :extended
    :quality :dominant}

   :maj9
   {:intervals [0 4 7 11 14]
    :symbol "maj9"
    :name "Major 9th"
    :category :extended
    :quality :major}

   :min9
   {:intervals [0 3 7 10 14]
    :symbol "m9"
    :name "Minor 9th"
    :category :extended
    :quality :minor}

   :add9
   {:intervals [0 4 7 14]
    :symbol "add9"
    :name "Add 9"
    :category :extended
    :quality :major}

   :min-add9
   {:intervals [0 3 7 14]
    :symbol "m(add9)"
    :name "Minor Add 9"
    :category :extended
    :quality :minor}

   ;; Extended Chords (11ths)
   :11
   {:intervals [0 4 7 10 14 17]
    :symbol "11"
    :name "Dominant 11th"
    :category :extended
    :quality :dominant}

   :maj11
   {:intervals [0 4 7 11 14 17]
    :symbol "maj11"
    :name "Major 11th"
    :category :extended
    :quality :major}

   :min11
   {:intervals [0 3 7 10 14 17]
    :symbol "m11"
    :name "Minor 11th"
    :category :extended
    :quality :minor}

   ;; Extended Chords (13ths)
   :13
   {:intervals [0 4 7 10 14 21]
    :symbol "13"
    :name "Dominant 13th"
    :category :extended
    :quality :dominant}

   :maj13
   {:intervals [0 4 7 11 14 21]
    :symbol "maj13"
    :name "Major 13th"
    :category :extended
    :quality :major}

   :min13
   {:intervals [0 3 7 10 14 21]
    :symbol "m13"
    :name "Minor 13th"
    :category :extended
    :quality :minor}

   ;; Altered Chords
   :7b5
   {:intervals [0 4 6 10]
    :symbol "7b5"
    :name "Dominant 7th Flat 5"
    :category :altered
    :quality :dominant}

   :7#5
   {:intervals [0 4 8 10]
    :symbol "7#5"
    :name "Dominant 7th Sharp 5"
    :category :altered
    :quality :dominant}

   :7b9
   {:intervals [0 4 7 10 13]
    :symbol "7b9"
    :name "Dominant 7th Flat 9"
    :category :altered
    :quality :dominant}

   :7#9
   {:intervals [0 4 7 10 15]
    :symbol "7#9"
    :name "Dominant 7th Sharp 9"
    :category :altered
    :quality :dominant}

   :7#11
   {:intervals [0 4 7 10 18]
    :symbol "7#11"
    :name "Dominant 7th Sharp 11"
    :category :altered
    :quality :dominant}

   :7b13
   {:intervals [0 4 7 10 20]
    :symbol "7b13"
    :name "Dominant 7th Flat 13"
    :category :altered
    :quality :dominant}

   :7alt
   {:intervals [0 4 6 10 13]
    :symbol "7alt"
    :name "Altered Dominant"
    :category :altered
    :quality :dominant}

   :7b9b5
   {:intervals [0 4 6 10 13]
    :symbol "7b9b5"
    :name "Dominant 7th Flat 9 Flat 5"
    :category :altered
    :quality :dominant}

   :7#9#5
   {:intervals [0 4 8 10 15]
    :symbol "7#9#5"
    :name "Dominant 7th Sharp 9 Sharp 5"
    :category :altered
    :quality :dominant}

   ;; Shell Voicings (for jazz)
   :maj7-shell
   {:intervals [0 4 11]
    :symbol "maj7"
    :name "Major 7th Shell"
    :category :shells
    :quality :major
    :shell? true}

   :7-shell
   {:intervals [0 4 10]
    :symbol "7"
    :name "Dominant 7th Shell"
    :category :shells
    :quality :dominant
    :shell? true}

   :min7-shell
   {:intervals [0 3 10]
    :symbol "m7"
    :name "Minor 7th Shell"
    :category :shells
    :quality :minor
    :shell? true}})

;; Atom containing all registered chord types. Can be extended at runtime.
(defonce chord-registry (atom initial-chords))

;; =============================================================================
;; Registry Functions
;; =============================================================================

(defn register-chord!
  "Register a new chord type.

   Arguments:
   - key: keyword identifier for the chord
   - intervals: vector of semitone distances from root
   - opts: map with :symbol, :name, :category, :quality, optionally :aliases

   Example:
   (register-chord! :maj7#11 [0 4 7 11 18]
                    {:symbol \"maj7#11\" :name \"Major 7 Sharp 11\" :category :extended})"
  [key intervals opts]
  (swap! chord-registry assoc key
         (merge {:intervals intervals} opts)))

(defn unregister-chord!
  "Remove a chord type from the registry."
  [key]
  (swap! chord-registry dissoc key))

(defn get-chord-def
  "Get the full definition map for a chord type."
  [chord-type]
  (get @chord-registry chord-type))

(defn list-chord-types
  "List all registered chord types.
   Options:
   - :category - filter by category
   - :quality - filter by quality"
  ([]
   (keys @chord-registry))
  ([{:keys [category quality]}]
   (cond->> @chord-registry
     category (filter (fn [[_ v]] (= (:category v) category)))
     quality (filter (fn [[_ v]] (= (:quality v) quality)))
     true (map first))))

(defn list-chord-categories
  "List all chord categories."
  []
  (->> @chord-registry
       vals
       (map :category)
       (remove nil?)
       distinct
       sort))

(defn chords-by-category
  "Group all chords by their category."
  []
  (group-by (fn [[_ v]] (:category v)) @chord-registry))

;; =============================================================================
;; Chord Construction
;; =============================================================================

(defn build-chord
  "Build a chord from a root note and chord type.

   Arguments:
   - root: root note keyword (e.g., :C, :F#)
   - chord-type: keyword identifying the chord type (e.g., :maj7, :min7)

   Options:
   - :prefer - :sharps (default) or :flats for note spelling

   Returns a vector of note keywords."
  ([root chord-type]
   (build-chord root chord-type {}))
  ([root chord-type {:keys [prefer] :or {prefer :sharps}}]
   (when-let [chord-def (get-chord-def chord-type)]
     (mapv #(core/transpose root % {:prefer prefer})
           (:intervals chord-def)))))

(defn build-chord-with-info
  "Build a chord with additional information.
   Returns a map with :notes, :root, :type, :symbol, :name."
  ([root chord-type]
   (build-chord-with-info root chord-type {}))
  ([root chord-type opts]
   (when-let [chord-def (get-chord-def chord-type)]
     {:root root
      :type chord-type
      :notes (build-chord root chord-type opts)
      :symbol (:symbol chord-def)
      :name (:name chord-def)
      :quality (:quality chord-def)})))

(defn chord-symbol
  "Get the full chord symbol string (e.g., \"Cmaj7\", \"F#m7\")."
  [root chord-type]
  (when-let [chord-def (get-chord-def chord-type)]
    (str (name root) (:symbol chord-def))))

(defn get-chord-tones
  "Get the intervals (chord tones) for a chord type."
  [chord-type]
  (when-let [chord-def (get-chord-def chord-type)]
    (:intervals chord-def)))

;; =============================================================================
;; Chord Extensions & Modifications
;; =============================================================================

(defn add-extension
  "Add an extension to a chord.
   - extension: :9, :11, :13, :b9, :#9, :#11, :b13"
  [chord-notes root extension]
  (let [extension-intervals {:9 14, :11 17, :13 21
                             :b9 13, :#9 15, :#11 18, :b13 20}
        interval (get extension-intervals extension)]
    (if interval
      (conj chord-notes (core/transpose root interval))
      chord-notes)))

(defn add-bass
  "Add a bass note (for slash chords).
   Returns {:notes [...] :bass note}."
  [chord-notes bass-note]
  {:notes chord-notes
   :bass bass-note})

(defn omit-note
  "Omit a note from a chord by its scale degree.
   Degree is 1-based (1=root, 3=third, 5=fifth, 7=seventh)."
  [chord-notes degree]
  (let [idx (dec degree)]
    (vec (concat (take idx chord-notes)
                 (drop (inc idx) chord-notes)))))

;; =============================================================================
;; Inversions
;; =============================================================================

(defn get-inversions
  "Get all inversions of a chord.
   Returns a vector of inversions, each is a vector of notes."
  [chord-notes]
  (let [n (count chord-notes)]
    (mapv (fn [inv]
            (vec (concat (drop inv chord-notes)
                         (take inv chord-notes))))
          (range n))))

(defn get-inversion
  "Get a specific inversion of a chord.
   - inversion: 0 (root position), 1 (first inversion), 2 (second), etc."
  [chord-notes inversion]
  (let [n (count chord-notes)
        inv (mod inversion n)]
    (vec (concat (drop inv chord-notes)
                 (take inv chord-notes)))))

(defn inversion-name
  "Get the name of an inversion (0=root, 1=first, 2=second, 3=third)."
  [inversion]
  (case inversion
    0 "Root Position"
    1 "First Inversion"
    2 "Second Inversion"
    3 "Third Inversion"
    (str "Inversion " inversion)))

;; =============================================================================
;; Chord Identification
;; =============================================================================

(defn normalize-intervals
  "Normalize a set of notes to intervals from the lowest note."
  [notes]
  (let [semitones (map core/normalize-note notes)
        bass (apply min semitones)]
    (sort (map #(mod (- % bass) 12) semitones))))

(defn match-chord-type
  "Try to match intervals to a chord type.
   Returns the chord type key or nil."
  [intervals]
  (let [interval-set (set intervals)]
    (some (fn [[chord-key chord-def]]
            (when (= (set (:intervals chord-def)) interval-set)
              chord-key))
          @chord-registry)))

(defn identify-chord
  "Identify a chord from a collection of notes.
   Returns a vector of possible identifications:
   [{:root :C :type :maj7 :inversion 0} ...]"
  [notes]
  (let [unique-notes (distinct notes)
        semitones (mapv core/normalize-note unique-notes)]
    (for [root-idx (range (count unique-notes))
          :let [root (nth unique-notes root-idx)
                root-semitone (nth semitones root-idx)
                intervals (sort (map #(mod (- % root-semitone) 12) semitones))
                chord-type (match-chord-type intervals)]
          :when chord-type]
      {:root root
       :type chord-type
       :inversion (if (zero? root-idx) 0
                      (- (count unique-notes) root-idx))
       :notes unique-notes})))

(defn identify-chord-best
  "Identify the most likely chord from notes.
   Prefers root position and common chord types."
  [notes]
  (let [candidates (identify-chord notes)]
    (first (sort-by (fn [{:keys [inversion type]}]
                      [(if (zero? inversion) 0 1)
                       (case (:category (get-chord-def type))
                         :triads 0
                         :sevenths 1
                         :extended 2
                         :altered 3
                         4)])
                    candidates))))

;; =============================================================================
;; Chord Relationships
;; =============================================================================

(defn common-tones
  "Find notes common to two chords."
  [chord1-notes chord2-notes]
  (let [s1 (set (map core/normalize-note chord1-notes))
        s2 (set (map core/normalize-note chord2-notes))]
    (clojure.set/intersection s1 s2)))

(defn voice-leading-distance
  "Calculate the voice leading distance between two chords.
   Lower values indicate smoother voice leading."
  [chord1-notes chord2-notes]
  (let [s1 (sort (map core/normalize-note chord1-notes))
        s2 (sort (map core/normalize-note chord2-notes))
        n (min (count s1) (count s2))]
    (reduce + (map (fn [n1 n2]
                     (min (Math/abs (- n1 n2))
                          (- 12 (Math/abs (- n1 n2)))))
                   (take n s1)
                   (take n s2)))))

(defn is-subset?
  "Check if chord1 is a subset of chord2 (all notes in chord1 are in chord2)."
  [chord1-notes chord2-notes]
  (let [s1 (set (map core/normalize-note chord1-notes))
        s2 (set (map core/normalize-note chord2-notes))]
    (clojure.set/subset? s1 s2)))

(defn related-chords
  "Find chord types that share notes with the given chord.
   Returns chords with at least min-common common tones."
  [root chord-type min-common]
  (let [chord-notes (build-chord root chord-type)
        chord-semitones (set (map core/normalize-note chord-notes))]
    (for [[type-key type-def] @chord-registry
          test-root core/chromatic-notes
          :let [test-notes (build-chord test-root type-key)
                test-semitones (set (map core/normalize-note test-notes))
                common (count (clojure.set/intersection chord-semitones test-semitones))]
          :when (and (>= common min-common)
                     (not (and (= root test-root)
                               (= chord-type type-key))))]
      {:root test-root
       :type type-key
       :common-tones common})))
