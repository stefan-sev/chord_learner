(ns chord-explorer.theory.scales
  "Scale definitions and operations with extensible registry.

   Adding new scales is simple:
   (register-scale! :my-scale [0 2 4 5 7 9 11] {:name \"My Scale\" :category :custom})"
  (:require [chord-explorer.theory.core :as core]))

;; =============================================================================
;; Scale Registry (Extensible)
;; =============================================================================

(def ^:private initial-scales
  "Built-in scale definitions.
   Each scale is defined by:
   - :intervals - semitone distances from root
   - :name - display name
   - :category - grouping for UI
   - :aliases - alternative names (optional)"

  {;; Diatonic Scales
   :major
   {:intervals [0 2 4 5 7 9 11]
    :name "Major"
    :category :diatonic
    :aliases [:ionian]}

   :natural-minor
   {:intervals [0 2 3 5 7 8 10]
    :name "Natural Minor"
    :category :diatonic
    :aliases [:aeolian :minor]}

   ;; Harmonic & Melodic Minor
   :harmonic-minor
   {:intervals [0 2 3 5 7 8 11]
    :name "Harmonic Minor"
    :category :minor-variants}

   :melodic-minor
   {:intervals [0 2 3 5 7 9 11]
    :name "Melodic Minor (Ascending)"
    :category :minor-variants}

   ;; Modes of Major Scale
   :ionian
   {:intervals [0 2 4 5 7 9 11]
    :name "Ionian"
    :category :modes
    :parent {:scale :major :degree 1}}

   :dorian
   {:intervals [0 2 3 5 7 9 10]
    :name "Dorian"
    :category :modes
    :parent {:scale :major :degree 2}}

   :phrygian
   {:intervals [0 1 3 5 7 8 10]
    :name "Phrygian"
    :category :modes
    :parent {:scale :major :degree 3}}

   :lydian
   {:intervals [0 2 4 6 7 9 11]
    :name "Lydian"
    :category :modes
    :parent {:scale :major :degree 4}}

   :mixolydian
   {:intervals [0 2 4 5 7 9 10]
    :name "Mixolydian"
    :category :modes
    :parent {:scale :major :degree 5}}

   :aeolian
   {:intervals [0 2 3 5 7 8 10]
    :name "Aeolian"
    :category :modes
    :parent {:scale :major :degree 6}}

   :locrian
   {:intervals [0 1 3 5 6 8 10]
    :name "Locrian"
    :category :modes
    :parent {:scale :major :degree 7}}

   ;; Modes of Harmonic Minor
   :locrian-nat6
   {:intervals [0 1 3 5 6 9 10]
    :name "Locrian Natural 6"
    :category :harmonic-minor-modes
    :parent {:scale :harmonic-minor :degree 2}}

   :ionian-augmented
   {:intervals [0 2 4 5 8 9 11]
    :name "Ionian Augmented"
    :category :harmonic-minor-modes
    :parent {:scale :harmonic-minor :degree 3}}

   :dorian-sharp4
   {:intervals [0 2 3 6 7 9 10]
    :name "Dorian #4"
    :category :harmonic-minor-modes
    :parent {:scale :harmonic-minor :degree 4}}

   :phrygian-dominant
   {:intervals [0 1 4 5 7 8 10]
    :name "Phrygian Dominant"
    :category :harmonic-minor-modes
    :parent {:scale :harmonic-minor :degree 5}}

   :lydian-sharp2
   {:intervals [0 3 4 6 7 9 11]
    :name "Lydian #2"
    :category :harmonic-minor-modes
    :parent {:scale :harmonic-minor :degree 6}}

   :super-locrian-bb7
   {:intervals [0 1 3 4 6 8 9]
    :name "Super Locrian bb7"
    :category :harmonic-minor-modes
    :parent {:scale :harmonic-minor :degree 7}}

   ;; Modes of Melodic Minor
   :melodic-minor-mode1
   {:intervals [0 2 3 5 7 9 11]
    :name "Melodic Minor"
    :category :melodic-minor-modes
    :parent {:scale :melodic-minor :degree 1}}

   :dorian-b2
   {:intervals [0 1 3 5 7 9 10]
    :name "Dorian b2"
    :category :melodic-minor-modes
    :parent {:scale :melodic-minor :degree 2}
    :aliases [:phrygian-nat6]}

   :lydian-augmented
   {:intervals [0 2 4 6 8 9 11]
    :name "Lydian Augmented"
    :category :melodic-minor-modes
    :parent {:scale :melodic-minor :degree 3}}

   :lydian-dominant
   {:intervals [0 2 4 6 7 9 10]
    :name "Lydian Dominant"
    :category :melodic-minor-modes
    :parent {:scale :melodic-minor :degree 4}
    :aliases [:lydian-b7 :overtone]}

   :mixolydian-b6
   {:intervals [0 2 4 5 7 8 10]
    :name "Mixolydian b6"
    :category :melodic-minor-modes
    :parent {:scale :melodic-minor :degree 5}
    :aliases [:hindu :aeolian-dominant]}

   :locrian-nat2
   {:intervals [0 2 3 5 6 8 10]
    :name "Locrian Natural 2"
    :category :melodic-minor-modes
    :parent {:scale :melodic-minor :degree 6}
    :aliases [:half-diminished]}

   :super-locrian
   {:intervals [0 1 3 4 6 8 10]
    :name "Super Locrian"
    :category :melodic-minor-modes
    :parent {:scale :melodic-minor :degree 7}
    :aliases [:altered :diminished-whole-tone]}

   ;; Pentatonic Scales
   :major-pentatonic
   {:intervals [0 2 4 7 9]
    :name "Major Pentatonic"
    :category :pentatonic}

   :minor-pentatonic
   {:intervals [0 3 5 7 10]
    :name "Minor Pentatonic"
    :category :pentatonic}

   ;; Blues Scales
   :blues
   {:intervals [0 3 5 6 7 10]
    :name "Blues"
    :category :blues}

   :major-blues
   {:intervals [0 2 3 4 7 9]
    :name "Major Blues"
    :category :blues}

   ;; Symmetric Scales
   :whole-tone
   {:intervals [0 2 4 6 8 10]
    :name "Whole Tone"
    :category :symmetric}

   :diminished-hw
   {:intervals [0 1 3 4 6 7 9 10]
    :name "Diminished (Half-Whole)"
    :category :symmetric}

   :diminished-wh
   {:intervals [0 2 3 5 6 8 9 11]
    :name "Diminished (Whole-Half)"
    :category :symmetric}

   :chromatic
   {:intervals [0 1 2 3 4 5 6 7 8 9 10 11]
    :name "Chromatic"
    :category :symmetric}

   ;; Bebop Scales
   :bebop-dominant
   {:intervals [0 2 4 5 7 9 10 11]
    :name "Bebop Dominant"
    :category :bebop}

   :bebop-major
   {:intervals [0 2 4 5 7 8 9 11]
    :name "Bebop Major"
    :category :bebop}

   :bebop-minor
   {:intervals [0 2 3 5 7 8 9 10]
    :name "Bebop Minor"
    :category :bebop}

   :bebop-dorian
   {:intervals [0 2 3 4 5 7 9 10]
    :name "Bebop Dorian"
    :category :bebop}})

;; Atom containing all registered scales. Can be extended at runtime.
(defonce scale-registry (atom initial-scales))

;; =============================================================================
;; Registry Functions
;; =============================================================================

(defn register-scale!
  "Register a new scale type.

   Arguments:
   - key: keyword identifier for the scale
   - intervals: vector of semitone distances from root
   - opts: map with :name, :category, and optionally :aliases, :parent

   Example:
   (register-scale! :hungarian-minor [0 2 3 6 7 8 11]
                    {:name \"Hungarian Minor\" :category :exotic})"
  [key intervals opts]
  (swap! scale-registry assoc key
         (merge {:intervals intervals} opts)))

(defn unregister-scale!
  "Remove a scale from the registry."
  [key]
  (swap! scale-registry dissoc key))

(defn get-scale-def
  "Get the full definition map for a scale type."
  [scale-type]
  (get @scale-registry scale-type))

(defn list-scales
  "List all registered scale types.
   Options:
   - :category - filter by category"
  ([]
   (keys @scale-registry))
  ([{:keys [category]}]
   (if category
     (->> @scale-registry
          (filter (fn [[_ v]] (= (:category v) category)))
          (map first))
     (keys @scale-registry))))

(defn list-categories
  "List all scale categories."
  []
  (->> @scale-registry
       vals
       (map :category)
       (remove nil?)
       distinct
       sort))

(defn scales-by-category
  "Group all scales by their category."
  []
  (group-by (fn [[_ v]] (:category v)) @scale-registry))

;; =============================================================================
;; Scale Operations
;; =============================================================================

(defn get-scale
  "Generate the notes of a scale from a root note.

   Arguments:
   - root: root note keyword (e.g., :C, :F#)
   - scale-type: keyword identifying the scale (e.g., :major, :dorian)

   Options:
   - :prefer - :sharps (default) or :flats for note spelling

   Returns a vector of note keywords."
  ([root scale-type]
   (get-scale root scale-type {}))
  ([root scale-type {:keys [prefer] :or {prefer :sharps}}]
   (when-let [scale-def (get-scale-def scale-type)]
     (let [root-semitone (core/normalize-note root)]
       (mapv (fn [interval]
               (core/transpose root interval {:prefer prefer}))
             (:intervals scale-def))))))

(defn get-scale-with-degrees
  "Generate scale notes with their scale degrees.
   Returns a vector of {:note :degree} maps."
  ([root scale-type]
   (get-scale-with-degrees root scale-type {}))
  ([root scale-type opts]
   (when-let [notes (get-scale root scale-type opts)]
     (mapv (fn [note degree]
             {:note note :degree degree})
           notes
           (range 1 (inc (count notes)))))))

(defn note-in-scale?
  "Check if a note is in a given scale."
  [note root scale-type]
  (when-let [scale-notes (get-scale root scale-type)]
    (some #(core/notes-equal? note %) scale-notes)))

(defn scale-degree
  "Get the scale degree (1-7) of a note in a scale.
   Returns nil if the note is not in the scale."
  [note root scale-type]
  (when-let [scale-notes (get-scale root scale-type)]
    (some (fn [[idx scale-note]]
            (when (core/notes-equal? note scale-note)
              (inc idx)))
          (map-indexed vector scale-notes))))

(defn transpose-scale
  "Transpose all notes in a scale by a number of semitones."
  [scale-notes semitones]
  (mapv #(core/transpose % semitones) scale-notes))

;; =============================================================================
;; Scale Relationships
;; =============================================================================

(defn relative-minor
  "Get the relative minor key of a major key.
   The relative minor is built on the 6th degree."
  [major-root]
  (core/transpose major-root -3))

(defn relative-major
  "Get the relative major key of a minor key.
   The relative major is built on the 3rd degree."
  [minor-root]
  (core/transpose minor-root 3))

(defn parallel-minor
  "Get the parallel minor of a major key (same root note)."
  [root]
  {:root root :scale-type :natural-minor})

(defn parallel-major
  "Get the parallel major of a minor key (same root note)."
  [root]
  {:root root :scale-type :major})

(defn parallel-scales
  "Get all parallel scales (scales sharing the same root note)."
  [root]
  (map (fn [scale-type]
         {:root root :scale-type scale-type})
       (list-scales)))

(defn mode-of
  "Get a mode of a scale by degree.
   E.g., (mode-of :C :major 2) returns D Dorian notes."
  [root scale-type degree]
  (when-let [parent-scale (get-scale root scale-type)]
    (let [mode-root (nth parent-scale (dec degree))
          rotated-intervals (let [intervals (:intervals (get-scale-def scale-type))
                                  start-interval (nth intervals (dec degree))]
                              (mapv #(mod (- % start-interval) 12)
                                    (concat (drop (dec degree) intervals)
                                            (take (dec degree) intervals))))]
      {:root mode-root
       :notes (mapv #(core/transpose mode-root %) rotated-intervals)})))

;; =============================================================================
;; Scale Analysis
;; =============================================================================

(defn common-tones
  "Find notes that are common between two scales."
  [root1 scale-type1 root2 scale-type2]
  (let [scale1 (set (map core/normalize-note (get-scale root1 scale-type1)))
        scale2 (set (map core/normalize-note (get-scale root2 scale-type2)))]
    (clojure.set/intersection scale1 scale2)))

(defn scale-difference
  "Find notes that differ between two scales."
  [root1 scale-type1 root2 scale-type2]
  (let [scale1 (set (map core/normalize-note (get-scale root1 scale-type1)))
        scale2 (set (map core/normalize-note (get-scale root2 scale-type2)))]
    {:only-in-first (clojure.set/difference scale1 scale2)
     :only-in-second (clojure.set/difference scale2 scale1)}))

(defn scale-contains-chord-tones?
  "Check if a scale contains all the given chord tones."
  [root scale-type chord-tones]
  (every? #(note-in-scale? % root scale-type) chord-tones))

(defn identify-scale
  "Identify possible scales given a set of notes.
   Returns a list of {:root :scale-type} that contain all given notes."
  [notes]
  (let [note-set (set (map core/normalize-note notes))]
    (for [root core/chromatic-notes
          [scale-type _] @scale-registry
          :let [scale-notes (set (map core/normalize-note (get-scale root scale-type)))]
          :when (clojure.set/subset? note-set scale-notes)]
      {:root root :scale-type scale-type})))
