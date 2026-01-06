(ns chord-explorer.theory.harmony
  "Diatonic harmony analysis, Roman numerals, and harmonic functions."
  (:require [chord-explorer.theory.core :as core]
            [chord-explorer.theory.scales :as scales]
            [chord-explorer.theory.chords :as chords]))

;; =============================================================================
;; Roman Numeral Representation
;; =============================================================================

(def roman-numerals
  "Roman numeral representations for scale degrees."
  {1 {:major "I" :minor "i"}
   2 {:major "II" :minor "ii"}
   3 {:major "III" :minor "iii"}
   4 {:major "IV" :minor "iv"}
   5 {:major "V" :minor "v"}
   6 {:major "VI" :minor "vi"}
   7 {:major "VII" :minor "vii"}})

(def numeral->degree
  "Map Roman numerals back to degrees."
  {"I" 1 "i" 1 "II" 2 "ii" 2 "III" 3 "iii" 3
   "IV" 4 "iv" 4 "V" 5 "v" 5 "VI" 6 "vi" 6 "VII" 7 "vii" 7
   "bII" 2 "bii" 2 "bIII" 3 "biii" 3 "#IV" 4 "#iv" 4
   "bV" 5 "bv" 5 "bVI" 6 "bvi" 6 "bVII" 7 "bvii" 7})

;; =============================================================================
;; Diatonic Chord Quality Maps
;; =============================================================================

;; =============================================================================
;; Dynamic Chord Quality Calculation
;; =============================================================================

(defn- interval-between-scale-degrees
  "Calculate the interval in semitones between two scale degrees.
   Wraps around for degrees beyond the scale length."
  [scale-intervals degree1 degree2]
  (let [n (count scale-intervals)
        idx1 (mod (dec degree1) n)
        idx2 (mod (dec degree2) n)
        interval1 (nth scale-intervals idx1)
        interval2 (nth scale-intervals idx2)
        diff (- interval2 interval1)]
    (if (neg? diff) (+ diff 12) diff)))

(defn- calculate-triad-quality
  "Determine triad quality from the 3rd and 5th intervals."
  [third-interval fifth-interval]
  (cond
    ;; Major: M3 (4) + P5 (7)
    (and (= third-interval 4) (= fifth-interval 7)) :major
    ;; Minor: m3 (3) + P5 (7)
    (and (= third-interval 3) (= fifth-interval 7)) :minor
    ;; Diminished: m3 (3) + d5 (6)
    (and (= third-interval 3) (= fifth-interval 6)) :diminished
    ;; Augmented: M3 (4) + A5 (8)
    (and (= third-interval 4) (= fifth-interval 8)) :augmented
    ;; Sus4: P4 (5) + P5 (7)
    (and (= third-interval 5) (= fifth-interval 7)) :sus4
    ;; Sus2: M2 (2) + P5 (7)
    (and (= third-interval 2) (= fifth-interval 7)) :sus2
    ;; Default to major if unusual
    :else :major))

(defn- calculate-seventh-quality
  "Determine seventh chord quality from intervals."
  [third-interval fifth-interval seventh-interval]
  (let [triad (calculate-triad-quality third-interval fifth-interval)]
    (cond
      ;; Major 7th chords
      (and (= triad :major) (= seventh-interval 11)) :maj7
      (and (= triad :minor) (= seventh-interval 11)) :min-maj7
      (and (= triad :augmented) (= seventh-interval 11)) :aug-maj7

      ;; Dominant/Minor 7th chords
      (and (= triad :major) (= seventh-interval 10)) :7
      (and (= triad :minor) (= seventh-interval 10)) :min7
      (and (= triad :augmented) (= seventh-interval 10)) :aug7

      ;; Diminished 7th chords
      (and (= triad :diminished) (= seventh-interval 10)) :half-dim7
      (and (= triad :diminished) (= seventh-interval 9)) :dim7

      ;; Default based on triad
      :else (case triad
              :major :maj7
              :minor :min7
              :diminished :half-dim7
              :augmented :aug7
              :maj7))))

(defn compute-chord-quality-for-degree
  "Dynamically compute the chord quality for a scale degree.
   Works with any scale type by analyzing the actual intervals."
  [scale-type degree seventh?]
  (let [scale-def (scales/get-scale-def scale-type)
        intervals (:intervals scale-def)
        n (count intervals)]
    (if (< n 5)
      ;; For scales with fewer than 5 notes (like pentatonic),
      ;; we can't build traditional tertian chords
      ;; Return power chord or sus chord based on available notes
      (let [second-degree (mod degree n)
            second-interval (interval-between-scale-degrees intervals degree (inc second-degree))]
        (if (>= second-interval 5) :sus4 :power))
      ;; For 7-note scales, calculate normally
      (let [;; For stacking thirds, we skip every other scale degree
            third-degree (+ degree 2)
            fifth-degree (+ degree 4)
            seventh-degree (+ degree 6)
            third-interval (interval-between-scale-degrees intervals degree third-degree)
            fifth-interval (interval-between-scale-degrees intervals degree fifth-degree)]
        (if seventh?
          (let [seventh-interval (interval-between-scale-degrees intervals degree seventh-degree)]
            (calculate-seventh-quality third-interval fifth-interval seventh-interval))
          (calculate-triad-quality third-interval fifth-interval))))))

;; =============================================================================
;; Pre-computed Chord Quality Maps (for common scales - optimization)
;; =============================================================================

(def diatonic-triads
  "Chord qualities for each scale degree in common scale types.
   Falls back to dynamic calculation for unlisted scales."
  {:major
   {1 :major, 2 :minor, 3 :minor, 4 :major,
    5 :major, 6 :minor, 7 :diminished}

   :natural-minor
   {1 :minor, 2 :diminished, 3 :major, 4 :minor,
    5 :minor, 6 :major, 7 :major}

   :harmonic-minor
   {1 :minor, 2 :diminished, 3 :augmented, 4 :minor,
    5 :major, 6 :major, 7 :diminished}

   :melodic-minor
   {1 :minor, 2 :minor, 3 :augmented, 4 :major,
    5 :major, 6 :diminished, 7 :diminished}

   :dorian
   {1 :minor, 2 :minor, 3 :major, 4 :major,
    5 :minor, 6 :diminished, 7 :major}

   :phrygian
   {1 :minor, 2 :major, 3 :major, 4 :minor,
    5 :diminished, 6 :major, 7 :minor}

   :lydian
   {1 :major, 2 :major, 3 :minor, 4 :diminished,
    5 :major, 6 :minor, 7 :minor}

   :mixolydian
   {1 :major, 2 :minor, 3 :diminished, 4 :major,
    5 :minor, 6 :minor, 7 :major}

   :locrian
   {1 :diminished, 2 :major, 3 :minor, 4 :minor,
    5 :major, 6 :major, 7 :minor}

   ;; Melodic minor modes
   :lydian-dominant
   {1 :major, 2 :major, 3 :minor, 4 :diminished,
    5 :minor, 6 :minor, 7 :major}

   :super-locrian
   {1 :diminished, 2 :minor, 3 :minor, 4 :major,
    5 :major, 6 :major, 7 :minor}

   :lydian-augmented
   {1 :augmented, 2 :major, 3 :major, 4 :minor,
    5 :diminished, 6 :minor, 7 :minor}

   :locrian-nat2
   {1 :diminished, 2 :minor, 3 :minor, 4 :minor,
    5 :major, 6 :major, 7 :major}

   ;; Harmonic minor modes
   :phrygian-dominant
   {1 :major, 2 :diminished, 3 :minor, 4 :minor,
    5 :major, 6 :augmented, 7 :diminished}

   ;; Symmetric scales
   :whole-tone
   {1 :augmented, 2 :augmented, 3 :augmented,
    4 :augmented, 5 :augmented, 6 :augmented}

   :diminished-hw
   {1 :diminished, 2 :minor, 3 :diminished, 4 :minor,
    5 :diminished, 6 :minor, 7 :diminished, 8 :minor}

   :diminished-wh
   {1 :minor, 2 :diminished, 3 :minor, 4 :diminished,
    5 :minor, 6 :diminished, 7 :minor, 8 :diminished}

   ;; Pentatonic scales - use power chords and sus chords since no true thirds
   :major-pentatonic
   {1 :major, 2 :sus4, 3 :minor, 4 :sus2, 5 :minor}

   :minor-pentatonic
   {1 :minor, 2 :sus4, 3 :major, 4 :sus2, 5 :major}

   ;; Blues scale - similar approach
   :blues
   {1 :minor, 2 :diminished, 3 :major, 4 :diminished, 5 :minor, 6 :major}

   :major-blues
   {1 :major, 2 :diminished, 3 :minor, 4 :diminished, 5 :major, 6 :minor}

   ;; Bebop scales (8 notes)
   :bebop-dominant
   {1 :major, 2 :minor, 3 :diminished, 4 :major,
    5 :minor, 6 :minor, 7 :major, 8 :diminished}

   :bebop-major
   {1 :major, 2 :minor, 3 :minor, 4 :major,
    5 :diminished, 6 :major, 7 :minor, 8 :diminished}

   :bebop-minor
   {1 :minor, 2 :minor, 3 :major, 4 :major,
    5 :diminished, 6 :minor, 7 :diminished, 8 :major}})

(def diatonic-sevenths
  "Seventh chord qualities for each scale degree."
  {:major
   {1 :maj7, 2 :min7, 3 :min7, 4 :maj7,
    5 :7, 6 :min7, 7 :half-dim7}

   :natural-minor
   {1 :min7, 2 :half-dim7, 3 :maj7, 4 :min7,
    5 :min7, 6 :maj7, 7 :7}

   :harmonic-minor
   {1 :min-maj7, 2 :half-dim7, 3 :aug-maj7, 4 :min7,
    5 :7, 6 :maj7, 7 :dim7}

   :melodic-minor
   {1 :min-maj7, 2 :min7, 3 :aug-maj7, 4 :7,
    5 :7, 6 :half-dim7, 7 :half-dim7}

   :dorian
   {1 :min7, 2 :min7, 3 :maj7, 4 :7,
    5 :min7, 6 :half-dim7, 7 :maj7}

   :phrygian
   {1 :min7, 2 :maj7, 3 :7, 4 :min7,
    5 :half-dim7, 6 :maj7, 7 :min7}

   :lydian
   {1 :maj7, 2 :7, 3 :min7, 4 :half-dim7,
    5 :maj7, 6 :min7, 7 :min7}

   :mixolydian
   {1 :7, 2 :min7, 3 :half-dim7, 4 :maj7,
    5 :min7, 6 :min7, 7 :maj7}

   :locrian
   {1 :half-dim7, 2 :maj7, 3 :min7, 4 :min7,
    5 :maj7, 6 :7, 7 :min7}

   ;; Melodic minor modes
   :lydian-dominant
   {1 :7, 2 :7, 3 :min7, 4 :half-dim7,
    5 :min7, 6 :min-maj7, 7 :maj7}

   :super-locrian
   {1 :half-dim7, 2 :min7, 3 :min-maj7, 4 :7,
    5 :maj7, 6 :7, 7 :min7}

   ;; Harmonic minor modes
   :phrygian-dominant
   {1 :7, 2 :half-dim7, 3 :min7, 4 :min7,
    5 :maj7, 6 :aug-maj7, 7 :dim7}

   ;; Symmetric scales
   :whole-tone
   {1 :aug7, 2 :aug7, 3 :aug7,
    4 :aug7, 5 :aug7, 6 :aug7}

   :diminished-hw
   {1 :dim7, 2 :min7, 3 :dim7, 4 :min7,
    5 :dim7, 6 :min7, 7 :dim7, 8 :min7}

   :diminished-wh
   {1 :min7, 2 :dim7, 3 :min7, 4 :dim7,
    5 :min7, 6 :dim7, 7 :min7, 8 :dim7}

   ;; Pentatonic scales - extended with 7ths where possible
   :major-pentatonic
   {1 :maj7, 2 :7sus4, 3 :min7, 4 :7sus2, 5 :min7}

   :minor-pentatonic
   {1 :min7, 2 :7sus4, 3 :maj7, 4 :7sus2, 5 :7}

   ;; Blues scale
   :blues
   {1 :min7, 2 :dim7, 3 :7, 4 :dim7, 5 :min7, 6 :7}

   :major-blues
   {1 :7, 2 :dim7, 3 :min7, 4 :dim7, 5 :7, 6 :min7}

   ;; Bebop scales (8 notes)
   :bebop-dominant
   {1 :7, 2 :min7, 3 :half-dim7, 4 :maj7,
    5 :min7, 6 :min7, 7 :maj7, 8 :dim7}

   :bebop-major
   {1 :maj7, 2 :min7, 3 :min7, 4 :maj7,
    5 :dim7, 6 :maj7, 7 :min7, 8 :dim7}

   :bebop-minor
   {1 :min7, 2 :min7, 3 :maj7, 4 :7,
    5 :dim7, 6 :min7, 7 :dim7, 8 :7}})

;; =============================================================================
;; Harmonic Functions
;; =============================================================================

(def harmonic-functions
  "Harmonic function assignments for each scale degree."
  {:major
   {1 :tonic, 2 :subdominant, 3 :tonic, 4 :subdominant,
    5 :dominant, 6 :tonic, 7 :dominant}

   :natural-minor
   {1 :tonic, 2 :subdominant, 3 :tonic, 4 :subdominant,
    5 :dominant, 6 :subdominant, 7 :subtonic}

   :harmonic-minor
   {1 :tonic, 2 :subdominant, 3 :tonic, 4 :subdominant,
    5 :dominant, 6 :subdominant, 7 :dominant}

   :melodic-minor
   {1 :tonic, 2 :subdominant, 3 :tonic, 4 :subdominant,
    5 :dominant, 6 :subdominant, 7 :dominant}

   :dorian
   {1 :tonic, 2 :subdominant, 3 :tonic, 4 :dominant,
    5 :tonic, 6 :subdominant, 7 :subtonic}

   :phrygian
   {1 :tonic, 2 :subdominant, 3 :tonic, 4 :subdominant,
    5 :subdominant, 6 :tonic, 7 :subtonic}

   :lydian
   {1 :tonic, 2 :dominant, 3 :tonic, 4 :dominant,
    5 :tonic, 6 :subdominant, 7 :subdominant}

   :mixolydian
   {1 :tonic, 2 :subdominant, 3 :dominant, 4 :tonic,
    5 :subdominant, 6 :subdominant, 7 :subtonic}

   :locrian
   {1 :tonic, 2 :tonic, 3 :subdominant, 4 :subdominant,
    5 :dominant, 6 :dominant, 7 :subdominant}

   ;; For non-7-note scales, use simpler functional harmony
   :major-pentatonic
   {1 :tonic, 2 :subdominant, 3 :tonic, 4 :dominant, 5 :tonic}

   :minor-pentatonic
   {1 :tonic, 2 :subdominant, 3 :subdominant, 4 :dominant, 5 :subtonic}

   :blues
   {1 :tonic, 2 :subdominant, 3 :subdominant, 4 :dominant, 5 :dominant, 6 :subtonic}

   :whole-tone
   {1 :tonic, 2 :tonic, 3 :tonic, 4 :tonic, 5 :tonic, 6 :tonic}

   :diminished-hw
   {1 :tonic, 2 :subdominant, 3 :tonic, 4 :subdominant,
    5 :dominant, 6 :subdominant, 7 :dominant, 8 :subdominant}

   :diminished-wh
   {1 :tonic, 2 :subdominant, 3 :tonic, 4 :subdominant,
    5 :dominant, 6 :subdominant, 7 :dominant, 8 :subdominant}

   ;; Bebop scales
   :bebop-dominant
   {1 :tonic, 2 :subdominant, 3 :dominant, 4 :tonic,
    5 :subdominant, 6 :subdominant, 7 :subtonic, 8 :dominant}

   :bebop-major
   {1 :tonic, 2 :subdominant, 3 :tonic, 4 :subdominant,
    5 :dominant, 6 :tonic, 7 :dominant, 8 :dominant}

   :bebop-minor
   {1 :tonic, 2 :subdominant, 3 :tonic, 4 :subdominant,
    5 :dominant, 6 :tonic, 7 :dominant, 8 :subtonic}

   :major-blues
   {1 :tonic, 2 :subdominant, 3 :subdominant, 4 :dominant, 5 :tonic, 6 :tonic}})

(defn get-harmonic-function
  "Get the harmonic function of a scale degree.
   Returns :tonic, :subdominant, :dominant, or :subtonic."
  [degree scale-type]
  (let [functions (or (get harmonic-functions scale-type)
                      (get harmonic-functions :major))]
    (get functions degree :unknown)))

(def function-abbreviations
  {:tonic "T"
   :subdominant "SD"
   :dominant "D"
   :subtonic "ST"
   :unknown "?"})

;; =============================================================================
;; Diatonic Chord Generation
;; =============================================================================

(defn diatonic-chord-type
  "Get the chord type for a scale degree.
   Uses pre-computed values when available, falls back to dynamic calculation.
   Options:
   - :seventh? - include seventh (default false)"
  ([degree scale-type]
   (diatonic-chord-type degree scale-type {}))
  ([degree scale-type {:keys [seventh?] :or {seventh? false}}]
   (let [lookup-map (if seventh? diatonic-sevenths diatonic-triads)
         cached-value (get-in lookup-map [scale-type degree])]
     (if cached-value
       cached-value
       ;; Fall back to dynamic calculation for unlisted scales
       (compute-chord-quality-for-degree scale-type degree seventh?)))))

(defn diatonic-chord
  "Build the diatonic chord for a scale degree.
   Returns {:root :type :notes :numeral :function :degree}."
  ([key-root scale-type degree]
   (diatonic-chord key-root scale-type degree {}))
  ([key-root scale-type degree {:keys [seventh? prefer] :or {seventh? false prefer :sharps}}]
   (let [scale-notes (scales/get-scale key-root scale-type {:prefer prefer})
         root (nth scale-notes (dec degree))
         chord-type (diatonic-chord-type degree scale-type {:seventh? seventh?})
         quality (case chord-type
                   (:major :maj7 :6) :major
                   (:minor :min7 :min-maj7 :min6) :minor
                   (:diminished :dim7) :diminished
                   (:half-dim7) :diminished
                   (:augmented :aug7 :aug-maj7) :augmented
                   :major)
         numeral-case (if (#{:major :augmented} quality) :major :minor)
         base-numeral (get-in roman-numerals [degree numeral-case])
         suffix (case chord-type
                  (:maj7 :min-maj7) "maj7"
                  (:7) "7"
                  (:min7) "7"
                  (:half-dim7) "\u00F8" ; ø symbol
                  (:dim7) "\u00B07"
                  (:diminished) "\u00B0"
                  (:augmented) "+"
                  "")
         numeral (str base-numeral suffix)]
     {:root root
      :type chord-type
      :notes (chords/build-chord root chord-type {:prefer prefer})
      :numeral numeral
      :degree degree
      :function (get-harmonic-function degree scale-type)
      :quality quality})))

(defn diatonic-chords
  "Generate all diatonic chords for a key.
   Options:
   - :seventh? - include seventh chords
   - :prefer - :sharps or :flats"
  ([key-root scale-type]
   (diatonic-chords key-root scale-type {}))
  ([key-root scale-type opts]
   (let [scale-def (scales/get-scale-def scale-type)
         num-degrees (count (:intervals scale-def))]
     (mapv #(diatonic-chord key-root scale-type % opts)
           (range 1 (inc num-degrees))))))

;; =============================================================================
;; Roman Numeral Analysis
;; =============================================================================

(defn chord-to-numeral
  "Analyze a chord and return its Roman numeral in a key.
   Returns nil if the chord is not diatonic."
  [chord-root chord-type key-root scale-type]
  (let [scale-notes (scales/get-scale key-root scale-type)
        degree (scales/scale-degree chord-root key-root scale-type)]
    (when degree
      (let [expected-type (diatonic-chord-type degree scale-type {:seventh? (>= (count (chords/get-chord-tones chord-type)) 4)})
            diatonic? (= expected-type chord-type)]
        (when diatonic?
          (:numeral (diatonic-chord key-root scale-type degree {:seventh? (>= (count (chords/get-chord-tones chord-type)) 4)})))))))

(defn numeral-to-chord
  "Convert a Roman numeral to a chord in a key.
   Handles basic numerals like 'ii', 'V7', 'viio'."
  [numeral key-root scale-type]
  (let [;; Parse the numeral
        upper? (re-matches #"^[IViv]+.*" numeral)
        base (re-find #"^[IViv]+" numeral)
        suffix (subs numeral (count base))
        degree (get numeral->degree (clojure.string/upper-case base))
        seventh? (clojure.string/includes? suffix "7")]
    (when degree
      (diatonic-chord key-root scale-type degree {:seventh? seventh?}))))

(defn analyze-chord-in-key
  "Analyze a chord's relationship to a key.
   Returns {:numeral :function :diatonic? :analysis}."
  [chord-root chord-type key-root scale-type]
  (let [numeral (chord-to-numeral chord-root chord-type key-root scale-type)
        degree (scales/scale-degree chord-root key-root scale-type)
        function (when degree (get-harmonic-function degree scale-type))]
    {:root chord-root
     :type chord-type
     :numeral numeral
     :degree degree
     :function function
     :diatonic? (boolean numeral)
     :key {:root key-root :scale-type scale-type}}))

;; =============================================================================
;; Key Analysis
;; =============================================================================

(defn possible-keys
  "Given a chord, find keys where it could be diatonic.
   Returns a list of {:key-root :scale-type :degree :function}."
  [chord-root chord-type]
  (for [key-root core/chromatic-notes
        scale-type [:major :natural-minor :harmonic-minor]
        :let [degree (scales/scale-degree chord-root key-root scale-type)
              expected (when degree (diatonic-chord-type degree scale-type {:seventh? (>= (count (chords/get-chord-tones chord-type)) 4)}))]
        :when (= expected chord-type)]
    {:key-root key-root
     :scale-type scale-type
     :degree degree
     :function (get-harmonic-function degree scale-type)}))

(defn common-keys
  "Find keys that contain all given chords as diatonic chords."
  [chords]
  (let [chord-keys (map (fn [{:keys [root type]}]
                          (set (map (fn [{:keys [key-root scale-type]}]
                                      [key-root scale-type])
                                    (possible-keys root type))))
                        chords)]
    (when (seq chord-keys)
      (apply clojure.set/intersection chord-keys))))

;; =============================================================================
;; Chord Tendency & Resolution
;; =============================================================================

(def resolution-tendencies
  "Common resolution tendencies for scale degrees."
  {;; Major key tendencies
   [:major 7] #{1}           ; vii -> I
   [:major 5] #{1}           ; V -> I
   [:major 4] #{5 3}         ; IV -> V or iii
   [:major 2] #{5 3}         ; ii -> V or iii
   [:major 6] #{5 4 2}       ; vi -> V, IV, or ii

   ;; Minor key tendencies
   [:natural-minor 7] #{1 3} ; VII -> i or III
   [:natural-minor 5] #{1}   ; v -> i (weak)
   [:harmonic-minor 5] #{1}  ; V -> i (strong)
   [:harmonic-minor 7] #{1}  ; vii -> i
   })

(defn resolution-targets
  "Get likely resolution targets for a chord in a key."
  [chord-root chord-type key-root scale-type]
  (let [degree (scales/scale-degree chord-root key-root scale-type)
        targets (get resolution-tendencies [scale-type degree] #{})]
    (mapv #(diatonic-chord key-root scale-type %) targets)))

(defn suggests-cadence?
  "Check if a chord sequence suggests a cadence pattern."
  [chord1 chord2 key-root scale-type]
  (let [deg1 (scales/scale-degree (:root chord1) key-root scale-type)
        deg2 (scales/scale-degree (:root chord2) key-root scale-type)]
    (cond
      (and (= deg1 5) (= deg2 1)) :authentic
      (and (= deg1 4) (= deg2 1)) :plagal
      (and (= deg1 5) (= deg2 6)) :deceptive
      (and (#{4 2} deg1) (= deg2 5)) :half
      :else nil)))

;; =============================================================================
;; Display Utilities
;; =============================================================================

(defn format-chord-name
  "Format a chord for display (e.g., 'Cmaj7', 'F#m7')."
  [root chord-type]
  (chords/chord-symbol root chord-type))

(defn format-numeral-with-function
  "Format a numeral with its function abbreviation."
  [numeral function]
  (str numeral " (" (get function-abbreviations function "?") ")"))

(defn format-diatonic-display
  "Format all diatonic chords for display."
  [key-root scale-type opts]
  (let [chords (diatonic-chords key-root scale-type opts)]
    (mapv (fn [c]
            {:display (format-chord-name (:root c) (:type c))
             :numeral (:numeral c)
             :function-abbr (get function-abbreviations (:function c))
             :chord c})
          chords)))
