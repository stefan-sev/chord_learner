(ns chord-explorer.db
  "Application state structure for Re-frame.")

(def default-db
  "Default application state."
  {:current-key :C
   :scale-type :major

   ;; Progression state
   :progression {:id nil
                 :name "New Progression"
                 :chords []}

   ;; UI state
   :selected-chord-index nil
   :selected-voicing nil
   :voicing-mode :piano       ; :piano or :guitar
   :show-seventh? false

   ;; Analysis panel
   :show-analysis? true
   :analysis-results nil

   ;; Diatonic chords cache
   :diatonic-chords []

   ;; Secondary dominants and modal interchange
   :secondary-dominants []
   :borrowed-chords []

   ;; Loading states
   :loading? false
   :error nil

   ;; Audio
   :audio-enabled? true
   :playback-tempo 120

   ;; Saved progressions
   :saved-progressions []
   :show-save-modal? false
   :show-load-modal? false})

(def note-order
  "Chromatic note order for dropdowns."
  [:C :C# :D :D# :E :F :F# :G :G# :A :A# :B])

(def scale-types
  "Available scale types for dropdown, organized by category."
  [;; Diatonic
   {:key :major :name "Major (Ionian)" :category "Diatonic"}
   {:key :natural-minor :name "Natural Minor (Aeolian)" :category "Diatonic"}

   ;; Minor Variants
   {:key :harmonic-minor :name "Harmonic Minor" :category "Minor Variants"}
   {:key :melodic-minor :name "Melodic Minor" :category "Minor Variants"}

   ;; Modes of Major
   {:key :dorian :name "Dorian" :category "Major Modes"}
   {:key :phrygian :name "Phrygian" :category "Major Modes"}
   {:key :lydian :name "Lydian" :category "Major Modes"}
   {:key :mixolydian :name "Mixolydian" :category "Major Modes"}
   {:key :locrian :name "Locrian" :category "Major Modes"}

   ;; Modes of Melodic Minor
   {:key :lydian-dominant :name "Lydian Dominant" :category "Melodic Minor Modes"}
   {:key :super-locrian :name "Super Locrian (Altered)" :category "Melodic Minor Modes"}
   {:key :lydian-augmented :name "Lydian Augmented" :category "Melodic Minor Modes"}
   {:key :locrian-nat2 :name "Locrian Natural 2" :category "Melodic Minor Modes"}

   ;; Modes of Harmonic Minor
   {:key :phrygian-dominant :name "Phrygian Dominant" :category "Harmonic Minor Modes"}

   ;; Pentatonic & Blues
   {:key :major-pentatonic :name "Major Pentatonic" :category "Pentatonic"}
   {:key :minor-pentatonic :name "Minor Pentatonic" :category "Pentatonic"}
   {:key :blues :name "Blues" :category "Blues"}
   {:key :major-blues :name "Major Blues" :category "Blues"}

   ;; Symmetric
   {:key :whole-tone :name "Whole Tone" :category "Symmetric"}
   {:key :diminished-hw :name "Diminished (Half-Whole)" :category "Symmetric"}
   {:key :diminished-wh :name "Diminished (Whole-Half)" :category "Symmetric"}

   ;; Bebop
   {:key :bebop-dominant :name "Bebop Dominant" :category "Bebop"}
   {:key :bebop-major :name "Bebop Major" :category "Bebop"}
   {:key :bebop-minor :name "Bebop Minor" :category "Bebop"}])

(def chord-types
  "Chord types for quick selection, organized by category."
  [;; Triads
   {:key :major :name "Major" :symbol "" :category "Triads"}
   {:key :minor :name "Minor" :symbol "m" :category "Triads"}
   {:key :diminished :name "Diminished" :symbol "dim" :category "Triads"}
   {:key :augmented :name "Augmented" :symbol "aug" :category "Triads"}
   {:key :sus4 :name "Sus4" :symbol "sus4" :category "Triads"}
   {:key :sus2 :name "Sus2" :symbol "sus2" :category "Triads"}

   ;; Seventh Chords
   {:key :maj7 :name "Major 7" :symbol "maj7" :category "Sevenths"}
   {:key :7 :name "Dominant 7" :symbol "7" :category "Sevenths"}
   {:key :min7 :name "Minor 7" :symbol "m7" :category "Sevenths"}
   {:key :min-maj7 :name "Minor Major 7" :symbol "m(maj7)" :category "Sevenths"}
   {:key :dim7 :name "Diminished 7" :symbol "dim7" :category "Sevenths"}
   {:key :half-dim7 :name "Half Diminished 7" :symbol "m7b5" :category "Sevenths"}
   {:key :7sus4 :name "7sus4" :symbol "7sus4" :category "Sevenths"}

   ;; Sixth Chords
   {:key :6 :name "Major 6" :symbol "6" :category "Sixths"}
   {:key :min6 :name "Minor 6" :symbol "m6" :category "Sixths"}

   ;; Extended Chords
   {:key :9 :name "Dominant 9" :symbol "9" :category "Extended"}
   {:key :maj9 :name "Major 9" :symbol "maj9" :category "Extended"}
   {:key :min9 :name "Minor 9" :symbol "m9" :category "Extended"}
   {:key :add9 :name "Add 9" :symbol "add9" :category "Extended"}
   {:key :11 :name "Dominant 11" :symbol "11" :category "Extended"}
   {:key :min11 :name "Minor 11" :symbol "m11" :category "Extended"}
   {:key :13 :name "Dominant 13" :symbol "13" :category "Extended"}

   ;; Altered Chords
   {:key :7b5 :name "7b5" :symbol "7b5" :category "Altered"}
   {:key :7#5 :name "7#5" :symbol "7#5" :category "Altered"}
   {:key :7b9 :name "7b9" :symbol "7b9" :category "Altered"}
   {:key :7#9 :name "7#9" :symbol "7#9" :category "Altered"}
   {:key :7#11 :name "7#11" :symbol "7#11" :category "Altered"}
   {:key :7alt :name "Altered" :symbol "7alt" :category "Altered"}])
