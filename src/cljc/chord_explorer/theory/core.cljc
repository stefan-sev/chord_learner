(ns chord-explorer.theory.core
  "Core music theory primitives: notes, intervals, and transposition.")

;; =============================================================================
;; Note Representation
;; =============================================================================

(def chromatic-notes
  "All 12 chromatic notes using sharps."
  [:C :C# :D :D# :E :F :F# :G :G# :A :A# :B])

(def chromatic-notes-flat
  "All 12 chromatic notes using flats."
  [:C :Db :D :Eb :E :F :Gb :G :Ab :A :Bb :B])

(def note->semitone
  "Map note keywords to semitone values (0-11)."
  {:C 0 :C# 1 :Db 1
   :D 2 :D# 3 :Eb 3
   :E 4 :Fb 4 :E# 5
   :F 5 :F# 6 :Gb 6
   :G 7 :G# 8 :Ab 8
   :A 9 :A# 10 :Bb 10
   :B 11 :Cb 11 :B# 0})

(def semitone->note-sharp
  "Map semitone values to note keywords using sharps."
  {0 :C, 1 :C#, 2 :D, 3 :D#, 4 :E, 5 :F,
   6 :F#, 7 :G, 8 :G#, 9 :A, 10 :A#, 11 :B})

(def semitone->note-flat
  "Map semitone values to note keywords using flats."
  {0 :C, 1 :Db, 2 :D, 3 :Eb, 4 :E, 5 :F,
   6 :Gb, 7 :G, 8 :Ab, 9 :A, 10 :Bb, 11 :B})

(def enharmonic-equivalents
  "Map notes to their enharmonic equivalents."
  {:C# :Db, :Db :C#
   :D# :Eb, :Eb :D#
   :E :Fb, :Fb :E, :E# :F, :F :E#
   :F# :Gb, :Gb :F#
   :G# :Ab, :Ab :G#
   :A# :Bb, :Bb :A#
   :B :Cb, :Cb :B, :B# :C, :C :B#})

;; =============================================================================
;; Interval Representation
;; =============================================================================

(def interval-names
  "Map interval keywords to semitone distances."
  {:P1 0   ; Perfect unison
   :m2 1   ; Minor 2nd
   :M2 2   ; Major 2nd
   :m3 3   ; Minor 3rd
   :M3 4   ; Major 3rd
   :P4 5   ; Perfect 4th
   :A4 6   ; Augmented 4th (tritone)
   :d5 6   ; Diminished 5th (tritone)
   :P5 7   ; Perfect 5th
   :m6 8   ; Minor 6th
   :M6 9   ; Major 6th
   :d7 9   ; Diminished 7th
   :m7 10  ; Minor 7th
   :M7 11  ; Major 7th
   :P8 12  ; Perfect octave
   :m9 13  ; Minor 9th
   :M9 14  ; Major 9th
   :m10 15 ; Minor 10th
   :M10 16 ; Major 10th
   :P11 17 ; Perfect 11th
   :A11 18 ; Augmented 11th
   :P12 19 ; Perfect 12th
   :m13 20 ; Minor 13th
   :M13 21 ; Major 13th
   :A13 22 ; Augmented 13th
   :P15 24 ; Double octave
   })

(def semitone->interval
  "Map semitone distances to interval names."
  {0 :P1, 1 :m2, 2 :M2, 3 :m3, 4 :M3, 5 :P4,
   6 :A4, 7 :P5, 8 :m6, 9 :M6, 10 :m7, 11 :M7, 12 :P8})

;; =============================================================================
;; Note Functions
;; =============================================================================

(defn normalize-note
  "Convert a note to its canonical semitone value (0-11)."
  [note]
  (get note->semitone note))

(defn note->string
  "Convert a note keyword to a display string."
  [note]
  (-> note name
      (clojure.string/replace "#" "\u266F")  ; Sharp symbol
      (clojure.string/replace "b" "\u266D"))) ; Flat symbol

(defn sharp-note?
  "Returns true if the note is a sharp."
  [note]
  (clojure.string/includes? (name note) "#"))

(defn flat-note?
  "Returns true if the note is a flat."
  [note]
  (and (clojure.string/includes? (name note) "b")
       (not= note :B)))

(defn natural-note?
  "Returns true if the note is a natural (no accidental)."
  [note]
  (contains? #{:C :D :E :F :G :A :B} note))

(defn get-enharmonic
  "Get the enharmonic equivalent of a note, or nil if none exists."
  [note]
  (get enharmonic-equivalents note))

(defn notes-equal?
  "Check if two notes are enharmonically equivalent."
  [note1 note2]
  (= (normalize-note note1) (normalize-note note2)))

(defn prefer-sharps
  "Convert a semitone value to a note, preferring sharps."
  [semitone]
  (get semitone->note-sharp (mod semitone 12)))

(defn prefer-flats
  "Convert a semitone value to a note, preferring flats."
  [semitone]
  (get semitone->note-flat (mod semitone 12)))

;; =============================================================================
;; Transposition Functions
;; =============================================================================

(defn transpose
  "Transpose a note by a number of semitones.
   Options:
   - :prefer :sharps (default) or :flats for spelling"
  ([note semitones]
   (transpose note semitones {:prefer :sharps}))
  ([note semitones {:keys [prefer] :or {prefer :sharps}}]
   (let [current (normalize-note note)
         new-value (mod (+ current semitones) 12)]
     (if (= prefer :flats)
       (prefer-flats new-value)
       (prefer-sharps new-value)))))

(defn transpose-preserving-spelling
  "Transpose a note while trying to preserve sharp/flat spelling.
   If the original note was flat, result will use flats."
  [note semitones]
  (let [prefer (cond
                 (flat-note? note) :flats
                 :else :sharps)]
    (transpose note semitones {:prefer prefer})))

;; =============================================================================
;; Interval Functions
;; =============================================================================

(defn interval-between
  "Get the interval in semitones between two notes.
   Always returns a positive value (0-11)."
  [note1 note2]
  (let [s1 (normalize-note note1)
        s2 (normalize-note note2)]
    (mod (- s2 s1) 12)))

(defn interval-name
  "Get the name of an interval given its semitone distance."
  [semitones]
  (get semitone->interval (mod semitones 12)))

(defn semitones->interval
  "Convert a semitone count to an interval keyword."
  [semitones]
  (interval-name semitones))

(defn interval->semitones
  "Convert an interval keyword to semitone count."
  [interval]
  (get interval-names interval))

;; =============================================================================
;; Note with Octave
;; =============================================================================

(defn note-with-octave
  "Create a note with octave specification.
   Returns {:note :C :octave 4} for middle C."
  [note octave]
  {:note note :octave octave})

(defn note->midi
  "Convert a note with octave to MIDI note number.
   Middle C (C4) = 60."
  [{:keys [note octave]}]
  (+ (* (+ octave 1) 12) (normalize-note note)))

(defn midi->note
  "Convert a MIDI note number to a note with octave.
   Options:
   - :prefer :sharps (default) or :flats"
  ([midi]
   (midi->note midi {:prefer :sharps}))
  ([midi {:keys [prefer] :or {prefer :sharps}}]
   (let [octave (- (quot midi 12) 1)
         semitone (mod midi 12)
         note (if (= prefer :flats)
                (prefer-flats semitone)
                (prefer-sharps semitone))]
     {:note note :octave octave})))

(defn transpose-with-octave
  "Transpose a note with octave by a number of semitones."
  [{:keys [note octave] :as note-with-oct} semitones]
  (let [midi (note->midi note-with-oct)
        new-midi (+ midi semitones)]
    (midi->note new-midi)))

;; =============================================================================
;; Utility Functions
;; =============================================================================

(defn all-notes
  "Get all 12 chromatic notes.
   Options:
   - :prefer :sharps (default) or :flats"
  ([]
   (all-notes {:prefer :sharps}))
  ([{:keys [prefer] :or {prefer :sharps}}]
   (if (= prefer :flats)
     chromatic-notes-flat
     chromatic-notes)))

(defn note-index
  "Get the index (0-11) of a note in the chromatic scale."
  [note]
  (normalize-note note))

(defn next-note
  "Get the next semitone up from a note."
  [note]
  (transpose note 1))

(defn prev-note
  "Get the previous semitone down from a note."
  [note]
  (transpose note -1))

(defn notes-in-range
  "Generate all notes between two MIDI note numbers, inclusive."
  ([start-midi end-midi]
   (notes-in-range start-midi end-midi {:prefer :sharps}))
  ([start-midi end-midi opts]
   (mapv #(midi->note % opts) (range start-midi (inc end-midi)))))
