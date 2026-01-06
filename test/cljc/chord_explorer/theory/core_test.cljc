(ns chord-explorer.theory.core-test
  (:require #?(:clj [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer-macros [deftest testing is are]])
            [chord-explorer.theory.core :as core]))

;; =============================================================================
;; Note Representation Tests
;; =============================================================================

(deftest note->semitone-test
  (testing "Natural notes map to correct semitones"
    (is (= 0 (core/normalize-note :C)))
    (is (= 2 (core/normalize-note :D)))
    (is (= 4 (core/normalize-note :E)))
    (is (= 5 (core/normalize-note :F)))
    (is (= 7 (core/normalize-note :G)))
    (is (= 9 (core/normalize-note :A)))
    (is (= 11 (core/normalize-note :B))))

  (testing "Sharp notes map correctly"
    (is (= 1 (core/normalize-note :C#)))
    (is (= 3 (core/normalize-note :D#)))
    (is (= 6 (core/normalize-note :F#)))
    (is (= 8 (core/normalize-note :G#)))
    (is (= 10 (core/normalize-note :A#))))

  (testing "Flat notes map correctly"
    (is (= 1 (core/normalize-note :Db)))
    (is (= 3 (core/normalize-note :Eb)))
    (is (= 6 (core/normalize-note :Gb)))
    (is (= 8 (core/normalize-note :Ab)))
    (is (= 10 (core/normalize-note :Bb))))

  (testing "Enharmonic equivalents have same semitone"
    (is (= (core/normalize-note :C#) (core/normalize-note :Db)))
    (is (= (core/normalize-note :F#) (core/normalize-note :Gb)))
    (is (= (core/normalize-note :B) (core/normalize-note :Cb)))))

;; =============================================================================
;; Transposition Tests
;; =============================================================================

(deftest transpose-test
  (testing "Transpose up by semitones"
    (is (= :C# (core/transpose :C 1)))
    (is (= :D (core/transpose :C 2)))
    (is (= :E (core/transpose :C 4)))
    (is (= :G (core/transpose :C 7))))

  (testing "Transpose wraps around octave"
    (is (= :C (core/transpose :C 12)))
    (is (= :D (core/transpose :C 14)))
    (is (= :A (core/transpose :G 2))))

  (testing "Transpose down (negative semitones)"
    (is (= :B (core/transpose :C -1)))
    (is (= :A (core/transpose :C -3)))
    (is (= :G (core/transpose :C -5))))

  (testing "Transpose with flat preference"
    (is (= :Db (core/transpose :C 1 {:prefer :flats})))
    (is (= :Eb (core/transpose :C 3 {:prefer :flats})))
    (is (= :Bb (core/transpose :C 10 {:prefer :flats})))))

;; =============================================================================
;; Interval Tests
;; =============================================================================

(deftest interval-between-test
  (testing "Intervals between natural notes"
    (is (= 0 (core/interval-between :C :C)))
    (is (= 2 (core/interval-between :C :D)))
    (is (= 4 (core/interval-between :C :E)))
    (is (= 5 (core/interval-between :C :F)))
    (is (= 7 (core/interval-between :C :G)))
    (is (= 9 (core/interval-between :C :A)))
    (is (= 11 (core/interval-between :C :B))))

  (testing "Intervals wrap correctly"
    (is (= 1 (core/interval-between :B :C)))
    (is (= 11 (core/interval-between :C :B)))
    (is (= 5 (core/interval-between :G :C))))

  (testing "Intervals with accidentals"
    (is (= 1 (core/interval-between :C :C#)))
    (is (= 3 (core/interval-between :C :Eb)))
    (is (= 6 (core/interval-between :C :F#)))))

(deftest interval-name-test
  (testing "Interval names"
    (is (= :P1 (core/interval-name 0)))
    (is (= :m2 (core/interval-name 1)))
    (is (= :M2 (core/interval-name 2)))
    (is (= :m3 (core/interval-name 3)))
    (is (= :M3 (core/interval-name 4)))
    (is (= :P4 (core/interval-name 5)))
    (is (= :A4 (core/interval-name 6)))
    (is (= :P5 (core/interval-name 7)))
    (is (= :m6 (core/interval-name 8)))
    (is (= :M6 (core/interval-name 9)))
    (is (= :m7 (core/interval-name 10)))
    (is (= :M7 (core/interval-name 11)))))

;; =============================================================================
;; Enharmonic Tests
;; =============================================================================

(deftest notes-equal-test
  (testing "Enharmonic equivalence"
    (is (core/notes-equal? :C# :Db))
    (is (core/notes-equal? :D# :Eb))
    (is (core/notes-equal? :F# :Gb))
    (is (core/notes-equal? :G# :Ab))
    (is (core/notes-equal? :A# :Bb)))

  (testing "Same note is equal"
    (is (core/notes-equal? :C :C))
    (is (core/notes-equal? :F# :F#)))

  (testing "Different notes are not equal"
    (is (not (core/notes-equal? :C :D)))
    (is (not (core/notes-equal? :C# :D#)))))

(deftest get-enharmonic-test
  (testing "Get enharmonic equivalents"
    (is (= :Db (core/get-enharmonic :C#)))
    (is (= :C# (core/get-enharmonic :Db)))
    (is (= :Gb (core/get-enharmonic :F#)))
    (is (= :F# (core/get-enharmonic :Gb)))))

;; =============================================================================
;; Note with Octave Tests
;; =============================================================================

(deftest midi-conversion-test
  (testing "Note to MIDI conversion"
    (is (= 60 (core/note->midi {:note :C :octave 4})))  ; Middle C
    (is (= 69 (core/note->midi {:note :A :octave 4})))  ; A440
    (is (= 48 (core/note->midi {:note :C :octave 3})))
    (is (= 72 (core/note->midi {:note :C :octave 5}))))

  (testing "MIDI to note conversion"
    (is (= {:note :C :octave 4} (core/midi->note 60)))
    (is (= {:note :A :octave 4} (core/midi->note 69)))
    (is (= {:note :C :octave 3} (core/midi->note 48))))

  (testing "Round-trip conversion"
    (let [note {:note :F# :octave 3}
          midi (core/note->midi note)]
      (is (= 4 (:octave (core/midi->note midi)))))))

(deftest transpose-with-octave-test
  (testing "Transpose note with octave"
    (is (= {:note :D :octave 4}
           (core/transpose-with-octave {:note :C :octave 4} 2)))
    (is (= {:note :C :octave 5}
           (core/transpose-with-octave {:note :C :octave 4} 12)))
    (is (= {:note :G :octave 3}
           (core/transpose-with-octave {:note :C :octave 4} -5)))))

;; =============================================================================
;; Utility Function Tests
;; =============================================================================

(deftest all-notes-test
  (testing "All chromatic notes with sharps"
    (let [notes (core/all-notes)]
      (is (= 12 (count notes)))
      (is (= :C (first notes)))
      (is (some #(= :C# %) notes))
      (is (not (some #(= :Db %) notes)))))

  (testing "All chromatic notes with flats"
    (let [notes (core/all-notes {:prefer :flats})]
      (is (= 12 (count notes)))
      (is (= :C (first notes)))
      (is (some #(= :Db %) notes))
      (is (not (some #(= :C# %) notes))))))

(deftest next-prev-note-test
  (testing "Next note"
    (is (= :C# (core/next-note :C)))
    (is (= :C (core/next-note :B)))
    (is (= :G (core/next-note :F#))))

  (testing "Previous note"
    (is (= :B (core/prev-note :C)))
    (is (= :A# (core/prev-note :B)))
    (is (= :F (core/prev-note :F#)))))
