(ns chord-explorer.theory.harmony-test
  (:require #?(:clj [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer-macros [deftest testing is are]])
            [chord-explorer.theory.harmony :as harmony]
            [chord-explorer.theory.core :as core]))

;; =============================================================================
;; Diatonic Chord Tests
;; =============================================================================

(deftest diatonic-chords-test
  (testing "C major diatonic triads"
    (let [chords (harmony/diatonic-chords :C :major)]
      (is (= 7 (count chords)))
      ;; Check each chord type
      (is (= :major (:type (nth chords 0))))  ; I = C
      (is (= :minor (:type (nth chords 1))))  ; ii = Dm
      (is (= :minor (:type (nth chords 2))))  ; iii = Em
      (is (= :major (:type (nth chords 3))))  ; IV = F
      (is (= :major (:type (nth chords 4))))  ; V = G
      (is (= :minor (:type (nth chords 5))))  ; vi = Am
      (is (= :diminished (:type (nth chords 6))))))  ; vii = Bdim

  (testing "C major diatonic sevenths"
    (let [chords (harmony/diatonic-chords :C :major {:seventh? true})]
      (is (= :maj7 (:type (nth chords 0))))    ; Imaj7 = Cmaj7
      (is (= :min7 (:type (nth chords 1))))    ; ii7 = Dm7
      (is (= :min7 (:type (nth chords 2))))    ; iii7 = Em7
      (is (= :maj7 (:type (nth chords 3))))    ; IVmaj7 = Fmaj7
      (is (= :7 (:type (nth chords 4))))       ; V7 = G7
      (is (= :min7 (:type (nth chords 5))))    ; vi7 = Am7
      (is (= :half-dim7 (:type (nth chords 6)))))) ; viio7 = Bm7b5

  (testing "A natural minor diatonic triads"
    (let [chords (harmony/diatonic-chords :A :natural-minor)]
      (is (= :minor (:type (nth chords 0))))      ; i = Am
      (is (= :diminished (:type (nth chords 1)))) ; iio = Bdim
      (is (= :major (:type (nth chords 2))))      ; III = C
      (is (= :minor (:type (nth chords 3))))      ; iv = Dm
      (is (= :minor (:type (nth chords 4))))      ; v = Em
      (is (= :major (:type (nth chords 5))))      ; VI = F
      (is (= :major (:type (nth chords 6)))))))   ; VII = G

(deftest diatonic-chord-roots-test
  (testing "C major chord roots"
    (let [chords (harmony/diatonic-chords :C :major)]
      (is (core/notes-equal? :C (:root (nth chords 0))))
      (is (core/notes-equal? :D (:root (nth chords 1))))
      (is (core/notes-equal? :E (:root (nth chords 2))))
      (is (core/notes-equal? :F (:root (nth chords 3))))
      (is (core/notes-equal? :G (:root (nth chords 4))))
      (is (core/notes-equal? :A (:root (nth chords 5))))
      (is (core/notes-equal? :B (:root (nth chords 6)))))))

;; =============================================================================
;; Roman Numeral Tests
;; =============================================================================

(deftest chord-numeral-test
  (testing "Diatonic chord numerals in C major"
    (is (= "I" (harmony/chord-to-numeral :C :major :C :major)))
    (is (= "ii" (harmony/chord-to-numeral :D :minor :C :major)))
    (is (= "iii" (harmony/chord-to-numeral :E :minor :C :major)))
    (is (= "IV" (harmony/chord-to-numeral :F :major :C :major)))
    (is (= "V" (harmony/chord-to-numeral :G :major :C :major)))
    (is (= "vi" (harmony/chord-to-numeral :A :minor :C :major))))

  (testing "Non-diatonic chord returns nil"
    (is (nil? (harmony/chord-to-numeral :Bb :major :C :major)))
    (is (nil? (harmony/chord-to-numeral :C :minor :C :major)))))

;; =============================================================================
;; Harmonic Function Tests
;; =============================================================================

(deftest harmonic-function-test
  (testing "Harmonic functions in major key"
    (is (= :tonic (harmony/get-harmonic-function 1 :major)))
    (is (= :subdominant (harmony/get-harmonic-function 2 :major)))
    (is (= :tonic (harmony/get-harmonic-function 3 :major)))
    (is (= :subdominant (harmony/get-harmonic-function 4 :major)))
    (is (= :dominant (harmony/get-harmonic-function 5 :major)))
    (is (= :tonic (harmony/get-harmonic-function 6 :major)))
    (is (= :dominant (harmony/get-harmonic-function 7 :major)))))

(deftest diatonic-chord-function-test
  (testing "Diatonic chords have correct functions"
    (let [chords (harmony/diatonic-chords :C :major)]
      (is (= :tonic (:function (nth chords 0))))       ; I
      (is (= :subdominant (:function (nth chords 1)))) ; ii
      (is (= :tonic (:function (nth chords 2))))       ; iii
      (is (= :subdominant (:function (nth chords 3)))) ; IV
      (is (= :dominant (:function (nth chords 4))))    ; V
      (is (= :tonic (:function (nth chords 5))))       ; vi
      (is (= :dominant (:function (nth chords 6))))))) ; vii

;; =============================================================================
;; Key Analysis Tests
;; =============================================================================

(deftest possible-keys-test
  (testing "C major chord is diatonic in multiple keys"
    (let [keys (harmony/possible-keys :C :major)]
      ;; C major is I in C, IV in G, V in F
      (is (some #(and (= :C (:key-root %)) (= :major (:scale-type %))) keys))
      (is (some #(and (= :G (:key-root %)) (= :major (:scale-type %))) keys))
      (is (some #(and (= :F (:key-root %)) (= :major (:scale-type %))) keys))))

  (testing "Am is diatonic in C major"
    (let [keys (harmony/possible-keys :A :minor)]
      (is (some #(and (= :C (:key-root %)) (= :major (:scale-type %))) keys)))))

;; =============================================================================
;; Cadence Tests
;; =============================================================================

(deftest cadence-detection-test
  (testing "Authentic cadence V -> I"
    (is (= :authentic
           (harmony/suggests-cadence?
            {:root :G}
            {:root :C}
            :C :major))))

  (testing "Plagal cadence IV -> I"
    (is (= :plagal
           (harmony/suggests-cadence?
            {:root :F}
            {:root :C}
            :C :major))))

  (testing "Deceptive cadence V -> vi"
    (is (= :deceptive
           (harmony/suggests-cadence?
            {:root :G}
            {:root :A}
            :C :major))))

  (testing "Half cadence ii -> V"
    (is (= :half
           (harmony/suggests-cadence?
            {:root :D}
            {:root :G}
            :C :major)))))

;; =============================================================================
;; Chord Formatting Tests
;; =============================================================================

(deftest format-chord-name-test
  (testing "Format chord names"
    (is (= "Cmaj7" (harmony/format-chord-name :C :maj7)))
    (is (= "Dm7" (harmony/format-chord-name :D :min7)))
    (is (= "G7" (harmony/format-chord-name :G :7)))
    (is (= "Am" (harmony/format-chord-name :A :minor)))))
