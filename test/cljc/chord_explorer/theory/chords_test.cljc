(ns chord-explorer.theory.chords-test
  (:require #?(:clj [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer-macros [deftest testing is are]])
            [chord-explorer.theory.chords :as chords]
            [chord-explorer.theory.core :as core]))

;; =============================================================================
;; Chord Construction Tests
;; =============================================================================

(deftest build-chord-test
  (testing "Major triads"
    (is (= [:C :E :G] (chords/build-chord :C :major)))
    (is (= [:G :B :D] (chords/build-chord :G :major)))
    (is (= [:F :A :C] (chords/build-chord :F :major))))

  (testing "Minor triads"
    (is (= [:A :C :E] (chords/build-chord :A :minor)))
    (is (= [:D :F :A] (chords/build-chord :D :minor)))
    (is (= [:E :G :B] (chords/build-chord :E :minor))))

  (testing "Diminished triads"
    (is (= [:B :D :F] (chords/build-chord :B :diminished)))
    (is (= [:C :D# :F#] (chords/build-chord :C :diminished))))

  (testing "Augmented triads"
    (is (= [:C :E :G#] (chords/build-chord :C :augmented)))
    (is (= [:G :B :D#] (chords/build-chord :G :augmented))))

  (testing "Suspended chords"
    (is (= [:C :D :G] (chords/build-chord :C :sus2)))
    (is (= [:C :F :G] (chords/build-chord :C :sus4)))))

(deftest build-seventh-chord-test
  (testing "Major 7th chords"
    (is (= [:C :E :G :B] (chords/build-chord :C :maj7)))
    (is (= [:F :A :C :E] (chords/build-chord :F :maj7))))

  (testing "Dominant 7th chords"
    (is (= [:G :B :D :F] (chords/build-chord :G :7)))
    (is (= [:C :E :G :A#] (chords/build-chord :C :7))))

  (testing "Minor 7th chords"
    (is (= [:D :F :A :C] (chords/build-chord :D :min7)))
    (is (= [:A :C :E :G] (chords/build-chord :A :min7))))

  (testing "Half-diminished 7th chords"
    (is (= [:B :D :F :A] (chords/build-chord :B :half-dim7))))

  (testing "Diminished 7th chords"
    ;; Cdim7 = C, Eb, Gb, Bbb (=A)
    (is (= [:C :D# :F# :A] (chords/build-chord :C :dim7)))))

;; =============================================================================
;; Extended Chord Tests
;; =============================================================================

(deftest build-extended-chord-test
  (testing "9th chords"
    ;; C9 = C E G Bb D
    (let [c9 (chords/build-chord :C :9)]
      (is (= 5 (count c9)))
      (is (= :C (first c9)))
      (is (= :D (last c9)))))

  (testing "11th chords"
    (let [c11 (chords/build-chord :C :11)]
      (is (= 6 (count c11)))))

  (testing "13th chords"
    (let [c13 (chords/build-chord :C :13)]
      (is (= 6 (count c13))))))

;; =============================================================================
;; Chord Symbol Tests
;; =============================================================================

(deftest chord-symbol-test
  (testing "Chord symbols"
    (is (= "C" (chords/chord-symbol :C :major)))
    (is (= "Am" (chords/chord-symbol :A :minor)))
    (is (= "Gmaj7" (chords/chord-symbol :G :maj7)))
    (is (= "D7" (chords/chord-symbol :D :7)))
    (is (= "Em7" (chords/chord-symbol :E :min7)))
    (is (= "Bdim" (chords/chord-symbol :B :diminished)))
    (is (= "Caug" (chords/chord-symbol :C :augmented)))))

;; =============================================================================
;; Inversion Tests
;; =============================================================================

(deftest inversion-test
  (testing "Get all inversions"
    (let [inversions (chords/get-inversions [:C :E :G])]
      (is (= 3 (count inversions)))
      (is (= [:C :E :G] (first inversions)))   ; Root position
      (is (= [:E :G :C] (second inversions)))  ; First inversion
      (is (= [:G :C :E] (nth inversions 2))))) ; Second inversion

  (testing "Get specific inversion"
    (is (= [:C :E :G] (chords/get-inversion [:C :E :G] 0)))
    (is (= [:E :G :C] (chords/get-inversion [:C :E :G] 1)))
    (is (= [:G :C :E] (chords/get-inversion [:C :E :G] 2)))))

;; =============================================================================
;; Chord Identification Tests
;; =============================================================================

(deftest identify-chord-test
  (testing "Identify major triad"
    (let [results (chords/identify-chord [:C :E :G])]
      (is (some #(and (= :C (:root %)) (= :major (:type %))) results))))

  (testing "Identify minor triad"
    (let [results (chords/identify-chord [:A :C :E])]
      (is (some #(and (= :A (:root %)) (= :minor (:type %))) results))))

  (testing "Identify dominant 7th"
    (let [results (chords/identify-chord [:G :B :D :F])]
      (is (some #(and (= :G (:root %)) (= :7 (:type %))) results))))

  (testing "Identify chord in inversion"
    (let [results (chords/identify-chord [:E :G :C])]
      ;; Should still find C major
      (is (some #(and (= :C (:root %)) (= :major (:type %))) results)))))

(deftest identify-chord-best-test
  (testing "Prefers root position"
    (let [result (chords/identify-chord-best [:C :E :G])]
      (is (= :C (:root result)))
      (is (= :major (:type result)))
      (is (= 0 (:inversion result))))))

;; =============================================================================
;; Add Extension Tests
;; =============================================================================

(deftest add-extension-test
  (testing "Add 9th"
    (let [chord [:C :E :G :A#]  ; C7
          with-9 (chords/add-extension chord :C :9)]
      (is (= 5 (count with-9)))
      (is (= :D (last with-9)))))

  (testing "Add #11"
    (let [chord [:C :E :G :B]  ; Cmaj7
          with-11 (chords/add-extension chord :C :#11)]
      (is (= 5 (count with-11)))
      (is (= :F# (last with-11))))))

;; =============================================================================
;; Registry Tests
;; =============================================================================

(deftest chord-registry-test
  (testing "List chord types"
    (let [types (set (chords/list-chord-types))]
      (is (contains? types :major))
      (is (contains? types :minor))
      (is (contains? types :maj7))
      (is (contains? types :7))
      (is (contains? types :min7))))

  (testing "Filter by category"
    (let [triads (set (chords/list-chord-types {:category :triads}))]
      (is (contains? triads :major))
      (is (contains? triads :minor))
      (is (contains? triads :diminished))
      (is (not (contains? triads :maj7)))))

  (testing "Register custom chord"
    (chords/register-chord! :test-chord [0 4 7 11 18]
                            {:symbol "test" :name "Test Chord" :category :test})
    (is (= [:C :E :G :B :F#] (chords/build-chord :C :test-chord)))
    (chords/unregister-chord! :test-chord)
    (is (nil? (chords/build-chord :C :test-chord)))))

;; =============================================================================
;; Voice Leading Tests
;; =============================================================================

(deftest voice-leading-distance-test
  (testing "Same chord has zero distance"
    (is (= 0 (chords/voice-leading-distance [:C :E :G] [:C :E :G]))))

  (testing "Close voicings have small distance"
    ;; C major to G major (G B D) - relatively close
    (let [dist (chords/voice-leading-distance [:C :E :G] [:G :B :D])]
      (is (< dist 12)))))  ; Should be reasonable

;; =============================================================================
;; Common Tones Tests
;; =============================================================================

(deftest common-tones-test
  (testing "Common tones between C and Am"
    (let [c-major [:C :E :G]
          a-minor [:A :C :E]
          common (chords/common-tones c-major a-minor)]
      (is (= 2 (count common)))))  ; C and E

  (testing "Common tones between C and F"
    (let [c-major [:C :E :G]
          f-major [:F :A :C]
          common (chords/common-tones c-major f-major)]
      (is (= 1 (count common))))))  ; C
