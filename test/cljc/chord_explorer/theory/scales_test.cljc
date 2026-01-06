(ns chord-explorer.theory.scales-test
  (:require #?(:clj [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer-macros [deftest testing is are]])
            [chord-explorer.theory.scales :as scales]
            [chord-explorer.theory.core :as core]))

;; =============================================================================
;; Scale Generation Tests
;; =============================================================================

(deftest get-scale-test
  (testing "C major scale"
    (is (= [:C :D :E :F :G :A :B]
           (scales/get-scale :C :major))))

  (testing "G major scale"
    (is (= [:G :A :B :C :D :E :F#]
           (scales/get-scale :G :major))))

  (testing "F major scale with flats"
    (is (= [:F :G :A :Bb :C :D :E]
           (scales/get-scale :F :major {:prefer :flats}))))

  (testing "A natural minor scale"
    (is (= [:A :B :C :D :E :F :G]
           (scales/get-scale :A :natural-minor))))

  (testing "C harmonic minor scale"
    (is (= [:C :D :D# :F :G :G# :B]
           (scales/get-scale :C :harmonic-minor))))

  (testing "C melodic minor scale"
    (is (= [:C :D :D# :F :G :A :B]
           (scales/get-scale :C :melodic-minor))))

  (testing "D dorian mode"
    (is (= [:D :E :F :G :A :B :C]
           (scales/get-scale :D :dorian {:prefer :flats})))))

;; =============================================================================
;; Mode Tests
;; =============================================================================

(deftest modes-test
  (testing "All modes of C major have correct notes"
    (is (= [:C :D :E :F :G :A :B] (scales/get-scale :C :ionian)))
    (is (= [:D :E :F :G :A :B :C] (scales/get-scale :D :dorian {:prefer :flats})))
    (is (= [:E :F :G :A :B :C :D] (scales/get-scale :E :phrygian {:prefer :flats})))
    (is (= [:F :G :A :B :C :D :E] (scales/get-scale :F :lydian)))
    (is (= [:G :A :B :C :D :E :F] (scales/get-scale :G :mixolydian {:prefer :flats})))
    (is (= [:A :B :C :D :E :F :G] (scales/get-scale :A :aeolian {:prefer :flats})))
    (is (= [:B :C :D :E :F :G :A] (scales/get-scale :B :locrian {:prefer :flats})))))

;; =============================================================================
;; Pentatonic and Blues Tests
;; =============================================================================

(deftest pentatonic-test
  (testing "C major pentatonic"
    (is (= [:C :D :E :G :A]
           (scales/get-scale :C :major-pentatonic))))

  (testing "A minor pentatonic"
    (is (= [:A :C :D :E :G]
           (scales/get-scale :A :minor-pentatonic {:prefer :flats})))))

(deftest blues-test
  (testing "A blues scale"
    (is (= [:A :C :D :D# :E :G]
           (scales/get-scale :A :blues)))))

;; =============================================================================
;; Note in Scale Tests
;; =============================================================================

(deftest note-in-scale-test
  (testing "Notes in C major"
    (is (scales/note-in-scale? :C :C :major))
    (is (scales/note-in-scale? :E :C :major))
    (is (scales/note-in-scale? :G :C :major))
    (is (not (scales/note-in-scale? :C# :C :major)))
    (is (not (scales/note-in-scale? :Bb :C :major))))

  (testing "Notes in G major"
    (is (scales/note-in-scale? :F# :G :major))
    (is (not (scales/note-in-scale? :F :G :major))))

  (testing "Enharmonic notes"
    (is (scales/note-in-scale? :Db :Db :major))
    (is (scales/note-in-scale? :C# :Db :major)))) ; C# = Db

;; =============================================================================
;; Scale Degree Tests
;; =============================================================================

(deftest scale-degree-test
  (testing "Scale degrees in C major"
    (is (= 1 (scales/scale-degree :C :C :major)))
    (is (= 2 (scales/scale-degree :D :C :major)))
    (is (= 3 (scales/scale-degree :E :C :major)))
    (is (= 4 (scales/scale-degree :F :C :major)))
    (is (= 5 (scales/scale-degree :G :C :major)))
    (is (= 6 (scales/scale-degree :A :C :major)))
    (is (= 7 (scales/scale-degree :B :C :major)))
    (is (nil? (scales/scale-degree :C# :C :major)))))

;; =============================================================================
;; Relative Key Tests
;; =============================================================================

(deftest relative-keys-test
  (testing "Relative minor of major keys"
    (is (core/notes-equal? :A (scales/relative-minor :C)))
    (is (core/notes-equal? :E (scales/relative-minor :G)))
    (is (core/notes-equal? :D (scales/relative-minor :F))))

  (testing "Relative major of minor keys"
    (is (core/notes-equal? :C (scales/relative-major :A)))
    (is (core/notes-equal? :G (scales/relative-major :E)))
    (is (core/notes-equal? :F (scales/relative-major :D)))))

;; =============================================================================
;; Registry Extension Tests
;; =============================================================================

(deftest scale-registry-test
  (testing "List scales includes common types"
    (let [all-scales (set (scales/list-scales))]
      (is (contains? all-scales :major))
      (is (contains? all-scales :natural-minor))
      (is (contains? all-scales :harmonic-minor))
      (is (contains? all-scales :dorian))
      (is (contains? all-scales :mixolydian))))

  (testing "Register custom scale"
    (scales/register-scale! :test-scale [0 2 4 6 8 10] {:name "Test Scale" :category :test})
    (is (= [:C :D :E :F# :G# :A#] (scales/get-scale :C :test-scale)))
    (scales/unregister-scale! :test-scale)
    (is (nil? (scales/get-scale :C :test-scale)))))

;; =============================================================================
;; Scale Category Tests
;; =============================================================================

(deftest scale-categories-test
  (testing "Scales can be filtered by category"
    (let [modes (set (scales/list-scales {:category :modes}))]
      (is (contains? modes :dorian))
      (is (contains? modes :phrygian))
      (is (contains? modes :lydian))
      (is (not (contains? modes :harmonic-minor))))))

;; =============================================================================
;; Common Tones Tests
;; =============================================================================

(deftest common-tones-test
  (testing "Common tones between C major and G major"
    (let [common (scales/common-tones :C :major :G :major)]
      ;; C major: C D E F G A B
      ;; G major: G A B C D E F#
      ;; Common: C D E G A B (6 notes)
      (is (= 6 (count common)))))

  (testing "Common tones between C major and C minor"
    (let [common (scales/common-tones :C :major :C :natural-minor)]
      ;; C major: C D E F G A B
      ;; C minor: C D Eb F G Ab Bb
      ;; Common: C D F G (4 notes)
      (is (= 4 (count common))))))

;; =============================================================================
;; Transpose Scale Tests
;; =============================================================================

(deftest transpose-scale-test
  (testing "Transpose C major up a whole step"
    (let [c-major (scales/get-scale :C :major)
          d-major (scales/transpose-scale c-major 2)]
      (is (= [:D :E :F# :G :A :B :C#] d-major)))))
