(ns chord-explorer.theory.progression-test
  (:require #?(:clj [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer-macros [deftest testing is are]])
            [chord-explorer.theory.progression :as prog]
            [chord-explorer.theory.core :as core]))

;; =============================================================================
;; Progression Creation Tests
;; =============================================================================

(deftest create-progression-test
  (testing "Create empty progression"
    (let [p (prog/create-progression :C :major)]
      (is (= :C (:key p)))
      (is (= :major (:scale-type p)))
      (is (empty? (:chords p)))
      (is (some? (:id p)))))

  (testing "Create progression with chords"
    (let [p (prog/create-progression :C :major
                                     [{:root :C :type :maj7}
                                      {:root :A :type :min7}
                                      {:root :D :type :min7}
                                      {:root :G :type :7}])]
      (is (= 4 (count (:chords p))))
      (is (every? :id (:chords p)))
      (is (every? :notes (:chords p))))))

;; =============================================================================
;; Chord Manipulation Tests
;; =============================================================================

(deftest add-chord-test
  (testing "Add chord to end"
    (let [p (prog/create-progression :C :major)
          p2 (prog/add-chord p {:root :C :type :major})]
      (is (= 1 (count (:chords p2))))
      (is (core/notes-equal? :C (:root (first (:chords p2)))))))

  (testing "Add chord at position"
    (let [p (-> (prog/create-progression :C :major)
                (prog/add-chord {:root :C :type :major})
                (prog/add-chord {:root :G :type :major}))]
      (let [p2 (prog/add-chord p {:root :F :type :major} 1)]
        (is (= 3 (count (:chords p2))))
        (is (core/notes-equal? :F (:root (nth (:chords p2) 1))))))))

(deftest remove-chord-test
  (testing "Remove chord by index"
    (let [p (-> (prog/create-progression :C :major)
                (prog/add-chord {:root :C :type :major})
                (prog/add-chord {:root :F :type :major})
                (prog/add-chord {:root :G :type :major}))]
      (let [p2 (prog/remove-chord p 1)]
        (is (= 2 (count (:chords p2))))
        (is (core/notes-equal? :C (:root (first (:chords p2)))))
        (is (core/notes-equal? :G (:root (second (:chords p2)))))))))

(deftest move-chord-test
  (testing "Move chord from position to position"
    (let [p (-> (prog/create-progression :C :major)
                (prog/add-chord {:root :C :type :major})
                (prog/add-chord {:root :F :type :major})
                (prog/add-chord {:root :G :type :major}))]
      (let [p2 (prog/move-chord p 2 0)]  ; Move G to beginning
        (is (core/notes-equal? :G (:root (first (:chords p2)))))
        (is (core/notes-equal? :C (:root (second (:chords p2)))))
        (is (core/notes-equal? :F (:root (nth (:chords p2) 2))))))))

;; =============================================================================
;; Transposition Tests
;; =============================================================================

(deftest transpose-progression-test
  (testing "Transpose progression up a whole step"
    (let [p (-> (prog/create-progression :C :major)
                (prog/add-chord {:root :C :type :maj7})
                (prog/add-chord {:root :D :type :min7})
                (prog/add-chord {:root :G :type :7}))]
      (let [p2 (prog/transpose-progression p 2)]
        (is (core/notes-equal? :D (:key p2)))
        (is (core/notes-equal? :D (:root (first (:chords p2)))))
        (is (core/notes-equal? :E (:root (second (:chords p2)))))
        (is (core/notes-equal? :A (:root (nth (:chords p2) 2)))))))

  (testing "Change key convenience function"
    (let [p (prog/create-progression :C :major
                                     [{:root :C :type :major}
                                      {:root :G :type :major}])]
      (let [p2 (prog/change-key p :G)]
        (is (core/notes-equal? :G (:key p2)))
        (is (core/notes-equal? :G (:root (first (:chords p2)))))
        (is (core/notes-equal? :D (:root (second (:chords p2)))))))))

;; =============================================================================
;; Chord Modification Tests
;; =============================================================================

(deftest modify-chord-type-test
  (testing "Change chord type"
    (let [p (prog/create-progression :C :major
                                     [{:root :C :type :major}])]
      (let [p2 (prog/modify-chord-type p 0 :maj7)]
        (is (= :maj7 (:type (first (:chords p2)))))
        (is (= 4 (count (:notes (first (:chords p2))))))))))

(deftest set-chord-duration-test
  (testing "Set chord duration"
    (let [p (prog/create-progression :C :major
                                     [{:root :C :type :major}])]
      (let [p2 (prog/set-chord-duration p 0 8)]
        (is (= 8 (:duration (first (:chords p2)))))))))

;; =============================================================================
;; Template Tests
;; =============================================================================

(deftest create-from-template-test
  (testing "Create I-IV-V-I progression"
    (let [p (prog/create-from-template :C :major :I-IV-V-I)]
      (is (= 4 (count (:chords p))))
      (is (core/notes-equal? :C (:root (nth (:chords p) 0))))
      (is (core/notes-equal? :F (:root (nth (:chords p) 1))))
      (is (core/notes-equal? :G (:root (nth (:chords p) 2))))
      (is (core/notes-equal? :C (:root (nth (:chords p) 3))))))

  (testing "Create ii-V-I progression with sevenths"
    (let [p (prog/create-from-template :C :major :ii-V-I {:seventh? true})]
      (is (= 3 (count (:chords p))))
      (is (= :min7 (:type (nth (:chords p) 0))))  ; ii7
      (is (= :7 (:type (nth (:chords p) 1))))     ; V7
      (is (= :maj7 (:type (nth (:chords p) 2))))))) ; Imaj7

;; =============================================================================
;; Analysis Tests
;; =============================================================================

(deftest analyze-progression-test
  (testing "Analyze adds analysis to each chord"
    (let [p (prog/create-progression :C :major
                                     [{:root :D :type :min7}
                                      {:root :G :type :7}
                                      {:root :C :type :maj7}])
          analyzed (prog/analyze-progression p)]
      (is (every? :analysis (:chords analyzed)))
      (is (= 2 (:degree (:analysis (nth (:chords analyzed) 0)))))
      (is (= 5 (:degree (:analysis (nth (:chords analyzed) 1)))))
      (is (= 1 (:degree (:analysis (nth (:chords analyzed) 2))))))))

(deftest get-numeral-sequence-test
  (testing "Get Roman numeral sequence"
    (let [p (prog/create-progression :C :major
                                     [{:root :D :type :minor}
                                      {:root :G :type :major}
                                      {:root :C :type :major}])
          numerals (prog/get-numeral-sequence p)]
      (is (= 3 (count numerals)))
      (is (= "ii" (nth numerals 0)))
      (is (= "V" (nth numerals 1)))
      (is (= "I" (nth numerals 2))))))

;; =============================================================================
;; Serialization Tests
;; =============================================================================

(deftest serialization-test
  (testing "Round-trip serialization"
    (let [p (prog/create-progression :C :major
                                     [{:root :C :type :maj7}
                                      {:root :A :type :min7}])
          serialized (prog/progression->map p)
          restored (prog/map->progression serialized)]
      (is (= :C (:key restored)))
      (is (= :major (:scale-type restored)))
      (is (= :C (:root (first (:chords restored)))))
      (is (= :maj7 (:type (first (:chords restored))))))))
