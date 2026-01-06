(ns chord-explorer.theory.analysis-test
  (:require #?(:clj [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer-macros [deftest testing is are]])
            [chord-explorer.theory.analysis :as analysis]
            [chord-explorer.theory.core :as core]))

;; =============================================================================
;; Secondary Dominant Tests
;; =============================================================================

(deftest secondary-dominant-test
  (testing "V/ii in C major is A7"
    (let [sec-dom (analysis/secondary-dominant :C :major 2)]
      (is (core/notes-equal? :A (:root sec-dom)))
      (is (= :7 (:type sec-dom)))
      (is (= "V/ii" (get-in sec-dom [:analysis :notation])))))

  (testing "V/V in C major is D7"
    (let [sec-dom (analysis/secondary-dominant :C :major 5)]
      (is (core/notes-equal? :D (:root sec-dom)))
      (is (= :7 (:type sec-dom)))
      (is (= "V/v" (get-in sec-dom [:analysis :notation])))))

  (testing "V/vi in C major is E7"
    (let [sec-dom (analysis/secondary-dominant :C :major 6)]
      (is (core/notes-equal? :E (:root sec-dom)))
      (is (= :7 (:type sec-dom))))))

(deftest all-secondary-dominants-test
  (testing "C major has 5 secondary dominants"
    (let [sec-doms (analysis/all-secondary-dominants :C :major)]
      (is (= 5 (count sec-doms)))
      ;; V/ii = A7, V/iii = B7, V/IV = C7, V/V = D7, V/vi = E7
      (is (some #(core/notes-equal? :A (:root %)) sec-doms))
      (is (some #(core/notes-equal? :B (:root %)) sec-doms))
      (is (some #(core/notes-equal? :C (:root %)) sec-doms))
      (is (some #(core/notes-equal? :D (:root %)) sec-doms))
      (is (some #(core/notes-equal? :E (:root %)) sec-doms)))))

(deftest identify-secondary-dominant-test
  (testing "A7 is V/ii in C major"
    (let [analysis (analysis/identify-secondary-dominant :A :7 :C :major)]
      (is (some? analysis))
      (is (= :secondary-dominant (:type analysis)))
      (is (= 2 (:target-degree analysis)))))

  (testing "D7 is V/V in C major"
    (let [analysis (analysis/identify-secondary-dominant :D :7 :C :major)]
      (is (= :secondary-dominant (:type analysis)))
      (is (= 5 (:target-degree analysis)))))

  (testing "C major triad is not a secondary dominant"
    (is (nil? (analysis/identify-secondary-dominant :C :major :C :major)))))

;; =============================================================================
;; Modal Interchange Tests
;; =============================================================================

(deftest borrowed-chord-test
  (testing "bVII in C major is Bb"
    (let [borrowed (analysis/borrowed-chord :C :bVII)]
      (is (core/notes-equal? :A# (:root borrowed)))  ; Bb = A#
      (is (= :major (:type borrowed)))))

  (testing "iv in C major is Fm"
    (let [borrowed (analysis/borrowed-chord :C :iv)]
      (is (core/notes-equal? :F (:root borrowed)))
      (is (= :minor (:type borrowed)))))

  (testing "bVI in C major is Ab"
    (let [borrowed (analysis/borrowed-chord :C :bVI)]
      (is (core/notes-equal? :G# (:root borrowed)))  ; Ab = G#
      (is (= :major (:type borrowed))))))

(deftest all-borrowed-chords-test
  (testing "C major has common borrowed chords"
    (let [borrowed (analysis/all-borrowed-chords :C)]
      (is (>= (count borrowed) 4)))))

;; =============================================================================
;; Tritone Substitution Tests
;; =============================================================================

(deftest tritone-substitute-test
  (testing "Tritone sub of G7 is Db7"
    (let [sub (analysis/tritone-substitute :G)]
      (is (core/notes-equal? :C# (:root sub)))  ; Db = C#
      (is (= :7 (:type sub)))))

  (testing "Tritone sub of C7 is Gb7"
    (let [sub (analysis/tritone-substitute :C)]
      (is (core/notes-equal? :F# (:root sub)))  ; Gb = F#
      (is (= :7 (:type sub))))))

(deftest identify-tritone-sub-test
  (testing "Db7 is tritone sub of V in C major"
    (let [analysis (analysis/identify-tritone-sub :Db :7 :C :major)]
      (is (some? analysis))
      (is (= :tritone-substitution (:type analysis)))
      (is (= :V (:replaces analysis))))))

;; =============================================================================
;; Comprehensive Analysis Tests
;; =============================================================================

(deftest analyze-chord-test
  (testing "Diatonic chord analysis"
    (let [analysis (analysis/analyze-chord :C :major :C :major)]
      (is (:diatonic? analysis))
      (is (= 1 (:degree analysis)))
      (is (= :tonic (:function analysis)))))

  (testing "Secondary dominant analysis"
    (let [analysis (analysis/analyze-chord :D :7 :C :major)]
      (is (not (:diatonic? analysis)))
      (is (some #(= :secondary-dominant (:type %)) (:analyses analysis)))))

  (testing "Non-diatonic, non-functional chord"
    (let [analysis (analysis/analyze-chord :Ab :major :C :major)]
      (is (not (:diatonic? analysis))))))

;; =============================================================================
;; Progression Analysis Tests
;; =============================================================================

(deftest analyze-progression-test
  (testing "Analyze ii-V-I progression"
    (let [chords [{:root :D :type :min7}
                  {:root :G :type :7}
                  {:root :C :type :maj7}]
          analysis (analysis/analyze-progression chords :C :major)]
      (is (= 3 (count analysis)))
      ;; ii
      (is (= 2 (:degree (nth analysis 0))))
      (is (:diatonic? (nth analysis 0)))
      ;; V
      (is (= 5 (:degree (nth analysis 1))))
      (is (:diatonic? (nth analysis 1)))
      ;; I
      (is (= 1 (:degree (nth analysis 2))))
      (is (:diatonic? (nth analysis 2))))))

;; =============================================================================
;; Next Chord Suggestion Tests
;; =============================================================================

(deftest suggest-next-chord-test
  (testing "Dominant suggests tonic"
    (let [suggestions (analysis/suggest-next-chord
                       {:root :G :type :7}
                       :C :major)]
      ;; Should suggest I (C) first
      (is (some #(and (core/notes-equal? :C (:root %))
                      (= :tonic (:function %)))
                suggestions))))

  (testing "Subdominant suggests dominant"
    (let [suggestions (analysis/suggest-next-chord
                       {:root :F :type :major}
                       :C :major)]
      ;; Should include V (G)
      (is (some #(core/notes-equal? :G (:root %)) suggestions)))))
