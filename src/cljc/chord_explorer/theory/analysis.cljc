(ns chord-explorer.theory.analysis
  "Advanced harmonic analysis: secondary dominants, modal interchange, and more."
  (:require [chord-explorer.theory.core :as core]
            [chord-explorer.theory.scales :as scales]
            [chord-explorer.theory.chords :as chords]
            [chord-explorer.theory.harmony :as harmony]))

;; =============================================================================
;; Secondary Dominants
;; =============================================================================

(def secondary-dominant-targets
  "Valid targets for secondary dominants in major keys.
   We can have V/ii, V/iii, V/IV, V/V, V/vi (not V/vii because it's diminished)."
  #{2 3 4 5 6})

(defn secondary-dominant
  "Calculate the secondary dominant (V/x) for a target scale degree.
   Returns the dominant chord that resolves to the target."
  ([key-root target-degree]
   (secondary-dominant key-root :major target-degree))
  ([key-root scale-type target-degree]
   (let [scale-notes (scales/get-scale key-root scale-type)
         target-root (nth scale-notes (dec target-degree))
         ;; The V of the target is a P5 above (7 semitones)
         dominant-root (core/transpose target-root 7)]
     {:root dominant-root
      :type :7
      :notes (chords/build-chord dominant-root :7)
      :analysis {:type :secondary-dominant
                 :notation (str "V/" (get-in harmony/roman-numerals [target-degree :minor]))
                 :target-degree target-degree
                 :target-root target-root
                 :resolves-to {:root target-root
                               :type (harmony/diatonic-chord-type target-degree scale-type)}}})))

(defn all-secondary-dominants
  "Get all secondary dominants for a key."
  ([key-root]
   (all-secondary-dominants key-root :major))
  ([key-root scale-type]
   (mapv #(secondary-dominant key-root scale-type %)
         secondary-dominant-targets)))

(defn identify-secondary-dominant
  "Check if a chord is a secondary dominant in a key.
   Returns the analysis if it is, nil otherwise."
  [chord-root chord-type key-root scale-type]
  (when (= chord-type :7)
    (let [scale-notes (scales/get-scale key-root scale-type)]
      (some (fn [target-degree]
              (let [target-root (nth scale-notes (dec target-degree))
                    expected-v (core/transpose target-root 7)]
                (when (core/notes-equal? chord-root expected-v)
                  {:type :secondary-dominant
                   :notation (str "V/" (get-in harmony/roman-numerals [target-degree :minor]))
                   :target-degree target-degree
                   :target-root target-root})))
            secondary-dominant-targets))))

;; =============================================================================
;; Secondary Leading Tone Chords
;; =============================================================================

(defn secondary-leading-tone
  "Calculate the secondary leading tone chord (viio/x) for a target degree.
   Returns the diminished chord that resolves to the target."
  ([key-root target-degree]
   (secondary-leading-tone key-root :major target-degree))
  ([key-root scale-type target-degree]
   (let [scale-notes (scales/get-scale key-root scale-type)
         target-root (nth scale-notes (dec target-degree))
         ;; The vii of the target is a half step below (11 semitones up)
         leading-root (core/transpose target-root 11)]
     {:root leading-root
      :type :dim7
      :notes (chords/build-chord leading-root :dim7)
      :analysis {:type :secondary-leading-tone
                 :notation (str "vii\u00B0/" (get-in harmony/roman-numerals [target-degree :minor]))
                 :target-degree target-degree
                 :target-root target-root}})))

(defn identify-secondary-leading-tone
  "Check if a chord is a secondary leading tone in a key."
  [chord-root chord-type key-root scale-type]
  (when (#{:dim7 :diminished :half-dim7} chord-type)
    (let [scale-notes (scales/get-scale key-root scale-type)]
      (some (fn [target-degree]
              (let [target-root (nth scale-notes (dec target-degree))
                    expected-lt (core/transpose target-root 11)]
                (when (core/notes-equal? chord-root expected-lt)
                  {:type :secondary-leading-tone
                   :notation (str "vii\u00B0/" (get-in harmony/roman-numerals [target-degree :minor]))
                   :target-degree target-degree
                   :target-root target-root})))
            secondary-dominant-targets))))

;; =============================================================================
;; Modal Interchange / Borrowed Chords
;; =============================================================================

(def parallel-modes-for-borrowing
  "Common modes to borrow from when in major."
  [:natural-minor :dorian :phrygian :lydian :mixolydian])

(def common-borrowed-chords
  "Common borrowed chords in major keys (from parallel minor).
   Maps degree alterations to the borrowed chord."
  {;; From parallel minor
   :bIII {:from :natural-minor :degree 3 :quality :major}
   :iv   {:from :natural-minor :degree 4 :quality :minor}
   :bVI  {:from :natural-minor :degree 6 :quality :major}
   :bVII {:from :natural-minor :degree 7 :quality :major}
   ;; Minor tonic
   :i    {:from :natural-minor :degree 1 :quality :minor}})

(defn borrowed-chord
  "Get a borrowed chord for a major key.
   numeral: :bIII, :iv, :bVI, :bVII, etc."
  [key-root borrowed-numeral]
  (when-let [info (get common-borrowed-chords borrowed-numeral)]
    (let [{:keys [from degree quality]} info
          parallel-scale (scales/get-scale key-root from)
          borrowed-root (nth parallel-scale (dec degree))
          chord-type (case quality
                       :major :major
                       :minor :minor
                       :diminished :diminished
                       :major)]
      {:root borrowed-root
       :type chord-type
       :notes (chords/build-chord borrowed-root chord-type)
       :analysis {:type :modal-interchange
                  :notation (name borrowed-numeral)
                  :source-mode from
                  :original-key key-root}})))

(defn all-borrowed-chords
  "Get all common borrowed chords for a major key."
  [key-root]
  (keep (fn [[numeral _]]
          (borrowed-chord key-root numeral))
        common-borrowed-chords))

(defn get-borrowed-numeral
  "Determine the numeral notation for a borrowed chord."
  [degree key-root chord-root scale-type]
  (let [major-scale (scales/get-scale key-root :major)
        diatonic-root (nth major-scale (dec degree))
        interval (core/interval-between diatonic-root chord-root)]
    (str (cond
           (= interval 11) "b"  ; lowered by half step
           (= interval 1) "#"   ; raised by half step
           :else "")
         (get-in harmony/roman-numerals [degree :minor]))))

(defn identify-modal-interchange
  "Check if a chord is borrowed from a parallel mode.
   Returns analysis if it is, nil otherwise."
  [chord-root chord-type key-root scale-type]
  (when (= scale-type :major)
    ;; Check if chord exists in parallel minor but not major
    (let [major-diatonic (set (map (fn [c] [(core/normalize-note (:root c)) (:type c)])
                                   (harmony/diatonic-chords key-root :major)))
          chord-sig [(core/normalize-note chord-root) chord-type]]
      (when-not (contains? major-diatonic chord-sig)
        ;; Check parallel modes
        (some (fn [mode]
                (let [mode-diatonic (harmony/diatonic-chords key-root mode)
                      matching (first (filter #(and (core/notes-equal? chord-root (:root %))
                                                    (= chord-type (:type %)))
                                              mode-diatonic))]
                  (when matching
                    {:type :modal-interchange
                     :source-mode mode
                     :degree (:degree matching)
                     :notation (str (get-borrowed-numeral (:degree matching) key-root chord-root scale-type))})))
              parallel-modes-for-borrowing)))))

;; =============================================================================
;; Tritone Substitution
;; =============================================================================

(defn tritone-substitute
  "Get the tritone substitute for a dominant chord.
   The tritone sub is a dominant chord a tritone (6 semitones) away."
  [chord-root]
  (let [sub-root (core/transpose chord-root 6)]
    {:root sub-root
     :type :7
     :notes (chords/build-chord sub-root :7)
     :analysis {:type :tritone-substitution
                :original-root chord-root
                :notation (str "Tr/" (name chord-root))}}))

(defn identify-tritone-sub
  "Check if a chord might be a tritone substitution.
   Returns the possible original dominant it could replace."
  [chord-root chord-type key-root scale-type]
  (when (= chord-type :7)
    ;; A tritone sub would replace V (degree 5)
    (let [scale-notes (scales/get-scale key-root scale-type)
          v-root (nth scale-notes 4)  ; 5th degree, 0-indexed
          tritone-of-v (core/transpose v-root 6)]
      (when (core/notes-equal? chord-root tritone-of-v)
        {:type :tritone-substitution
         :replaces :V
         :original-root v-root
         :notation "bII7"}))))

;; =============================================================================
;; Comprehensive Chord Analysis
;; =============================================================================

(defn analyze-chord
  "Perform comprehensive analysis of a chord in a key context.
   Returns a map with all applicable analyses."
  [chord-root chord-type key-root scale-type]
  (let [basic (harmony/analyze-chord-in-key chord-root chord-type key-root scale-type)
        secondary-dom (identify-secondary-dominant chord-root chord-type key-root scale-type)
        secondary-lt (identify-secondary-leading-tone chord-root chord-type key-root scale-type)
        modal-int (identify-modal-interchange chord-root chord-type key-root scale-type)
        tritone (identify-tritone-sub chord-root chord-type key-root scale-type)]
    (merge basic
           {:analyses (filterv some?
                               [(when (:diatonic? basic) {:type :diatonic})
                                secondary-dom
                                secondary-lt
                                modal-int
                                tritone])})))

(defn get-analysis-label
  "Get a display label for the primary analysis of a chord."
  [analysis]
  (let [analyses (:analyses analysis)
        chord-root (:root analysis)
        chord-type (:type analysis)]
    (cond
      (:diatonic? analysis)
      (:numeral analysis)

      (first (filter #(= (:type %) :secondary-dominant) analyses))
      (:notation (first (filter #(= (:type %) :secondary-dominant) analyses)))

      (first (filter #(= (:type %) :modal-interchange) analyses))
      (:notation (first (filter #(= (:type %) :modal-interchange) analyses)))

      (first (filter #(= (:type %) :tritone-substitution) analyses))
      (:notation (first (filter #(= (:type %) :tritone-substitution) analyses)))

      :else
      (str "(" (name chord-root) (get-in (chords/get-chord-def chord-type) [:symbol]) ")"))))

;; =============================================================================
;; Progression Analysis
;; =============================================================================

(defn analyze-progression
  "Analyze a chord progression in a key context.
   Takes a vector of {:root :type} maps."
  [chords key-root scale-type]
  (mapv (fn [chord]
          (analyze-chord (:root chord) (:type chord) key-root scale-type))
        chords))

(defn find-cadences
  "Identify cadence patterns in a progression."
  [progression key-root scale-type]
  (let [analyzed (analyze-progression progression key-root scale-type)]
    (keep-indexed
     (fn [idx _]
       (when (< idx (dec (count analyzed)))
         (let [c1 (nth analyzed idx)
               c2 (nth analyzed (inc idx))
               cadence (harmony/suggests-cadence?
                        {:root (:root c1)}
                        {:root (:root c2)}
                        key-root scale-type)]
           (when cadence
             {:index idx
              :type cadence
              :chords [c1 c2]}))))
     analyzed)))

(defn suggest-next-chord
  "Suggest likely next chords based on harmonic tendencies."
  [current-chord key-root scale-type]
  (let [analysis (analyze-chord (:root current-chord) (:type current-chord) key-root scale-type)
        function (:function analysis)
        degree (:degree analysis)]
    (cond
      ;; Dominant wants to resolve to tonic
      (= function :dominant)
      [(harmony/diatonic-chord key-root scale-type 1)
       (harmony/diatonic-chord key-root scale-type 6)]  ; Deceptive

      ;; Subdominant wants to go to dominant or tonic
      (= function :subdominant)
      [(harmony/diatonic-chord key-root scale-type 5)
       (harmony/diatonic-chord key-root scale-type 1)]

      ;; Tonic can go anywhere
      (= function :tonic)
      (harmony/diatonic-chords key-root scale-type)

      ;; Secondary dominant resolves to target
      (some #(= (:type %) :secondary-dominant) (:analyses analysis))
      (let [sec-dom (first (filter #(= (:type %) :secondary-dominant) (:analyses analysis)))]
        [(harmony/diatonic-chord key-root scale-type (:target-degree sec-dom))])

      :else
      (harmony/diatonic-chords key-root scale-type))))

;; =============================================================================
;; Chord Substitution Suggestions
;; =============================================================================

(defn suggest-substitutions
  "Suggest possible chord substitutions."
  [chord-root chord-type key-root scale-type]
  (let [analysis (analyze-chord chord-root chord-type key-root scale-type)
        suggestions []
        ;; Tritone sub for dominants
        suggestions (if (= chord-type :7)
                      (conj suggestions (tritone-substitute chord-root))
                      suggestions)
        ;; Related ii-V for dominant
        suggestions (if (= chord-type :7)
                      (let [ii-root (core/transpose chord-root 5)]
                        (conj suggestions
                              {:type :related-ii
                               :chord {:root ii-root :type :min7}
                               :analysis {:type :related-ii-v
                                          :notation (str "ii-V of " (name chord-root))}}))
                      suggestions)]
    suggestions))
