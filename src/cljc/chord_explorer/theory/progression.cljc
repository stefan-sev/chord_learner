(ns chord-explorer.theory.progression
  "Chord progression operations and manipulation."
  (:require [chord-explorer.theory.core :as core]
            [chord-explorer.theory.chords :as chords]
            [chord-explorer.theory.harmony :as harmony]
            [chord-explorer.theory.analysis :as analysis]))

;; =============================================================================
;; Progression Data Structure
;; =============================================================================

(defn create-chord-entry
  "Create a chord entry for a progression.
   Options:
   - :duration - beat duration (default 4)
   - :voicing - specific voicing to use"
  ([root chord-type]
   (create-chord-entry root chord-type {}))
  ([root chord-type {:keys [duration voicing] :or {duration 4}}]
   {:id (str (random-uuid))
    :root root
    :type chord-type
    :duration duration
    :voicing voicing
    :notes (chords/build-chord root chord-type)}))

(defn create-progression
  "Create a new chord progression.
   Arguments:
   - key-root: the key of the progression
   - scale-type: the scale type
   - chords: vector of chord entries or {:root :type} maps"
  ([key-root scale-type]
   (create-progression key-root scale-type []))
  ([key-root scale-type chords]
   {:id (str (random-uuid))
    :name "Untitled Progression"
    :key key-root
    :scale-type scale-type
    :chords (mapv (fn [c]
                    (if (:id c)
                      c
                      (create-chord-entry (:root c) (:type c) c)))
                  chords)
    :created-at #?(:clj (java.util.Date.)
                   :cljs (js/Date.))
    :updated-at #?(:clj (java.util.Date.)
                   :cljs (js/Date.))}))

;; =============================================================================
;; Progression Manipulation
;; =============================================================================

(defn add-chord
  "Add a chord to the progression at a position.
   If position is nil, adds to the end."
  ([progression chord]
   (add-chord progression chord nil))
  ([progression chord position]
   (let [entry (if (:id chord)
                 chord
                 (create-chord-entry (:root chord) (:type chord) chord))
         chords (:chords progression)
         new-chords (if (nil? position)
                      (conj chords entry)
                      (vec (concat (take position chords)
                                   [entry]
                                   (drop position chords))))]
     (assoc progression
            :chords new-chords
            :updated-at #?(:clj (java.util.Date.)
                           :cljs (js/Date.))))))

(defn remove-chord
  "Remove a chord from the progression by index."
  [progression index]
  (let [chords (:chords progression)]
    (when (and (>= index 0) (< index (count chords)))
      (assoc progression
             :chords (vec (concat (take index chords)
                                  (drop (inc index) chords)))
             :updated-at #?(:clj (java.util.Date.)
                            :cljs (js/Date.))))))

(defn remove-chord-by-id
  "Remove a chord from the progression by its id."
  [progression chord-id]
  (assoc progression
         :chords (vec (remove #(= (:id %) chord-id) (:chords progression)))
         :updated-at #?(:clj (java.util.Date.)
                        :cljs (js/Date.))))

(defn replace-chord
  "Replace a chord at a specific index."
  [progression index new-chord]
  (let [entry (if (:id new-chord)
                new-chord
                (create-chord-entry (:root new-chord) (:type new-chord) new-chord))]
    (assoc-in progression [:chords index] entry)))

(defn move-chord
  "Move a chord from one position to another."
  [progression from-index to-index]
  (let [chords (:chords progression)
        chord (nth chords from-index)
        without (vec (concat (take from-index chords)
                             (drop (inc from-index) chords)))
        new-chords (vec (concat (take to-index without)
                                [chord]
                                (drop to-index without)))]
    (assoc progression
           :chords new-chords
           :updated-at #?(:clj (java.util.Date.)
                          :cljs (js/Date.)))))

(defn duplicate-chord
  "Duplicate a chord at the given index."
  [progression index]
  (let [chord (get-in progression [:chords index])
        new-chord (assoc chord :id (str (random-uuid)))]
    (add-chord progression new-chord (inc index))))

(defn clear-progression
  "Remove all chords from the progression."
  [progression]
  (assoc progression
         :chords []
         :updated-at #?(:clj (java.util.Date.)
                        :cljs (js/Date.))))

;; =============================================================================
;; Transposition
;; =============================================================================

(defn transpose-chord
  "Transpose a single chord entry by semitones."
  [chord-entry semitones]
  (let [new-root (core/transpose (:root chord-entry) semitones)]
    (assoc chord-entry
           :root new-root
           :notes (chords/build-chord new-root (:type chord-entry)))))

(defn transpose-progression
  "Transpose the entire progression by semitones."
  [progression semitones]
  (let [new-key (core/transpose (:key progression) semitones)
        new-chords (mapv #(transpose-chord % semitones) (:chords progression))]
    (assoc progression
           :key new-key
           :chords new-chords
           :updated-at #?(:clj (java.util.Date.)
                          :cljs (js/Date.)))))

(defn change-key
  "Change the key of the progression (transposes all chords).
   This is a convenience wrapper around transpose-progression."
  [progression new-key]
  (let [current-key (:key progression)
        semitones (core/interval-between current-key new-key)]
    (transpose-progression progression semitones)))

;; =============================================================================
;; Chord Modifications
;; =============================================================================

(defn modify-chord-type
  "Change the type of a chord at a specific index."
  [progression index new-type]
  (let [chord (get-in progression [:chords index])
        new-chord (assoc chord
                         :type new-type
                         :notes (chords/build-chord (:root chord) new-type))]
    (assoc-in progression [:chords index] new-chord)))

(defn add-extension-to-chord
  "Add an extension to a chord at a specific index."
  [progression index extension]
  (let [chord (get-in progression [:chords index])
        new-notes (chords/add-extension (:notes chord) (:root chord) extension)]
    (assoc-in progression [:chords index :notes] new-notes)))

(defn set-chord-duration
  "Set the duration of a chord at a specific index."
  [progression index duration]
  (assoc-in progression [:chords index :duration] duration))

(defn set-chord-voicing
  "Set the voicing for a chord at a specific index."
  [progression index voicing]
  (assoc-in progression [:chords index :voicing] voicing))

;; =============================================================================
;; Analysis
;; =============================================================================

(defn analyze-progression
  "Add analysis to each chord in the progression."
  [progression]
  (let [key-root (:key progression)
        scale-type (:scale-type progression)]
    (update progression :chords
            (fn [chords]
              (mapv (fn [chord]
                      (assoc chord :analysis
                             (analysis/analyze-chord
                              (:root chord)
                              (:type chord)
                              key-root
                              scale-type)))
                    chords)))))

(defn get-numeral-sequence
  "Get the Roman numeral sequence for a progression."
  [progression]
  (let [analyzed (analyze-progression progression)]
    (mapv #(analysis/get-analysis-label (:analysis %))
          (:chords analyzed))))

(defn find-patterns
  "Find common harmonic patterns in the progression."
  [progression]
  (let [analyzed (analyze-progression progression)
        chords (:chords analyzed)
        patterns []]
    ;; Find ii-V-I patterns
    (reduce
     (fn [acc idx]
       (if (< idx (- (count chords) 2))
         (let [c1 (nth chords idx)
               c2 (nth chords (+ idx 1))
               c3 (nth chords (+ idx 2))
               d1 (:degree (:analysis c1))
               d2 (:degree (:analysis c2))
               d3 (:degree (:analysis c3))]
           (cond-> acc
             (and (= d1 2) (= d2 5) (= d3 1))
             (conj {:pattern :ii-V-I :start-index idx})

             (and (= d1 4) (= d2 5) (= d3 1))
             (conj {:pattern :IV-V-I :start-index idx})))
         acc))
     patterns
     (range (count chords)))))

;; =============================================================================
;; Common Progressions
;; =============================================================================

(def common-progression-templates
  "Templates for common chord progressions."
  {:I-IV-V-I    [1 4 5 1]
   :I-V-vi-IV   [1 5 6 4]
   :ii-V-I      [2 5 1]
   :I-vi-IV-V   [1 6 4 5]
   :I-IV-vi-V   [1 4 6 5]
   :vi-IV-I-V   [6 4 1 5]
   :I-V-vi-iii-IV-I-IV-V [1 5 6 3 4 1 4 5]
   :i-iv-VII-III [1 4 7 3]  ; Minor key
   :i-VI-III-VII [1 6 3 7]  ; Minor key (Andalusian)
   :I-II-IV-I   [1 2 4 1]   ; Blues-influenced
   })

(defn create-from-template
  "Create a progression from a template.
   Options:
   - :seventh? - use seventh chords"
  ([key-root scale-type template-key]
   (create-from-template key-root scale-type template-key {}))
  ([key-root scale-type template-key {:keys [seventh?] :or {seventh? false}}]
   (let [degrees (get common-progression-templates template-key)
         chords (mapv (fn [degree]
                        (let [d (harmony/diatonic-chord key-root scale-type degree
                                                        {:seventh? seventh?})]
                          {:root (:root d)
                           :type (:type d)}))
                      degrees)]
     (create-progression key-root scale-type chords))))

;; =============================================================================
;; Progression Suggestions
;; =============================================================================

(defn suggest-next-chords
  "Suggest possible next chords for a progression."
  [progression]
  (if (empty? (:chords progression))
    ;; Start with tonic
    [(harmony/diatonic-chord (:key progression) (:scale-type progression) 1)]
    ;; Suggest based on last chord
    (let [last-chord (last (:chords progression))]
      (analysis/suggest-next-chord
       {:root (:root last-chord) :type (:type last-chord)}
       (:key progression)
       (:scale-type progression)))))

(defn suggest-completion
  "Suggest a completion for an incomplete progression (end with a cadence)."
  [progression]
  (let [key-root (:key progression)
        scale-type (:scale-type progression)
        last-chord (last (:chords progression))
        last-analysis (when last-chord
                        (analysis/analyze-chord
                         (:root last-chord)
                         (:type last-chord)
                         key-root scale-type))]
    (cond
      ;; Empty - suggest ii-V-I
      (empty? (:chords progression))
      [(harmony/diatonic-chord key-root scale-type 2)
       (harmony/diatonic-chord key-root scale-type 5)
       (harmony/diatonic-chord key-root scale-type 1)]

      ;; On dominant - suggest tonic
      (= (:function last-analysis) :dominant)
      [(harmony/diatonic-chord key-root scale-type 1)]

      ;; On subdominant - suggest V-I
      (= (:function last-analysis) :subdominant)
      [(harmony/diatonic-chord key-root scale-type 5)
       (harmony/diatonic-chord key-root scale-type 1)]

      ;; Otherwise - suggest IV-V-I
      :else
      [(harmony/diatonic-chord key-root scale-type 4)
       (harmony/diatonic-chord key-root scale-type 5)
       (harmony/diatonic-chord key-root scale-type 1)])))

;; =============================================================================
;; Serialization
;; =============================================================================

(defn progression->map
  "Convert a progression to a plain map for serialization."
  [progression]
  (-> progression
      (update :chords (fn [chords]
                        (mapv (fn [c]
                                (-> c
                                    (update :root name)
                                    (update :type name)
                                    (update :notes #(mapv name %))))
                              chords)))
      (update :key name)
      (update :scale-type name)))

(defn map->progression
  "Convert a plain map back to a progression."
  [m]
  (-> m
      (update :chords (fn [chords]
                        (mapv (fn [c]
                                (-> c
                                    (update :root keyword)
                                    (update :type keyword)
                                    (update :notes #(mapv keyword %))))
                              chords)))
      (update :key keyword)
      (update :scale-type keyword)))
