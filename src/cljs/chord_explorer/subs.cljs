(ns chord-explorer.subs
  "Re-frame subscriptions."
  (:require [re-frame.core :as rf]
            [chord-explorer.theory.core :as core]
            [chord-explorer.theory.scales :as scales]
            [chord-explorer.theory.chords :as chords]
            [chord-explorer.theory.voicings :as voicings]
            [chord-explorer.theory.guitar :as guitar]
            [chord-explorer.theory.analysis :as analysis]))

;; =============================================================================
;; Basic State Subscriptions
;; =============================================================================

(rf/reg-sub
 :current-key
 (fn [db _]
   (:current-key db)))

(rf/reg-sub
 :scale-type
 (fn [db _]
   (:scale-type db)))

(rf/reg-sub
 :show-seventh?
 (fn [db _]
   (:show-seventh? db)))

(rf/reg-sub
 :voicing-mode
 (fn [db _]
   (:voicing-mode db)))

(rf/reg-sub
 :show-analysis?
 (fn [db _]
   (:show-analysis? db)))

(rf/reg-sub
 :audio-enabled?
 (fn [db _]
   (:audio-enabled? db)))

(rf/reg-sub
 :playback-tempo
 (fn [db _]
   (:playback-tempo db)))

(rf/reg-sub
 :error
 (fn [db _]
   (:error db)))

(rf/reg-sub
 :loading?
 (fn [db _]
   (:loading? db)))

;; =============================================================================
;; Key Display
;; =============================================================================

(rf/reg-sub
 :key-display
 :<- [:current-key]
 :<- [:scale-type]
 (fn [[key-root scale-type] _]
   (let [scale-def (scales/get-scale-def scale-type)]
     {:root key-root
      :scale-type scale-type
      :name (str (name key-root) " " (:name scale-def))})))

;; =============================================================================
;; Scale Notes
;; =============================================================================

(rf/reg-sub
 :current-scale-notes
 :<- [:current-key]
 :<- [:scale-type]
 (fn [[key-root scale-type] _]
   (scales/get-scale key-root scale-type)))

;; =============================================================================
;; Diatonic Chords
;; =============================================================================

(rf/reg-sub
 :diatonic-chords
 (fn [db _]
   (:diatonic-chords db)))

(rf/reg-sub
 :diatonic-chords-by-function
 :<- [:diatonic-chords]
 (fn [chords _]
   (group-by :function chords)))

;; =============================================================================
;; Secondary Dominants & Borrowed Chords
;; =============================================================================

(rf/reg-sub
 :secondary-dominants
 (fn [db _]
   (:secondary-dominants db)))

(rf/reg-sub
 :secondary-dominants-with-targets
 :<- [:secondary-dominants]
 :<- [:diatonic-chords]
 (fn [[sec-doms diatonic-chords] _]
   (when (and (seq sec-doms) (seq diatonic-chords))
     (mapv (fn [sd]
             (let [target-degree (get-in sd [:analysis :target-degree])
                   target-chord (first (filter #(= (:degree %) target-degree) diatonic-chords))]
               (assoc sd :target-chord target-chord)))
           sec-doms))))

(rf/reg-sub
 :borrowed-chords
 (fn [db _]
   (:borrowed-chords db)))

;; =============================================================================
;; Progression
;; =============================================================================

(rf/reg-sub
 :progression
 (fn [db _]
   (:progression db)))

(rf/reg-sub
 :progression-chords
 :<- [:progression]
 (fn [progression _]
   (:chords progression)))

(rf/reg-sub
 :progression-name
 :<- [:progression]
 (fn [progression _]
   (:name progression)))

(rf/reg-sub
 :progression-length
 :<- [:progression-chords]
 (fn [chords _]
   (count chords)))

(rf/reg-sub
 :progression-empty?
 :<- [:progression-length]
 (fn [length _]
   (zero? length)))

;; =============================================================================
;; Selected Chord
;; =============================================================================

(rf/reg-sub
 :selected-chord-index
 (fn [db _]
   (:selected-chord-index db)))

(rf/reg-sub
 :selected-chord
 :<- [:progression-chords]
 :<- [:selected-chord-index]
 (fn [[chords index] _]
   (when index
     (nth chords index nil))))

(rf/reg-sub
 :selected-chord-info
 :<- [:selected-chord]
 :<- [:current-key]
 :<- [:scale-type]
 (fn [[chord key-root scale-type] _]
   (when chord
     (let [analysis (analysis/analyze-chord (:root chord) (:type chord)
                                            key-root scale-type)
           chord-def (chords/get-chord-def (:type chord))]
       {:root (:root chord)
        :type (:type chord)
        :notes (:notes chord)
        :symbol (chords/chord-symbol (:root chord) (:type chord))
        :name (:name chord-def)
        :quality (:quality chord-def)
        :numeral (:numeral analysis)
        :function (:function analysis)
        :diatonic? (:diatonic? analysis)
        :analyses (:analyses analysis)}))))

;; =============================================================================
;; Analysis Results
;; =============================================================================

(rf/reg-sub
 :analysis-results
 (fn [db _]
   (:analysis-results db)))

(rf/reg-sub
 :progression-with-analysis
 :<- [:progression-chords]
 :<- [:analysis-results]
 (fn [[chords analysis-results] _]
   (if analysis-results
     (mapv (fn [chord analysis]
             (assoc chord :analysis analysis))
           chords analysis-results)
     chords)))

(rf/reg-sub
 :numeral-sequence
 :<- [:analysis-results]
 (fn [results _]
   (when results
     (mapv analysis/get-analysis-label results))))

;; =============================================================================
;; Voicings
;; =============================================================================

(rf/reg-sub
 :selected-voicing
 (fn [db _]
   (:selected-voicing db)))

(rf/reg-sub
 :available-piano-voicings
 :<- [:selected-chord]
 (fn [chord _]
   (when chord
     (voicings/get-all-voicings (:root chord) (:type chord)))))

(rf/reg-sub
 :available-guitar-voicings
 :<- [:selected-chord]
 (fn [chord _]
   (when chord
     (guitar/get-guitar-voicings (:root chord) (:type chord)))))

(rf/reg-sub
 :available-voicings
 :<- [:voicing-mode]
 :<- [:available-piano-voicings]
 :<- [:available-guitar-voicings]
 (fn [[mode piano-voicings guitar-voicings] _]
   (case mode
     :piano piano-voicings
     :guitar guitar-voicings
     [])))

;; =============================================================================
;; Chord Palette (for quick selection)
;; =============================================================================

(rf/reg-sub
 :chord-palette
 :<- [:diatonic-chords]
 :<- [:secondary-dominants]
 :<- [:borrowed-chords]
 (fn [[diatonic sec-doms borrowed] _]
   {:diatonic diatonic
    :secondary-dominants sec-doms
    :borrowed borrowed}))

;; =============================================================================
;; Saved Progressions
;; =============================================================================

(rf/reg-sub
 :saved-progressions
 (fn [db _]
   (:saved-progressions db)))

(rf/reg-sub
 :show-save-modal?
 (fn [db _]
   (:show-save-modal? db)))

(rf/reg-sub
 :show-load-modal?
 (fn [db _]
   (:show-load-modal? db)))

(rf/reg-sub
 :progression-id
 :<- [:progression]
 (fn [progression _]
   (:id progression)))

;; =============================================================================
;; Scale Fretboard
;; =============================================================================

(rf/reg-sub
 :scale-fretboard-notes
 :<- [:current-scale-notes]
 (fn [scale-notes _]
   (when (seq scale-notes)
     (guitar/scale-notes-on-fretboard scale-notes))))

(rf/reg-sub
 :scale-positions
 :<- [:current-scale-notes]
 (fn [scale-notes _]
   (when (seq scale-notes)
     (guitar/scale-positions scale-notes))))

(rf/reg-sub
 :selected-fretboard-position
 (fn [db _]
   (or (:selected-fretboard-position db) "Open")))

(rf/reg-sub
 :scale-in-selected-position
 :<- [:current-scale-notes]
 :<- [:selected-fretboard-position]
 (fn [[scale-notes position] _]
   (when (seq scale-notes)
     (let [positions (guitar/scale-positions scale-notes)
           pos-data (get positions position)]
       (:notes pos-data)))))

(rf/reg-sub
 :scale-fretboard-with-position
 :<- [:current-scale-notes]
 :<- [:selected-fretboard-position]
 (fn [[scale-notes position] _]
   (when (seq scale-notes)
     (let [all-notes (guitar/scale-notes-on-fretboard scale-notes)
           positions (guitar/scale-positions scale-notes)
           pos-data (get positions position)
           start-fret (:start-fret pos-data)
           end-fret (:end-fret pos-data)]
       (mapv (fn [note]
               (assoc note :in-position? (<= start-fret (:fret note) end-fret)))
             all-notes)))))

(rf/reg-sub
 :selected-chord-notes-set
 :<- [:selected-chord]
 (fn [chord _]
   (when chord
     (set (map core/normalize-note (:notes chord))))))

(defn interval->chord-function
  "Map a semitone interval to its chord function."
  [interval]
  (cond
    (= interval 0) :root
    (#{3 4} interval) :third        ; minor 3rd (3) or major 3rd (4)
    (#{2 5} interval) :second       ; sus2 (2) or sus4 (5)
    (#{6 7 8} interval) :fifth      ; dim5 (6), perfect 5th (7), aug5 (8)
    (#{9 10 11} interval) :seventh  ; dim7 (9), dom7 (10), maj7 (11)
    (#{13 14} interval) :ninth      ; b9 (13), 9 (14)
    (#{15 16 17 18} interval) :eleventh ; 11 (17), #11 (18)
    (#{20 21} interval) :thirteenth ; b13 (20), 13 (21)
    :else :extension))

(rf/reg-sub
 :selected-chord-tones-map
 :<- [:selected-chord]
 (fn [chord _]
   (when chord
     (let [chord-def (chords/get-chord-def (:type chord))
           intervals (:intervals chord-def)
           root-semitone (core/normalize-note (:root chord))]
       ;; Create a map of semitone -> chord function
       (into {}
             (map-indexed
              (fn [idx interval]
                (let [note-semitone (mod (+ root-semitone interval) 12)]
                  [note-semitone (interval->chord-function interval)]))
              intervals))))))
