(ns chord-explorer.handlers
  "Request handlers for API endpoints."
  (:require [chord-explorer.theory.core :as core]
            [chord-explorer.theory.scales :as scales]
            [chord-explorer.theory.chords :as chords]
            [chord-explorer.theory.harmony :as harmony]
            [chord-explorer.theory.analysis :as analysis]
            [chord-explorer.theory.progression :as progression]
            [chord-explorer.theory.voicings :as voicings]
            [chord-explorer.theory.guitar :as guitar]))

;; =============================================================================
;; Utility Functions
;; =============================================================================

(defn- parse-root
  "Parse a root note from string to keyword."
  [root-str]
  (keyword (clojure.string/replace root-str #"[sS]" "#")))

(defn- parse-keyword
  "Parse a kebab-case string to keyword."
  [s]
  (keyword s))

(defn- serialize-note
  "Convert a note keyword to string."
  [note]
  (when note (name note)))

(defn- serialize-chord
  "Serialize chord data for JSON response."
  [chord]
  (-> chord
      (update :root serialize-note)
      (update :type name)
      (update :notes #(mapv serialize-note %))))

(defn- serialize-voicing
  "Serialize voicing data for JSON response."
  [voicing]
  (when voicing
    (-> voicing
        (update :root serialize-note)
        (update :chord-type name)
        (update :voicing-type name)
        (update :notes (fn [notes]
                         (mapv #(-> %
                                    (update :note serialize-note))
                               notes))))))

(defn- response
  "Create a successful JSON response."
  [data]
  {:status 200
   :body data})

(defn- error-response
  "Create an error JSON response."
  [status message]
  {:status status
   :body {:error message}})

;; =============================================================================
;; Health Check
;; =============================================================================

(defn health-check
  "Health check endpoint."
  [_]
  (response {:status "ok"
             :version "0.1.0"}))

;; =============================================================================
;; Scale Handlers
;; =============================================================================

(defn list-scales
  "List all available scale types."
  [_]
  (let [all-scales (scales/list-scales)
        by-category (scales/scales-by-category)]
    (response
     {:scales (mapv (fn [scale-key]
                      (let [def (scales/get-scale-def scale-key)]
                        {:key (name scale-key)
                         :name (:name def)
                         :category (when (:category def) (name (:category def)))}))
                    all-scales)
      :categories (mapv name (scales/list-categories))})))

(defn get-scale
  "Get notes of a specific scale."
  [request]
  (let [root-str (get-in request [:path-params :root])
        scale-type-str (get-in request [:path-params :scale-type])
        root (parse-root root-str)
        scale-type (parse-keyword scale-type-str)
        scale-notes (scales/get-scale root scale-type)]
    (if scale-notes
      (response {:root (serialize-note root)
                 :scale-type (name scale-type)
                 :notes (mapv serialize-note scale-notes)
                 :name (:name (scales/get-scale-def scale-type))})
      (error-response 404 "Scale type not found"))))

;; =============================================================================
;; Chord Handlers
;; =============================================================================

(defn list-chord-types
  "List all available chord types."
  [_]
  (let [all-types (chords/list-chord-types)]
    (response
     {:chord-types (mapv (fn [type-key]
                           (let [def (chords/get-chord-def type-key)]
                             {:key (name type-key)
                              :name (:name def)
                              :symbol (:symbol def)
                              :category (when (:category def) (name (:category def)))}))
                         all-types)
      :categories (mapv name (chords/list-chord-categories))})))

(defn get-chord
  "Build a specific chord."
  [request]
  (let [root-str (get-in request [:path-params :root])
        chord-type-str (get-in request [:path-params :chord-type])
        root (parse-root root-str)
        chord-type (parse-keyword chord-type-str)
        chord-info (chords/build-chord-with-info root chord-type)]
    (if chord-info
      (response {:root (serialize-note root)
                 :chord-type (name chord-type)
                 :symbol (:symbol chord-info)
                 :name (:name chord-info)
                 :notes (mapv serialize-note (:notes chord-info))
                 :quality (when (:quality chord-info) (name (:quality chord-info)))})
      (error-response 404 "Chord type not found"))))

;; =============================================================================
;; Diatonic Chord Handlers
;; =============================================================================

(defn get-diatonic-chords
  "Get all diatonic chords for a key."
  [request]
  (let [root-str (get-in request [:path-params :root])
        scale-type-str (get-in request [:path-params :scale-type])
        seventh? (= "true" (get-in request [:query-params "seventh"]))
        root (parse-root root-str)
        scale-type (parse-keyword scale-type-str)
        chords (harmony/diatonic-chords root scale-type {:seventh? seventh?})]
    (if chords
      (response {:key (serialize-note root)
                 :scale-type (name scale-type)
                 :seventh? seventh?
                 :chords (mapv serialize-chord chords)})
      (error-response 404 "Scale type not found"))))

;; =============================================================================
;; Analysis Handlers
;; =============================================================================

(defn analyze-progression
  "Analyze a chord progression."
  [request]
  (let [body (:body request)
        key-root (parse-root (:key body))
        scale-type (parse-keyword (:scale-type body))
        chords (mapv (fn [c]
                       {:root (parse-root (:root c))
                        :type (parse-keyword (:type c))})
                     (:chords body))
        analyzed (analysis/analyze-progression chords key-root scale-type)]
    (response {:key (serialize-note key-root)
               :scale-type (name scale-type)
               :analysis (mapv (fn [a]
                                 {:root (serialize-note (:root a))
                                  :type (name (:type a))
                                  :numeral (:numeral a)
                                  :degree (:degree a)
                                  :function (when (:function a) (name (:function a)))
                                  :diatonic? (:diatonic? a)
                                  :label (analysis/get-analysis-label a)})
                               analyzed)})))

(defn get-secondary-dominants
  "Get all secondary dominants for a key."
  [request]
  (let [root-str (get-in request [:path-params :root])
        root (parse-root root-str)
        sec-doms (analysis/all-secondary-dominants root)]
    (response {:key (serialize-note root)
               :secondary-dominants
               (mapv (fn [sd]
                       {:root (serialize-note (:root sd))
                        :type (name (:type sd))
                        :notes (mapv serialize-note (:notes sd))
                        :notation (get-in sd [:analysis :notation])
                        :target-degree (get-in sd [:analysis :target-degree])
                        :target-root (serialize-note (get-in sd [:analysis :target-root]))})
                     sec-doms)})))

(defn get-modal-interchange
  "Get all borrowed chords for a key."
  [request]
  (let [root-str (get-in request [:path-params :root])
        root (parse-root root-str)
        borrowed (analysis/all-borrowed-chords root)]
    (response {:key (serialize-note root)
               :borrowed-chords
               (mapv (fn [b]
                       {:root (serialize-note (:root b))
                        :type (name (:type b))
                        :notes (mapv serialize-note (:notes b))
                        :notation (get-in b [:analysis :notation])
                        :source-mode (when-let [m (get-in b [:analysis :source-mode])]
                                       (name m))})
                     borrowed)})))

;; =============================================================================
;; Voicing Handlers
;; =============================================================================

(defn get-piano-voicings
  "Get piano voicings for a chord."
  [request]
  (let [root-str (get-in request [:path-params :root])
        chord-type-str (get-in request [:path-params :chord-type])
        root (parse-root root-str)
        chord-type (parse-keyword chord-type-str)
        all-voicings (voicings/get-all-voicings root chord-type)]
    (response {:root (serialize-note root)
               :chord-type (name chord-type)
               :voicings (mapv serialize-voicing all-voicings)})))

(defn get-guitar-voicings
  "Get guitar voicings for a chord."
  [request]
  (let [root-str (get-in request [:path-params :root])
        chord-type-str (get-in request [:path-params :chord-type])
        root (parse-root root-str)
        chord-type (parse-keyword chord-type-str)
        all-voicings (guitar/get-guitar-voicings root chord-type)]
    (response {:root (serialize-note root)
               :chord-type (name chord-type)
               :voicings (mapv (fn [v]
                                 {:name (:name v)
                                  :frets (:frets v)
                                  :fingers (:fingers v)
                                  :barre (:barre v)
                                  :position (:position v)
                                  :difficulty (name (:difficulty v))
                                  :category (name (:category v))})
                               all-voicings)})))

;; =============================================================================
;; Transpose Handler
;; =============================================================================

(defn transpose-progression
  "Transpose a progression by semitones."
  [request]
  (let [body (:body request)
        key-root (parse-root (:key body))
        scale-type (parse-keyword (:scale-type body))
        chords (mapv (fn [c]
                       {:root (parse-root (:root c))
                        :type (parse-keyword (:type c))})
                     (:chords body))
        semitones (:semitones body)
        prog (progression/create-progression key-root scale-type chords)
        transposed (progression/transpose-progression prog semitones)]
    (response {:key (serialize-note (:key transposed))
               :scale-type (name (:scale-type transposed))
               :chords (mapv (fn [c]
                               {:root (serialize-note (:root c))
                                :type (name (:type c))
                                :notes (mapv serialize-note (:notes c))})
                             (:chords transposed))})))

;; =============================================================================
;; Template Handlers
;; =============================================================================

(defn list-progression-templates
  "List available progression templates."
  [_]
  (response {:templates (mapv (fn [[k v]]
                                {:key (name k)
                                 :degrees v})
                              progression/common-progression-templates)}))

(defn get-progression-from-template
  "Generate a progression from a template."
  [request]
  (let [template-str (get-in request [:path-params :template-key])
        root-str (get-in request [:path-params :root])
        scale-type-str (get-in request [:path-params :scale-type])
        seventh? (= "true" (get-in request [:query-params "seventh"]))
        template-key (parse-keyword template-str)
        root (parse-root root-str)
        scale-type (parse-keyword scale-type-str)
        prog (progression/create-from-template root scale-type template-key
                                               {:seventh? seventh?})]
    (if prog
      (response {:key (serialize-note (:key prog))
                 :scale-type (name (:scale-type prog))
                 :template (name template-key)
                 :chords (mapv (fn [c]
                                 {:root (serialize-note (:root c))
                                  :type (name (:type c))
                                  :notes (mapv serialize-note (:notes c))})
                               (:chords prog))})
      (error-response 404 "Template not found"))))
