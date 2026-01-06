(ns chord-explorer.events
  "Re-frame event handlers."
  (:require [re-frame.core :as rf]
            [chord-explorer.db :as db]
            [chord-explorer.theory.core :as core]
            [chord-explorer.theory.scales :as scales]
            [chord-explorer.theory.chords :as chords]
            [chord-explorer.theory.harmony :as harmony]
            [chord-explorer.theory.analysis :as analysis]
            [chord-explorer.theory.progression :as progression]
            [chord-explorer.theory.voicings :as voicings]
            [chord-explorer.theory.guitar :as guitar]))

;; =============================================================================
;; Initialization
;; =============================================================================

(rf/reg-event-db
 :initialize-db
 (fn [_ _]
   db/default-db))

(rf/reg-event-fx
 :initialize-app
 (fn [{:keys [db]} _]
   {:db db
    :dispatch-n [[:update-diatonic-chords]
                 [:update-secondary-dominants]
                 [:update-borrowed-chords]
                 [:load-saved-progressions-list]]}))

;; =============================================================================
;; Key and Scale Selection
;; =============================================================================

(rf/reg-event-fx
 :set-key
 (fn [{:keys [db]} [_ key-root]]
   {:db (assoc db :current-key key-root)
    :dispatch-n [[:update-diatonic-chords]
                 [:update-secondary-dominants]
                 [:update-borrowed-chords]
                 [:update-analysis]]}))

(rf/reg-event-fx
 :set-scale-type
 (fn [{:keys [db]} [_ scale-type]]
   {:db (assoc db :scale-type scale-type)
    :dispatch-n [[:update-diatonic-chords]
                 [:update-secondary-dominants]
                 [:update-borrowed-chords]
                 [:update-analysis]]}))

(rf/reg-event-db
 :toggle-seventh
 (fn [db _]
   (let [new-value (not (:show-seventh? db))]
     (assoc db :show-seventh? new-value))))

;; =============================================================================
;; Diatonic Chords Update
;; =============================================================================

(rf/reg-event-db
 :update-diatonic-chords
 (fn [db _]
   (let [key-root (:current-key db)
         scale-type (:scale-type db)
         seventh? (:show-seventh? db)
         diatonic (harmony/diatonic-chords key-root scale-type {:seventh? seventh?})]
     (assoc db :diatonic-chords diatonic))))

;; =============================================================================
;; Secondary Dominants & Modal Interchange
;; =============================================================================

(rf/reg-event-db
 :update-secondary-dominants
 (fn [db _]
   (let [key-root (:current-key db)
         scale-type (:scale-type db)
         sec-doms (analysis/all-secondary-dominants key-root scale-type)]
     (assoc db :secondary-dominants sec-doms))))

(rf/reg-event-db
 :update-borrowed-chords
 (fn [db _]
   (let [key-root (:current-key db)
         borrowed (analysis/all-borrowed-chords key-root)]
     (assoc db :borrowed-chords borrowed))))

;; =============================================================================
;; Progression Manipulation
;; =============================================================================

(rf/reg-event-fx
 :add-chord-to-progression
 (fn [{:keys [db]} [_ chord]]
   (let [current-chords (get-in db [:progression :chords])
         new-chord (progression/create-chord-entry (:root chord) (:type chord))
         new-chords (conj current-chords new-chord)]
     {:db (assoc-in db [:progression :chords] new-chords)
      :dispatch [:update-analysis]})))

(rf/reg-event-fx
 :remove-chord-from-progression
 (fn [{:keys [db]} [_ index]]
   (let [current-chords (get-in db [:progression :chords])
         new-chords (vec (concat (take index current-chords)
                                 (drop (inc index) current-chords)))]
     {:db (-> db
              (assoc-in [:progression :chords] new-chords)
              (assoc :selected-chord-index nil))
      :dispatch [:update-analysis]})))

(rf/reg-event-fx
 :move-chord
 (fn [{:keys [db]} [_ from-index to-index]]
   (let [current-chords (get-in db [:progression :chords])
         chord (nth current-chords from-index)
         without (vec (concat (take from-index current-chords)
                              (drop (inc from-index) current-chords)))
         new-chords (vec (concat (take to-index without)
                                 [chord]
                                 (drop to-index without)))]
     {:db (assoc-in db [:progression :chords] new-chords)
      :dispatch [:update-analysis]})))

(rf/reg-event-fx
 :clear-progression
 (fn [{:keys [db]} _]
   {:db (-> db
            (assoc-in [:progression :chords] [])
            (assoc :selected-chord-index nil)
            (assoc :analysis-results nil))}))

(rf/reg-event-db
 :set-progression-name
 (fn [db [_ name]]
   (assoc-in db [:progression :name] name)))

;; =============================================================================
;; Chord Selection
;; =============================================================================

(rf/reg-event-db
 :select-chord
 (fn [db [_ index]]
   (assoc db :selected-chord-index index)))

(rf/reg-event-db
 :deselect-chord
 (fn [db _]
   (assoc db :selected-chord-index nil)))

(rf/reg-event-db
 :modify-selected-chord-type
 (fn [db [_ new-type]]
   (if-let [index (:selected-chord-index db)]
     (let [chord (get-in db [:progression :chords index])
           new-notes (chords/build-chord (:root chord) new-type)]
       (-> db
           (assoc-in [:progression :chords index :type] new-type)
           (assoc-in [:progression :chords index :notes] new-notes)))
     db)))

;; =============================================================================
;; Transposition
;; =============================================================================

(rf/reg-event-fx
 :transpose-progression
 (fn [{:keys [db]} [_ semitones]]
   (let [current-key (:current-key db)
         new-key (core/transpose current-key semitones)
         current-chords (get-in db [:progression :chords])
         new-chords (mapv (fn [chord]
                            (let [new-root (core/transpose (:root chord) semitones)]
                              (assoc chord
                                     :root new-root
                                     :notes (chords/build-chord new-root (:type chord)))))
                          current-chords)]
     {:db (-> db
              (assoc :current-key new-key)
              (assoc-in [:progression :chords] new-chords))
      :dispatch-n [[:update-diatonic-chords]
                   [:update-secondary-dominants]
                   [:update-borrowed-chords]
                   [:update-analysis]]})))

;; =============================================================================
;; Analysis
;; =============================================================================

(rf/reg-event-db
 :update-analysis
 (fn [db _]
   (let [key-root (:current-key db)
         scale-type (:scale-type db)
         chords (get-in db [:progression :chords])]
     (if (seq chords)
       (let [analyzed (analysis/analyze-progression
                       (mapv #(select-keys % [:root :type]) chords)
                       key-root scale-type)]
         (assoc db :analysis-results analyzed))
       (assoc db :analysis-results nil)))))

(rf/reg-event-db
 :toggle-analysis
 (fn [db _]
   (update db :show-analysis? not)))

;; =============================================================================
;; Voicing Selection
;; =============================================================================

(rf/reg-event-db
 :set-voicing-mode
 (fn [db [_ mode]]
   (assoc db :voicing-mode mode)))

(rf/reg-event-db
 :select-voicing
 (fn [db [_ voicing]]
   (assoc db :selected-voicing voicing)))

;; =============================================================================
;; Templates
;; =============================================================================

(rf/reg-event-fx
 :load-template
 (fn [{:keys [db]} [_ template-key]]
   (let [key-root (:current-key db)
         scale-type (:scale-type db)
         seventh? (:show-seventh? db)
         prog (progression/create-from-template key-root scale-type template-key
                                                {:seventh? seventh?})]
     (when prog
       {:db (-> db
                (assoc-in [:progression :chords] (:chords prog))
                (assoc-in [:progression :name] (name template-key)))
        :dispatch [:update-analysis]}))))

;; =============================================================================
;; Audio
;; =============================================================================

(rf/reg-event-db
 :toggle-audio
 (fn [db _]
   (update db :audio-enabled? not)))

(rf/reg-event-db
 :set-tempo
 (fn [db [_ tempo]]
   (assoc db :playback-tempo tempo)))

;; =============================================================================
;; Error Handling
;; =============================================================================

(rf/reg-event-db
 :set-error
 (fn [db [_ error]]
   (assoc db :error error)))

(rf/reg-event-db
 :clear-error
 (fn [db _]
   (assoc db :error nil)))

;; =============================================================================
;; Local Storage Effects
;; =============================================================================

(def storage-key "chord-explorer-progressions")

(rf/reg-fx
 :local-storage/set
 (fn [{:keys [key value]}]
   (.setItem js/localStorage key (js/JSON.stringify (clj->js value)))))

(rf/reg-fx
 :local-storage/remove
 (fn [key]
   (.removeItem js/localStorage key)))

(defn load-progressions-from-storage []
  (when-let [data (.getItem js/localStorage storage-key)]
    (js->clj (js/JSON.parse data) :keywordize-keys true)))

;; =============================================================================
;; Progression Persistence Events
;; =============================================================================

(rf/reg-event-fx
 :save-progression
 (fn [{:keys [db]} [_ name]]
   (let [progression (:progression db)
         prog-id (or (:id progression) (str (random-uuid)))
         prog-name (or name (:name progression) "Untitled")
         prog-to-save {:id prog-id
                       :name prog-name
                       :key (:current-key db)
                       :scale-type (:scale-type db)
                       :chords (mapv #(select-keys % [:root :type :id])
                                     (:chords progression))
                       :saved-at (.toISOString (js/Date.))}
         existing (or (load-progressions-from-storage) [])
         updated (conj (vec (remove #(= (:id %) prog-id) existing)) prog-to-save)]
     {:db (-> db
              (assoc-in [:progression :id] prog-id)
              (assoc-in [:progression :name] prog-name)
              (assoc :saved-progressions updated))
      :local-storage/set {:key storage-key :value updated}})))

(rf/reg-event-fx
 :load-saved-progression
 (fn [{:keys [db]} [_ prog-id]]
   (let [progressions (or (load-progressions-from-storage) [])
         prog (first (filter #(= (:id %) prog-id) progressions))]
     (when prog
       {:db (-> db
                (assoc :current-key (keyword (:key prog)))
                (assoc :scale-type (keyword (:scale-type prog)))
                (assoc :progression {:id (:id prog)
                                     :name (:name prog)
                                     :chords (mapv (fn [c]
                                                     (let [root (keyword (:root c))
                                                           type (keyword (:type c))]
                                                       {:id (or (:id c) (str (random-uuid)))
                                                        :root root
                                                        :type type
                                                        :notes (chords/build-chord root type)}))
                                                   (:chords prog))}))
        :dispatch-n [[:update-diatonic-chords]
                     [:update-secondary-dominants]
                     [:update-borrowed-chords]
                     [:update-analysis]]}))))

(rf/reg-event-fx
 :delete-saved-progression
 (fn [{:keys [db]} [_ prog-id]]
   (let [existing (or (load-progressions-from-storage) [])
         updated (vec (remove #(= (:id %) prog-id) existing))]
     {:db (assoc db :saved-progressions updated)
      :local-storage/set {:key storage-key :value updated}})))

(rf/reg-event-db
 :load-saved-progressions-list
 (fn [db _]
   (assoc db :saved-progressions (or (load-progressions-from-storage) []))))

(rf/reg-event-db
 :toggle-save-modal
 (fn [db _]
   (update db :show-save-modal? not)))

(rf/reg-event-db
 :toggle-load-modal
 (fn [db _]
   (update db :show-load-modal? not)))

(rf/reg-event-db
 :set-fretboard-position
 (fn [db [_ position]]
   (assoc db :selected-fretboard-position position)))
