(ns chord-explorer.routes
  "API routes using Reitit."
  (:require [reitit.ring :as ring]
            [reitit.coercion.spec]
            [reitit.ring.coercion :as coercion]
            [chord-explorer.handlers :as handlers]))

(def api-routes
  "API route definitions."
  [["/api"
    ["/health"
     {:get {:handler handlers/health-check}}]

    ;; Scale endpoints
    ["/scales"
     {:get {:handler handlers/list-scales}}]

    ["/scales/:scale-type/:root"
     {:get {:handler handlers/get-scale
            :parameters {:path {:scale-type string?
                                :root string?}}}}]

    ;; Chord endpoints
    ["/chords"
     {:get {:handler handlers/list-chord-types}}]

    ["/chords/:chord-type/:root"
     {:get {:handler handlers/get-chord
            :parameters {:path {:chord-type string?
                                :root string?}}}}]

    ;; Diatonic chords
    ["/diatonic/:scale-type/:root"
     {:get {:handler handlers/get-diatonic-chords
            :parameters {:path {:scale-type string?
                                :root string?}}}}]

    ;; Analysis endpoints
    ["/analyze"
     {:post {:handler handlers/analyze-progression}}]

    ["/secondary-dominants/:root"
     {:get {:handler handlers/get-secondary-dominants
            :parameters {:path {:root string?}}}}]

    ["/modal-interchange/:root"
     {:get {:handler handlers/get-modal-interchange
            :parameters {:path {:root string?}}}}]

    ;; Voicing endpoints
    ["/voicings/piano/:chord-type/:root"
     {:get {:handler handlers/get-piano-voicings
            :parameters {:path {:chord-type string?
                                :root string?}}}}]

    ["/voicings/guitar/:chord-type/:root"
     {:get {:handler handlers/get-guitar-voicings
            :parameters {:path {:chord-type string?
                                :root string?}}}}]

    ;; Transpose endpoint
    ["/transpose"
     {:post {:handler handlers/transpose-progression}}]

    ;; Progression templates
    ["/templates"
     {:get {:handler handlers/list-progression-templates}}]

    ["/templates/:template-key/:root/:scale-type"
     {:get {:handler handlers/get-progression-from-template
            :parameters {:path {:template-key string?
                                :root string?
                                :scale-type string?}}}}]]])

(def app-routes
  "Complete application routes."
  (ring/ring-handler
   (ring/router
    api-routes
    {:data {:coercion reitit.coercion.spec/coercion
            :middleware [coercion/coerce-exceptions-middleware
                         coercion/coerce-request-middleware
                         coercion/coerce-response-middleware]}})
   (ring/routes
    (ring/create-resource-handler {:path "/"})
    (ring/create-default-handler
     {:not-found (constantly {:status 404
                              :body {:error "Not Found"}})}))))
