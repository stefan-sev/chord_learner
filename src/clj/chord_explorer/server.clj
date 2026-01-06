(ns chord-explorer.server
  "Ring server setup with middleware."
  (:require [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.middleware.cors :refer [wrap-cors]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.not-modified :refer [wrap-not-modified]]
            [chord-explorer.routes :as routes])
  (:gen-class))

(def app
  "Main Ring application with all middleware."
  (-> routes/app-routes
      (wrap-json-body {:keywords? true})
      wrap-json-response
      (wrap-cors :access-control-allow-origin [#".*"]
                 :access-control-allow-methods [:get :post :put :delete :options])
      (wrap-resource "public")
      wrap-content-type
      wrap-not-modified))

(defonce server (atom nil))

(defn start-server
  "Start the server on the specified port."
  [port]
  (when @server
    (.stop @server))
  (reset! server
          (run-jetty app
                     {:port port
                      :join? false}))
  (println (str "Server started on http://localhost:" port)))

(defn stop-server
  "Stop the running server."
  []
  (when @server
    (.stop @server)
    (reset! server nil)
    (println "Server stopped")))

(defn -main
  "Application entry point."
  [& args]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "3002"))]
    (start-server port)))

;; For Figwheel compatibility
(def handler app)
