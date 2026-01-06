(ns build
  "Build script for Chord Explorer."
  (:require [clojure.tools.build.api :as b]))

(def lib 'chord-explorer/chord-explorer)
(def version "0.1.0-SNAPSHOT")
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def uber-file (format "target/%s-%s-standalone.jar" (name lib) version))

(defn clean
  "Clean build artifacts."
  [_]
  (b/delete {:path "target"})
  (b/delete {:path "resources/public/js/compiled"}))

(defn uber
  "Build an uberjar."
  [_]
  (clean nil)
  (b/copy-dir {:src-dirs ["src/clj" "src/cljc" "resources"]
               :target-dir class-dir})
  (b/compile-clj {:basis basis
                  :src-dirs ["src/clj" "src/cljc"]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis
           :main 'chord-explorer.server}))
