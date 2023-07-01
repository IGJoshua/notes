(ns build
  (:require
   [clojure.tools.build.api :as b]))

(def source-dirs ["src/"])

(def target-dir "target/")
(def class-dir (str target-dir "classes/"))

(def basis (b/create-basis {:project "deps.edn"}))

(def jar-file (str target-dir "cljnotes.jar"))

(defn clean
  "Deletes the `target/` directory."
  [opts]
  (b/delete {:path target-dir}))

(defn uber
  "Generates a `coffi.jar` file in the `target/` directory.
  This is a thin jar including only the sources."
  [opts]
  (b/copy-dir {:target-dir class-dir
               :src-dirs source-dirs})
  (b/compile-clj {:basis basis
                  :src-dirs source-dirs
                  :class-dir class-dir
                  :compile-opts {:direct-linking true
                                 :elide-meta [:doc :file :line]}})
  (b/uber {:class-dir class-dir
           :uber-file jar-file
           :basis basis
           :main 'notes.core}))
