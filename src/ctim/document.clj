(ns ctim.document
  (:gen-class)
  (:require [clojure.java.io :as io]
            [ctim.schemas
             [judgement :as j]]
            [flanders.markdown :as fm]))

(defn -main [& _args_]
  (spit (io/file
         (io/resource "judgement.md"))
        (fm/->markdown j/Judgement)))
