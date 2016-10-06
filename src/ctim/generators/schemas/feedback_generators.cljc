(ns ctim.generators.schemas.feedback-generators
  (:require [clj-momo.lib.time :as time]
            [clojure.test.check.generators :as gen]
            [ctim.schemas.feedback :as csf]
            [ctim.schemas.common :as schemas-common]
            [ctim.generators.common
             :refer [complete leaf-generators maybe]
             :as common]
            [ctim.generators.id :as gen-id]
            [flanders.schema :as fs]
            [schema-generators.generators :as seg]))

(def gen-feedback
  (gen/fmap
   (fn [[s id]]
     (assoc s :id id))
   (gen/tuple (seg/generator (fs/get-schema csf/StoredFeedback))
              (gen-id/gen-short-id-of-type :feedback))))

(def gen-new-feedback
  (gen/fmap
   (fn [[s id e-id [start-time end-time]]]
     (cond-> (dissoc s :id :judgement_id)
       id (assoc :id id)
       e-id (assoc :entity_id e-id)))
   (gen/tuple
    (seg/generator (fs/get-schema csf/NewFeedback))
    (gen-id/gen-short-id-of-type :feedback)
    (gen-id/gen-short-id-of-type :judgement))))
