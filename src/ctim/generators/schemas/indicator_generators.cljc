(ns ctim.generators.schemas.indicator-generators
  (:require [clj-momo.lib.time :as time]
            [clojure.test.check.generators :as gen]
            [ctim.schemas.common :as schema-common]
            [ctim.schemas.indicator :as csi]
            [ctim.generators.common
             :refer [complete leaf-generators maybe]
             :as common]
            [ctim.generators.id :as gen-id]
            [flanders.schema :as fs]
            [schema-generators.generators :as seg]))

(def gen-short-id
  (gen-id/gen-short-id-of-type :indicator))

(def gen-indicator
  (gen/fmap
   (fn [[s id]]
     (assoc s :id id))
   (gen/tuple (seg/generator (fs/get-schema csi/StoredIndicator))
              gen-short-id)))

(defn gen-new-indicator_ [gen-id]
  (gen/fmap
   (fn [[s id [start-time end-time]]]
     (cond-> (dissoc s :id :valid_time)
       id (assoc :id id)
       start-time (assoc-in [:valid_time :start_time] start-time)
       end-time (assoc-in [:valid_time :end_time] end-time)))
   (gen/tuple
    (seg/generator (fs/get-schema csi/NewIndicator))
    gen-id
    ;; complete doesn't seem to generate :valid_time values, so do it manually
    common/gen-valid-time-tuple)))

(def gen-new-indicator
  (gen-new-indicator_
   (maybe gen-short-id)))

(def gen-new-indicator-with-id
  (gen-new-indicator_
   gen-short-id))
