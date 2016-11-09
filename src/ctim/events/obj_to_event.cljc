(ns ctim.events.obj-to-event
  (:require [clj-momo.lib.time :as t]
            [clojure.data :refer [diff]]
            [ctim.events.schemas :as vs]
            [ctim.schemas.actor :refer [StoredActor]]
            [ctim.schemas.campaign :refer [StoredCampaign]]
            [schema.core :as s]))

(s/defn to-create-event :- vs/CreateEvent
  "Create a CreateEvent from a StoredX object"
  ([object]
   (to-create-event object (:id object)))
  ([object id]
   {:owner (:owner object)
    :entity object
    :timestamp (t/now)
    :id id
    :type vs/CreateEventType}))

(defn diff-to-list-of-triplet
  "Given the output of a `diff` between maps return a list
  of edit distance operation under the form of an atom triplet."
  [[diff-before diff-after _]]
  (concat
   (map (fn [k]
          (if (contains? diff-after k)
            [k "modified" {(get diff-before k)
                           (get diff-after k)}]
            [k "deleted" {}]))
        (keys diff-before))
   (map (fn [k] [k "added" {}])
        (remove #(contains? diff-before %)
                (keys diff-after)))))


(s/defn to-update-event :- vs/UpdateEvent
  "transform an object (generally a `StoredObject`) to an `UpdateEvent`.
   The two arguments `object` and `prev-object` should have the same schema.
   The fields should contains enough information to retrieve all informations.
   But the complete object is given for simplicity."

  ([object prev-object]
   (to-update-event object prev-object (:id object)))
  ([object prev-object id]
   {:owner (:owner object)
    :entity object
    :timestamp (t/now)
    :id id
    :type vs/UpdateEventType
    :fields (diff-to-list-of-triplet
             (diff object prev-object))
    ;; I believe the `metadata` shoudld be `{s/Any s/Any}`
    ;; in the following schema:
    ;; `ctim.events.schemas/UpdateTriple`
    }))

(s/defn to-delete-event :- vs/DeleteEvent
  "transform an object (generally a `StoredObject`) to its corresponding `Event`"
  ([object]
   (to-delete-event object (:id object)))
  ([object id]
   {:owner (:owner object)
    :entity object
    :timestamp (t/now)
    :id id
    :type vs/DeleteEventType}))
