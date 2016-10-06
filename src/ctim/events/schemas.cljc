(ns ctim.events.schemas
  (:require [ctim.schemas.verdict :as v]
            [flanders.schema :as fs]
            [schema.core :as s]
            [schema-tools.core :as st]))

(s/defschema ModelEventBase
  {:owner s/Str
   (s/optional-key :timestamp) s/Inst
   :entity {s/Any s/Any}
   :id s/Str
   (s/optional-key :http-params) {s/Any s/Any}})

(def CreateEventType "CreatedModel")

(s/defschema CreateEvent
  (st/merge
   ModelEventBase
   {:type (s/eq CreateEventType)}))

(def UpdateEventType "UpdatedModel")

(s/defschema UpdateTriple
  [(s/one s/Keyword "field")
   (s/one s/Str "action")
   (s/one {s/Any s/Any} "metadata")])

(s/defschema UpdateEvent
  (st/merge
   ModelEventBase
   {:type (s/eq UpdateEventType)
    :fields [UpdateTriple]}))

(def DeleteEventType "DeletedModel")

(s/defschema DeleteEvent
  (st/merge
   ModelEventBase
   {:type (s/eq DeleteEventType)}))

(def VerdictChangeEventType "VerdictChange")

(s/defschema VerdictChangeEvent
  (st/merge
   ModelEventBase
   {:type (s/eq VerdictChangeEventType)
    :judgement_id s/Str
    :verdict (fs/->schema-tree v/Verdict)}))

(def event-types
  ["CreatedModel" "UpdatedModel" "DeletedModel" "VerdictChange"])

(s/defschema Event
  (s/conditional
   #(= CreateEventType        (:type %)) CreateEvent
   #(= UpdateEventType        (:type %)) UpdateEvent
   #(= DeleteEventType        (:type %)) DeleteEvent
   #(= VerdictChangeEventType (:type %)) VerdictChangeEvent))
