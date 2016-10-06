(ns ctim.schemas.actor
  (:require [ctim.schemas.common :as c]
            [ctim.schemas.relationships :as rel]
            [ctim.schemas.vocabularies :as v]
            #?(:clj  [flanders.core :as f :refer [def-entity-type]]
               :cljs [flanders.core :as f :refer-macros [def-entity-type]])))

(def TypeIdentifier
  (f/eq "actor"))

(def-entity-type Actor
  "http://stixproject.github.io/data-model/1.2/ta/ThreatActorType/"
  c/base-entity-entries
  c/sourced-object-entries
  (f/required-entries
   (f/entry :type TypeIdentifier)
   (f/entry :valid_time c/ValidTime)
   (f/entry :actor_type v/ThreatActorType))
  (f/optional-entries
   (f/entry :identity c/Identity)
   (f/entry :motivation v/Motivation)
   (f/entry :sophistication v/Sophistication)
   (f/entry :intended_effect v/IntendedEffect)
   (f/entry :planning_and_operational_support f/any-str)
   (f/entry :observed_TTPs rel/RelatedTTPs)
   (f/entry :associated_campaigns rel/RelatedCampaigns)
   (f/entry :associated_actors rel/RelatedActors)
   (f/entry :confidence v/HighMedLow))
  ;; Not provided: handling
  ;; Not provided: related_packages (deprecated)
  )

(def-entity-type NewActor
  "Schema for submitting new Actors"
  (:entries Actor)
  c/base-new-entity-entries
  (f/optional-entries
   (f/entry :type TypeIdentifier)
   (f/entry :valid_time c/ValidTime)))

(def-entity-type StoredActor
  "An actor as stored in the data store"
  (:entries Actor)
  c/base-stored-entity-entries)
