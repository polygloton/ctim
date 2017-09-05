(ns ctim.specs.fields-test
  (:require
   [clj-momo.test-helpers.core :as mth]
   [clojure.spec :as spec]
   [clojure.test :refer [are deftest testing use-fixtures]]
   [ctim.examples
    [actors :refer [actor-minimal
                    new-actor-minimal
                    stored-actor-minimal]]
    [campaigns :refer [campaign-minimal
                       new-campaign-minimal
                       stored-campaign-minimal]]
    [coas :refer [coa-maximal
                  new-coa-maximal
                  stored-coa-maximal
                  coa-minimal
                  new-coa-minimal
                  stored-coa-minimal]]
    [exploit-targets :refer [exploit-target-maximal
                             new-exploit-target-maximal
                             stored-exploit-target-maximal
                             exploit-target-minimal
                             new-exploit-target-minimal
                             stored-exploit-target-minimal]]
    [incidents :refer [incident-minimal
                       new-incident-minimal
                       stored-incident-minimal]]
    [indicators :refer [indicator-minimal
                        new-indicator-minimal
                        stored-indicator-minimal]]
    [judgements :refer [judgement-minimal
                        new-judgement-minimal
                        stored-judgement-minimal]]
    [sightings :refer [sighting-minimal
                       new-sighting-minimal
                       stored-sighting-minimal]]
    [ttps :refer [ttp-minimal
                  new-ttp-minimal
                  stored-ttp-minimal]]]
   [ctim.schemas
    [actor :as actor]
    [campaign :as campaign]
    [coa :as coa]
    [exploit-target :as exploit-target]
    [incident :as incident]
    [indicator :as indicator]
    [judgement :as judgement]
    [sighting :as sighting]
    [ttp :as ttp]]
   [ctim.test-helpers.core :as th :refer [rand-str]]))

(use-fixtures :once
  th/fixture-spec-validation
  th/fixture-fast-gen
  mth/fixture-schema-validation
  (th/fixture-spec actor/Actor
                   "test.actor")
  (th/fixture-spec actor/NewActor
                   "test.new-actor")
  (th/fixture-spec actor/StoredActor
                   "test.stored-actor")

  (th/fixture-spec campaign/Campaign
                   "test.campaign")
  (th/fixture-spec campaign/NewCampaign
                   "test.new-campaign")
  (th/fixture-spec campaign/StoredCampaign
                   "test.stored-campaign")

  (th/fixture-spec coa/COA
                   "test.coa")
  (th/fixture-spec coa/NewCOA
                   "test.new-coa")
  (th/fixture-spec coa/StoredCOA
                   "test.stored-coa")

  (th/fixture-spec exploit-target/ExploitTarget
                   "test.exploit-target")
  (th/fixture-spec exploit-target/NewExploitTarget
                   "test.new-exploit-target")
  (th/fixture-spec exploit-target/StoredExploitTarget
                   "test.stored-exploit-target")

  (th/fixture-spec incident/Incident
                   "test.incident")
  (th/fixture-spec incident/NewIncident
                   "test.new-incident")
  (th/fixture-spec incident/StoredIncident
                   "test.stored-incident")

  (th/fixture-spec indicator/Indicator
                   "test.indicator")
  (th/fixture-spec indicator/NewIndicator
                   "test.new-indicator")
  (th/fixture-spec indicator/StoredIndicator
                   "test.stored-indicator")

  (th/fixture-spec judgement/Judgement
                   "test.judgement")
  (th/fixture-spec judgement/NewJudgement
                   "test.new-judgement")
  (th/fixture-spec judgement/StoredJudgement
                   "test.stored-judgement")

  (th/fixture-spec sighting/Sighting
                   "test.sighting")
  (th/fixture-spec sighting/NewSighting
                   "test.new-sighting")
  (th/fixture-spec sighting/StoredSighting
                   "test.stored-sighting")

  (th/fixture-spec ttp/TTP
                   "test.ttp")
  (th/fixture-spec ttp/NewTTP
                   "test.new-ttp")
  (th/fixture-spec ttp/StoredTTP
                   "test.stored-ttp"))

(deftest test-field-validators
  (testing ":description"
    (are [expected desc spec entity]
        (= expected
           (spec/valid? spec
                        (assoc entity
                               :description desc)))

        false nil :test.actor/map        actor-minimal
        false nil :test.new-actor/map    new-actor-minimal
        false nil :test.stored-actor/map stored-actor-minimal
        true  ""  :test.actor/map        actor-minimal
        true  ""  :test.new-actor/map    new-actor-minimal
        true  ""  :test.stored-actor/map stored-actor-minimal
        true  (rand-str 100)  :test.actor/map        actor-minimal
        true  (rand-str 100)  :test.new-actor/map    new-actor-minimal
        true  (rand-str 100)  :test.stored-actor/map stored-actor-minimal
        true  (rand-str 1000) :test.actor/map        actor-minimal
        true  (rand-str 1000) :test.new-actor/map    new-actor-minimal
        true  (rand-str 1000) :test.stored-actor/map stored-actor-minimal
        true  (rand-str 5000) :test.actor/map        actor-minimal
        true  (rand-str 5000) :test.new-actor/map    new-actor-minimal
        true  (rand-str 5000) :test.stored-actor/map stored-actor-minimal
        false (rand-str 5001) :test.actor/map        actor-minimal
        false (rand-str 5001) :test.new-actor/map    new-actor-minimal
        false (rand-str 5001) :test.stored-actor/map stored-actor-minimal
        false (rand-str 10000) :test.actor/map        actor-minimal
        false (rand-str 10000) :test.new-actor/map    new-actor-minimal
        false (rand-str 10000) :test.stored-actor/map stored-actor-minimal

        false nil :test.campaign/map        campaign-minimal
        false nil :test.new-campaign/map    new-campaign-minimal
        false nil :test.stored-campaign/map stored-campaign-minimal
        true  ""  :test.campaign/map        campaign-minimal
        true  ""  :test.new-campaign/map    new-campaign-minimal
        true  ""  :test.stored-campaign/map stored-campaign-minimal
        true  (rand-str 100)  :test.campaign/map        campaign-minimal
        true  (rand-str 100)  :test.new-campaign/map    new-campaign-minimal
        true  (rand-str 100)  :test.stored-campaign/map stored-campaign-minimal
        true  (rand-str 1000) :test.campaign/map        campaign-minimal
        true  (rand-str 1000) :test.new-campaign/map    new-campaign-minimal
        true  (rand-str 1000) :test.stored-campaign/map stored-campaign-minimal
        true  (rand-str 5000) :test.campaign/map        campaign-minimal
        true  (rand-str 5000) :test.new-campaign/map    new-campaign-minimal
        true  (rand-str 5000) :test.stored-campaign/map stored-campaign-minimal
        false (rand-str 5001) :test.campaign/map        campaign-minimal
        false (rand-str 5001) :test.new-campaign/map    new-campaign-minimal
        false (rand-str 5001) :test.stored-campaign/map stored-campaign-minimal
        false (rand-str 10000) :test.campaign/map        campaign-minimal
        false (rand-str 10000) :test.new-campaign/map    new-campaign-minimal
        false (rand-str 10000) :test.stored-campaign/map stored-campaign-minimal

        false nil :test.coa/map        coa-minimal
        false nil :test.new-coa/map    new-coa-minimal
        false nil :test.stored-coa/map stored-coa-minimal
        true  ""  :test.coa/map        coa-minimal
        true  ""  :test.new-coa/map    new-coa-minimal
        true  ""  :test.stored-coa/map stored-coa-minimal
        true  (rand-str 100)  :test.coa/map        coa-minimal
        true  (rand-str 100)  :test.new-coa/map    new-coa-minimal
        true  (rand-str 100)  :test.stored-coa/map stored-coa-minimal
        true  (rand-str 1000) :test.coa/map        coa-minimal
        true  (rand-str 1000) :test.new-coa/map    new-coa-minimal
        true  (rand-str 1000) :test.stored-coa/map stored-coa-minimal
        true  (rand-str 5000) :test.coa/map        coa-minimal
        true  (rand-str 5000) :test.new-coa/map    new-coa-minimal
        true  (rand-str 5000) :test.stored-coa/map stored-coa-minimal
        false (rand-str 5001) :test.coa/map        coa-minimal
        false (rand-str 5001) :test.new-coa/map    new-coa-minimal
        false (rand-str 5001) :test.stored-coa/map stored-coa-minimal
        false (rand-str 10000) :test.coa/map        coa-minimal
        false (rand-str 10000) :test.new-coa/map    new-coa-minimal
        false (rand-str 10000) :test.stored-coa/map stored-coa-minimal

        false nil :test.exploit-target/map        exploit-target-minimal
        false nil :test.new-exploit-target/map    new-exploit-target-minimal
        false nil :test.stored-exploit-target/map stored-exploit-target-minimal
        true  ""  :test.exploit-target/map        exploit-target-minimal
        true  ""  :test.new-exploit-target/map    new-exploit-target-minimal
        true  ""  :test.stored-exploit-target/map stored-exploit-target-minimal
        true  (rand-str 100)  :test.exploit-target/map        exploit-target-minimal
        true  (rand-str 100)  :test.new-exploit-target/map    new-exploit-target-minimal
        true  (rand-str 100)  :test.stored-exploit-target/map stored-exploit-target-minimal
        true  (rand-str 1000) :test.exploit-target/map        exploit-target-minimal
        true  (rand-str 1000) :test.new-exploit-target/map    new-exploit-target-minimal
        true  (rand-str 1000) :test.stored-exploit-target/map stored-exploit-target-minimal
        true  (rand-str 5000) :test.exploit-target/map        exploit-target-minimal
        true  (rand-str 5000) :test.new-exploit-target/map    new-exploit-target-minimal
        true  (rand-str 5000) :test.stored-exploit-target/map stored-exploit-target-minimal
        false (rand-str 5001) :test.exploit-target/map        exploit-target-minimal
        false (rand-str 5001) :test.new-exploit-target/map    new-exploit-target-minimal
        false (rand-str 5001) :test.stored-exploit-target/map stored-exploit-target-minimal
        false (rand-str 10000) :test.exploit-target/map        exploit-target-minimal
        false (rand-str 10000) :test.new-exploit-target/map    new-exploit-target-minimal
        false (rand-str 10000) :test.stored-exploit-target/map stored-exploit-target-minimal

        false nil :test.incident/map        incident-minimal
        false nil :test.new-incident/map    new-incident-minimal
        false nil :test.stored-incident/map stored-incident-minimal
        true  ""  :test.incident/map        incident-minimal
        true  ""  :test.new-incident/map    new-incident-minimal
        true  ""  :test.stored-incident/map stored-incident-minimal
        true  (rand-str 100)  :test.incident/map        incident-minimal
        true  (rand-str 100)  :test.new-incident/map    new-incident-minimal
        true  (rand-str 100)  :test.stored-incident/map stored-incident-minimal
        true  (rand-str 1000) :test.incident/map        incident-minimal
        true  (rand-str 1000) :test.new-incident/map    new-incident-minimal
        true  (rand-str 1000) :test.stored-incident/map stored-incident-minimal
        true  (rand-str 5000) :test.incident/map        incident-minimal
        true  (rand-str 5000) :test.new-incident/map    new-incident-minimal
        true  (rand-str 5000) :test.stored-incident/map stored-incident-minimal
        false (rand-str 5001) :test.incident/map        incident-minimal
        false (rand-str 5001) :test.new-incident/map    new-incident-minimal
        false (rand-str 5001) :test.stored-incident/map stored-incident-minimal
        false (rand-str 10000) :test.incident/map        incident-minimal
        false (rand-str 10000) :test.new-incident/map    new-incident-minimal
        false (rand-str 10000) :test.stored-incident/map stored-incident-minimal

        false nil :test.indicator/map        indicator-minimal
        false nil :test.new-indicator/map    new-indicator-minimal
        false nil :test.stored-indicator/map stored-indicator-minimal
        true  ""  :test.indicator/map        indicator-minimal
        true  ""  :test.new-indicator/map    new-indicator-minimal
        true  ""  :test.stored-indicator/map stored-indicator-minimal
        true  (rand-str 100)  :test.indicator/map        indicator-minimal
        true  (rand-str 100)  :test.new-indicator/map    new-indicator-minimal
        true  (rand-str 100)  :test.stored-indicator/map stored-indicator-minimal
        true  (rand-str 1000) :test.indicator/map        indicator-minimal
        true  (rand-str 1000) :test.new-indicator/map    new-indicator-minimal
        true  (rand-str 1000) :test.stored-indicator/map stored-indicator-minimal
        true  (rand-str 5000) :test.indicator/map        indicator-minimal
        true  (rand-str 5000) :test.new-indicator/map    new-indicator-minimal
        true  (rand-str 5000) :test.stored-indicator/map stored-indicator-minimal
        false (rand-str 5001) :test.indicator/map        indicator-minimal
        false (rand-str 5001) :test.new-indicator/map    new-indicator-minimal
        false (rand-str 5001) :test.stored-indicator/map stored-indicator-minimal
        false (rand-str 10000) :test.indicator/map        indicator-minimal
        false (rand-str 10000) :test.new-indicator/map    new-indicator-minimal
        false (rand-str 10000) :test.stored-indicator/map stored-indicator-minimal

        false nil :test.sighting/map        sighting-minimal
        false nil :test.new-sighting/map    new-sighting-minimal
        false nil :test.stored-sighting/map stored-sighting-minimal
        true  ""  :test.sighting/map        sighting-minimal
        true  ""  :test.new-sighting/map    new-sighting-minimal
        true  ""  :test.stored-sighting/map stored-sighting-minimal
        true  (rand-str 100)  :test.sighting/map        sighting-minimal
        true  (rand-str 100)  :test.new-sighting/map    new-sighting-minimal
        true  (rand-str 100)  :test.stored-sighting/map stored-sighting-minimal
        true  (rand-str 1000) :test.sighting/map        sighting-minimal
        true  (rand-str 1000) :test.new-sighting/map    new-sighting-minimal
        true  (rand-str 1000) :test.stored-sighting/map stored-sighting-minimal
        true  (rand-str 5000) :test.sighting/map        sighting-minimal
        true  (rand-str 5000) :test.new-sighting/map    new-sighting-minimal
        true  (rand-str 5000) :test.stored-sighting/map stored-sighting-minimal
        false (rand-str 5001) :test.sighting/map        sighting-minimal
        false (rand-str 5001) :test.new-sighting/map    new-sighting-minimal
        false (rand-str 5001) :test.stored-sighting/map stored-sighting-minimal
        false (rand-str 10000) :test.sighting/map        sighting-minimal
        false (rand-str 10000) :test.new-sighting/map    new-sighting-minimal
        false (rand-str 10000) :test.stored-sighting/map stored-sighting-minimal

        false nil :test.ttp/map        ttp-minimal
        false nil :test.new-ttp/map    new-ttp-minimal
        false nil :test.stored-ttp/map stored-ttp-minimal
        true  ""  :test.ttp/map        ttp-minimal
        true  ""  :test.new-ttp/map    new-ttp-minimal
        true  ""  :test.stored-ttp/map stored-ttp-minimal
        true  (rand-str 100)  :test.ttp/map        ttp-minimal
        true  (rand-str 100)  :test.new-ttp/map    new-ttp-minimal
        true  (rand-str 100)  :test.stored-ttp/map stored-ttp-minimal
        true  (rand-str 1000) :test.ttp/map        ttp-minimal
        true  (rand-str 1000) :test.new-ttp/map    new-ttp-minimal
        true  (rand-str 1000) :test.stored-ttp/map stored-ttp-minimal
        true  (rand-str 5000) :test.ttp/map        ttp-minimal
        true  (rand-str 5000) :test.new-ttp/map    new-ttp-minimal
        true  (rand-str 5000) :test.stored-ttp/map stored-ttp-minimal
        false (rand-str 5001) :test.ttp/map        ttp-minimal
        false (rand-str 5001) :test.new-ttp/map    new-ttp-minimal
        false (rand-str 5001) :test.stored-ttp/map stored-ttp-minimal
        false (rand-str 10000) :test.ttp/map        ttp-minimal
        false (rand-str 10000) :test.new-ttp/map    new-ttp-minimal
        false (rand-str 10000) :test.stored-ttp/map stored-ttp-minimal))

  (testing ":short_description"
    (are [expected title spec entity]
        (= expected
           (spec/valid? spec
                        (assoc entity
                               :short_description title)))

        false nil :test.actor/map        actor-minimal
        false nil :test.new-actor/map    new-actor-minimal
        false nil :test.stored-actor/map stored-actor-minimal
        true  ""  :test.actor/map        actor-minimal
        true  ""  :test.new-actor/map    new-actor-minimal
        true  ""  :test.stored-actor/map stored-actor-minimal
        true  (rand-str 100)  :test.actor/map        actor-minimal
        true  (rand-str 100)  :test.new-actor/map    new-actor-minimal
        true  (rand-str 100)  :test.stored-actor/map stored-actor-minimal
        true  (rand-str 2048) :test.actor/map        actor-minimal
        true  (rand-str 2048) :test.new-actor/map    new-actor-minimal
        true  (rand-str 2048) :test.stored-actor/map stored-actor-minimal
        false (rand-str 2049) :test.actor/map        actor-minimal
        false (rand-str 2049) :test.new-actor/map    new-actor-minimal
        false (rand-str 2049) :test.stored-actor/map stored-actor-minimal
        false (rand-str 5000) :test.actor/map        actor-minimal
        false (rand-str 5000) :test.new-actor/map    new-actor-minimal
        false (rand-str 5000) :test.stored-actor/map stored-actor-minimal

        false nil :test.campaign/map        campaign-minimal
        false nil :test.new-campaign/map    new-campaign-minimal
        false nil :test.stored-campaign/map stored-campaign-minimal
        true  ""  :test.campaign/map        campaign-minimal
        true  ""  :test.new-campaign/map    new-campaign-minimal
        true  ""  :test.stored-campaign/map stored-campaign-minimal
        true  (rand-str 100)  :test.campaign/map        campaign-minimal
        true  (rand-str 100)  :test.new-campaign/map    new-campaign-minimal
        true  (rand-str 100)  :test.stored-campaign/map stored-campaign-minimal
        true  (rand-str 2048) :test.campaign/map        campaign-minimal
        true  (rand-str 2048) :test.new-campaign/map    new-campaign-minimal
        true  (rand-str 2048) :test.stored-campaign/map stored-campaign-minimal
        false (rand-str 2049) :test.campaign/map        campaign-minimal
        false (rand-str 2049) :test.new-campaign/map    new-campaign-minimal
        false (rand-str 2049) :test.stored-campaign/map stored-campaign-minimal
        false (rand-str 5000) :test.campaign/map        campaign-minimal
        false (rand-str 5000) :test.new-campaign/map    new-campaign-minimal
        false (rand-str 5000) :test.stored-campaign/map stored-campaign-minimal

        false nil :test.coa/map        coa-minimal
        false nil :test.new-coa/map    new-coa-minimal
        false nil :test.stored-coa/map stored-coa-minimal
        true  ""  :test.coa/map        coa-minimal
        true  ""  :test.new-coa/map    new-coa-minimal
        true  ""  :test.stored-coa/map stored-coa-minimal
        true  (rand-str 100)  :test.coa/map        coa-minimal
        true  (rand-str 100)  :test.new-coa/map    new-coa-minimal
        true  (rand-str 100)  :test.stored-coa/map stored-coa-minimal
        true  (rand-str 2048) :test.coa/map        coa-minimal
        true  (rand-str 2048) :test.new-coa/map    new-coa-minimal
        true  (rand-str 2048) :test.stored-coa/map stored-coa-minimal
        false (rand-str 2049) :test.coa/map        coa-minimal
        false (rand-str 2049) :test.new-coa/map    new-coa-minimal
        false (rand-str 2049) :test.stored-coa/map stored-coa-minimal
        false (rand-str 5000) :test.coa/map        coa-minimal
        false (rand-str 5000) :test.new-coa/map    new-coa-minimal
        false (rand-str 5000) :test.stored-coa/map stored-coa-minimal

        false nil :test.exploit-target/map        exploit-target-minimal
        false nil :test.new-exploit-target/map    new-exploit-target-minimal
        false nil :test.stored-exploit-target/map stored-exploit-target-minimal
        true  ""  :test.exploit-target/map        exploit-target-minimal
        true  ""  :test.new-exploit-target/map    new-exploit-target-minimal
        true  ""  :test.stored-exploit-target/map stored-exploit-target-minimal
        true  (rand-str 100)  :test.exploit-target/map        exploit-target-minimal
        true  (rand-str 100)  :test.new-exploit-target/map    new-exploit-target-minimal
        true  (rand-str 100)  :test.stored-exploit-target/map stored-exploit-target-minimal
        true  (rand-str 2048) :test.exploit-target/map        exploit-target-minimal
        true  (rand-str 2048) :test.new-exploit-target/map    new-exploit-target-minimal
        true  (rand-str 2048) :test.stored-exploit-target/map stored-exploit-target-minimal
        false (rand-str 2049) :test.exploit-target/map        exploit-target-minimal
        false (rand-str 2049) :test.new-exploit-target/map    new-exploit-target-minimal
        false (rand-str 2049) :test.stored-exploit-target/map stored-exploit-target-minimal
        false (rand-str 5000) :test.exploit-target/map        exploit-target-minimal
        false (rand-str 5000) :test.new-exploit-target/map    new-exploit-target-minimal
        false (rand-str 5000) :test.stored-exploit-target/map stored-exploit-target-minimal

        false nil :test.incident/map        incident-minimal
        false nil :test.new-incident/map    new-incident-minimal
        false nil :test.stored-incident/map stored-incident-minimal
        true  ""  :test.incident/map        incident-minimal
        true  ""  :test.new-incident/map    new-incident-minimal
        true  ""  :test.stored-incident/map stored-incident-minimal
        true  (rand-str 100)  :test.incident/map        incident-minimal
        true  (rand-str 100)  :test.new-incident/map    new-incident-minimal
        true  (rand-str 100)  :test.stored-incident/map stored-incident-minimal
        true  (rand-str 2048) :test.incident/map        incident-minimal
        true  (rand-str 2048) :test.new-incident/map    new-incident-minimal
        true  (rand-str 2048) :test.stored-incident/map stored-incident-minimal
        false (rand-str 2049) :test.incident/map        incident-minimal
        false (rand-str 2049) :test.new-incident/map    new-incident-minimal
        false (rand-str 2049) :test.stored-incident/map stored-incident-minimal
        false (rand-str 5000) :test.incident/map        incident-minimal
        false (rand-str 5000) :test.new-incident/map    new-incident-minimal
        false (rand-str 5000) :test.stored-incident/map stored-incident-minimal

        false nil :test.indicator/map        indicator-minimal
        false nil :test.new-indicator/map    new-indicator-minimal
        false nil :test.stored-indicator/map stored-indicator-minimal
        true  ""  :test.indicator/map        indicator-minimal
        true  ""  :test.new-indicator/map    new-indicator-minimal
        true  ""  :test.stored-indicator/map stored-indicator-minimal
        true  (rand-str 100)  :test.indicator/map        indicator-minimal
        true  (rand-str 100)  :test.new-indicator/map    new-indicator-minimal
        true  (rand-str 100)  :test.stored-indicator/map stored-indicator-minimal
        true  (rand-str 2048) :test.indicator/map        indicator-minimal
        true  (rand-str 2048) :test.new-indicator/map    new-indicator-minimal
        true  (rand-str 2048) :test.stored-indicator/map stored-indicator-minimal
        false (rand-str 2049) :test.indicator/map        indicator-minimal
        false (rand-str 2049) :test.new-indicator/map    new-indicator-minimal
        false (rand-str 2049) :test.stored-indicator/map stored-indicator-minimal
        false (rand-str 5000) :test.indicator/map        indicator-minimal
        false (rand-str 5000) :test.new-indicator/map    new-indicator-minimal
        false (rand-str 5000) :test.stored-indicator/map stored-indicator-minimal

        false nil :test.sighting/map        sighting-minimal
        false nil :test.new-sighting/map    new-sighting-minimal
        false nil :test.stored-sighting/map stored-sighting-minimal
        true  ""  :test.sighting/map        sighting-minimal
        true  ""  :test.new-sighting/map    new-sighting-minimal
        true  ""  :test.stored-sighting/map stored-sighting-minimal
        true  (rand-str 100)  :test.sighting/map        sighting-minimal
        true  (rand-str 100)  :test.new-sighting/map    new-sighting-minimal
        true  (rand-str 100)  :test.stored-sighting/map stored-sighting-minimal
        true  (rand-str 2048) :test.sighting/map        sighting-minimal
        true  (rand-str 2048) :test.new-sighting/map    new-sighting-minimal
        true  (rand-str 2048) :test.stored-sighting/map stored-sighting-minimal
        false (rand-str 2049) :test.sighting/map        sighting-minimal
        false (rand-str 2049) :test.new-sighting/map    new-sighting-minimal
        false (rand-str 2049) :test.stored-sighting/map stored-sighting-minimal
        false (rand-str 5000) :test.sighting/map        sighting-minimal
        false (rand-str 5000) :test.new-sighting/map    new-sighting-minimal
        false (rand-str 5000) :test.stored-sighting/map stored-sighting-minimal

        false nil :test.ttp/map        ttp-minimal
        false nil :test.new-ttp/map    new-ttp-minimal
        false nil :test.stored-ttp/map stored-ttp-minimal
        true  ""  :test.ttp/map        ttp-minimal
        true  ""  :test.new-ttp/map    new-ttp-minimal
        true  ""  :test.stored-ttp/map stored-ttp-minimal
        true  (rand-str 100)  :test.ttp/map        ttp-minimal
        true  (rand-str 100)  :test.new-ttp/map    new-ttp-minimal
        true  (rand-str 100)  :test.stored-ttp/map stored-ttp-minimal
        true  (rand-str 2048) :test.ttp/map        ttp-minimal
        true  (rand-str 2048) :test.new-ttp/map    new-ttp-minimal
        true  (rand-str 2048) :test.stored-ttp/map stored-ttp-minimal
        false (rand-str 2049) :test.ttp/map        ttp-minimal
        false (rand-str 2049) :test.new-ttp/map    new-ttp-minimal
        false (rand-str 2049) :test.stored-ttp/map stored-ttp-minimal
        false (rand-str 5000) :test.ttp/map        ttp-minimal
        false (rand-str 5000) :test.new-ttp/map    new-ttp-minimal
        false (rand-str 5000) :test.stored-ttp/map stored-ttp-minimal))

  (testing ":language"
    (are [expected title spec entity]
        (= expected
           (spec/valid? spec
                        (assoc entity
                               :language title)))

        false nil :test.actor/map        actor-minimal
        false nil :test.new-actor/map    new-actor-minimal
        false nil :test.stored-actor/map stored-actor-minimal
        true  ""  :test.actor/map        actor-minimal
        true  ""  :test.new-actor/map    new-actor-minimal
        true  ""  :test.stored-actor/map stored-actor-minimal
        true  (rand-str 100)  :test.actor/map        actor-minimal
        true  (rand-str 100)  :test.new-actor/map    new-actor-minimal
        true  (rand-str 100)  :test.stored-actor/map stored-actor-minimal
        true  (rand-str 1024) :test.actor/map        actor-minimal
        true  (rand-str 1024) :test.new-actor/map    new-actor-minimal
        true  (rand-str 1024) :test.stored-actor/map stored-actor-minimal
        false (rand-str 1025) :test.actor/map        actor-minimal
        false (rand-str 1025) :test.new-actor/map    new-actor-minimal
        false (rand-str 1025) :test.stored-actor/map stored-actor-minimal
        false (rand-str 5000) :test.actor/map        actor-minimal
        false (rand-str 5000) :test.new-actor/map    new-actor-minimal
        false (rand-str 5000) :test.stored-actor/map stored-actor-minimal

        false nil :test.campaign/map        campaign-minimal
        false nil :test.new-campaign/map    new-campaign-minimal
        false nil :test.stored-campaign/map stored-campaign-minimal
        true  ""  :test.campaign/map        campaign-minimal
        true  ""  :test.new-campaign/map    new-campaign-minimal
        true  ""  :test.stored-campaign/map stored-campaign-minimal
        true  (rand-str 100)  :test.campaign/map        campaign-minimal
        true  (rand-str 100)  :test.new-campaign/map    new-campaign-minimal
        true  (rand-str 100)  :test.stored-campaign/map stored-campaign-minimal
        true  (rand-str 1024) :test.campaign/map        campaign-minimal
        true  (rand-str 1024) :test.new-campaign/map    new-campaign-minimal
        true  (rand-str 1024) :test.stored-campaign/map stored-campaign-minimal
        false (rand-str 1025) :test.campaign/map        campaign-minimal
        false (rand-str 1025) :test.new-campaign/map    new-campaign-minimal
        false (rand-str 1025) :test.stored-campaign/map stored-campaign-minimal
        false (rand-str 5000) :test.campaign/map        campaign-minimal
        false (rand-str 5000) :test.new-campaign/map    new-campaign-minimal
        false (rand-str 5000) :test.stored-campaign/map stored-campaign-minimal

        false nil :test.coa/map        coa-minimal
        false nil :test.new-coa/map    new-coa-minimal
        false nil :test.stored-coa/map stored-coa-minimal
        true  ""  :test.coa/map        coa-minimal
        true  ""  :test.new-coa/map    new-coa-minimal
        true  ""  :test.stored-coa/map stored-coa-minimal
        true  (rand-str 100)  :test.coa/map        coa-minimal
        true  (rand-str 100)  :test.new-coa/map    new-coa-minimal
        true  (rand-str 100)  :test.stored-coa/map stored-coa-minimal
        true  (rand-str 1024) :test.coa/map        coa-minimal
        true  (rand-str 1024) :test.new-coa/map    new-coa-minimal
        true  (rand-str 1024) :test.stored-coa/map stored-coa-minimal
        false (rand-str 1025) :test.coa/map        coa-minimal
        false (rand-str 1025) :test.new-coa/map    new-coa-minimal
        false (rand-str 1025) :test.stored-coa/map stored-coa-minimal
        false (rand-str 5000) :test.coa/map        coa-minimal
        false (rand-str 5000) :test.new-coa/map    new-coa-minimal
        false (rand-str 5000) :test.stored-coa/map stored-coa-minimal

        false nil :test.exploit-target/map        exploit-target-minimal
        false nil :test.new-exploit-target/map    new-exploit-target-minimal
        false nil :test.stored-exploit-target/map stored-exploit-target-minimal
        true  ""  :test.exploit-target/map        exploit-target-minimal
        true  ""  :test.new-exploit-target/map    new-exploit-target-minimal
        true  ""  :test.stored-exploit-target/map stored-exploit-target-minimal
        true  (rand-str 100)  :test.exploit-target/map        exploit-target-minimal
        true  (rand-str 100)  :test.new-exploit-target/map    new-exploit-target-minimal
        true  (rand-str 100)  :test.stored-exploit-target/map stored-exploit-target-minimal
        true  (rand-str 1024) :test.exploit-target/map        exploit-target-minimal
        true  (rand-str 1024) :test.new-exploit-target/map    new-exploit-target-minimal
        true  (rand-str 1024) :test.stored-exploit-target/map stored-exploit-target-minimal
        false (rand-str 1025) :test.exploit-target/map        exploit-target-minimal
        false (rand-str 1025) :test.new-exploit-target/map    new-exploit-target-minimal
        false (rand-str 1025) :test.stored-exploit-target/map stored-exploit-target-minimal
        false (rand-str 5000) :test.exploit-target/map        exploit-target-minimal
        false (rand-str 5000) :test.new-exploit-target/map    new-exploit-target-minimal
        false (rand-str 5000) :test.stored-exploit-target/map stored-exploit-target-minimal

        false nil :test.incident/map        incident-minimal
        false nil :test.new-incident/map    new-incident-minimal
        false nil :test.stored-incident/map stored-incident-minimal
        true  ""  :test.incident/map        incident-minimal
        true  ""  :test.new-incident/map    new-incident-minimal
        true  ""  :test.stored-incident/map stored-incident-minimal
        true  (rand-str 100)  :test.incident/map        incident-minimal
        true  (rand-str 100)  :test.new-incident/map    new-incident-minimal
        true  (rand-str 100)  :test.stored-incident/map stored-incident-minimal
        true  (rand-str 1024) :test.incident/map        incident-minimal
        true  (rand-str 1024) :test.new-incident/map    new-incident-minimal
        true  (rand-str 1024) :test.stored-incident/map stored-incident-minimal
        false (rand-str 1025) :test.incident/map        incident-minimal
        false (rand-str 1025) :test.new-incident/map    new-incident-minimal
        false (rand-str 1025) :test.stored-incident/map stored-incident-minimal
        false (rand-str 5000) :test.incident/map        incident-minimal
        false (rand-str 5000) :test.new-incident/map    new-incident-minimal
        false (rand-str 5000) :test.stored-incident/map stored-incident-minimal

        false nil :test.indicator/map        indicator-minimal
        false nil :test.new-indicator/map    new-indicator-minimal
        false nil :test.stored-indicator/map stored-indicator-minimal
        true  ""  :test.indicator/map        indicator-minimal
        true  ""  :test.new-indicator/map    new-indicator-minimal
        true  ""  :test.stored-indicator/map stored-indicator-minimal
        true  (rand-str 100)  :test.indicator/map        indicator-minimal
        true  (rand-str 100)  :test.new-indicator/map    new-indicator-minimal
        true  (rand-str 100)  :test.stored-indicator/map stored-indicator-minimal
        true  (rand-str 1024) :test.indicator/map        indicator-minimal
        true  (rand-str 1024) :test.new-indicator/map    new-indicator-minimal
        true  (rand-str 1024) :test.stored-indicator/map stored-indicator-minimal
        false (rand-str 1025) :test.indicator/map        indicator-minimal
        false (rand-str 1025) :test.new-indicator/map    new-indicator-minimal
        false (rand-str 1025) :test.stored-indicator/map stored-indicator-minimal
        false (rand-str 5000) :test.indicator/map        indicator-minimal
        false (rand-str 5000) :test.new-indicator/map    new-indicator-minimal
        false (rand-str 5000) :test.stored-indicator/map stored-indicator-minimal

        false nil :test.sighting/map        sighting-minimal
        false nil :test.new-sighting/map    new-sighting-minimal
        false nil :test.stored-sighting/map stored-sighting-minimal
        true  ""  :test.sighting/map        sighting-minimal
        true  ""  :test.new-sighting/map    new-sighting-minimal
        true  ""  :test.stored-sighting/map stored-sighting-minimal
        true  (rand-str 100)  :test.sighting/map        sighting-minimal
        true  (rand-str 100)  :test.new-sighting/map    new-sighting-minimal
        true  (rand-str 100)  :test.stored-sighting/map stored-sighting-minimal
        true  (rand-str 1024) :test.sighting/map        sighting-minimal
        true  (rand-str 1024) :test.new-sighting/map    new-sighting-minimal
        true  (rand-str 1024) :test.stored-sighting/map stored-sighting-minimal
        false (rand-str 1025) :test.sighting/map        sighting-minimal
        false (rand-str 1025) :test.new-sighting/map    new-sighting-minimal
        false (rand-str 1025) :test.stored-sighting/map stored-sighting-minimal
        false (rand-str 5000) :test.sighting/map        sighting-minimal
        false (rand-str 5000) :test.new-sighting/map    new-sighting-minimal
        false (rand-str 5000) :test.stored-sighting/map stored-sighting-minimal

        false nil :test.ttp/map        ttp-minimal
        false nil :test.new-ttp/map    new-ttp-minimal
        false nil :test.stored-ttp/map stored-ttp-minimal
        true  ""  :test.ttp/map        ttp-minimal
        true  ""  :test.new-ttp/map    new-ttp-minimal
        true  ""  :test.stored-ttp/map stored-ttp-minimal
        true  (rand-str 100)  :test.ttp/map        ttp-minimal
        true  (rand-str 100)  :test.new-ttp/map    new-ttp-minimal
        true  (rand-str 100)  :test.stored-ttp/map stored-ttp-minimal
        true  (rand-str 1024) :test.ttp/map        ttp-minimal
        true  (rand-str 1024) :test.new-ttp/map    new-ttp-minimal
        true  (rand-str 1024) :test.stored-ttp/map stored-ttp-minimal
        false (rand-str 1025) :test.ttp/map        ttp-minimal
        false (rand-str 1025) :test.new-ttp/map    new-ttp-minimal
        false (rand-str 1025) :test.stored-ttp/map stored-ttp-minimal
        false (rand-str 5000) :test.ttp/map        ttp-minimal
        false (rand-str 5000) :test.new-ttp/map    new-ttp-minimal
        false (rand-str 5000) :test.stored-ttp/map stored-ttp-minimal))

  (testing ":title"
    (are [expected title spec entity]
        (= expected
           (spec/valid? spec
                        (assoc entity
                               :title title)))

        false nil :test.actor/map        actor-minimal
        false nil :test.new-actor/map    new-actor-minimal
        false nil :test.stored-actor/map stored-actor-minimal
        true  ""  :test.actor/map        actor-minimal
        true  ""  :test.new-actor/map    new-actor-minimal
        true  ""  :test.stored-actor/map stored-actor-minimal
        true  (rand-str 100)  :test.actor/map        actor-minimal
        true  (rand-str 100)  :test.new-actor/map    new-actor-minimal
        true  (rand-str 100)  :test.stored-actor/map stored-actor-minimal
        true  (rand-str 1024) :test.actor/map        actor-minimal
        true  (rand-str 1024) :test.new-actor/map    new-actor-minimal
        true  (rand-str 1024) :test.stored-actor/map stored-actor-minimal
        false (rand-str 1025) :test.actor/map        actor-minimal
        false (rand-str 1025) :test.new-actor/map    new-actor-minimal
        false (rand-str 1025) :test.stored-actor/map stored-actor-minimal
        false (rand-str 5000) :test.actor/map        actor-minimal
        false (rand-str 5000) :test.new-actor/map    new-actor-minimal
        false (rand-str 5000) :test.stored-actor/map stored-actor-minimal

        false nil :test.campaign/map        campaign-minimal
        false nil :test.new-campaign/map    new-campaign-minimal
        false nil :test.stored-campaign/map stored-campaign-minimal
        true  ""  :test.campaign/map        campaign-minimal
        true  ""  :test.new-campaign/map    new-campaign-minimal
        true  ""  :test.stored-campaign/map stored-campaign-minimal
        true  (rand-str 100)  :test.campaign/map        campaign-minimal
        true  (rand-str 100)  :test.new-campaign/map    new-campaign-minimal
        true  (rand-str 100)  :test.stored-campaign/map stored-campaign-minimal
        true  (rand-str 1024) :test.campaign/map        campaign-minimal
        true  (rand-str 1024) :test.new-campaign/map    new-campaign-minimal
        true  (rand-str 1024) :test.stored-campaign/map stored-campaign-minimal
        false (rand-str 1025) :test.campaign/map        campaign-minimal
        false (rand-str 1025) :test.new-campaign/map    new-campaign-minimal
        false (rand-str 1025) :test.stored-campaign/map stored-campaign-minimal
        false (rand-str 5000) :test.campaign/map        campaign-minimal
        false (rand-str 5000) :test.new-campaign/map    new-campaign-minimal
        false (rand-str 5000) :test.stored-campaign/map stored-campaign-minimal

        false nil :test.coa/map        coa-minimal
        false nil :test.new-coa/map    new-coa-minimal
        false nil :test.stored-coa/map stored-coa-minimal
        true  ""  :test.coa/map        coa-minimal
        true  ""  :test.new-coa/map    new-coa-minimal
        true  ""  :test.stored-coa/map stored-coa-minimal
        true  (rand-str 100)  :test.coa/map        coa-minimal
        true  (rand-str 100)  :test.new-coa/map    new-coa-minimal
        true  (rand-str 100)  :test.stored-coa/map stored-coa-minimal
        true  (rand-str 1024) :test.coa/map        coa-minimal
        true  (rand-str 1024) :test.new-coa/map    new-coa-minimal
        true  (rand-str 1024) :test.stored-coa/map stored-coa-minimal
        false (rand-str 1025) :test.coa/map        coa-minimal
        false (rand-str 1025) :test.new-coa/map    new-coa-minimal
        false (rand-str 1025) :test.stored-coa/map stored-coa-minimal
        false (rand-str 5000) :test.coa/map        coa-minimal
        false (rand-str 5000) :test.new-coa/map    new-coa-minimal
        false (rand-str 5000) :test.stored-coa/map stored-coa-minimal

        false nil :test.exploit-target/map        exploit-target-minimal
        false nil :test.new-exploit-target/map    new-exploit-target-minimal
        false nil :test.stored-exploit-target/map stored-exploit-target-minimal
        true  ""  :test.exploit-target/map        exploit-target-minimal
        true  ""  :test.new-exploit-target/map    new-exploit-target-minimal
        true  ""  :test.stored-exploit-target/map stored-exploit-target-minimal
        true  (rand-str 100)  :test.exploit-target/map        exploit-target-minimal
        true  (rand-str 100)  :test.new-exploit-target/map    new-exploit-target-minimal
        true  (rand-str 100)  :test.stored-exploit-target/map stored-exploit-target-minimal
        true  (rand-str 1024) :test.exploit-target/map        exploit-target-minimal
        true  (rand-str 1024) :test.new-exploit-target/map    new-exploit-target-minimal
        true  (rand-str 1024) :test.stored-exploit-target/map stored-exploit-target-minimal
        false (rand-str 1025) :test.exploit-target/map        exploit-target-minimal
        false (rand-str 1025) :test.new-exploit-target/map    new-exploit-target-minimal
        false (rand-str 1025) :test.stored-exploit-target/map stored-exploit-target-minimal
        false (rand-str 5000) :test.exploit-target/map        exploit-target-minimal
        false (rand-str 5000) :test.new-exploit-target/map    new-exploit-target-minimal
        false (rand-str 5000) :test.stored-exploit-target/map stored-exploit-target-minimal

        false nil :test.incident/map        incident-minimal
        false nil :test.new-incident/map    new-incident-minimal
        false nil :test.stored-incident/map stored-incident-minimal
        true  ""  :test.incident/map        incident-minimal
        true  ""  :test.new-incident/map    new-incident-minimal
        true  ""  :test.stored-incident/map stored-incident-minimal
        true  (rand-str 100)  :test.incident/map        incident-minimal
        true  (rand-str 100)  :test.new-incident/map    new-incident-minimal
        true  (rand-str 100)  :test.stored-incident/map stored-incident-minimal
        true  (rand-str 1024) :test.incident/map        incident-minimal
        true  (rand-str 1024) :test.new-incident/map    new-incident-minimal
        true  (rand-str 1024) :test.stored-incident/map stored-incident-minimal
        false (rand-str 1025) :test.incident/map        incident-minimal
        false (rand-str 1025) :test.new-incident/map    new-incident-minimal
        false (rand-str 1025) :test.stored-incident/map stored-incident-minimal
        false (rand-str 5000) :test.incident/map        incident-minimal
        false (rand-str 5000) :test.new-incident/map    new-incident-minimal
        false (rand-str 5000) :test.stored-incident/map stored-incident-minimal

        false nil :test.indicator/map        indicator-minimal
        false nil :test.new-indicator/map    new-indicator-minimal
        false nil :test.stored-indicator/map stored-indicator-minimal
        true  ""  :test.indicator/map        indicator-minimal
        true  ""  :test.new-indicator/map    new-indicator-minimal
        true  ""  :test.stored-indicator/map stored-indicator-minimal
        true  (rand-str 100)  :test.indicator/map        indicator-minimal
        true  (rand-str 100)  :test.new-indicator/map    new-indicator-minimal
        true  (rand-str 100)  :test.stored-indicator/map stored-indicator-minimal
        true  (rand-str 1024) :test.indicator/map        indicator-minimal
        true  (rand-str 1024) :test.new-indicator/map    new-indicator-minimal
        true  (rand-str 1024) :test.stored-indicator/map stored-indicator-minimal
        false (rand-str 1025) :test.indicator/map        indicator-minimal
        false (rand-str 1025) :test.new-indicator/map    new-indicator-minimal
        false (rand-str 1025) :test.stored-indicator/map stored-indicator-minimal
        false (rand-str 5000) :test.indicator/map        indicator-minimal
        false (rand-str 5000) :test.new-indicator/map    new-indicator-minimal
        false (rand-str 5000) :test.stored-indicator/map stored-indicator-minimal

        false nil :test.sighting/map        sighting-minimal
        false nil :test.new-sighting/map    new-sighting-minimal
        false nil :test.stored-sighting/map stored-sighting-minimal
        true  ""  :test.sighting/map        sighting-minimal
        true  ""  :test.new-sighting/map    new-sighting-minimal
        true  ""  :test.stored-sighting/map stored-sighting-minimal
        true  (rand-str 100)  :test.sighting/map        sighting-minimal
        true  (rand-str 100)  :test.new-sighting/map    new-sighting-minimal
        true  (rand-str 100)  :test.stored-sighting/map stored-sighting-minimal
        true  (rand-str 1024) :test.sighting/map        sighting-minimal
        true  (rand-str 1024) :test.new-sighting/map    new-sighting-minimal
        true  (rand-str 1024) :test.stored-sighting/map stored-sighting-minimal
        false (rand-str 1025) :test.sighting/map        sighting-minimal
        false (rand-str 1025) :test.new-sighting/map    new-sighting-minimal
        false (rand-str 1025) :test.stored-sighting/map stored-sighting-minimal
        false (rand-str 5000) :test.sighting/map        sighting-minimal
        false (rand-str 5000) :test.new-sighting/map    new-sighting-minimal
        false (rand-str 5000) :test.stored-sighting/map stored-sighting-minimal

        false nil :test.ttp/map        ttp-minimal
        false nil :test.new-ttp/map    new-ttp-minimal
        false nil :test.stored-ttp/map stored-ttp-minimal
        true  ""  :test.ttp/map        ttp-minimal
        true  ""  :test.new-ttp/map    new-ttp-minimal
        true  ""  :test.stored-ttp/map stored-ttp-minimal
        true  (rand-str 100)  :test.ttp/map        ttp-minimal
        true  (rand-str 100)  :test.new-ttp/map    new-ttp-minimal
        true  (rand-str 100)  :test.stored-ttp/map stored-ttp-minimal
        true  (rand-str 1024) :test.ttp/map        ttp-minimal
        true  (rand-str 1024) :test.new-ttp/map    new-ttp-minimal
        true  (rand-str 1024) :test.stored-ttp/map stored-ttp-minimal
        false (rand-str 1025) :test.ttp/map        ttp-minimal
        false (rand-str 1025) :test.new-ttp/map    new-ttp-minimal
        false (rand-str 1025) :test.stored-ttp/map stored-ttp-minimal
        false (rand-str 5000) :test.ttp/map        ttp-minimal
        false (rand-str 5000) :test.new-ttp/map    new-ttp-minimal
        false (rand-str 5000) :test.stored-ttp/map stored-ttp-minimal))

  (testing ":source"
    (are [expected uri spec entity]
        (= expected
           (spec/valid? spec
                        (assoc entity
                               :source uri)))

        false nil :test.actor/map        actor-minimal
        false nil :test.new-actor/map    new-actor-minimal
        false nil :test.stored-actor/map stored-actor-minimal
        true  ""  :test.actor/map        actor-minimal
        true  ""  :test.new-actor/map    new-actor-minimal
        true  ""  :test.stored-actor/map stored-actor-minimal
        true  (rand-str 100)  :test.actor/map        actor-minimal
        true  (rand-str 100)  :test.new-actor/map    new-actor-minimal
        true  (rand-str 100)  :test.stored-actor/map stored-actor-minimal
        true  (rand-str 2048) :test.actor/map        actor-minimal
        true  (rand-str 2048) :test.new-actor/map    new-actor-minimal
        true  (rand-str 2048) :test.stored-actor/map stored-actor-minimal
        false (rand-str 2049) :test.actor/map        actor-minimal
        false (rand-str 2049) :test.new-actor/map    new-actor-minimal
        false (rand-str 2049) :test.stored-actor/map stored-actor-minimal
        false (rand-str 5000) :test.actor/map        actor-minimal
        false (rand-str 5000) :test.new-actor/map    new-actor-minimal
        false (rand-str 5000) :test.stored-actor/map stored-actor-minimal

        false nil :test.campaign/map        campaign-minimal
        false nil :test.new-campaign/map    new-campaign-minimal
        false nil :test.stored-campaign/map stored-campaign-minimal
        true  ""  :test.campaign/map        campaign-minimal
        true  ""  :test.new-campaign/map    new-campaign-minimal
        true  ""  :test.stored-campaign/map stored-campaign-minimal
        true  (rand-str 100)  :test.campaign/map        campaign-minimal
        true  (rand-str 100)  :test.new-campaign/map    new-campaign-minimal
        true  (rand-str 100)  :test.stored-campaign/map stored-campaign-minimal
        true  (rand-str 2048) :test.campaign/map        campaign-minimal
        true  (rand-str 2048) :test.new-campaign/map    new-campaign-minimal
        true  (rand-str 2048) :test.stored-campaign/map stored-campaign-minimal
        false (rand-str 2049) :test.campaign/map        campaign-minimal
        false (rand-str 2049) :test.new-campaign/map    new-campaign-minimal
        false (rand-str 2049) :test.stored-campaign/map stored-campaign-minimal
        false (rand-str 5000) :test.campaign/map        campaign-minimal
        false (rand-str 5000) :test.new-campaign/map    new-campaign-minimal
        false (rand-str 5000) :test.stored-campaign/map stored-campaign-minimal

        false nil :test.coa/map        coa-minimal
        false nil :test.new-coa/map    new-coa-minimal
        false nil :test.stored-coa/map stored-coa-minimal
        true  ""  :test.coa/map        coa-minimal
        true  ""  :test.new-coa/map    new-coa-minimal
        true  ""  :test.stored-coa/map stored-coa-minimal
        true  (rand-str 100)  :test.coa/map        coa-minimal
        true  (rand-str 100)  :test.new-coa/map    new-coa-minimal
        true  (rand-str 100)  :test.stored-coa/map stored-coa-minimal
        true  (rand-str 2048) :test.coa/map        coa-minimal
        true  (rand-str 2048) :test.new-coa/map    new-coa-minimal
        true  (rand-str 2048) :test.stored-coa/map stored-coa-minimal
        false (rand-str 2049) :test.coa/map        coa-minimal
        false (rand-str 2049) :test.new-coa/map    new-coa-minimal
        false (rand-str 2049) :test.stored-coa/map stored-coa-minimal
        false (rand-str 5000) :test.coa/map        coa-minimal
        false (rand-str 5000) :test.new-coa/map    new-coa-minimal
        false (rand-str 5000) :test.stored-coa/map stored-coa-minimal

        false nil :test.exploit-target/map        exploit-target-minimal
        false nil :test.new-exploit-target/map    new-exploit-target-minimal
        false nil :test.stored-exploit-target/map stored-exploit-target-minimal
        true  ""  :test.exploit-target/map        exploit-target-minimal
        true  ""  :test.new-exploit-target/map    new-exploit-target-minimal
        true  ""  :test.stored-exploit-target/map stored-exploit-target-minimal
        true  (rand-str 100)  :test.exploit-target/map        exploit-target-minimal
        true  (rand-str 100)  :test.new-exploit-target/map    new-exploit-target-minimal
        true  (rand-str 100)  :test.stored-exploit-target/map stored-exploit-target-minimal
        true  (rand-str 2048) :test.exploit-target/map        exploit-target-minimal
        true  (rand-str 2048) :test.new-exploit-target/map    new-exploit-target-minimal
        true  (rand-str 2048) :test.stored-exploit-target/map stored-exploit-target-minimal
        false (rand-str 2049) :test.exploit-target/map        exploit-target-minimal
        false (rand-str 2049) :test.new-exploit-target/map    new-exploit-target-minimal
        false (rand-str 2049) :test.stored-exploit-target/map stored-exploit-target-minimal
        false (rand-str 5000) :test.exploit-target/map        exploit-target-minimal
        false (rand-str 5000) :test.new-exploit-target/map    new-exploit-target-minimal
        false (rand-str 5000) :test.stored-exploit-target/map stored-exploit-target-minimal

        false nil :test.incident/map        incident-minimal
        false nil :test.new-incident/map    new-incident-minimal
        false nil :test.stored-incident/map stored-incident-minimal
        true  ""  :test.incident/map        incident-minimal
        true  ""  :test.new-incident/map    new-incident-minimal
        true  ""  :test.stored-incident/map stored-incident-minimal
        true  (rand-str 100)  :test.incident/map        incident-minimal
        true  (rand-str 100)  :test.new-incident/map    new-incident-minimal
        true  (rand-str 100)  :test.stored-incident/map stored-incident-minimal
        true  (rand-str 2048) :test.incident/map        incident-minimal
        true  (rand-str 2048) :test.new-incident/map    new-incident-minimal
        true  (rand-str 2048) :test.stored-incident/map stored-incident-minimal
        false (rand-str 2049) :test.incident/map        incident-minimal
        false (rand-str 2049) :test.new-incident/map    new-incident-minimal
        false (rand-str 2049) :test.stored-incident/map stored-incident-minimal
        false (rand-str 5000) :test.incident/map        incident-minimal
        false (rand-str 5000) :test.new-incident/map    new-incident-minimal
        false (rand-str 5000) :test.stored-incident/map stored-incident-minimal

        false nil :test.indicator/map        indicator-minimal
        false nil :test.new-indicator/map    new-indicator-minimal
        false nil :test.stored-indicator/map stored-indicator-minimal
        true  ""  :test.indicator/map        indicator-minimal
        true  ""  :test.new-indicator/map    new-indicator-minimal
        true  ""  :test.stored-indicator/map stored-indicator-minimal
        true  (rand-str 100)  :test.indicator/map        indicator-minimal
        true  (rand-str 100)  :test.new-indicator/map    new-indicator-minimal
        true  (rand-str 100)  :test.stored-indicator/map stored-indicator-minimal
        true  (rand-str 2048) :test.indicator/map        indicator-minimal
        true  (rand-str 2048) :test.new-indicator/map    new-indicator-minimal
        true  (rand-str 2048) :test.stored-indicator/map stored-indicator-minimal
        false (rand-str 2049) :test.indicator/map        indicator-minimal
        false (rand-str 2049) :test.new-indicator/map    new-indicator-minimal
        false (rand-str 2049) :test.stored-indicator/map stored-indicator-minimal
        false (rand-str 5000) :test.indicator/map        indicator-minimal
        false (rand-str 5000) :test.new-indicator/map    new-indicator-minimal
        false (rand-str 5000) :test.stored-indicator/map stored-indicator-minimal

        false nil :test.judgement/map        judgement-minimal
        false nil :test.new-judgement/map    new-judgement-minimal
        false nil :test.stored-judgement/map stored-judgement-minimal
        true  ""  :test.judgement/map        judgement-minimal
        true  ""  :test.new-judgement/map    new-judgement-minimal
        true  ""  :test.stored-judgement/map stored-judgement-minimal
        true  (rand-str 100)  :test.judgement/map        judgement-minimal
        true  (rand-str 100)  :test.new-judgement/map    new-judgement-minimal
        true  (rand-str 100)  :test.stored-judgement/map stored-judgement-minimal
        true  (rand-str 2048) :test.judgement/map        judgement-minimal
        true  (rand-str 2048) :test.new-judgement/map    new-judgement-minimal
        true  (rand-str 2048) :test.stored-judgement/map stored-judgement-minimal
        false (rand-str 2049) :test.judgement/map        judgement-minimal
        false (rand-str 2049) :test.new-judgement/map    new-judgement-minimal
        false (rand-str 2049) :test.stored-judgement/map stored-judgement-minimal
        false (rand-str 5000) :test.judgement/map        judgement-minimal
        false (rand-str 5000) :test.new-judgement/map    new-judgement-minimal
        false (rand-str 5000) :test.stored-judgement/map stored-judgement-minimal

        false nil :test.sighting/map        sighting-minimal
        false nil :test.new-sighting/map    new-sighting-minimal
        false nil :test.stored-sighting/map stored-sighting-minimal
        true  ""  :test.sighting/map        sighting-minimal
        true  ""  :test.new-sighting/map    new-sighting-minimal
        true  ""  :test.stored-sighting/map stored-sighting-minimal
        true  (rand-str 100)  :test.sighting/map        sighting-minimal
        true  (rand-str 100)  :test.new-sighting/map    new-sighting-minimal
        true  (rand-str 100)  :test.stored-sighting/map stored-sighting-minimal
        true  (rand-str 2048) :test.sighting/map        sighting-minimal
        true  (rand-str 2048) :test.new-sighting/map    new-sighting-minimal
        true  (rand-str 2048) :test.stored-sighting/map stored-sighting-minimal
        false (rand-str 2049) :test.sighting/map        sighting-minimal
        false (rand-str 2049) :test.new-sighting/map    new-sighting-minimal
        false (rand-str 2049) :test.stored-sighting/map stored-sighting-minimal
        false (rand-str 5000) :test.sighting/map        sighting-minimal
        false (rand-str 5000) :test.new-sighting/map    new-sighting-minimal
        false (rand-str 5000) :test.stored-sighting/map stored-sighting-minimal

        false nil :test.ttp/map        ttp-minimal
        false nil :test.new-ttp/map    new-ttp-minimal
        false nil :test.stored-ttp/map stored-ttp-minimal
        true  ""  :test.ttp/map        ttp-minimal
        true  ""  :test.new-ttp/map    new-ttp-minimal
        true  ""  :test.stored-ttp/map stored-ttp-minimal
        true  (rand-str 100)  :test.ttp/map        ttp-minimal
        true  (rand-str 100)  :test.new-ttp/map    new-ttp-minimal
        true  (rand-str 100)  :test.stored-ttp/map stored-ttp-minimal
        true  (rand-str 2048) :test.ttp/map        ttp-minimal
        true  (rand-str 2048) :test.new-ttp/map    new-ttp-minimal
        true  (rand-str 2048) :test.stored-ttp/map stored-ttp-minimal
        false (rand-str 2049) :test.ttp/map        ttp-minimal
        false (rand-str 2049) :test.new-ttp/map    new-ttp-minimal
        false (rand-str 2049) :test.stored-ttp/map stored-ttp-minimal
        false (rand-str 5000) :test.ttp/map        ttp-minimal
        false (rand-str 5000) :test.new-ttp/map    new-ttp-minimal
        false (rand-str 5000) :test.stored-ttp/map stored-ttp-minimal))

  (testing ":source_uri"
    (are [expected uri spec entity]
        (= expected
           (spec/valid? spec
                        (assoc entity
                               :source_uri uri)))

        false nil :test.actor/map        actor-minimal
        false nil :test.new-actor/map    new-actor-minimal
        false nil :test.stored-actor/map stored-actor-minimal
        true  ""  :test.actor/map        actor-minimal
        true  ""  :test.new-actor/map    new-actor-minimal
        true  ""  :test.stored-actor/map stored-actor-minimal
        true  (rand-str 100)  :test.actor/map        actor-minimal
        true  (rand-str 100)  :test.new-actor/map    new-actor-minimal
        true  (rand-str 100)  :test.stored-actor/map stored-actor-minimal
        true  (rand-str 2048) :test.actor/map        actor-minimal
        true  (rand-str 2048) :test.new-actor/map    new-actor-minimal
        true  (rand-str 2048) :test.stored-actor/map stored-actor-minimal
        false (rand-str 2049) :test.actor/map        actor-minimal
        false (rand-str 2049) :test.new-actor/map    new-actor-minimal
        false (rand-str 2049) :test.stored-actor/map stored-actor-minimal
        false (rand-str 5000) :test.actor/map        actor-minimal
        false (rand-str 5000) :test.new-actor/map    new-actor-minimal
        false (rand-str 5000) :test.stored-actor/map stored-actor-minimal

        false nil :test.campaign/map        campaign-minimal
        false nil :test.new-campaign/map    new-campaign-minimal
        false nil :test.stored-campaign/map stored-campaign-minimal
        true  ""  :test.campaign/map        campaign-minimal
        true  ""  :test.new-campaign/map    new-campaign-minimal
        true  ""  :test.stored-campaign/map stored-campaign-minimal
        true  (rand-str 100)  :test.campaign/map        campaign-minimal
        true  (rand-str 100)  :test.new-campaign/map    new-campaign-minimal
        true  (rand-str 100)  :test.stored-campaign/map stored-campaign-minimal
        true  (rand-str 2048) :test.campaign/map        campaign-minimal
        true  (rand-str 2048) :test.new-campaign/map    new-campaign-minimal
        true  (rand-str 2048) :test.stored-campaign/map stored-campaign-minimal
        false (rand-str 2049) :test.campaign/map        campaign-minimal
        false (rand-str 2049) :test.new-campaign/map    new-campaign-minimal
        false (rand-str 2049) :test.stored-campaign/map stored-campaign-minimal
        false (rand-str 5000) :test.campaign/map        campaign-minimal
        false (rand-str 5000) :test.new-campaign/map    new-campaign-minimal
        false (rand-str 5000) :test.stored-campaign/map stored-campaign-minimal

        false nil :test.coa/map        coa-minimal
        false nil :test.new-coa/map    new-coa-minimal
        false nil :test.stored-coa/map stored-coa-minimal
        true  ""  :test.coa/map        coa-minimal
        true  ""  :test.new-coa/map    new-coa-minimal
        true  ""  :test.stored-coa/map stored-coa-minimal
        true  (rand-str 100)  :test.coa/map        coa-minimal
        true  (rand-str 100)  :test.new-coa/map    new-coa-minimal
        true  (rand-str 100)  :test.stored-coa/map stored-coa-minimal
        true  (rand-str 2048) :test.coa/map        coa-minimal
        true  (rand-str 2048) :test.new-coa/map    new-coa-minimal
        true  (rand-str 2048) :test.stored-coa/map stored-coa-minimal
        false (rand-str 2049) :test.coa/map        coa-minimal
        false (rand-str 2049) :test.new-coa/map    new-coa-minimal
        false (rand-str 2049) :test.stored-coa/map stored-coa-minimal
        false (rand-str 5000) :test.coa/map        coa-minimal
        false (rand-str 5000) :test.new-coa/map    new-coa-minimal
        false (rand-str 5000) :test.stored-coa/map stored-coa-minimal

        false nil :test.exploit-target/map        exploit-target-minimal
        false nil :test.new-exploit-target/map    new-exploit-target-minimal
        false nil :test.stored-exploit-target/map stored-exploit-target-minimal
        true  ""  :test.exploit-target/map        exploit-target-minimal
        true  ""  :test.new-exploit-target/map    new-exploit-target-minimal
        true  ""  :test.stored-exploit-target/map stored-exploit-target-minimal
        true  (rand-str 100)  :test.exploit-target/map        exploit-target-minimal
        true  (rand-str 100)  :test.new-exploit-target/map    new-exploit-target-minimal
        true  (rand-str 100)  :test.stored-exploit-target/map stored-exploit-target-minimal
        true  (rand-str 2048) :test.exploit-target/map        exploit-target-minimal
        true  (rand-str 2048) :test.new-exploit-target/map    new-exploit-target-minimal
        true  (rand-str 2048) :test.stored-exploit-target/map stored-exploit-target-minimal
        false (rand-str 2049) :test.exploit-target/map        exploit-target-minimal
        false (rand-str 2049) :test.new-exploit-target/map    new-exploit-target-minimal
        false (rand-str 2049) :test.stored-exploit-target/map stored-exploit-target-minimal
        false (rand-str 5000) :test.exploit-target/map        exploit-target-minimal
        false (rand-str 5000) :test.new-exploit-target/map    new-exploit-target-minimal
        false (rand-str 5000) :test.stored-exploit-target/map stored-exploit-target-minimal

        false nil :test.incident/map        incident-minimal
        false nil :test.new-incident/map    new-incident-minimal
        false nil :test.stored-incident/map stored-incident-minimal
        true  ""  :test.incident/map        incident-minimal
        true  ""  :test.new-incident/map    new-incident-minimal
        true  ""  :test.stored-incident/map stored-incident-minimal
        true  (rand-str 100)  :test.incident/map        incident-minimal
        true  (rand-str 100)  :test.new-incident/map    new-incident-minimal
        true  (rand-str 100)  :test.stored-incident/map stored-incident-minimal
        true  (rand-str 2048) :test.incident/map        incident-minimal
        true  (rand-str 2048) :test.new-incident/map    new-incident-minimal
        true  (rand-str 2048) :test.stored-incident/map stored-incident-minimal
        false (rand-str 2049) :test.incident/map        incident-minimal
        false (rand-str 2049) :test.new-incident/map    new-incident-minimal
        false (rand-str 2049) :test.stored-incident/map stored-incident-minimal
        false (rand-str 5000) :test.incident/map        incident-minimal
        false (rand-str 5000) :test.new-incident/map    new-incident-minimal
        false (rand-str 5000) :test.stored-incident/map stored-incident-minimal

        false nil :test.indicator/map        indicator-minimal
        false nil :test.new-indicator/map    new-indicator-minimal
        false nil :test.stored-indicator/map stored-indicator-minimal
        true  ""  :test.indicator/map        indicator-minimal
        true  ""  :test.new-indicator/map    new-indicator-minimal
        true  ""  :test.stored-indicator/map stored-indicator-minimal
        true  (rand-str 100)  :test.indicator/map        indicator-minimal
        true  (rand-str 100)  :test.new-indicator/map    new-indicator-minimal
        true  (rand-str 100)  :test.stored-indicator/map stored-indicator-minimal
        true  (rand-str 2048) :test.indicator/map        indicator-minimal
        true  (rand-str 2048) :test.new-indicator/map    new-indicator-minimal
        true  (rand-str 2048) :test.stored-indicator/map stored-indicator-minimal
        false (rand-str 2049) :test.indicator/map        indicator-minimal
        false (rand-str 2049) :test.new-indicator/map    new-indicator-minimal
        false (rand-str 2049) :test.stored-indicator/map stored-indicator-minimal
        false (rand-str 5000) :test.indicator/map        indicator-minimal
        false (rand-str 5000) :test.new-indicator/map    new-indicator-minimal
        false (rand-str 5000) :test.stored-indicator/map stored-indicator-minimal

        false nil :test.judgement/map        judgement-minimal
        false nil :test.new-judgement/map    new-judgement-minimal
        false nil :test.stored-judgement/map stored-judgement-minimal
        true  ""  :test.judgement/map        judgement-minimal
        true  ""  :test.new-judgement/map    new-judgement-minimal
        true  ""  :test.stored-judgement/map stored-judgement-minimal
        true  (rand-str 100)  :test.judgement/map        judgement-minimal
        true  (rand-str 100)  :test.new-judgement/map    new-judgement-minimal
        true  (rand-str 100)  :test.stored-judgement/map stored-judgement-minimal
        true  (rand-str 2048) :test.judgement/map        judgement-minimal
        true  (rand-str 2048) :test.new-judgement/map    new-judgement-minimal
        true  (rand-str 2048) :test.stored-judgement/map stored-judgement-minimal
        false (rand-str 2049) :test.judgement/map        judgement-minimal
        false (rand-str 2049) :test.new-judgement/map    new-judgement-minimal
        false (rand-str 2049) :test.stored-judgement/map stored-judgement-minimal
        false (rand-str 5000) :test.judgement/map        judgement-minimal
        false (rand-str 5000) :test.new-judgement/map    new-judgement-minimal
        false (rand-str 5000) :test.stored-judgement/map stored-judgement-minimal

        false nil :test.sighting/map        sighting-minimal
        false nil :test.new-sighting/map    new-sighting-minimal
        false nil :test.stored-sighting/map stored-sighting-minimal
        true  ""  :test.sighting/map        sighting-minimal
        true  ""  :test.new-sighting/map    new-sighting-minimal
        true  ""  :test.stored-sighting/map stored-sighting-minimal
        true  (rand-str 100)  :test.sighting/map        sighting-minimal
        true  (rand-str 100)  :test.new-sighting/map    new-sighting-minimal
        true  (rand-str 100)  :test.stored-sighting/map stored-sighting-minimal
        true  (rand-str 2048) :test.sighting/map        sighting-minimal
        true  (rand-str 2048) :test.new-sighting/map    new-sighting-minimal
        true  (rand-str 2048) :test.stored-sighting/map stored-sighting-minimal
        false (rand-str 2049) :test.sighting/map        sighting-minimal
        false (rand-str 2049) :test.new-sighting/map    new-sighting-minimal
        false (rand-str 2049) :test.stored-sighting/map stored-sighting-minimal
        false (rand-str 5000) :test.sighting/map        sighting-minimal
        false (rand-str 5000) :test.new-sighting/map    new-sighting-minimal
        false (rand-str 5000) :test.stored-sighting/map stored-sighting-minimal

        false nil :test.ttp/map        ttp-minimal
        false nil :test.new-ttp/map    new-ttp-minimal
        false nil :test.stored-ttp/map stored-ttp-minimal
        true  ""  :test.ttp/map        ttp-minimal
        true  ""  :test.new-ttp/map    new-ttp-minimal
        true  ""  :test.stored-ttp/map stored-ttp-minimal
        true  (rand-str 100)  :test.ttp/map        ttp-minimal
        true  (rand-str 100)  :test.new-ttp/map    new-ttp-minimal
        true  (rand-str 100)  :test.stored-ttp/map stored-ttp-minimal
        true  (rand-str 2048) :test.ttp/map        ttp-minimal
        true  (rand-str 2048) :test.new-ttp/map    new-ttp-minimal
        true  (rand-str 2048) :test.stored-ttp/map stored-ttp-minimal
        false (rand-str 2049) :test.ttp/map        ttp-minimal
        false (rand-str 2049) :test.new-ttp/map    new-ttp-minimal
        false (rand-str 2049) :test.stored-ttp/map stored-ttp-minimal
        false (rand-str 5000) :test.ttp/map        ttp-minimal
        false (rand-str 5000) :test.new-ttp/map    new-ttp-minimal
        false (rand-str 5000) :test.stored-ttp/map stored-ttp-minimal))

  (testing "Actor"
    (testing ":planning_and_operational_support"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc entity
                                 :planning_and_operational_support value)))

          false nil :test.actor/map        actor-minimal
          false nil :test.new-actor/map    new-actor-minimal
          false nil :test.stored-actor/map stored-actor-minimal
          true  ""  :test.actor/map        actor-minimal
          true  ""  :test.new-actor/map    new-actor-minimal
          true  ""  :test.stored-actor/map stored-actor-minimal
          true  (rand-str 100)  :test.actor/map        actor-minimal
          true  (rand-str 100)  :test.new-actor/map    new-actor-minimal
          true  (rand-str 100)  :test.stored-actor/map stored-actor-minimal
          true  (rand-str 1000) :test.actor/map        actor-minimal
          true  (rand-str 1000) :test.new-actor/map    new-actor-minimal
          true  (rand-str 1000) :test.stored-actor/map stored-actor-minimal
          true  (rand-str 5000) :test.actor/map        actor-minimal
          true  (rand-str 5000) :test.new-actor/map    new-actor-minimal
          true  (rand-str 5000) :test.stored-actor/map stored-actor-minimal
          false (rand-str 5001) :test.actor/map        actor-minimal
          false (rand-str 5001) :test.new-actor/map    new-actor-minimal
          false (rand-str 5001) :test.stored-actor/map stored-actor-minimal
          false (rand-str 10000) :test.actor/map        actor-minimal
          false (rand-str 10000) :test.new-actor/map    new-actor-minimal
          false (rand-str 10000) :test.stored-actor/map stored-actor-minimal)))

  (testing "Campaign"
    (testing ":campaign_type"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc entity
                                 :campaign_type value)))

          false nil :test.campaign/map        campaign-minimal
          false nil :test.new-campaign/map    new-campaign-minimal
          false nil :test.stored-campaign/map stored-campaign-minimal
          true  ""  :test.campaign/map        campaign-minimal
          true  ""  :test.new-campaign/map    new-campaign-minimal
          true  ""  :test.stored-campaign/map stored-campaign-minimal
          true  (rand-str 100)  :test.campaign/map        campaign-minimal
          true  (rand-str 100)  :test.new-campaign/map    new-campaign-minimal
          true  (rand-str 100)  :test.stored-campaign/map stored-campaign-minimal
          true  (rand-str 1024) :test.campaign/map        campaign-minimal
          true  (rand-str 1024) :test.new-campaign/map    new-campaign-minimal
          true  (rand-str 1024) :test.stored-campaign/map stored-campaign-minimal
          false (rand-str 1025) :test.campaign/map        campaign-minimal
          false (rand-str 1025) :test.new-campaign/map    new-campaign-minimal
          false (rand-str 1025) :test.stored-campaign/map stored-campaign-minimal
          false (rand-str 5000) :test.campaign/map        campaign-minimal
          false (rand-str 5000) :test.new-campaign/map    new-campaign-minimal
          false (rand-str 5000) :test.stored-campaign/map stored-campaign-minimal))

    (testing ":names"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc entity
                                 :names value)))
          false nil :test.campaign/map        campaign-minimal
          false nil :test.new-campaign/map    new-campaign-minimal
          false nil :test.stored-campaign/map stored-campaign-minimal
          false [nil] :test.campaign/map        campaign-minimal
          false [nil] :test.new-campaign/map    new-campaign-minimal
          false [nil] :test.stored-campaign/map stored-campaign-minimal
          true [""] :test.campaign/map        campaign-minimal
          true [""] :test.new-campaign/map    new-campaign-minimal
          true [""] :test.stored-campaign/map stored-campaign-minimal
          true [(rand-str 100)] :test.campaign/map          campaign-minimal
          true [(rand-str 100)] :test.new-campaign/map      new-campaign-minimal
          true [(rand-str 100)] :test.stored-campaign/map   stored-campaign-minimal
          true [(rand-str 1024)] :test.campaign/map         campaign-minimal
          true [(rand-str 1024)] :test.new-campaign/map     new-campaign-minimal
          true [(rand-str 1024)] :test.stored-campaign/map  stored-campaign-minimal
          false [(rand-str 1025)] :test.campaign/map        campaign-minimal
          false [(rand-str 1025)] :test.new-campaign/map    new-campaign-minimal
          false [(rand-str 1025)] :test.stored-campaign/map stored-campaign-minimal
          false [(rand-str 5000)] :test.campaign/map        campaign-minimal
          false [(rand-str 5000)] :test.new-campaign/map    new-campaign-minimal
          false [(rand-str 5000)] :test.stored-campaign/map stored-campaign-minimal)))

  (testing "COA"
    (testing "[:open_c2_coa :id]"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc-in entity
                                    [:open_c2_coa :id] value)))

          false nil :test.coa/map        coa-maximal
          false nil :test.new-coa/map    new-coa-maximal
          false nil :test.stored-coa/map stored-coa-maximal
          true  ""  :test.coa/map        coa-maximal
          true  ""  :test.new-coa/map    new-coa-maximal
          true  ""  :test.stored-coa/map stored-coa-maximal
          true  (rand-str 100)  :test.coa/map        coa-maximal
          true  (rand-str 100)  :test.new-coa/map    new-coa-maximal
          true  (rand-str 100)  :test.stored-coa/map stored-coa-maximal
          true  (rand-str 1024) :test.coa/map        coa-maximal
          true  (rand-str 1024) :test.new-coa/map    new-coa-maximal
          true  (rand-str 1024) :test.stored-coa/map stored-coa-maximal
          false (rand-str 1025) :test.coa/map        coa-maximal
          false (rand-str 1025) :test.new-coa/map    new-coa-maximal
          false (rand-str 1025) :test.stored-coa/map stored-coa-maximal
          false (rand-str 5000) :test.coa/map        coa-maximal
          false (rand-str 5000) :test.new-coa/map    new-coa-maximal
          false (rand-str 5000) :test.stored-coa/map stored-coa-maximal))

    (testing "[:open_c2_coa :target :type]"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc-in entity
                                    [:open_c2_coa :target :type] value)))

          false nil :test.coa/map        coa-maximal
          false nil :test.new-coa/map    new-coa-maximal
          false nil :test.stored-coa/map stored-coa-maximal
          true  ""  :test.coa/map        coa-maximal
          true  ""  :test.new-coa/map    new-coa-maximal
          true  ""  :test.stored-coa/map stored-coa-maximal
          true  (rand-str 100)  :test.coa/map        coa-maximal
          true  (rand-str 100)  :test.new-coa/map    new-coa-maximal
          true  (rand-str 100)  :test.stored-coa/map stored-coa-maximal
          true  (rand-str 1024) :test.coa/map        coa-maximal
          true  (rand-str 1024) :test.new-coa/map    new-coa-maximal
          true  (rand-str 1024) :test.stored-coa/map stored-coa-maximal
          false (rand-str 1025) :test.coa/map        coa-maximal
          false (rand-str 1025) :test.new-coa/map    new-coa-maximal
          false (rand-str 1025) :test.stored-coa/map stored-coa-maximal
          false (rand-str 5000) :test.coa/map        coa-maximal
          false (rand-str 5000) :test.new-coa/map    new-coa-maximal
          false (rand-str 5000) :test.stored-coa/map stored-coa-maximal))

    (testing "[:open_c2_coa :target :specifiers]"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc-in entity
                                    [:open_c2_coa :target :specifiers] value)))

          false nil :test.coa/map        coa-maximal
          false nil :test.new-coa/map    new-coa-maximal
          false nil :test.stored-coa/map stored-coa-maximal
          true  ""  :test.coa/map        coa-maximal
          true  ""  :test.new-coa/map    new-coa-maximal
          true  ""  :test.stored-coa/map stored-coa-maximal
          true  (rand-str 100)  :test.coa/map        coa-maximal
          true  (rand-str 100)  :test.new-coa/map    new-coa-maximal
          true  (rand-str 100)  :test.stored-coa/map stored-coa-maximal
          true  (rand-str 1024) :test.coa/map        coa-maximal
          true  (rand-str 1024) :test.new-coa/map    new-coa-maximal
          true  (rand-str 1024) :test.stored-coa/map stored-coa-maximal
          false (rand-str 1025) :test.coa/map        coa-maximal
          false (rand-str 1025) :test.new-coa/map    new-coa-maximal
          false (rand-str 1025) :test.stored-coa/map stored-coa-maximal
          false (rand-str 5000) :test.coa/map        coa-maximal
          false (rand-str 5000) :test.new-coa/map    new-coa-maximal
          false (rand-str 5000) :test.stored-coa/map stored-coa-maximal))

    (testing "[:open_c2_coa :actuator :specifiers]"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc-in entity
                                    [:open_c2_coa :actuator :specifiers] value)))

          false nil :test.coa/map        coa-maximal
          false nil :test.new-coa/map    new-coa-maximal
          false nil :test.stored-coa/map stored-coa-maximal
          false [nil] :test.coa/map        coa-maximal
          false [nil] :test.new-coa/map    new-coa-maximal
          false [nil] :test.stored-coa/map stored-coa-maximal
          true  [""]  :test.coa/map        coa-maximal
          true  [""]  :test.new-coa/map    new-coa-maximal
          true  [""]  :test.stored-coa/map stored-coa-maximal
          true  [(rand-str 100)]  :test.coa/map        coa-maximal
          true  [(rand-str 100)]  :test.new-coa/map    new-coa-maximal
          true  [(rand-str 100)]  :test.stored-coa/map stored-coa-maximal
          true  [(rand-str 1024)] :test.coa/map        coa-maximal
          true  [(rand-str 1024)] :test.new-coa/map    new-coa-maximal
          true  [(rand-str 1024)] :test.stored-coa/map stored-coa-maximal
          false [(rand-str 1025)] :test.coa/map        coa-maximal
          false [(rand-str 1025)] :test.new-coa/map    new-coa-maximal
          false [(rand-str 1025)] :test.stored-coa/map stored-coa-maximal
          false [(rand-str 5000)] :test.coa/map        coa-maximal
          false [(rand-str 5000)] :test.new-coa/map    new-coa-maximal
          false [(rand-str 5000)] :test.stored-coa/map stored-coa-maximal))

    (testing "[:open_c2_coa :modifiers :additional_properties :context]"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc-in entity
                                    [:open_c2_coa :modifiers
                                     :additional_properties :context]
                                    value)))

          false nil :test.coa/map        coa-maximal
          false nil :test.new-coa/map    new-coa-maximal
          false nil :test.stored-coa/map stored-coa-maximal
          true  ""  :test.coa/map        coa-maximal
          true  ""  :test.new-coa/map    new-coa-maximal
          true  ""  :test.stored-coa/map stored-coa-maximal
          true  (rand-str 100)  :test.coa/map        coa-maximal
          true  (rand-str 100)  :test.new-coa/map    new-coa-maximal
          true  (rand-str 100)  :test.stored-coa/map stored-coa-maximal
          true  (rand-str 1024) :test.coa/map        coa-maximal
          true  (rand-str 1024) :test.new-coa/map    new-coa-maximal
          true  (rand-str 1024) :test.stored-coa/map stored-coa-maximal
          false (rand-str 1025) :test.coa/map        coa-maximal
          false (rand-str 1025) :test.new-coa/map    new-coa-maximal
          false (rand-str 1025) :test.stored-coa/map stored-coa-maximal
          false (rand-str 5000) :test.coa/map        coa-maximal
          false (rand-str 5000) :test.new-coa/map    new-coa-maximal
          false (rand-str 5000) :test.stored-coa/map stored-coa-maximal))

    (testing "[:open_c2_coa :modifiers :frequency]"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc-in entity
                                    [:open_c2_coa :modifiers :frequency]
                                    value)))

          false nil :test.coa/map        coa-maximal
          false nil :test.new-coa/map    new-coa-maximal
          false nil :test.stored-coa/map stored-coa-maximal
          true  ""  :test.coa/map        coa-maximal
          true  ""  :test.new-coa/map    new-coa-maximal
          true  ""  :test.stored-coa/map stored-coa-maximal
          true  (rand-str 100)  :test.coa/map        coa-maximal
          true  (rand-str 100)  :test.new-coa/map    new-coa-maximal
          true  (rand-str 100)  :test.stored-coa/map stored-coa-maximal
          true  (rand-str 1024) :test.coa/map        coa-maximal
          true  (rand-str 1024) :test.new-coa/map    new-coa-maximal
          true  (rand-str 1024) :test.stored-coa/map stored-coa-maximal
          false (rand-str 1025) :test.coa/map        coa-maximal
          false (rand-str 1025) :test.new-coa/map    new-coa-maximal
          false (rand-str 1025) :test.stored-coa/map stored-coa-maximal
          false (rand-str 5000) :test.coa/map        coa-maximal
          false (rand-str 5000) :test.new-coa/map    new-coa-maximal
          false (rand-str 5000) :test.stored-coa/map stored-coa-maximal))

    (testing "[:open_c2_coa :modifiers :id]"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc-in entity
                                    [:open_c2_coa :modifiers :id]
                                    value)))

          false nil :test.coa/map        coa-maximal
          false nil :test.new-coa/map    new-coa-maximal
          false nil :test.stored-coa/map stored-coa-maximal
          true  ""  :test.coa/map        coa-maximal
          true  ""  :test.new-coa/map    new-coa-maximal
          true  ""  :test.stored-coa/map stored-coa-maximal
          true  (rand-str 100)  :test.coa/map        coa-maximal
          true  (rand-str 100)  :test.new-coa/map    new-coa-maximal
          true  (rand-str 100)  :test.stored-coa/map stored-coa-maximal
          true  (rand-str 1024) :test.coa/map        coa-maximal
          true  (rand-str 1024) :test.new-coa/map    new-coa-maximal
          true  (rand-str 1024) :test.stored-coa/map stored-coa-maximal
          false (rand-str 1025) :test.coa/map        coa-maximal
          false (rand-str 1025) :test.new-coa/map    new-coa-maximal
          false (rand-str 1025) :test.stored-coa/map stored-coa-maximal
          false (rand-str 5000) :test.coa/map        coa-maximal
          false (rand-str 5000) :test.new-coa/map    new-coa-maximal
          false (rand-str 5000) :test.stored-coa/map stored-coa-maximal))

    (testing "[:open_c2_coa :modifiers :source]"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc-in entity
                                    [:open_c2_coa :modifiers :source]
                                    value)))

          false nil :test.coa/map        coa-maximal
          false nil :test.new-coa/map    new-coa-maximal
          false nil :test.stored-coa/map stored-coa-maximal
          true  ""  :test.coa/map        coa-maximal
          true  ""  :test.new-coa/map    new-coa-maximal
          true  ""  :test.stored-coa/map stored-coa-maximal
          true  (rand-str 100)  :test.coa/map        coa-maximal
          true  (rand-str 100)  :test.new-coa/map    new-coa-maximal
          true  (rand-str 100)  :test.stored-coa/map stored-coa-maximal
          true  (rand-str 1024) :test.coa/map        coa-maximal
          true  (rand-str 1024) :test.new-coa/map    new-coa-maximal
          true  (rand-str 1024) :test.stored-coa/map stored-coa-maximal
          false (rand-str 1025) :test.coa/map        coa-maximal
          false (rand-str 1025) :test.new-coa/map    new-coa-maximal
          false (rand-str 1025) :test.stored-coa/map stored-coa-maximal
          false (rand-str 5000) :test.coa/map        coa-maximal
          false (rand-str 5000) :test.new-coa/map    new-coa-maximal
          false (rand-str 5000) :test.stored-coa/map stored-coa-maximal))

    (testing "[:open_c2_coa :modifiers :option]"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc-in entity
                                    [:open_c2_coa :modifiers :option]
                                    value)))

          false nil :test.coa/map        coa-maximal
          false nil :test.new-coa/map    new-coa-maximal
          false nil :test.stored-coa/map stored-coa-maximal
          true  ""  :test.coa/map        coa-maximal
          true  ""  :test.new-coa/map    new-coa-maximal
          true  ""  :test.stored-coa/map stored-coa-maximal
          true  (rand-str 100)  :test.coa/map        coa-maximal
          true  (rand-str 100)  :test.new-coa/map    new-coa-maximal
          true  (rand-str 100)  :test.stored-coa/map stored-coa-maximal
          true  (rand-str 1024) :test.coa/map        coa-maximal
          true  (rand-str 1024) :test.new-coa/map    new-coa-maximal
          true  (rand-str 1024) :test.stored-coa/map stored-coa-maximal
          false (rand-str 1025) :test.coa/map        coa-maximal
          false (rand-str 1025) :test.new-coa/map    new-coa-maximal
          false (rand-str 1025) :test.stored-coa/map stored-coa-maximal
          false (rand-str 5000) :test.coa/map        coa-maximal
          false (rand-str 5000) :test.new-coa/map    new-coa-maximal
          false (rand-str 5000) :test.stored-coa/map stored-coa-maximal))

    (testing ":objective"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc entity
                                 :objective value)))

          false nil :test.coa/map        coa-maximal
          false nil :test.new-coa/map    new-coa-maximal
          false nil :test.stored-coa/map stored-coa-maximal
          false [nil] :test.coa/map        coa-maximal
          false [nil] :test.new-coa/map    new-coa-maximal
          false [nil] :test.stored-coa/map stored-coa-maximal
          true  [""]  :test.coa/map        coa-maximal
          true  [""]  :test.new-coa/map    new-coa-maximal
          true  [""]  :test.stored-coa/map stored-coa-maximal
          true  [(rand-str 100)]  :test.coa/map        coa-maximal
          true  [(rand-str 100)]  :test.new-coa/map    new-coa-maximal
          true  [(rand-str 100)]  :test.stored-coa/map stored-coa-maximal
          true  [(rand-str 1024)] :test.coa/map        coa-maximal
          true  [(rand-str 1024)] :test.new-coa/map    new-coa-maximal
          true  [(rand-str 1024)] :test.stored-coa/map stored-coa-maximal
          false [(rand-str 1025)] :test.coa/map        coa-maximal
          false [(rand-str 1025)] :test.new-coa/map    new-coa-maximal
          false [(rand-str 1025)] :test.stored-coa/map stored-coa-maximal
          false [(rand-str 5000)] :test.coa/map        coa-maximal
          false [(rand-str 5000)] :test.new-coa/map    new-coa-maximal
          false [(rand-str 5000)] :test.stored-coa/map stored-coa-maximal))

    (testing ":impact"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc entity
                                 :impact value)))

          false nil :test.coa/map        coa-minimal
          false nil :test.new-coa/map    new-coa-minimal
          false nil :test.stored-coa/map stored-coa-minimal
          true  ""  :test.coa/map        coa-minimal
          true  ""  :test.new-coa/map    new-coa-minimal
          true  ""  :test.stored-coa/map stored-coa-minimal
          true  (rand-str 100)  :test.coa/map        coa-minimal
          true  (rand-str 100)  :test.new-coa/map    new-coa-minimal
          true  (rand-str 100)  :test.stored-coa/map stored-coa-minimal
          true  (rand-str 1024) :test.coa/map        coa-minimal
          true  (rand-str 1024) :test.new-coa/map    new-coa-minimal
          true  (rand-str 1024) :test.stored-coa/map stored-coa-minimal
          false (rand-str 1025) :test.coa/map        coa-minimal
          false (rand-str 1025) :test.new-coa/map    new-coa-minimal
          false (rand-str 1025) :test.stored-coa/map stored-coa-minimal
          false (rand-str 5000) :test.coa/map        coa-minimal
          false (rand-str 5000) :test.new-coa/map    new-coa-minimal
          false (rand-str 5000) :test.stored-coa/map stored-coa-minimal)))

  (testing "ExploitTarget"
    (testing "[:vulnerability 0 :title]"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc-in entity
                                    [:vulnerability 0 :title] value)))

          false nil :test.exploit-target/map        exploit-target-maximal
          false nil :test.new-exploit-target/map    new-exploit-target-maximal
          false nil :test.stored-exploit-target/map stored-exploit-target-maximal
          true  ""  :test.exploit-target/map        exploit-target-maximal
          true  ""  :test.new-exploit-target/map    new-exploit-target-maximal
          true  ""  :test.stored-exploit-target/map stored-exploit-target-maximal
          true  (rand-str 100)  :test.exploit-target/map        exploit-target-maximal
          true  (rand-str 100)  :test.new-exploit-target/map    new-exploit-target-maximal
          true  (rand-str 100)  :test.stored-exploit-target/map stored-exploit-target-maximal
          true  (rand-str 1024) :test.exploit-target/map        exploit-target-maximal
          true  (rand-str 1024) :test.new-exploit-target/map    new-exploit-target-maximal
          true  (rand-str 1024) :test.stored-exploit-target/map stored-exploit-target-maximal
          false (rand-str 1025) :test.exploit-target/map        exploit-target-maximal
          false (rand-str 1025) :test.new-exploit-target/map    new-exploit-target-maximal
          false (rand-str 1025) :test.stored-exploit-target/map stored-exploit-target-maximal
          false (rand-str 5000) :test.exploit-target/map        exploit-target-maximal
          false (rand-str 5000) :test.new-exploit-target/map    new-exploit-target-maximal
          false (rand-str 5000) :test.stored-exploit-target/map stored-exploit-target-maximal))

    (testing "[:vulnerability 0 :description]"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc-in entity
                                    [:vulnerability 0 :description] value)))

          false nil :test.exploit-target/map        exploit-target-maximal
          false nil :test.new-exploit-target/map    new-exploit-target-maximal
          false nil :test.stored-exploit-target/map stored-exploit-target-maximal
          true  ""  :test.exploit-target/map        exploit-target-maximal
          true  ""  :test.new-exploit-target/map    new-exploit-target-maximal
          true  ""  :test.stored-exploit-target/map stored-exploit-target-maximal
          true  (rand-str 100)  :test.exploit-target/map        exploit-target-maximal
          true  (rand-str 100)  :test.new-exploit-target/map    new-exploit-target-maximal
          true  (rand-str 100)  :test.stored-exploit-target/map stored-exploit-target-maximal
          true  (rand-str 1000) :test.exploit-target/map        exploit-target-maximal
          true  (rand-str 1000) :test.new-exploit-target/map    new-exploit-target-maximal
          true  (rand-str 1000) :test.stored-exploit-target/map stored-exploit-target-maximal
          true  (rand-str 5000) :test.exploit-target/map        exploit-target-maximal
          true  (rand-str 5000) :test.new-exploit-target/map    new-exploit-target-maximal
          true  (rand-str 5000) :test.stored-exploit-target/map stored-exploit-target-maximal
          false (rand-str 5001) :test.exploit-target/map        exploit-target-maximal
          false (rand-str 5001) :test.new-exploit-target/map    new-exploit-target-maximal
          false (rand-str 5001) :test.stored-exploit-target/map stored-exploit-target-maximal
          false (rand-str 10000) :test.exploit-target/map        exploit-target-maximal
          false (rand-str 10000) :test.new-exploit-target/map    new-exploit-target-maximal
          false (rand-str 10000) :test.stored-exploit-target/map stored-exploit-target-maximal))

    (testing "[:vulnerability 0 :short_description]"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc-in entity
                                    [:vulnerability 0 :short_description] value)))

          false nil :test.exploit-target/map        exploit-target-maximal
          false nil :test.new-exploit-target/map    new-exploit-target-maximal
          false nil :test.stored-exploit-target/map stored-exploit-target-maximal
          true  ""  :test.exploit-target/map        exploit-target-maximal
          true  ""  :test.new-exploit-target/map    new-exploit-target-maximal
          true  ""  :test.stored-exploit-target/map stored-exploit-target-maximal
          true  (rand-str 100)  :test.exploit-target/map        exploit-target-maximal
          true  (rand-str 100)  :test.new-exploit-target/map    new-exploit-target-maximal
          true  (rand-str 100)  :test.stored-exploit-target/map stored-exploit-target-maximal
          true  (rand-str 1024) :test.exploit-target/map        exploit-target-maximal
          true  (rand-str 1024) :test.new-exploit-target/map    new-exploit-target-maximal
          true  (rand-str 1024) :test.stored-exploit-target/map stored-exploit-target-maximal
          false (rand-str 1025) :test.exploit-target/map        exploit-target-maximal
          false (rand-str 1025) :test.new-exploit-target/map    new-exploit-target-maximal
          false (rand-str 1025) :test.stored-exploit-target/map stored-exploit-target-maximal
          false (rand-str 5000) :test.exploit-target/map        exploit-target-maximal
          false (rand-str 5000) :test.new-exploit-target/map    new-exploit-target-maximal
          false (rand-str 5000) :test.stored-exploit-target/map stored-exploit-target-maximal))

    (testing "[:vulnerability 0 :cve_id]"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc-in entity
                                    [:vulnerability 0 :cve_id] value)))

          false nil :test.exploit-target/map        exploit-target-maximal
          false nil :test.new-exploit-target/map    new-exploit-target-maximal
          false nil :test.stored-exploit-target/map stored-exploit-target-maximal
          true  ""  :test.exploit-target/map        exploit-target-maximal
          true  ""  :test.new-exploit-target/map    new-exploit-target-maximal
          true  ""  :test.stored-exploit-target/map stored-exploit-target-maximal
          true  (rand-str 100)  :test.exploit-target/map        exploit-target-maximal
          true  (rand-str 100)  :test.new-exploit-target/map    new-exploit-target-maximal
          true  (rand-str 100)  :test.stored-exploit-target/map stored-exploit-target-maximal
          true  (rand-str 1024) :test.exploit-target/map        exploit-target-maximal
          true  (rand-str 1024) :test.new-exploit-target/map    new-exploit-target-maximal
          true  (rand-str 1024) :test.stored-exploit-target/map stored-exploit-target-maximal
          false (rand-str 1025) :test.exploit-target/map        exploit-target-maximal
          false (rand-str 1025) :test.new-exploit-target/map    new-exploit-target-maximal
          false (rand-str 1025) :test.stored-exploit-target/map stored-exploit-target-maximal
          false (rand-str 5000) :test.exploit-target/map        exploit-target-maximal
          false (rand-str 5000) :test.new-exploit-target/map    new-exploit-target-maximal
          false (rand-str 5000) :test.stored-exploit-target/map stored-exploit-target-maximal))

    (testing "[:vulnerability 0 :source]"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc-in entity
                                    [:vulnerability 0 :source] value)))

          false nil :test.exploit-target/map        exploit-target-maximal
          false nil :test.new-exploit-target/map    new-exploit-target-maximal
          false nil :test.stored-exploit-target/map stored-exploit-target-maximal
          true  ""  :test.exploit-target/map        exploit-target-maximal
          true  ""  :test.new-exploit-target/map    new-exploit-target-maximal
          true  ""  :test.stored-exploit-target/map stored-exploit-target-maximal
          true  (rand-str 100)  :test.exploit-target/map        exploit-target-maximal
          true  (rand-str 100)  :test.new-exploit-target/map    new-exploit-target-maximal
          true  (rand-str 100)  :test.stored-exploit-target/map stored-exploit-target-maximal
          true  (rand-str 1024) :test.exploit-target/map        exploit-target-maximal
          true  (rand-str 1024) :test.new-exploit-target/map    new-exploit-target-maximal
          true  (rand-str 1024) :test.stored-exploit-target/map stored-exploit-target-maximal
          false (rand-str 1025) :test.exploit-target/map        exploit-target-maximal
          false (rand-str 1025) :test.new-exploit-target/map    new-exploit-target-maximal
          false (rand-str 1025) :test.stored-exploit-target/map stored-exploit-target-maximal
          false (rand-str 5000) :test.exploit-target/map        exploit-target-maximal
          false (rand-str 5000) :test.new-exploit-target/map    new-exploit-target-maximal
          false (rand-str 5000) :test.stored-exploit-target/map stored-exploit-target-maximal))

    (testing "[:vulnerability 0 :affected_software]"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc-in entity
                                    [:vulnerability 0 :affected_software] value)))

          false nil :test.exploit-target/map        exploit-target-maximal
          false nil :test.new-exploit-target/map    new-exploit-target-maximal
          false nil :test.stored-exploit-target/map stored-exploit-target-maximal
          false [nil] :test.exploit-target/map        exploit-target-maximal
          false [nil] :test.new-exploit-target/map    new-exploit-target-maximal
          false [nil] :test.stored-exploit-target/map stored-exploit-target-maximal
          true  [""]  :test.exploit-target/map        exploit-target-maximal
          true  [""]  :test.new-exploit-target/map    new-exploit-target-maximal
          true  [""]  :test.stored-exploit-target/map stored-exploit-target-maximal
          true  [(rand-str 100)]  :test.exploit-target/map        exploit-target-maximal
          true  [(rand-str 100)]  :test.new-exploit-target/map    new-exploit-target-maximal
          true  [(rand-str 100)]  :test.stored-exploit-target/map stored-exploit-target-maximal
          true  [(rand-str 1024)] :test.exploit-target/map        exploit-target-maximal
          true  [(rand-str 1024)] :test.new-exploit-target/map    new-exploit-target-maximal
          true  [(rand-str 1024)] :test.stored-exploit-target/map stored-exploit-target-maximal
          false [(rand-str 1025)] :test.exploit-target/map        exploit-target-maximal
          false [(rand-str 1025)] :test.new-exploit-target/map    new-exploit-target-maximal
          false [(rand-str 1025)] :test.stored-exploit-target/map stored-exploit-target-maximal
          false [(rand-str 5000)] :test.exploit-target/map        exploit-target-maximal
          false [(rand-str 5000)] :test.new-exploit-target/map    new-exploit-target-maximal
          false [(rand-str 5000)] :test.stored-exploit-target/map stored-exploit-target-maximal))

    (testing "[:weakness 0 :description]"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc-in entity
                                    [:weakness 0 :description] value)))

          false nil :test.exploit-target/map        exploit-target-maximal
          false nil :test.new-exploit-target/map    new-exploit-target-maximal
          false nil :test.stored-exploit-target/map stored-exploit-target-maximal
          true  ""  :test.exploit-target/map        exploit-target-maximal
          true  ""  :test.new-exploit-target/map    new-exploit-target-maximal
          true  ""  :test.stored-exploit-target/map stored-exploit-target-maximal
          true  (rand-str 100)  :test.exploit-target/map        exploit-target-maximal
          true  (rand-str 100)  :test.new-exploit-target/map    new-exploit-target-maximal
          true  (rand-str 100)  :test.stored-exploit-target/map stored-exploit-target-maximal
          true  (rand-str 1000) :test.exploit-target/map        exploit-target-maximal
          true  (rand-str 1000) :test.new-exploit-target/map    new-exploit-target-maximal
          true  (rand-str 1000) :test.stored-exploit-target/map stored-exploit-target-maximal
          true  (rand-str 5000) :test.exploit-target/map        exploit-target-maximal
          true  (rand-str 5000) :test.new-exploit-target/map    new-exploit-target-maximal
          true  (rand-str 5000) :test.stored-exploit-target/map stored-exploit-target-maximal
          false (rand-str 5001) :test.exploit-target/map        exploit-target-maximal
          false (rand-str 5001) :test.new-exploit-target/map    new-exploit-target-maximal
          false (rand-str 5001) :test.stored-exploit-target/map stored-exploit-target-maximal
          false (rand-str 10000) :test.exploit-target/map        exploit-target-maximal
          false (rand-str 10000) :test.new-exploit-target/map    new-exploit-target-maximal
          false (rand-str 10000) :test.stored-exploit-target/map stored-exploit-target-maximal))

    (testing "[:weakness 0 :cwe_id]"
      (are [expected value spec entity]
          (= expected
             (spec/valid? spec
                          (assoc-in entity
                                    [:weakness 0 :cwe_id] value)))

          false nil :test.exploit-target/map        exploit-target-maximal
          false nil :test.new-exploit-target/map    new-exploit-target-maximal
          false nil :test.stored-exploit-target/map stored-exploit-target-maximal
          true  ""  :test.exploit-target/map        exploit-target-maximal
          true  ""  :test.new-exploit-target/map    new-exploit-target-maximal
          true  ""  :test.stored-exploit-target/map stored-exploit-target-maximal
          true  (rand-str 100)  :test.exploit-target/map        exploit-target-maximal
          true  (rand-str 100)  :test.new-exploit-target/map    new-exploit-target-maximal
          true  (rand-str 100)  :test.stored-exploit-target/map stored-exploit-target-maximal
          true  (rand-str 1024) :test.exploit-target/map        exploit-target-maximal
          true  (rand-str 1024) :test.new-exploit-target/map    new-exploit-target-maximal
          true  (rand-str 1024) :test.stored-exploit-target/map stored-exploit-target-maximal
          false (rand-str 1025) :test.exploit-target/map        exploit-target-maximal
          false (rand-str 1025) :test.new-exploit-target/map    new-exploit-target-maximal
          false (rand-str 1025) :test.stored-exploit-target/map stored-exploit-target-maximal
          false (rand-str 5000) :test.exploit-target/map        exploit-target-maximal
          false (rand-str 5000) :test.new-exploit-target/map    new-exploit-target-maximal
          false (rand-str 5000) :test.stored-exploit-target/map stored-exploit-target-maximal))
    )


  )