(ns ctim.generators.verdict-test
  (:require [clj-momo.test-helpers.core :as mth]
            [clojure.test :refer [use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [ctim.schemas.verdict :as verdict]
            [ctim.test-helpers
             [core :as th]
             [properties :as property]]
            [flanders.utils :as fu]))

(use-fixtures :once
  th/fixture-spec-validation
  th/fixture-fast-gen
  mth/fixture-schema-validation
  (th/fixture-spec verdict/Verdict
                   "test.verdict")
  (th/fixture-spec verdict/StoredVerdict
                   "test.stored-verdict")
  (th/fixture-spec (fu/require-all verdict/Verdict)
                   "test.max.verdict")
  (th/fixture-spec (fu/require-all verdict/StoredVerdict)
                   "test.max.stored-verdict"))

;; Verdict

(defspec ^:gen spec-generated-verdict-is-valid
  (property/generated-entity-is-valid :test.verdict/map))

(defspec ^:gen spec-generated-max-verdict-is-valid
  (property/generated-entity-is-valid :test.max.verdict/map))

;; Stored Verdict

(defspec ^:gen spec-generated-stored-verdict-is-valid
  (property/generated-entity-is-valid :test.stored-verdict/map))

(defspec ^:gen spec-generated-max-stored-verdict-is-valid
  (property/generated-entity-is-valid :test.max.stored-verdict/map))

(defspec ^:gen spec-generated-stored-verdict-id-is-valid
  (property/generated-entity-id-is-valid :test.stored-verdict/map
                                         "verdict"))
