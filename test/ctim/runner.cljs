(ns ctim.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [ctim.domain.disposition-test]
            [ctim.domain.id-test]
            [ctim.domain.sorting-test]
            [ctim.generators.id-test]
            [ctim.schemas.actor-test]
            [ctim.schemas.attack-pattern-test]
            [ctim.schemas.campaign-test]
            [ctim.schemas.coa-test]
            [ctim.schemas.data-table-test]
            [ctim.schemas.exploit-target-test]
            [ctim.schemas.feedback-test]
            [ctim.schemas.incident-test]
            [ctim.schemas.indicator-test]
            [ctim.schemas.investigation-test]
            [ctim.schemas.judgement-test]
            [ctim.schemas.malware-test]
            [ctim.schemas.relationship]
            [ctim.schemas.schema-version-test]
            [ctim.schemas.sighting-test]
            [ctim.schemas.tool-test]
            [ctim.schemas.verdict-test]))

(doo-tests 'ctim.domain.disposition-test
           'ctim.domain.id-test
           'ctim.domain.sorting-test
           'ctim.generators.id-test
           'ctim.schemas.actor-test
           'ctim.schemas.attack-pattern-test
           'ctim.schemas.campaign-test
           'ctim.schemas.coa-test
           'ctim.schemas.data-table-test
           'ctim.schemas.feedback-test
           'ctim.schemas.exploit-target-test
           'ctim.schemas.incident-test
           'ctim.schemas.indicator-test
           'ctim.schemas.investigation-test
           'ctim.schemas.judgement-test
           'ctim.schemas.malware-test
           'ctim.schemas.relationship
           'ctim.schemas.schema-version-test
           'ctim.schemas.sighting-test
           'ctim.schemas.tool-test
           'ctim.schemas.verdict-test)
