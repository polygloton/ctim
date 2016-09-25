(ns flanders.core
  (:refer-clojure :exclude [int key keyword map name str type])
  (:require #?(:clj  [clojure.core :as core]
               :cljs [cljs.core :as core])
            [clojure.zip :as z]
            [flanders.types :as ft]
            [schema.core :as s]))

;; ----------------------------------------------------------------------
;; Defining Branch Nodes
;; ----------------------------------------------------------------------

(declare key)

(defn entry [key_ type & {:keys [description required?]
                         :or {required? true}}]
  (let [key_ (if (keyword? key_) (key key_) key_)]
    (ft/->MapEntry key_ type required? description)))

(defn map
  "Make a MapType containing the given entries.
  Note that nothing is done to enforce uniqueness of the entries keys.
  Duplicate keys are handled at the discretion of the fn that is
  walking the DDL tree, though it should be assumed that later
  duplicates replace earlier ones (as when merging maps)."
  [entries & {:keys [description name]}]
  (ft/->MapType entries name description))

(defn map-of
  "Build a MapType with a map of options followed by lists of entries.
   Useful when using composing lists of required-entries,
  optional-entries, and predefined entries."
  [{:keys [description name]}
   & field-lists]
  (ft/->MapType (apply concat field-lists) name description))

(defn seq-of [type & {:keys [description]}]
  (ft/->SequenceOfType type description))

;; ----------------------------------------------------------------------
;; Defining Leaf Nodes
;; ----------------------------------------------------------------------

(defn bool [& {:keys [description equals]}]
  (ft/map->BooleanType {:description description
                        :open? (not equals)
                        :default (when equals equals)}))

(defn inst [& {:keys [description]}]
  (ft/->InstType description))

(defn int [& {:keys [description equals]}]
  (ft/map->IntegerType {:description description
                        :open? (not equals)
                        :values (when equals #{equals})
                        :default (when equals equals)}))

(defn keyword [& {:keys [description equals]}]
  (ft/map->KeywordType {:description description
                        :open? (not equals)
                        :values (when equals #{equals})
                        :default (when equals equals)}))

(defn key [equals & {:keys [description]}]
  (ft/map->KeywordType {:description description
                        :open? false
                        :values #{equals}
                        :default equals}))

(defn str [& {:keys [description equals]}]
  (ft/map->StringType {:description description
                       :open? (not equals)
                       :values (when equals #{equals})
                       :default (when equals equals)}))

(defn enum [values & {:keys [default open? description]
                      :or {open? false}
                      :as opts}]
  (let [v (first values)]
    (cond
      (integer? v) (ft/map->IntegerType (merge opts {:values values :open? open?}))
      (keyword? v) (ft/map->KeywordType (merge opts {:values values :open? open?}))
      (string? v)  (ft/map->StringType  (merge opts {:values values :open? open?})))))

(defn eq [value & {:keys [description]}]
  (enum #{value}
        :open? false
        :default value
        :description description))

(def any-int (int))

(def any-inst (inst))

(def any-keyword (keyword))

(def any-str (str))

(def any-string-seq (seq-of any-str))

;; ----------------------------------------------------------------------
;; Helpers
;; ----------------------------------------------------------------------

(defn required-entries [& entries]
  (core/map #(assoc % :required? true) entries))

(defn optional-entries [& entries]
  (core/map #(assoc % :required? false) entries))

;; ----------------------------------------------------------------------
;; Macros
;; ----------------------------------------------------------------------

(defmacro def-entity-type [name description & map-entries]
  `(def ~name
     (map-of {:description ~description
              :name ~(core/str name)}
             ~@map-entries)))
