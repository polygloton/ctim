# Judgement

A judgement about the intent or nature of an Observable.  For example, is it malicious, meaning is is malware and subverts system operations.  It could also be clean and be from a known benign, or trusted source.  It could also be common, something so widespread that it's not likely to be malicious.


## MapEntry: :id -> String

* This entry is required

### Keyword Key

* Plumatic Schema: :id

### String Value

A string uniquely identifying an entity.

* Plumatic Schema: Str

## MapEntry: :type -> String

* This entry is required

### Keyword Key

* Plumatic Schema: :type

### String Value

* Plumatic Schema: java.lang.String

## MapEntry: :schema_version -> String

* This entry is required

### Keyword Key

* Plumatic Schema: :schema_version

### String Value

* Plumatic Schema: (enum "0.1.8")
* Must equal: "0.1.8"

## MapEntry: :uri -> String

* This entry is optional

### Keyword Key

* Plumatic Schema: :uri

### String Value

A URI

* Plumatic Schema: Str

## MapEntry: :revision -> Integer

* This entry is optional

### Keyword Key

* Plumatic Schema: :revision

### Integer Value

* Plumatic Schema: Int

## MapEntry: :external_ids -> [String]

* This entry is optional
* This entry's type is sequential (allows zero or more values)

### Keyword Key

* Plumatic Schema: :external_ids

#### String Value

* Plumatic Schema: [java.lang.String]

## MapEntry: :timestamp -> Inst (Date)

* This entry is optional

### Keyword Key

* Plumatic Schema: :timestamp

### Inst (Date) Value

Schema definition for all date or timestamp values in GUNDAM.

* Plumatic Schema: Inst

## MapEntry: :language -> String

* This entry is optional

### Keyword Key

* Plumatic Schema: :language

### String Value

* Plumatic Schema: java.lang.String

## MapEntry: :tlp -> String

* This entry is optional

### Keyword Key

* Plumatic Schema: :tlp

### String Value

* Plumatic Schema: (enum "white" "green" "red" "amber")
* Default: green
* Allowed Values: ("amber" "green" "red" "white")

## MapEntry: :source_uri -> String

* This entry is optional

### Keyword Key

* Plumatic Schema: :source_uri

### String Value

A URI

* Plumatic Schema: Str

## MapEntry: :type -> String

* This entry is required

### Keyword Key

* Plumatic Schema: :type

### String Value

* Plumatic Schema: (enum "judgement")
* Must equal: "judgement"

## MapEntry: :observable -> Map

* This entry is required

### Keyword Key

* Plumatic Schema: :observable

### Map Value


#### MapEntry: :value -> String

* This entry is required

##### Keyword Key

* Plumatic Schema: :value

##### String Value

* Plumatic Schema: java.lang.String

#### MapEntry: :type -> String

* This entry is required

##### Keyword Key

* Plumatic Schema: :type

##### String Value

* Plumatic Schema: (enum "device" "url" "pki-serial" "user" "ipv6" "email" "sha256" "sha1" "md5" "ip" "domain" "imei" "imsi" "amp-device")
* Allowed Values: ("amp-device" "device" "domain" "email" "imei" "imsi" "ip" "ipv6" "md5" "pki-serial" "sha1" "sha256" "url" "user")

## MapEntry: :disposition -> Integer

* This entry is required

### Keyword Key

* Plumatic Schema: :disposition

### Integer Value

* Plumatic Schema: (enum 1 4 3 2 5)
* Allowed Values: (1 2 3 4 5)

## MapEntry: :disposition_name -> String

* This entry is required

### Keyword Key

* Plumatic Schema: :disposition_name

### String Value

* Plumatic Schema: (enum "Common" "Unknown" "Suspicious" "Malicious" "Clean")
* Allowed Values: ("Clean" "Common" "Malicious" "Suspicious" "Unknown")

## MapEntry: :priority -> Integer

* This entry is required

### Keyword Key

* Plumatic Schema: :priority

### Integer Value

A value 0-100 that determine the priority of a judgement. Curated feeds of black/white lists, for example known good products within your organizations, should use a 95. All automated systems should use a priority of 90, or less.  Human judgements should have a priority of 100, so that humans can always override machines.

* Plumatic Schema: Int

## MapEntry: :confidence -> String

* This entry is required

### Keyword Key

* Plumatic Schema: :confidence

### String Value

* Plumatic Schema: (enum "Medium" "Unknown" "None" "High" "Low")
* Allowed Values: ("High" "Low" "Medium" "None" "Unknown")

## MapEntry: :severity -> Integer

* This entry is required

### Keyword Key

* Plumatic Schema: :severity

### Integer Value

* Plumatic Schema: Int

## MapEntry: :valid_time -> Map

* This entry is required

### Keyword Key

* Plumatic Schema: :valid_time

### Map Value


#### MapEntry: :start_time -> Inst (Date)

* This entry is optional

##### Keyword Key

* Plumatic Schema: :start_time

##### Inst (Date) Value

Schema definition for all date or timestamp values in GUNDAM.

* Plumatic Schema: Inst

#### MapEntry: :end_time -> Inst (Date)

* This entry is optional

##### Keyword Key

* Plumatic Schema: :end_time

##### Inst (Date) Value

Schema definition for all date or timestamp values in GUNDAM.

* Plumatic Schema: Inst

## MapEntry: :reason -> String

* This entry is optional

### Keyword Key

* Plumatic Schema: :reason

### String Value

* Plumatic Schema: java.lang.String

## MapEntry: :reason_uri -> String

* This entry is optional

### Keyword Key

* Plumatic Schema: :reason_uri

### String Value

A URI

* Plumatic Schema: Str

## MapEntry: :indicators -> [Map]

* This entry is optional
* This entry's type is sequential (allows zero or more values)

### Keyword Key

* Plumatic Schema: :indicators

#### Map Value

Related Indicators


##### MapEntry: :indicator_id -> String

* This entry is required

###### Keyword Key

* Plumatic Schema: :indicator_id

###### String Value

An entity ID, or a URI referring to a remote one

* Plumatic Schema: Str

##### MapEntry: :confidence -> String

* This entry is optional

###### Keyword Key

* Plumatic Schema: :confidence

###### String Value

* Plumatic Schema: (enum "Medium" "Unknown" "None" "High" "Low")
* Allowed Values: ("High" "Low" "Medium" "None" "Unknown")

##### MapEntry: :source -> String

* This entry is optional

###### Keyword Key

* Plumatic Schema: :source

###### String Value

* Plumatic Schema: java.lang.String

##### MapEntry: :relationship -> String

* This entry is optional

###### Keyword Key

* Plumatic Schema: :relationship

###### String Value

* Plumatic Schema: java.lang.String
