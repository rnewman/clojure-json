(ns org.danlarkin.json.encoder2
  (:use [clojure.contrib.java :only [as-str]])
  (:import [java.io Writer]))

(set! *warn-on-reflection* true)

(defprotocol Jsonable
  (to-json [value #^Writer writer indent-size indent])
  (start-token [_])
  (end-token [_]))

(defn start-token-object [_] "{")
(defn end-token-object [_] "}")
(defn start-token-array [_] "[")
(defn end-token-array [_] "]")

(definline maybe-write-newline [#^Writer writer indent-size]
  `(when-not (zero? ~indent-size)
     (.write ~writer (int \newline))))

(defn to-json-nil [value #^Writer writer indent-size indent]
  (.write writer "null"))

(def #^{:private true} escape-map
     {\u0008 "\\b"
      \u0009 "\\t"
      \u000A "\\n"
      \u000C "\\f"
      \u000D "\\r"
      \u0022 "\\\""
      \u005C "\\\\"})

(defn- escaped-char
  "Given a char, return either the char or an escaped representation.
   If a character must be escaped and there is a shortened 'backslash'
   escape sequence available, it is used.  Otherwise the character is
   escaped as backslash-u-4-hex-digits.  The / (solidus) character can
   be escaped with a backslash but that is not required and this code
   does not."
  [#^Character c]
  (let [quick-escape (escape-map c)]
    (cond
      quick-escape quick-escape
      (or (= c (char 0x20)) (= c (char 0x21))) c
      (and (>= (.compareTo c (char 0x23)) 0)
           (<= (.compareTo c (char 0x5B)) 0)) c
           (>= (.compareTo c (char 0x5D)) 0) c
           :else (format "\\u%04X" (int c)))))

(defn to-json-string
  "Returns an escaped (per RFC4627, section 2.5) version of the input string"
  [value #^Writer writer indent-size indent]
  (.write writer #^String (apply str (repeat indent " ")))
  (.write writer "\"")
  (let [#^String v (as-str value)]
    (dotimes [i (.length v)]
      (.write writer (str (escaped-char (.charAt v i))))))
  (.write writer "\""))

(defn to-json-literal [value #^java.io.Writer writer indent-size indent]
  (.write writer #^String (apply str (repeat indent " ")))
  (.write writer (str value)))

(defn to-json-collection [value #^java.io.Writer writer indent-size indent]
  (.write writer #^String (apply str (repeat indent " ")))
  (.write writer #^String (start-token value))
  (maybe-write-newline writer indent-size)
  (when-let [v (first value)]
    (to-json v writer indent-size (+ indent indent-size)))
  (doseq [v (rest value)]
    (.write writer ",")
    (maybe-write-newline writer indent-size)
    (to-json v writer indent-size (+ indent indent-size)))
  (maybe-write-newline writer indent-size)
  (.write writer #^String (apply str (repeat indent " ")))
  (.write writer #^String (end-token value)))

(defn to-json-map-entry [value #^Writer writer indent-size indent]
  (to-json (let [k (key value)]
             (if (keyword? k)
               (subs (str k) 1)
               (str k))) writer indent-size indent)
  (.write writer ":")
  (to-json (val value) writer indent-size indent))

(extend java.lang.CharSequence Jsonable
        {:to-json to-json-string})

(extend java.lang.Boolean Jsonable
        {:to-json to-json-literal})

(extend clojure.lang.Named Jsonable
        {:to-json to-json-string})

(extend nil Jsonable
        {:to-json to-json-nil})

(extend java.lang.Number Jsonable
        {:to-json to-json-literal})

(extend clojure.lang.IPersistentMap Jsonable
        {:to-json to-json-collection
         :start-token start-token-object
         :end-token end-token-object})

(extend java.util.Map$Entry Jsonable
        {:to-json to-json-map-entry})

(extend clojure.lang.IPersistentCollection Jsonable
        {:to-json to-json-collection
         :start-token start-token-array
         :end-token end-token-array})

(defn encode-to-str2
  "Takes an arbitrarily nested clojure datastructure
   and returns a JSON-encoded string representation
   in a java.lang.String."
  [value & opts]
  (let [writer (java.io.StringWriter.)
        opts (apply hash-map opts)
        indent-size (:indent opts 0)]
    (to-json value writer indent-size 0)
    (.toString writer)))
