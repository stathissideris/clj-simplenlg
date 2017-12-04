(ns clj-simplenlg.util
  (:refer-clojure :exclude [subs])
  (:require [clojure.string :as str]))

(defn capitalize-first
  "Capitalizes first letter and leaves the rest of the string unchanged"
  [^CharSequence s]
  (let [s (.toString s)]
    (if (< (count s) 2)
      (.toUpperCase s)
      (str (.toUpperCase (clojure.core/subs s 0 1))
           (clojure.core/subs s 1)))))

(defn camel->kebab [from]
  (let [s (str/split (name from) #"(?=[A-Z])" )]
    (apply str (interpose "-" (map str/lower-case s)))))

(defn kebab->camel [from]
  (let [s (str/split (name from) #"\-")]
    (apply str (first s) (map str/capitalize (next s)))))

(defn kebab->snake [from]
  (-> from
      (str/replace "-" "_")
      str/upper-case))

(defn subs [s start end]
  (when (and s start end)
    (let [len (delay (.length s))]
      (clojure.core/subs
       s
       (if (or (zero? start) (pos? start))
         start
         (- @len (- start)))
       (if (or (zero? end) (pos? end))
         end
         (- @len (- end)))))))
