(ns clj-simplenlg.reflection
  (:refer-clojure :exclude [methods])
  (:require [clj-simplenlg.util :as util]
            [clojure.string :as str]))

(defn- superclasses [clazz]
  (when-let [super (.getSuperclass clazz)]
    (cons super (lazy-seq (superclasses super)))))

(defn- methods [^Class class]
  (.getMethods class))

(defn- setter-method [clazz field-kw]
  (let [method-name (->> (util/kebab->camel field-kw)
                         util/capitalize-first
                         (str "set"))]
    (first (filter #(= (.getName %) method-name) (mapcat methods (cons clazz (superclasses clazz)))))))

(defn setter [clazz field-kw]
  (when-let [setter (setter-method clazz field-kw)]
    (fn [object value]
      (.invoke setter object (object-array [value]))
      object)))

(defn- adder-method [clazz field-kw]
  (let [method-name (as-> (util/kebab->camel field-kw) $
                      (str/replace $ #"s$" "")
                      (util/capitalize-first $)
                      (str "add" $))]
    (first (filter #(= (.getName %) method-name) (mapcat methods (cons clazz (superclasses clazz)))))))

(defn adder [clazz field-kw]
  (when-let [adder (adder-method clazz field-kw)]
    (fn [object value]
      (.invoke adder object (object-array [value]))
      object)))
