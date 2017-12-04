(ns clj-simplenlg.core
  (:require [clojure.spec.alpha :as s]
            [clj-simplenlg.reflection :as ref]
            [clj-simplenlg.util :as util])
  (:import [simplenlg.lexicon Lexicon]
           [simplenlg.framework NLGFactory NLGElement]
           [simplenlg.realiser.english Realiser]
           [simplenlg.features Feature]
           [simplenlg.features Tense Person NumberAgreement Form InterrogativeType]))

(def lexicon (Lexicon/getDefaultLexicon))
(def factory (NLGFactory. lexicon))
(def realiser (Realiser. lexicon))

(->> (.createSentence factory "my dog is happy")
     (.realiseSentence realiser))
"My dog is happy."

(->> (doto (.createClause factory)
       (.setSubject "Mary")
       (.setVerb "chase")
       (.setObject "the monkey"))
     (.realiseSentence realiser))
"Mary chases the monkey."

(->> (doto (.createClause factory)
       (.setSubject "Mary")
       (.setVerb "chase")
       (.setObject "the monkey")
       (.setFeature Feature/TENSE Tense/PAST))
     (.realiseSentence realiser))
"Mary chased the monkey."

(->> (doto (.createClause factory)
       (.setSubject "Mary")
       (.setVerb "chase")
       (.setObject "the monkey")
       (.setFeature Feature/TENSE Tense/FUTURE))
     (.realiseSentence realiser))
"Mary will chase the monkey."

(->> (doto (.createClause factory)
       (.setSubject "Mary")
       (.setVerb "chase")
       (.setObject "the monkey")
       (.setFeature Feature/TENSE Tense/FUTURE)
       (.setFeature Feature/NEGATED true))
     (.realiseSentence realiser))
"Mary will not chase the monkey."

(->> (doto (.createClause factory)
       (.setSubject "Mary")
       (.setVerb "chase")
       (.setObject "the monkey")
       (.setFeature Feature/TENSE Tense/FUTURE)
       (.setFeature Feature/INTERROGATIVE_TYPE InterrogativeType/YES_NO))
     (.realiseSentence realiser))
"Will Mary chase the monkey?"

(->> (doto (.createClause factory)
       (.setSubject "Mary")
       (.setVerb "chase")
       (.setObject "the monkey")
       (.setFeature Feature/TENSE Tense/FUTURE)
       (.setFeature Feature/NEGATED true)
       (.setFeature Feature/INTERROGATIVE_TYPE InterrogativeType/YES_NO))
     (.realiseSentence realiser))
"Will Mary not chase the monkey?"

(->> (doto (.createClause factory)
       (.setSubject "Mary")
       (.setVerb "chase")
       (.setFeature Feature/INTERROGATIVE_TYPE InterrogativeType/WHO_OBJECT))
     (.realiseSentence realiser))
"Who does Mary chase?"

(->> (doto (.createClause factory)
       (.setSubject "Mary")
       (.setVerb "chase")
       (.setFeature Feature/TENSE Tense/FUTURE)
       (.setFeature Feature/INTERROGATIVE_TYPE InterrogativeType/WHO_OBJECT))
     (.realiseSentence realiser))
"Who will Mary chase?"

(->> (doto (.createClause factory)
       (.setSubject "Mary")
       (.setVerb "chase")
       (.setFeature Feature/TENSE Tense/FUTURE)
       (.setFeature Feature/INTERROGATIVE_TYPE InterrogativeType/WHO_OBJECT)
       (.setComplement "despite her exhaustion"))
     (.realiseSentence realiser))
"Who will Mary chase despite her exhaustion?"

(->> (doto (.createClause factory)
       (.setSubject (doto (.createNounPhrase factory "Mary")
                      (.addModifier "crazy")))
       (.setVerb (doto (.createVerbPhrase factory "chase")
                   (.addModifier "furiously")))
       (.setObject (doto (.createNounPhrase factory "monkey")
                      (.setDeterminer "the")
                      (.addModifier "dirty"))))
     (.realiseSentence realiser))
"Crazy Mary chases furiously the dirty monkey."

(->> (doto (.createClause factory)
       (.setSubject (doto (.createNounPhrase factory "Mary")
                      (.addModifier "crazy")))
       (.setVerb (doto (.createVerbPhrase factory "chase")
                   (.addModifier "furiously")))
       (.setObject (doto (.createNounPhrase factory "monkey")
                      (.setDeterminer "the")
                      (.addModifier "dirty")))
       (.addComplement (doto (.createPrepositionPhrase factory)
                          (.setPreposition "in")
                          (.addComplement (doto (.createNounPhrase factory "park")
                                            (.setDeterminer "the")))))
       (.addComplement "with John"))
     (.realiseSentence realiser))
"Crazy Mary chases furiously the dirty monkey in the park with John."

(->> (doto (.createClause factory)
       (.setSubject (doto (.createNounPhrase factory "Mary")
                      (.addModifier "crazy")
                      (.setFeature Feature/POSSESSIVE true)
                      (.addComplement "dog")))
       (.setVerb (doto (.createVerbPhrase factory "chase")
                   (.addModifier "furiously")))
       (.setObject (doto (.createNounPhrase factory "monkey")
                      (.setDeterminer "the")
                      (.addModifier "dirty")))
       (.addComplement (doto (.createPrepositionPhrase factory)
                          (.setPreposition "in")
                          (.addComplement (doto (.createNounPhrase factory "park")
                                            (.setDeterminer "the")))))
       (.addComplement "with John"))
     (.realiseSentence realiser))
"Crazy Mary's dog chases furiously the dirty monkey in the park with John."

(s/def ::element (partial instance? NLGElement))

(def features
  {:adjective-ordering?          {:key  Feature/ADJECTIVE_ORDERING
                                  :spec boolean?}
   :aggregate-auxiliary?         {:key  Feature/AGGREGATE_AUXILIARY
                                  :spec boolean?}
   :appositive?                  {:key  Feature/APPOSITIVE
                                  :spec boolean?}
   :complementiser               {:key  Feature/COMPLEMENTISER
                                  :spec (s/or :e ::element
                                              :s string?)}
   :conjunction                  {:key  Feature/CONJUNCTION
                                  :spec string?}
   :conjunction-type             {:key  Feature/CONJUNCTION_TYPE
                                  :spec any?} ;;TODO documentation inconsistent
   :cue-phrase                   {:key  Feature/CUE_PHRASE
                                  :spec ::element}
   :elided?                      {:key  Feature/ELIDED
                                  :spec boolean?}
   :form                         {:key  Feature/FORM
                                  :enum Form}
   :interrogative-type           {:key  Feature/INTERROGATIVE_TYPE
                                  :enum InterrogativeType}
   :?                            {:key  Feature/INTERROGATIVE_TYPE
                                  :enum InterrogativeType}
   :comparative?                 {:key Feature/IS_COMPARATIVE
                                  :spec boolean?}
   :superlative?                 {:key Feature/IS_SUPERLATIVE
                                  :spec boolean?}
   :modal                        {:key Feature/MODAL
                                  :spec string?}
   :negated?                     {:key Feature/NEGATED
                                  :spec string?}
   :number                       {:key  Feature/NUMBER
                                  :enum NumberAgreement}
   :particle                     {:key  Feature/PARTICLE
                                  :spec (s/or :e ::element
                                              :s string?)}
   :passive?                     {:key Feature/PASSIVE
                                  :spec boolean?}
   :perfect?                     {:key Feature/PERFECT
                                  :spec boolean?}
   :person                       {:key Feature/PERSON
                                  :enum Person}
   :possessive?                  {:key Feature/POSSESSIVE
                                  :spec boolean?}
   :progressive?                 {:key Feature/PROGRESSIVE
                                  :spec boolean?}
   :pronominal?                  {:key  Feature/PRONOMINAL
                                  :spec boolean?}
   :raise-specifier?             {:key  Feature/RAISE_SPECIFIER
                                  :spec boolean?}
   :suppress-genitive-in-gerund? {:key  Feature/SUPPRESS_GENITIVE_IN_GERUND
                                  :spec boolean?}
   :supressed-complementiser?    {:key  Feature/SUPRESSED_COMPLEMENTISER
                                  :spec boolean?}
   :tense                        {:key  Feature/TENSE
                                  :enum Tense}})

(defn normalize [[x m & rest :as content]]
  (cond (nil? m) [x {}]
        (map? m) content
        :else (vec (concat [x {} m] rest))))

(declare make)

(defn enum-value [clazz v]
  (clojure.lang.Reflector/invokeStaticMethod clazz "valueOf" (to-array [(util/kebab->snake (name v))])))

(defn- set-feature* [x k v]
  (.setFeature x k v))

(defn set-feature! [x k v]
  (when (get features k)
    (if-let [spec (-> features (get k) :spec)]
      (do
        (when-not (s/valid? spec v)
          (throw (ex-info "Value is not valid for feature"
                          {:key k
                           :value v})))
        (set-feature* x (-> features (get k) :key) v))
      (set-feature* x
                    (-> features (get k) :key)
                    (enum-value (-> features (get k) :enum) v)))
    :ok))

(defn set-attr! [x k v]
  (if (set-feature! x k v)
    x
    (if-let [add! (ref/adder (class x) k)]
      (do
        (if (seq? v)
          (doseq [value v]
            (add! x (make value)))
          (add! x (make v)))
        x)
      (if-let [s! (ref/setter (class x) k)]
        (do (s! x v) x)
        (throw (ex-info "setter not found"
                        {:object x
                         :class  (class x)
                         :field  k
                         :value  v})))))) ;;TODO collections and features

(defn set-attrs! [x attrs]
  (doseq [[k v] attrs]
    (set-attr! x k (make v)))
  x)

(defmulti make (fn [x] (if (vector? x) (first x) :default)))
(defmethod make :default [x] x)
(defmethod make :clause [x]
  (let [[_ attr subject verb object] (normalize x)]
    (-> (.createClause factory (make subject) (make verb) (make object))
        (set-attrs! attr))))

(defmethod make :np [x]
  (let [[_ attr specifier noun] (normalize x)]
    (-> (if noun
          (.createNounPhrase factory (make specifier) (make noun))
          (.createNounPhrase factory (make specifier)))
        (set-attrs! attr))))

(defmethod make :vp [x]
  (let [[_ attr verb] (normalize x)]
    (-> (.createVerbPhrase factory (make verb))
        (set-attrs! attr))))

(defn realise [content]
  ;;(-> realiser (.realise content) .getRealisation)
  (-> realiser (.realiseSentence content)))

;;(realise (make [:clause {:complement "with me"} "mary" "chase" "the monkey"]))

(comment
 (-> [:clause
      {:subject    [:np {:modifier "crazy"} "Mary"]
       :verb       [:vp {:modifier "furiously"} "chase"]
       :object     [:np {:plural true} "the" "monkey"]
       :complement "despite her exhaustion"}]
     make
     realise))
;;"Crazy Mary chases furiously the monkeys despite her exhaustion."

(comment
 (-> [:clause
      {:subject    [:np {:modifier "crazy"} "Mary"]
       :verb       [:vp {:modifier "furiously"} "chase"]
       :object     [:np {:plural true} "the" "monkey"]
       :complement "despite her exhaustion"
       :tense      :past}]
     make
     realise))
;;"Crazy Mary chased furiously the monkeys despite her exhaustion."

[:clause
 {:tense      :future
  :?          :who-object
  :subject    [:noun-phrase {:modifier "crazy"} "Mary"]
  :verb       [:verb-phrase {:modifier "furiously"} "chase"]
  :complement "despite her exhaustion"}]
