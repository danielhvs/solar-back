(ns servidor.util
  (:require
    [clojure.string :as str]
    [clojurewerkz.money.amounts :as ma]
    [clojurewerkz.money.currencies :as mc]
    [clojurewerkz.money.format :as mf])
  (:import
    (java.util Locale)))

(declare so-numeros parse-float-generico parse-float ->reais formata-real formata-fluxo soma)

(def PT_br (Locale. "pt" "BR"))

(defn parse-float-generico [dado]
  (Float/parseFloat
    (str/replace dado #"," ".")))

(defn parse-float "espera uma string com ," [dado]
  (Float/parseFloat
    (str/replace
      (str/replace dado #"\." "")
      #"," ".")))

(defn- ajusta [real]
  (/
    (double
      (bigint (* 100 real)))
    100))

(defn ->reais [valor]
  (let [reais (ajusta valor)]
    (ma/round
      (ma/amount-of mc/BRL reais) 2 :floor)))

(defn formata-real [dado]
  (mf/format
    (->reais dado)
    PT_br))

; fixme: "dup" com acima
(defn formata-fluxo [dado]
  (mf/format dado PT_br))

(defn soma [coll]
  (reduce + coll))

(defn so-numeros [id]
  (when id
    (str/replace id #"\D+" "")))
