(ns servidor.dados
  (:require
    [clojure.string :as str]
    [environ.core :refer [env]]
    [monger.collection :as mc]
    [monger.core :as mg]
    [monger.operators :as mo]
    [monger.query :as mq]
    [servidor.util :as u])
  (:import
    (org.bson.types ObjectId))
  (:gen-class))

(declare filtro-contem edita! insere! busca-painel-nome busca-produto-cod busca-produtos obtem obtem-poupanca-maior-ano remove! obtem-transformadores)

(def db (atom {}))

;; Banco
(defn- conecta-bd []
  (when (empty? @db)
    (let [uri (env :mongo-uri)]
      (doall
        (reset! db (mg/connect-via-uri uri)))))
  @db)

(defn- rename [from to]
  (let [db (:db (conecta-bd))]
    (mc/rename db from to true)))

(defn- fn-regex [valor]
  (str "(.*" valor ")"))

;; Filtros para ignorar selos ou dados muito grandes
(def PARAMETROS
  [[:poupanca {} {}]
   [:painel {:disponivel true} {:selo 0}]
   [:marca {} {}]
   [:telhado {} {}]
   [:parametros {} {}]
   [:logins {} {}]])

(defn filtro-contem [palavra]
  {mo/$regex (str ".*"
                  (fn-regex palavra)
                  ".*")})

(defn- monta-filtros-kwp [filtros]
  (if-let [filtro-kwp (:filtro-kwp filtros)]
    (if-let [[min max] (str/split filtro-kwp #"-")]
      (if (not= "todos" filtro-kwp)
        {:kwp {mo/$gte (u/parse-float-generico min)
               mo/$lte (u/parse-float-generico max)}}
        {})
      {})
    {}))

(defn- monta-filtros [filtros]
  (conj
    (monta-filtros-kwp filtros)
    {mo/$and
       [(if (= (:marca filtros) "todos") {} {:MARCA_INVERSOR (:marca filtros)})
        (if (= (:painel filtros) "todos") {} {:MARCA_PAINEL (:painel filtros)})
        (if (= (:telhado filtros) "todos") {} {:TIPO_ESTRUTURA (:telhado filtros)})
        {:descricao (filtro-contem (str/upper-case (:palavra filtros)))}]}))

(defn edita! [mapa]
  (let [db (:db (conecta-bd))
        id (ObjectId. (:_id mapa))
        registro (:reg mapa)
        chave (:coll mapa)
        coll (name chave)]
    (if (= :painel chave)
      (mc/update db coll {:_id id} {mo/$set registro})
      (mc/update-by-id db coll id {mo/$set registro}))))

(defn insere! [mapa]
  (let [db (:db (conecta-bd))
        chave (:coll mapa)
        registro (:reg mapa)]
    (mc/insert db (name chave) registro)))

(defn busca-painel-nome [nome]
  (let [db (:db (conecta-bd))]
    (first (mc/find-maps db "painel" {:nome nome}))))

(defn busca-produto-cod [cod]
  (let [db (:db (conecta-bd))]
    (first (mc/find-maps db "produtos" {:codigo cod}))))

(defn busca-produtos [per-page filtros]
  (println "Filtros: " (monta-filtros filtros))
  (let [db (:db (conecta-bd))]
    (mq/with-collection db "produtos"
      (mq/find (monta-filtros filtros))
      (mq/fields [:codigo :preco :precoeup :descricao :categoria :foto :kwp :w :MARCA_PAINEL :MARCA_INVERSOR :TIPO_ESTRUTURA])
      ;; it is VERY IMPORTANT to use array maps with sort
      (mq/sort (array-map :kwp 1))
      (mq/paginate :page 1 :per-page (Integer/valueOf per-page)))))

(defn obtem
  ([]
   (let [db (:db (conecta-bd))]
     (apply
       merge
       (doall
         (map
           (fn [v] (let [chave (first v)
                         filtro (second v)
                         colunas (get v 2)]
                     {chave (mc/find-maps db (name chave) filtro colunas)}))
           PARAMETROS)))))
  ([chave filtro colunas]
   (let [db (:db (conecta-bd))]
     (mc/find-maps db (name chave) filtro colunas)))
  ([chave filtro]
   (let [db (:db (conecta-bd))]
     (mc/find-maps db (name chave) filtro)))
  ([chave]
   (obtem chave {})))

(defn obtem-poupanca-maior-ano []
  (let [poupancas
          (let [db (:db (conecta-bd))]
            (mq/with-collection db "poupanca"
              (mq/find {})
              (mq/sort (array-map :ano 1))))]
    (->> poupancas
         (sort-by :ano)
         (last))))

(defn remove! [mapa]
  (let [db (:db (conecta-bd))
        k (-> mapa keys first)
        id (ObjectId. (k mapa))
        coll (name k)]
    (mc/remove-by-id db coll id)))

(defn obtem-transformadores
  ([cod] (first (obtem :transformadores {:codigo cod})))
  ([] (obtem :transformadores {})))
