(ns servidor.negocio
  (:require
    [clojure.data.codec.base64 :as b64]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojurewerkz.money.amounts :as ma]
    [cuerdas.core :as c]
    [monger.operators :as mo]
    [servidor.dados :as d]
    [servidor.util :as u])
  (:import
    (java.util Locale))
  (:gen-class))
(declare agora aplica-aumento Area arquivo-selo beneficios-conta-mes composicao conta-luz-1 data-assinatura eng-responsavel figura-selo filtro-orcado fluxo-caixa geracao-mensal geracao-anual geradores indice->valor junta-composicoes juros-aplicacao-5-anos marca nome-arquivo normaliza-nome-cliente obtem-pendencias-paineis painel precos preco-venda-gerador primeiro-nome-upper produtos-energia-solar qtd-placas rentabilidade-sistema repete tempo-garantia texto-painel total-gasto-5-anos)
(def fatores [0.995 1.105 1.105 0.995 0.98 0.77 0.97 0.985 0.875 1.055 1.08 1.095])

(def PT_br (Locale. "pt" "BR"))

(defn- primeiros-valores [inicial percentual-aumento meses]
  (let [valores (take
                  meses (iterate
                          #(aplica-aumento percentual-aumento %)
                          inicial))]
    valores))

(def indices
  (for [mes (range 12)
        ano (range 5)]
    (+ mes (* 12 ano))))

(defn- qtd-placas-um-gerador [kwp w]
  (/ (* 1000 kwp) w))

(defn- arredonda-baixo [preco]
  (+ 9
     (* 10
        (Math/floor
          (/
            (Math/floor preco)
            10)))))

(def COEFICIENTE_AJUSTE 1.005)
(defn preco-venda [percentual-aumento preco-base]
  (arredonda-baixo (* COEFICIENTE_AJUSTE percentual-aumento preco-base)))

(defn preco-venda-gerador [percentual-aumento {:keys [preco]}]
  (preco-venda percentual-aumento (u/parse-float preco)))

(defn- texto-componente [texto]
  (str/join "solar"
            (rest (str/split texto #" "))))

;; é um numero e depois texto
(defn- numero-componente [texto]
  (first (str/split texto #" ")))

(defn- mapear-componentes [c]
  {(keyword (texto-componente c)) (numero-componente c)})

(defn- chave-para-nome [chave]
  (subs
    (str
      (keyword chave)) 1))

(defn- mapa->composicao [mapa]
  (for [k (keys (into (sorted-map) mapa))]
    (str (k mapa) " " (str/replace (chave-para-nome k) #"solar" " "))))

(defn- receitas-caixa [conta-atual aumento-anual]
  (let [valores
          (for [mes (range 12)]
            (map u/->reais (take 25 (iterate
                                      (fn [e] (aplica-aumento aumento-anual e))
                                      (* conta-atual (nth fatores mes))))))]
    (reduce (fn [as bs] (map ma/plus as bs)) valores)))

(defn- data-ou-todos [chave mapa]
  (if-let [data-as-long (chave mapa)]
    (let [unix-timestamp (cond (= chave :end-date) (+ data-as-long 86399000)
                               :else data-as-long)]
      (new java.util.Date unix-timestamp))
    nil))

(defn- periodo [inicio fim]
  (cond
    (and inicio fim) {mo/$gte inicio
                      mo/$lte fim}
    inicio {mo/$gte inicio}
    fim {mo/$lte fim}
    :else nil))

(defn agora
  ([mascara]
   (.format (java.text.SimpleDateFormat. mascara PT_br) (new java.util.Date)))
  ([]
   (agora "dd/MM/yyyy")))

(defn aplica-aumento [percentual valor]
  (+ valor
     (* valor (/ percentual 100))))

(defn Area [placas]
  (* 2.6 placas))

(defn arquivo-selo [str-base64]
  (when str-base64
    (let [aleatorio (rand 10000)
          nome-arquivo (str (agora "yyyyMMddhhmmss") aleatorio ".jpg")
          arquivo-tmp (str (agora "yyyymmddhhmmss") aleatorio ".txt")]
      (spit arquivo-tmp str-base64)
      (with-open [in (io/input-stream arquivo-tmp)
                  out (io/output-stream nome-arquivo)]
        (b64/decoding-transfer in out))
      nome-arquivo)))

(defn beneficios-conta-mes [aumento-anual conta-atual mes]
  (take 5 (iterate
            #(aplica-aumento aumento-anual %)
            (* conta-atual (nth fatores (dec mes))))))

; fixme: normalizar o br

(defn- parse-html-descricao [html]
  (let [tokens (str/split html #"<br/>")]
    (map (fn [e] (str (str/trim (c/strip-tags e)) "\n")) tokens)))

(defn- min-ignora-nil [a b]
  (if a
    (if b
      (min a b)
      a)
    b))

(defn composicao [descricao-tecnica]
  (let [raw descricao-tecnica
        idx (str/index-of raw "O gerador de energia fotovoltaico de")
        last-idx (or (reduce min-ignora-nil
                             [(str/index-of raw "<strong" idx)
                              (str/index-of raw "<img" idx)])
                     (count raw))]
    (parse-html-descricao
      (subs raw idx last-idx))))

(defn conta-luz-1 [conta-atual]
  (* conta-atual (nth fatores 0)))

(defn data-assinatura []
  (str
    (agora "dd") " de "
    (agora "LLLL") " de "
    (agora "yyyy")))

(defn eng-responsavel []
  "JOAO DA SILVA")

(defn figura-selo [nome]
  (arquivo-selo
    (:selo (first
             (d/obtem :painel {:nome (str/upper-case nome)})))))

(defn filtro-orcado [mapa]
  (let [filtro
          (assoc mapa
            :nome-vendedor (or (:logins mapa) "todos")
            :TIPO_ESTRUTURA (or (:telhado mapa) "todos")
            :MARCA_INVERSOR (or (:marca mapa) "todos")
            :MARCA_PAINEL (or (:painel mapa) "todos")
            :request.nome-cliente (let [nome (normaliza-nome-cliente (:request.nome-cliente mapa))]
                                    (if (str/blank? nome)
                                      "todos"
                                      (d/filtro-contem nome)))
            :request.cpf-cliente (let [cpf (u/so-numeros (:request.cpf-cliente mapa))]
                                   (if (str/blank? cpf) "todos" cpf))
            :request.tel-cliente (let [tel (u/so-numeros (:request.tel-cliente mapa))]
                                   (if (str/blank? tel) "todos" tel))
            :data (or (periodo
                        (data-ou-todos :start-date mapa)
                        (data-ou-todos :end-date mapa))
                      "todos"))]
    (->
      (select-keys filtro
                   (keys (filter (fn [e] (not= "todos" (val e))) filtro)))
      (dissoc :logins :telhado :marca :painel :end-date :start-date))))

(defn fluxo-caixa [total conta-atual aumento-anual]
  (let [receitas (receitas-caixa conta-atual aumento-anual)
        inicial [(ma/multiply (u/->reais total) -1)]]
    (reduce (fn [a b] (concat a [(ma/plus b (-> a last))]))
            inicial
            receitas)))

(defn geracao-mensal [qtd-placas w]
  (* 0.001 (* qtd-placas w 30 4.03)))

(defn geracao-anual [qtd-placas w]
  (* 12 (geracao-mensal qtd-placas w)))

(defn geradores [per-page filtros]
  (d/busca-produtos per-page filtros))

(defn indice->valor [i inicial perc]
  (nth (primeiros-valores inicial perc 60) i))

;; first é a introducao
;; rest é os componentes
(defn junta-composicoes [componentes]
  (let [mapeados
          (map mapear-componentes componentes)
        resultado-mapa (apply merge-with
                              #(+ (Integer/valueOf %1) (Integer/valueOf %2))
                              mapeados)]
    (mapa->composicao resultado-mapa)))

(defn juros-aplicacao-5-anos [total]
  (let [valores (for [i indices] (indice->valor i total 1))]
    (- (last valores)
       (first valores))))

(defn marca [gerador]
  (let [descricao (:descricao gerador)
        marcas (filter #(not= nil %)
                       (map #(when (str/includes? (str/upper-case descricao) %) %)
                            (map :nome (d/obtem :marca))))]
    (:marca (reduce (fn [a b] (cond
                                (<= (:index a) (:index b)) a
                                (< (:index b) (:index a)) b))
                    (map (fn [m] {:marca m
                                  :index (str/index-of descricao m)}) marcas)))))

(defn nome-arquivo [cpf tel percentual]
  (let [id (cond (not (str/blank? cpf)) (u/so-numeros cpf)
                 (not (str/blank? tel)) (u/so-numeros tel)
                 :else "")]
    (str "PTC-" id "-" (agora "YYYY") "-U000-F" percentual)))

(defn normaliza-nome-cliente [nome]
  (when nome
    (let [nomes (-> nome
                    (str/split #" "))]
      (str/join " "
                (map (comp str/upper-case str/trim)
                     (filter (complement str/blank?) nomes))))))

(defn obtem-pendencias-paineis
  "Retorna mapa com chaves :selo? e :texto?"
  []
  (let [paineis (d/obtem :painel
                         {mo/$and [{:disponivel true}
                                   {mo/$or
                                      [{mo/$or [{:texto {mo/$exists false}}
                                                {:texto {mo/$eq ""}}]}
                                       {:selo {mo/$exists false}}]}]}
                         {:substring {:conteudo {mo/$substr [:$selo 0 1]}}
                          :nome 1
                          :texto 1})]
    (map
      (fn [e] {:nome (:nome e)
               :selo? (not (str/blank? (-> e :substring :conteudo)))
               :texto? (not (str/blank? (-> e :texto)))})
      paineis)))

(defn painel [gerador]
  (:MARCA_PAINEL gerador))

(defn precos [produtos percentual-vendedor percentual-solar]
  (let [preco-base (u/soma (map #(u/parse-float %) (map :preco produtos)))
        preco (preco-venda percentual-solar preco-base)
        preco-instalacao (* 0.01 percentual-vendedor preco)
        total (+ preco preco-instalacao)]
    {:total total
     :preco-instalacao preco-instalacao
     :preco preco}))

(defn primeiro-nome-upper [nome]
  (first (str/split (str/upper-case nome) #" ")))

(defn produtos-energia-solar [cod]
  (select-keys (d/busca-produto-cod cod)
               [:descricao
                :descricao_tecnica
                :codigo
                :dimensoes
                :peso]))

(defn qtd-placas [kwps w]
  (u/soma (map #(qtd-placas-um-gerador % w) kwps)))

(defn rentabilidade-sistema [conta-luz aplicacao]
  (/ (* 12 conta-luz) aplicacao))

(defn tempo-garantia [fabricante]
  (case fabricante
    "FRONIUS" 7
    "GROWATT" 10
    5))

;; acessa banco
(defn texto-painel [gerador]
  (let [nome-painel (painel gerador)]
    (:texto
      (first (filter #(= nome-painel (:nome %))
                     (d/obtem :painel))))))

(defn total-gasto-5-anos [aumento-anual conta-atual]
  (reduce +
          (flatten
            (for [i (range 12)]
              (beneficios-conta-mes aumento-anual conta-atual (inc i))))))
