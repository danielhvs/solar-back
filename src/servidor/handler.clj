(ns servidor.handler
  (:require
    [clj-http.client :as client]
    [clojure.data.json :as json]
    [clojure.java.io :as io]
    [clojure.string :as s]
    [compojure.core :as cpj]
    [compojure.handler :refer [site]]
    [compojure.route :as route]
    [environ.core :refer [env]]
    [ring.adapter.jetty :as jetty]
    [ring.middleware.cors :refer [wrap-cors]]
    [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
    [ring.middleware.keyword-params :refer [wrap-keyword-params]]
    [ring.middleware.multipart-params :refer [wrap-multipart-params]]
    [ring.util.io :as ring]
    [ring.util.response :as r]
    [servidor.dados :as d]
    [servidor.negocio :as n]
    [servidor.util :as u]
    [servidor.view :as v])
  (:import
    (java.io File)
    (javax.imageio ImageIO)
    (org.bson.types ObjectId))
  (:gen-class))

(def ISO_8601 "yyyy-MM-dd'T'HH:mm:ssZ")

(defn- chama-imagem-aldo
  "Retorna nil se aldo estiver fora mas loga"
  [url]
  (try
    (let [nova-url (s/replace url "https" "http")]
      (:body (client/get nova-url {:as :stream
                                   :debug true
                                   :async? false})))
    (catch Exception e
      (println e))))

(defn obtem-imagem-solar [url]
  (let [md5 (:md5 (first (d/obtem "conteudos" {:url url})))
        conteudo (:conteudo (first (d/obtem "md5sums" {:md5 md5})))]
    (println "md5: " md5)
    (n/arquivo-selo conteudo)))

(defn obtem-imagem
  "Retorna nil se nao vier do aldo"
  [gerador]
  (let [url (:foto gerador)]
    (if-let [foto (obtem-imagem-solar url)]
      foto
      (when-let [img (chama-imagem-aldo url)]
        (let [nome-arq "imagem-aldo.tmp"]
          (io/copy img (File. nome-arq))
          nome-arq)))))

(defn normaliza-vals [chave valor]
  (cond (= chave :_id) (str valor)
        (= (type valor) java.util.Date) (.format (java.text.SimpleDateFormat. ISO_8601) valor)
        :else valor))

(defn le-payload!
  ([request val-fn]
   (json/read-str (slurp (:body request))
                  :key-fn keyword
                  :val-fn val-fn))
  ([request]
   (le-payload! request (fn [_ v] v))))

(defn resposta [body]
  (-> (r/response body)
      (r/header "Access-Control-Allow-Origin" "*")))

(defn gera-pdf [produtos request]
  (ring/piped-input-stream
    (let [gerador (:gerador (first produtos))
          imagem-gerador (obtem-imagem gerador)
          imagem-selo (n/arquivo-selo
                        (-> gerador
                            (n/painel)
                            (d/busca-painel-nome)
                            (:selo)))
          produtos-com-imagem (map #(assoc % :imagem-gerador imagem-gerador :imagem-selo imagem-selo)
                                   produtos)]
      (v/pdf produtos-com-imagem request))))

(defn produtos [request]
  (let [r (le-payload! request)
        {:keys [per-page filtros percentual-solar]} r
        geradores (n/geradores per-page filtros)
        produtos (map #(assoc % :preco (u/formata-real
                                         (n/preco-venda-gerador percentual-solar %))
                         :qtd-placas (Math/round (n/qtd-placas [(:kwp %)]
                                                               (:w %))))
                      geradores)]
    (resposta
      (json/write-str produtos :value-fn normaliza-vals))))

;; fixme: performance: so pegar os campos que precisa
(defn transformadores [request]
  (let [r (le-payload! request)
        {:keys [percentual-solar]} r
        transformadores (d/obtem-transformadores)
        retorno (map #(assoc % :preco (u/formata-real
                                        (n/preco-venda-gerador percentual-solar %)))
                     transformadores)]
    (resposta
      (json/write-str retorno :value-fn normaliza-vals))))

(defn login [request]
  (let [r (le-payload! request)]
    (if-let [usuario (seq (filter #(= (:cpf %) (:cpf r))
                                  (d/obtem :logins)))]
      (resposta (json/write-str {:status "OK" :login (dissoc (first usuario) :_id)}))
      (resposta (json/write-str {:status "Login inválido"})))))

(defn obtem-produtos [produtos]
  (map (fn [e]
         (let [cod-transf (:transformador e)]
           (doall (if cod-transf
                    (assoc e
                      :transformador (d/obtem-transformadores cod-transf)
                      :gerador (d/busca-produto-cod (:gerador e)))
                    (assoc e
                      :gerador (d/busca-produto-cod (:gerador e)))))))
       produtos))

(defn- resposta-orcamento [produtos request]
  (-> (resposta (gera-pdf produtos request))
      (r/header "Content-Type" "application/pdf")
      (r/header "Content-disposition" "attachment; filename=\"yourfile.pdf\"")))

;; https://stackoverflow.com/questions/22581644/how-to-serve-the-stream-pdf-with-ring
(defn orcamento [request]
  (let [http-request (le-payload! request)
        produtos (obtem-produtos (:produtos http-request))
        r (assoc http-request
            :poupanca-atual (d/obtem-poupanca-maior-ano)
            :nome-cliente (n/normaliza-nome-cliente (:nome-cliente http-request))
            :tel-cliente (u/so-numeros (:tel-cliente http-request))
            :cpf-cliente (u/so-numeros (:cpf-cliente http-request)))]
    (resposta-orcamento produtos r)))

(defn orcamentos [request]
  (let [r (le-payload! request)
        todos (d/obtem :orcamentos (n/filtro-orcado r))]
    (resposta (json/write-str todos :value-fn normaliza-vals))))

(defn ver-orcamento [id]
  (let [orcamento (first (d/obtem :orcamentos {:_id (ObjectId. id)}))]
    (resposta-orcamento (:produtos orcamento) (:request orcamento))))

(defn deleta [request]
  (let [r (le-payload! request)]
    (d/remove! r)
    (resposta (json/write-str {:status "OK"}))))

(defn insere [request]
  (let [r (le-payload! request)]
    (d/insere! r)
    (resposta (json/write-str {:status "OK"}))))

(defn remove-header-base64 [conteudo]
  (subs
    (subs conteudo (s/index-of conteudo "base64,")) 7))

(defn selo [request]
  (let [r (:multipart-params request)
        conteudo-base64 (remove-header-base64 (get r "file"))]
    (d/edita! {:_id (get r "_id")
               :reg {:selo conteudo-base64}
               :coll :painel})
    (resposta (json/write-str {:status "OK"}))))

(defn edita [request]
  (let [r (le-payload! request)]
    (d/edita! r)
    (resposta (json/write-str {:status "OK"}))))

(defn pendencias-paineis []
  (resposta (json/write-str (n/obtem-pendencias-paineis) :value-fn normaliza-vals)))

(defn paineis []
  (resposta (json/write-str (d/obtem :painel {:disponivel true} {:selo 0}) :value-fn normaliza-vals)))

(defn parametros []
  (resposta (json/write-str (d/obtem) :value-fn normaliza-vals)))

(defn detalhes [codigo]
  (resposta
    (json/write-str (n/produtos-energia-solar codigo)
                    :value-fn normaliza-vals)))

(defn nome-arquivo [request]
  (let [r (:params request)]
    (resposta
      (json/write-str {:nome-arquivo (n/nome-arquivo
                                       (:cpf r)
                                       (:tel r)
                                       (:perc r))}))))
(defn jpeg-response [image-data]
  (-> image-data
      (r/response)
      (r/content-type "image/jpeg")))

(defn binario-jpg [nome-painel]
  (let [image (ImageIO/read (File. (n/figura-selo nome-painel)))
        image-file (File. "nada.jpg")]
    (ImageIO/write image "jpg" image-file)
    (jpeg-response image-file)))

(defn opcoes []
  (-> (r/response "")
      (r/header "Access-Control-Allow-Origin" "*")
      (r/header "Allow" "POST")
      (r/header "Access-Control-Allow-Methods" "POST")
      (r/header "Access-Control-Allow-Headers" "content-type")))

;; Rotas
(cpj/defroutes app-routes
  (cpj/GET "/parametros" [] (parametros))
  (cpj/GET "/paineis" [] (paineis))
  (cpj/GET "/init" [] (resposta (json/write-str {:status "OK"})))
  (cpj/GET "/detalhes/:codigo" [codigo] (detalhes codigo))
  (cpj/GET "/ver-orcamento/:id" [id] (ver-orcamento id))
  (cpj/GET "/selos/:painel/:horario" [painel] (binario-jpg painel))
  (cpj/GET "/nome-arquivo/" request [request] (nome-arquivo request))
  (cpj/GET "/pendencias-paineis" [] (pendencias-paineis))
  (cpj/POST "/orcamentos" request [request] (orcamentos request))
  (cpj/POST "/transformadores" request [request] (transformadores request))
  (cpj/POST "/produtos" request [request] (produtos request))
  (cpj/POST "/edita" request [request] (edita request))
  (cpj/POST "/insere" request [request] (insere request))
  (cpj/POST "/remove" request [request] (deleta request))
  (cpj/POST "/orcamento" request [request] (orcamento request))
  (cpj/POST "/login" request [request] (login request))
  (cpj/POST "/selo" request [request] (selo request))
  (cpj/OPTIONS "/produtos" [] (opcoes))
  (cpj/OPTIONS "/transformadores" [] (opcoes))
  (cpj/OPTIONS "/orcamento" [] (opcoes))
  (cpj/OPTIONS "/orcamentos" [] (opcoes))
  (cpj/OPTIONS "/remove" [] (opcoes))
  (cpj/OPTIONS "/edita" [] (opcoes))
  (cpj/OPTIONS "/selo" [] (opcoes))
  (cpj/OPTIONS "/insere" [] (opcoes))
  (cpj/OPTIONS "/login" [] (opcoes))
  (route/not-found "Not Found"))

(defn debug [mapa chaves]
  (reduce #(str "\n" %1 "\n" %2) (select-keys mapa chaves)))

(defn wrap-debug [handler]
  (fn [request]
    (println "REQUEST:\n"
             #_(str (keys request) "\n")
             (debug request [:uri :request-method]))
    (let [response (handler request)]
      (println "RESPONSE:\n"
               #_(str (keys response) "\n")
               (debug response [:status]))
      response)))

(def app
  (wrap-debug
    (wrap-keyword-params
      (wrap-multipart-params
        (wrap-cors
          (wrap-defaults app-routes api-defaults)
          :access-control-allow-origin [#".*"])))))

(defn -main [& _]
  (println "-main chamado")
  (flush)
  (let [port (Integer. (or (env :port) 3000))]
    (println "Porta: " port)
    (flush)
    (jetty/run-jetty (site #'app) {:port port :join? false})))
