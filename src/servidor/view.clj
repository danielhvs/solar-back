(ns servidor.view
  (:require
    [clj-pdf.core :as pdf]
    [clj-pdf.graphics-2d :as g]
    [clojure.string :as str]
    [incanter.charts :as charts]
    [incanter.core :as incanter]
    [servidor.negocio :as n]
    [servidor.util :as u])
  (:import
    (java.awt.image BufferedImage)
    (java.io File)
    (java.lang String)
    (java.util Locale)
    (javax.imageio ImageIO))
  (:gen-class))

(declare pdf)

(def page-width 595)
(def page-height 841)
(def left-margin 85)
(def right-margin 55)
(def top-margin 46)
(def bottom-margin 25)
(def verde [0 167 198])
(def verde-escuro [60 118 146])
(def laranja [255 99 71])
(def verde-claro [91 228 255])
(def branco [255 255 255])
(def preto [0 0 0])
(def cinza-fundo [217 217 217])
(def cinza [84 85 102])
(def cinza-claro [240 240 240])
(def fundo-secao [128 128 128])
(def cor-secao [242 242 242])
(def fatores [0.995 1.105 1.105 0.995 0.98 0.77 0.97 0.985 0.875 1.055 1.08 1.095])
(def meses ["JAN" "FEV" "MAR" "ABR" "MAI" "JUN" "JUL" "AGO" "SET" "OUT" "NOV" "DEZ"])
(def PT_br (Locale. "pt" "BR"))

(defn- repete-char [n c]
  (str/join (repeat n c)))

(defn- formata-com-decimais [valor]
  (String/format PT_br "%.2f" (to-array [(Double/valueOf (String/valueOf valor))])))

(defn- formata-percentual [percent]
  (str (formata-com-decimais (* 100 percent)) "%"))

(def stylesheet
  {:solar {:color cinza}
   :right {:align :right}
   :fixme {:color laranja :style :bold}
   :indice {:style :bold-italic :color cinza}
   :center {:align :center}
   :meses {:set-border [:left] :color preto :valign :middle :align :center :background-color verde-claro :size 7}
   :dados {:set-border [:right] :color preto :valign :middle :align :right :background-color branco :size 7}
   :heading {:border true :color preto :valign :middle :align :center :background-color verde :size 7}})

(defn- grafico-barras [valores]
  (let [barras (charts/bar-chart meses valores
                                 :y-label ""
                                 :title "Geração (kWh)"
                                 :x-label "Geração (kWh)")
        plot (.getPlot barras)
        nome-arquivo (str "resources/barras." (n/agora "yyyyMMddhhmmss") ".png")]
    (.setPaint (.getRenderer plot) (new java.awt.Color 167 198 0))
    (incanter/save barras nome-arquivo)
    nome-arquivo))

(defn- buffered-image [img-data]
  (cond
    (string? img-data) (ImageIO/read (File. ^String img-data))
    (instance? BufferedImage img-data) img-data))

(defn- header-e-rodape []
  [:graphics {:under true :translate [0 45]}
   (fn [g2d]
     (let [fator 2]
       (doto g2d
         (.drawImage (buffered-image "resources/logo.png")
                     420
                     -35
                     (* fator 30)
                     (* fator 10)
                     nil)
         (.drawImage (buffered-image "resources/rodape.png")
                     (- left-margin 2)
                     (- page-height 60)
                     (+ 9 (- page-width left-margin right-margin))
                     12
                     nil))))])

(defn- secao [titulo]
  [:pdf-table {:horizontal-align :right
               :background-color fundo-secao
               :width-percent 100}
   [1]
   [[:pdf-cell {:color cor-secao
                :height 39
                :style :bold
                :size 16
                :set-border [:top :bottom]
                :border-width 15
                :border-color branco
                :padding-bottom 13
                :align :right
                :valign :middle}
     titulo]]])

(defn- capa-titulo [texto]
  [:heading {:style {:style :bold :size 20 :color verde :align :right}} texto])

(defn- capa-texto [texto]
  [:heading.solar {:style {:size 12 :align :right}} texto])

(defn- s1-capa [nome-arquivo nome-cliente data-revisao]
  [[:spacer 5]
   [:image {:xscale 0.65 :yscale 0.65 :align :right} "resources/logo-original.png"]
   [:spacer 13]
   (capa-titulo "SISTEMA DE GERAÇÃO\nFOTOVOLTAICO")
   [:spacer 3]
   (capa-texto "Proposta Técnica Comercial")
   (capa-texto nome-arquivo)
   [:spacer 11]
   [:heading {:style {:size 14 :color verde :align :right}} "Cliente: " [:chunk {:color preto} nome-cliente]]
   [:heading.solar {:style {:size 14 :align :right}} "Data Revisão: " [:chunk {:color preto} data-revisao]]])

(defn- pdf-cell-revisoes-titulo [dado]
  [:pdf-cell.solar {:style :bold :align :center :valign :middle} dado])

(defn- pdf-cell-revisoes [dado]
  [:pdf-cell.solar {:height 40 :align :center :valign :middle} dado])

(defn- s2-revisoes [nome-cliente nome-arquivo data-revisao nome-verif nome-aprov]
  [(secao "SISTEMA DE GERAÇÃO FOTOVOLTAICO")
   (header-e-rodape)
   [:spacer 3]
   [:pdf-table {:horizontal-align :center
                :border-color cinza-fundo
                :width-percent 100
                :header [[[:pdf-cell.solar {:style :bold :colspan 6 :align :center} (str "ÍNDICE DE REVISÕES - " nome-arquivo)]]
                         (map #(pdf-cell-revisoes-titulo %) ["REV." "DATA" "DESCRIÇÃO" "POR" "VERIF." "APROV."])]}
    [1 2 4 2 2 2]
    (map #(pdf-cell-revisoes %) ["1" data-revisao "EMISSÃO INICIAL" nome-verif nome-verif nome-aprov])
    (map #(pdf-cell-revisoes %) [" " " " "               " "    " " " " "])
    (map #(pdf-cell-revisoes %) [" " " " "               " "    " " " " "])
    (map #(pdf-cell-revisoes %) [" " " " "               " "    " " " " "])
    (map #(pdf-cell-revisoes %) [" " " " "               " "    " " " " "])
    (map #(pdf-cell-revisoes %) [" " " " "               " "    " " " " "])
    (map #(pdf-cell-revisoes %) [" " " " "               " "    " " " " "])
    (map #(pdf-cell-revisoes %) [" " " " "               " "    " " " " "])
    (map #(pdf-cell-revisoes %) [" " " " "               " "    " " " " "])]
   [:spacer 3]
   [:phrase.solar (str "Arquivo: " nome-arquivo " - Energia Fotovoltaica\n")]
   [:spacer 3]
   [:phrase.solar (str "A solar e o cliente " nome-cliente " obrigam-se, por si, neste sentido, a complexidade dos estudos efetuados não pode mais se dissociar da gestão inovadora da qual fazemos parte. Não obstante, a estrutura atual da organização afeta positivamente a correta previsão dos paradigmas corporativos. O que temos que ter sempre em mente é que a adoção de políticas descentralizadoras desafia a capacidade de equalização do impacto na agilidade decisória. Acima de tudo, é fundamental ressaltar que o início da atividade geral de formação de atitudes exige a precisão e a definição dos conhecimentos estratégicos para atingir a excelência.")]])

(defn- s3-indice []
  [(secao "LISTA DE CONTEÚDO")
   (header-e-rodape)
   [:spacer 2]
   [:list {:symbol " "}
    [:phrase.indice "SISTEMA DE GERAÇÃO FOTOVOLTAICO" (repete-char 101 ".") " 2"]
    [:spacer]
    [:phrase.indice "ESCOPO DA PROPOSTA" (repete-char 133 ".") " 4"]
    [:spacer]
    [:phrase.indice "CÁLCULO DO SISTEMA" (repete-char 135 ".") "4"]
    [:spacer]
    [:phrase.indice "BENEFÍCIOS PARA O SEU BOLSO" (repete-char 115 ".") " 5"]
    [:spacer]
    [:phrase.indice "INDICES FINANCEIROS" (repete-char 134 ".") " 6"]
    [:spacer]
    [:phrase.indice "SEU GERADOR FOTOVOLTAICO" (repete-char 117 ".") "7"]
    [:spacer]
    [:phrase.indice "PREÇO DO SEU GERADOR FOTOVOLTAICO" (repete-char 94 ".") "7"]
    [:spacer]
    [:phrase.indice "GARANTIA" (repete-char 158 ".") "8"]
    [:spacer]
    [:phrase.indice "FORMALIZAÇÃO" (repete-char 146 ".") "9"]]])

(defn- s4-escopo []
  [(secao "ESCOPO DA PROPOSTA")
   (header-e-rodape)
   [:spacer]
   [:list {:roman true}
    [:phrase.solar "Desenvolvimento dos projetos fotovoltaicos"]
    [:phrase.solar "Fornecimento de documentação necessária para regulamentação da obra;"]
    [:phrase.solar "Fornecimento de profissionais qualificados para a execução dos serviços;  "]
    [:phrase.solar "Fornecimento de EPIs aos profissionais solar;  "]
    [:phrase.solar "Fornecimento dos equipamentos da unidade geradora (placas, inversor, String box); "]]
   [:phrase.solar "Execução dos projetos desenvolvido nesta proposta respeitando todas as normas pertinentes."]])

(defn- celula-alinhada [c1 c2]
  [[:pdf-cell {:set-border [:left] :align :left :valign :middle :style :bold} c1]
   [:pdf-cell {:set-border [:right] :align :right :valign :middle} c2]])

(defn- s5-calculo [produtos kgm2 custo]
  (let [geradores (map :gerador produtos)
        kwps (map :kwp geradores)
        w (:w (first geradores)) ; w é igual para todos devido à regra restritiva
        qtd-placas (n/qtd-placas kwps w)
        kwhm (n/geracao-mensal qtd-placas w)
        kwha (n/geracao-anual qtd-placas w)
        area (formata-com-decimais (n/Area qtd-placas))
        conta-atual (* kwhm custo)]
    [(secao "CÁLCULO DO SISTEMA")
     (header-e-rodape)
     [:spacer]
     [:phrase.solar (str "Para atender sua demanda, sugerimos um sistema gerador de " (formata-com-decimais (u/soma kwps)) " kWp.")]
     [:pdf-table {:border-color verde
                  :width-percent 100
                  :cell-border true :background-color cinza-claro}
      [1 1]
      [[:pdf-cell {:set-border [:left :top] :align :left :valign :middle :style :bold} "Quantidade de placas fotovoltaica"]
       [:pdf-cell {:set-border [:right :top] :align :right :valign :middle} (str (Math/round qtd-placas) " de " w " W")]]
      (celula-alinhada (str "Produção anual de energia") (str (Math/round kwha) " kWh/ano aproximadamente"))
      (celula-alinhada (str "Área mínima ocupada pelo sistema") (str area " m² de área útil"))
      [[:pdf-cell {:set-border [:left] :align :left :valign :middle :style :bold} (str "Peso médio por m²")]
       [:pdf-cell {:set-border [:right] :align :right :valign :middle} (str kgm2 " kg/m²")]]
      [[:pdf-cell {:set-border [:left :bottom] :align :left :valign :middle :style :bold} "Geração mensal de energia"]
       [:pdf-cell {:set-border [:right :bottom] :align :right :valign :middle} (str (Math/round kwhm) " kWh/mês aproximadamente")]]]
     [:spacer 2]
     [:image {:align :center :yscale 0.85 :xscale 0.95}
      (grafico-barras
        (for [i (range 12)]
          [(* kwhm (nth fatores i))]))]
     [:spacer 3]
     [:phrase.solar {:style :bold} (str "Geração mensal média de " (Math/round kwhm) " kWh equivalente a uma conta atual de " (u/formata-real conta-atual) " mensal.")]]))

(defn- beneficios-conta [aumento-anual conta-atual mes]
  (let [valores (take 5 (iterate
                          #(n/aplica-aumento aumento-anual %)
                          (* conta-atual (nth fatores (dec mes)))))]
    [[:pdf-cell.meses (str mes)] [:pdf-cell.dados (u/formata-real (nth valores 0))]
     [:pdf-cell.meses (str mes)] [:pdf-cell.dados (u/formata-real (nth valores 1))]
     [:pdf-cell.meses (str mes)] [:pdf-cell.dados (u/formata-real (nth valores 2))]
     [:pdf-cell.meses (str mes)] [:pdf-cell.dados (u/formata-real (nth valores 3))]
     [:pdf-cell.meses (str mes)] [:pdf-cell.dados (u/formata-real (nth valores 4))]]))

;; fixme: dry
(defn- s6-beneficios [produtos request]
  (let [geradores (map :gerador produtos)
        transformadores (map :transformador (filter :transformador produtos))
        {:keys [aumento-anual percentual custo percentual-solar]} request
        total (:total (n/precos (flatten (into transformadores geradores))
                                percentual percentual-solar))
        w (:w (first geradores))
        qtd-placas (n/qtd-placas (map :kwp geradores) w)
        kwhm (n/geracao-mensal qtd-placas w)
        conta-atual (* kwhm custo)
        gasto (n/total-gasto-5-anos aumento-anual conta-atual)
        juros (n/juros-aplicacao-5-anos total)]
    [(secao "BENEFÍCIOS PARA O SEU BOLSO")
     (header-e-rodape)
     [:spacer 2]
     [:phrase.solar (str "A conta de luz tem sofrido um aumento médio anual de " (formata-com-decimais aumento-anual) "%. Considerando o valor gasto baseado em sua conta de luz atual temos:")
      [:pdf-table {:width-percent 100
                   :header [[[:pdf-cell {:color preto :size 7 :colspan 3 :align :left :background-color verde-claro} "Conta mensal"]]
                            [[:pdf-cell {:color preto :size 7 :colspan 1 :align :right :background-color branco} (u/formata-real conta-atual)]]
                            [[:pdf-cell {:color preto :size 7 :colspan 2 :align :left :background-color verde-claro} "Aumento anual"]]
                            [[:pdf-cell {:color preto :size 7 :colspan 1 :align :right :background-color branco} (str (formata-com-decimais aumento-anual) "%")]]
                            [[:pdf-cell {:color preto :size 7 :colspan 3 :align :right :background-color branco :set-border []} ""]]]
                   :horizontal-align :center
                   :cell-border true :background-color cinza-claro}
       (vec (flatten (repeat 5 [2 3])))
       [[:pdf-cell.heading "Ano 1"] [:pdf-cell.heading "Conta"]
        [:pdf-cell.heading "Ano 2"] [:pdf-cell.heading "Conta"]
        [:pdf-cell.heading "Ano 3"] [:pdf-cell.heading "Conta"]
        [:pdf-cell.heading "Ano 4"] [:pdf-cell.heading "Conta"]
        [:pdf-cell.heading "Ano 5"] [:pdf-cell.heading "Conta"]]
       (beneficios-conta aumento-anual conta-atual 1)
       (beneficios-conta aumento-anual conta-atual 2)
       (beneficios-conta aumento-anual conta-atual 3)
       (beneficios-conta aumento-anual conta-atual 4)
       (beneficios-conta aumento-anual conta-atual 5)
       (beneficios-conta aumento-anual conta-atual 6)
       (beneficios-conta aumento-anual conta-atual 7)
       (beneficios-conta aumento-anual conta-atual 8)
       (beneficios-conta aumento-anual conta-atual 9)
       (beneficios-conta aumento-anual conta-atual 10)
       (beneficios-conta aumento-anual conta-atual 11)
       (beneficios-conta aumento-anual conta-atual 12)
       [[:pdf-cell {:color preto :colspan 7 :align :left :background-color branco} "Total gasto em 5 anos"]
        [:pdf-cell.right {:color preto :colspan 3 :background-color branco}
         (u/formata-real (n/total-gasto-5-anos aumento-anual conta-atual))]]]
      [:spacer]
      [:phrase.solar (str "Total estimado gasto em 5 anos pago a distribuidora: "
                          (u/formata-real
                            (reduce +
                                    (flatten
                                      (for [i (range 12)]
                                        (n/beneficios-conta-mes aumento-anual conta-atual (inc i)))))))]
      [:spacer 2]
      [:phrase.solar (str "Outro cálculo que podemos fazer considera investir o valor do custo do gerador fotovoltáico aplicado em um fundo de renda fixa com rendimento de 1% ao mês.")]
      (into
        [:pdf-table {:width-percent 100
                     :header [[[:pdf-cell {:color preto :size 7 :background-color verde-claro :colspan 3 :align :left} "Juros"]]
                              [[:pdf-cell {:color preto :size 7 :colspan 1 :align :right :background-color branco} "1%"]]
                              [[:pdf-cell {:color preto :size 7 :colspan 2 :align :left :background-color verde-claro} "Aplicação"]]
                              [[:pdf-cell {:color preto :size 7 :colspan 2 :align :center :background-color branco} (u/formata-real total)]]
                              [[:pdf-cell {:color preto :colspan 2 :align :center :background-color branco :set-border []}]]]
                     :horizontal-align :center
                     :cell-border true :background-color cinza-claro}
         (vec (flatten (repeat 5 [2 3])))
         [[:pdf-cell.heading "Ano 1"] [:pdf-cell.heading "Conta"]
          [:pdf-cell.heading "Ano 2"] [:pdf-cell.heading "Conta"]
          [:pdf-cell.heading "Ano 3"] [:pdf-cell.heading "Conta"]
          [:pdf-cell.heading "Ano 4"] [:pdf-cell.heading "Conta"]
          [:pdf-cell.heading "Ano 5"] [:pdf-cell.heading "Conta"]]]
        (concat
          (let [valores (for [i n/indices] (n/indice->valor i total 1))]
            (map vec (partition 10
                                (map-indexed (fn [i e] [(let [meses (= 0 (mod i 2))]
                                                          (if meses :pdf-cell.meses :pdf-cell.dados))
                                                        (str e)])
                                             (interleave
                                               (flatten
                                                 (for [i (range 12)]
                                                   (repeat 5 (inc i))))
                                               (map u/formata-real valores))))))
          [[[:pdf-cell {:background-color branco :colspan 7 :color preto :align :left} "Rendimento total"]
            [:pdf-cell.right {:background-color branco :color preto :colspan 3}
             (u/formata-real (n/juros-aplicacao-5-anos total))]]]))
      [:spacer]
      [:phrase.solar
       (str "Você deixará de pagar em 5 anos aproximadamente " (u/formata-real gasto) " em contas de luz. Caso o valor do investimento seja aplicado em um fundo de renda fixa renderá juros de " (u/formata-real juros) ".")]
      [:spacer 2]
      [:phrase.solar {:style :underline} "Ou seja, você pagará a mais nesse período para a distribuidora:"]
      [:phrase.solar {:style :bold} (str " " (u/formata-real (- gasto juros)))]]]))

(defn- cor-cell7 [ano]
  (if (zero? (mod ano 2))
    verde-claro
    branco))

(defn- pdf-cell7 [{:keys [ano fluxo]}]
  [[:pdf-cell {:size 6 :align :right :background-color (cor-cell7 ano)} (str ano)]
   [:pdf-cell {:size 6 :align :right :background-color (cor-cell7 ano)} (u/formata-fluxo fluxo)]])

(defn- s7-indices [produtos request]
  (let [geradores (map :gerador produtos)
        transformadores (map :transformador (filter :transformador produtos))
        {:keys [percentual custo aumento-anual percentual-solar poupanca-atual]} request
        total (:total (n/precos (flatten (into transformadores geradores))
                                percentual percentual-solar))
        w (:w (first geradores))
        qtd-placas (n/qtd-placas (map :kwp geradores) w)
        kwhm (n/geracao-mensal qtd-placas w)
        conta-atual (* kwhm custo)
        fluxo (n/fluxo-caixa total conta-atual aumento-anual)
        poupanca (* 0.01 (:valor poupanca-atual))
        ano-poupanca (:ano poupanca-atual)
        rentabilidade (n/rentabilidade-sistema (n/conta-luz-1 conta-atual) total)]
    [(secao "ÍNDICES FINANCEIROS")
     (header-e-rodape)
     [:spacer 2]
     [:phrase.solar {:style :bold} "Retorno sobre o investimento"]
     [:spacer]
     [:list {:symbol "•    " :indent 8}
      [:phrase.solar {:style :bold} "A rentabilidade de seu sistema será de " (formata-percentual rentabilidade) "  por ano!!"]
      [:phrase.solar (str "Rendimento da poupança em " ano-poupanca ": " (formata-percentual poupanca))]
      [:phrase.solar "Seu rendimento com o sistema será " (formata-percentual (- rentabilidade poupanca)) " superior do que se o dinheiro for investido em uma poupança."]]
     [:spacer 2]
     [:phrase.solar {:style :bold} "Fluxo de Caixa Cumulativo"]
     [:spacer]
     [:phrase.solar "Esse indicador nos mostra que no decorrer de 25 anos, levando em consideração o valor do seu investimento e o valor economizado ao longo do tempo de garantia do produto."]
     (into
       [:pdf-table {:border-color cinza-claro
                    :width-percent 20
                    :header [[[:pdf-cell.solar {:valign :middle :size 6 :align :center :background-color verde :color branco :style :bold} "Anos"]
                              [:pdf-cell.solar {:valign :middle :size 6 :align :center :background-color verde :color branco :style :bold} "Fluxo de Caixa"]]]
                    :horizontal-align :center
                    :cell-border true}
        [3 8]]
       (map pdf-cell7
            (for [a (range 26)]
              {:ano a :fluxo (nth fluxo a)})))
     [:phrase.solar {:style :bold} "Observações:"]
     [:list {:symbol "* "}
      [:phrase.solar "A posição do telhado pode levar a perdas no desempenho do sistema, sendo elas:"]]
     [:spacer]
     (into
       [:pdf-table {:width-percent 50
                    :horizontal-align :center
                    :cell-border true}
        [1 1]]
       (map (fn [[a b]] [[:pdf-cell {:align :left} a] [:pdf-cell {:align :left} b]])
            [["Norte" "0%"] ["Nordeste e Noroeste" "3% a 8%"] ["Leste e Oeste" "12% a 20%"] ["Sul" "Superior a 20%"]]))]))

(defn- frase [texto]
  [:phrase.solar {:size 8} texto])

(defn- texto-transformador [t]
  (str "1 "
       (:descricao t)))

(defn- texto-transformadores [ts]
  (map texto-transformador ts))

(defn- coluna-imagem [imagem]
  [[:pdf-cell.solar {:align :center} [:image {:xscale 0.4 :yscale 0.4 :align :center} imagem]]])

(defn- s8-seu-gerador [produtos]
  (let [geradores (map :gerador produtos)
        transformadores (flatten (map :transformador (filter :transformador produtos)))
        {:keys [imagem-selo imagem-gerador]} (first produtos)
        textos (map #(n/composicao %) (map :descricao_tecnica geradores))
        kwp (u/soma (map :kwp geradores))
        componentes (into (texto-transformadores transformadores)
                          (flatten (map #(rest %) textos)))]
    [(secao "SEU GERADOR FOTOVOLTAICO")
     (header-e-rodape)
     [:spacer]
     [:phrase.solar (str "O gerador de energia fotovoltaico de " (formata-com-decimais kwp) " kWp é composto por:")]
     (into [:list {:symbol "•    " :indent 16}]
           (map frase (n/junta-composicoes componentes)))
     [:spacer 2]
     (when (or imagem-gerador imagem-selo)
       [:pdf-table {:horizontal-align :center :cell-border false}
        nil
        (-> (when imagem-gerador (coluna-imagem imagem-gerador))
            (into [])
            (into (when imagem-selo (coluna-imagem imagem-selo))))])]))

(defn- pdf-cell-precos [d1 d2 d3 d4 d5]
  [[:pdf-cell.solar {:align :center :valign :middle} d1]
   [:pdf-cell.solar {:align :left :valign :middle} d2]
   [:pdf-cell.solar {:align :center :valign :middle} d3]
   [:pdf-cell.solar {:align :right :valign :middle} d4]
   [:pdf-cell.solar {:align :right :valign :middle} d5]])

(defn- pdf-cell-preco [dado posicao]
  [:pdf-cell {:background-color verde-claro :align posicao :valign :middle} dado])

(defn- formata-preco-instalacao [preco-instalacao]
  (let [preco (u/formata-real preco-instalacao)]
    (cond (= "R$0,00" preco) "Incluso"
          :else (u/formata-real preco-instalacao))))

(defn- s9-preco [produtos percentual percentual-solar]
  (let [geradores (map :gerador produtos)
        transformadores (map :transformador (filter :transformador produtos))
        {:keys [preco preco-instalacao total]} (n/precos
                                                 (flatten (into transformadores geradores))
                                                 percentual percentual-solar)
        kwp (u/soma (map :kwp geradores))]
    [(secao "PREÇO DO SEU GERADOR FOTOVOLTAICO")
     (header-e-rodape)
     [:pdf-table {:horizontal-align :center :cell-border false
                  :width-percent 100
                  :header [[(pdf-cell-preco "Item" :center) (pdf-cell-preco "Material" :left) (pdf-cell-preco "Qnt." :center) (pdf-cell-preco "Preço Unit." :right) (pdf-cell-preco "Preço Total" :right)]]}
      [2 10 2 5 5]
      (pdf-cell-precos "1"
                       (str "Gerador Solar " (formata-com-decimais kwp) "kWp")
                       "1"
                       (u/formata-real preco) (u/formata-real preco))

      (pdf-cell-precos "2"
                       (str "Projeto + Homologação + Instalação")
                       "1"
                       (formata-preco-instalacao preco-instalacao)
                       (formata-preco-instalacao preco-instalacao))
      [[:pdf-cell {:style :bold :set-border [:top] :size 16 :align :left :colspan 4} "TOTAL"]
       [:pdf-cell.solar {:style :bold :set-border [:top] :align :right :size 14} (u/formata-real total)]]]
     [:spacer 2]
     [:phrase.solar "FINANCIAMENTO *Proposta sujeita a alteração. *Equipamento importado."]
     [:spacer 1]
     [:image {:xscale 0.6 :yscale 0.6 :align :center} "resources/bancos.png"]]))

(defn- negrito [texto]
  [:chunk {:style :bold} texto])

(defn- s10-garantia [gerador]
  (let [fabricante (n/marca gerador)]
    [(secao "GARANTIA")
     (header-e-rodape)
     [:spacer 2]
     [:phrase.solar "A solar Solar fundada em 2000, dedica-se ao segmento de energia fotovoltaica desde 1970. Possuindo mais de 999 projetos aprovados na Aneel, a solar se destaca sendo líder na região noroeste de Sao Paulo. As instalações são realizadas sempre priorizando o rendimento energético de forma segura e sustentável. A equipe técnica é formada exclusivamente por engenheiros e técnicos formados, em que todos os instaladores são formados no SENAI em instalações elétricas e ainda possuem certificados NR-10 (trabalho com eletricidade) e NR-35 (trabalho em altura)."]
     [:spacer 2]
     [:chunk {:x 0 :y -100}
      [:image {:width 130 :height 110 :align :left} "resources/selo.png"]]
     [:spacer 2]
     [:phrase.solar "                                                    Selo Portal Solar comprova as instalações solar Solar junto a"]
     [:phrase.solar "                                                                    Aneel"]
     [:spacer 6]
     [:phrase.solar "Garantia dos serviços prestados pela solar: 3 anos."]
     [:spacer]
     [:phrase.solar (str "Garantia dos Inversores " fabricante ": " (n/tempo-garantia fabricante) " anos de garantia ao registrar o seu inversor na fabricante. As condições da garantia valem para a primeira instalação dos inversores.")]
     [:spacer]
     [:phrase.solar (n/texto-painel gerador)]
     [:spacer]
     [:phrase.solar "Oferecemos aos nossos clientes atendimento 24 horas, 7 dias por semana. Nosso prazo máximo para resolução é de 24h.

O engenheiro responsável pelo seu serviço será:"]
     [:list {:symbol "•    " :indent 16}
      [:phrase.solar "Nome: " (negrito "Joao da Silva")]
      [:phrase.solar "Telefone: " (negrito "99 9999-9999")]
      [:phrase.solar "E-mail: " [:chunk {:color verde-escuro :style :underline} "joao@email.com"]]]
     [:spacer 6]
     [:chunk {:x 60 :y -48}
      [:image {:width 64 :height 64} "resources/telefone.png"]]
     [:paragraph.solar {:align :center :style :bold} "Telefone 24h: (99) 99999-9999" [:chunk {:x 0 :y 1}
                                                                                      [:image {:width 16 :height 16} "resources/whatsapp.png"]]]]))

(defn- s11-formalizacao [nome-cliente cidade]
  [(secao "FORMALIZAÇÃO")
   (header-e-rodape)
   [:spacer 1]
   [:phrase.solar "
As partes concordam integralmente com as condições apresentadas neste contrato e concordam que as informações apresentadas no Check List Técnico acima estão corretas e são verdadeiras: "]
   [:spacer 18]
   [:pdf-table {:horizontal-align :right
                :cell-border true
                :width-percent 100}
    [10 1 10]
    [[:pdf-cell.solar {:align :center :set-border [:top]} "JOAO DA SILVA"]
     [:pdf-cell.solar {:align :center :set-border []} ""]
     [:pdf-cell.solar {:align :center :set-border [:top]} nome-cliente]]
    [[:pdf-cell.solar {:size 8 :style :bold :align :center :set-border []} "Sócio Administrador"]
     [:pdf-cell.solar {:align :center :set-border []} ""]
     [:pdf-cell.solar {:size 8 :style :bold :align :center :set-border []} "Cliente"]]]
   [:spacer 14]
   [:phrase.solar {:style :bold} (str (str/upper-case cidade) ", " (n/data-assinatura) ".")]
   [:spacer]
   [:phrase.solar {:style :bold}
    "
BLA BLA TECNOLOGIA LTDA ME
Av. Rio, 1111 – Sao Paulo
8888 888 8888 – 44 4444 4444 – 44 99999 9999
CNPJ 99.999.999/0001-99/SP
REGISTRO NO CREA-PR 99999
    "]
   [:spacer 2]
   [:phrase.solar "
Proposta válida por 99 dias.
Essa proposta apenas terá validade após o preenchimento do Check-list Técnico. "]])

(defn pdf [produtos request]
  (fn [output-stream]
    (try
      (pdf/pdf
        (let [{:keys [cpf-cliente tel-cliente nome-cliente custo percentual nome-vendedor cidade percentual-solar]} request
              nome-arq (n/nome-arquivo cpf-cliente tel-cliente percentual)]
          (reduce into
                  [[{:footer {:text (str nome-arq (str/join (repeat 107 " ")))
                              :align :right
                              :color cinza
                              :footer-separator "/"
                              :start-page 2}
                     :register-system-fonts? true
                     :pages true
                     :stylesheet stylesheet
                     :left-margin left-margin
                     :right-margin right-margin
                     :top-margin top-margin
                     :bottom-margin bottom-margin
                     :letterhead []
                     :font {:size 11 :encoding :unicode}}]
                   (s1-capa nome-arq nome-cliente (n/agora))
                   [[:pagebreak]]
                   (s2-revisoes nome-cliente nome-arq (n/agora) (n/primeiro-nome-upper nome-vendedor) (n/eng-responsavel))
                   [[:pagebreak]]
                   (s4-escopo)
                   (s5-calculo produtos "16,00" custo)
                   [[:pagebreak]]
                   (s6-beneficios produtos request)
                   [[:pagebreak]]
                   (s7-indices produtos request)
                   [[:pagebreak]]
                   (s8-seu-gerador produtos)
                   [[:spacer]]
                   (s9-preco produtos percentual percentual-solar)
                   [[:pagebreak]]
                   (s10-garantia (first (map :gerador produtos)))
                   [[:pagebreak]]
                   (s11-formalizacao nome-cliente cidade)]))
        output-stream)
      (catch Exception e (println e)))))
