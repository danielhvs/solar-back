(defproject servidor "1.0.0"
  :description "Servidor para orcamento solar"
  :url "http://localhost:3000"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [compojure "1.6.2"]
                 [ring-cors "0.1.13"]
                 [dk.ative/docjure "1.12.0"]
                 [clojurewerkz/money "1.10.0"]
                 [org.jfree/jfreechart "1.0.19"]
                 [incanter "1.9.3"]
                 [clj-pdf "2.4.0"]
                 [environ "1.0.0"]
                 [cuerdas "0.3.1"]
                 [com.novemberain/monger "3.5.0"]
                 [org.clojure/data.xml "0.0.8"]
                 [org.clojure/data.codec "0.1.1"]
                 [org.clojure/data.json "0.2.6"]
                 [clj-http "3.10.1"]
                 [ring/ring-jetty-adapter "1.4.0"]
                 [ring/ring-defaults "0.3.2"]]
  :plugins [[lein-cljfmt "0.8.0"]
            [environ/environ.lein "0.3.1"]
            [lein-ring "0.12.5"]]
  :uberjar-name "solar-backend.jar"
  :ring {:handler servidor.handler/app
         :auto-reload? true}
  :main ^:skip-aot servidor.handler
  :profiles
    {:uberjar {:main myproject.web :aot :all}
     :production {:env {:production true}}
     :dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                          [ring/ring-mock "0.4.0"]]}})
