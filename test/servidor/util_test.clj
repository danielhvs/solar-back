(ns servidor.util-test
  (:require
    [clojure.test :refer :all]
    [servidor.util :as u]))

(deftest parse-float
  (let [normaliza #(String/valueOf %)]
    (testing "parse-float passing ,"
      (is (= (normaliza 1234.54)
             (normaliza (u/parse-float "1.234,54"))))
      (is (= (normaliza 1.234)
             (normaliza (u/parse-float "1,234"))))
      (is (= (normaliza 1.23)
             (normaliza (u/parse-float "1,23")))))
    (testing "apenas para aldo, nao para o front-end-solar"
      (is (not= (normaliza 1.234)
                (normaliza (u/parse-float "1.234")))))))

