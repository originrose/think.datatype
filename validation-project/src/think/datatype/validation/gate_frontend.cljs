(ns think.datatype.validation.gate-frontend
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [think.datatype.base-macros :as dt-macros])
  (:require [cljs.core.async :as async :refer [<!]]
            [reagent.core :refer [atom]]
            [think.gate.core :as gate]
            [think.gate.model :as model]
            [think.datatype.base :as dt-base]))

(enable-console-print!)

(def state* (atom nil))

(defn test-base
  []
  (with-out-str
    (try
      (let [data (dt-base/make-array-of-type :byte [1 2 3 4 5])]
        (println {:array-type (str (type data))
                  :array-value (str data)}))
    (catch :default exception
      (println "Exception!!" exception)))))

(defmethod gate/component "default"
  [& args]
  (fn [& args]
    [:div "Server's aanswer: " (test-base)]))

(defmethod gate/component "otherthing"
  [& args]
  (fn [& args]
    [:div "Some other thing!"]))

(gate/start-frontend)
