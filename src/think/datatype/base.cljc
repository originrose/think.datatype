(ns think.datatype.base
  "Datatype library primitives shared between clojurescript and clojure.
Contains:
 - base protocols
  - functions that operate purely at the protocol level."
  #?(:clj (:require [think.datatype.base-macros :as dt-macros])
     :cljs (:require-macros [think.datatype.base-macros :as dt-macros])))


(def datatypes
  [:byte
   :short
   :int
   :long
   :float
   :double])

(def datatype-sizes
  [1
   2
   4
   8
   4
   8])

(def datatype-size-map
  (into {} (map vec (partition 2 (interleave datatypes datatype-sizes)))))

(defn datatype->byte-size
  ^long [datatype] (get datatype-size-map datatype))

(defprotocol PDatatype
  (get-datatype [item]))

#?(:cljs
   (do
    (defn- alloc-buffer
      [datatype elem-count]
      (new js/ArrayBuffer (* (datatype->byte-size datatype)
                             elem-count)))

    (defn- normalize-elem-count-or-seq
      "returns [elem-count data-seq]
  where data-seq may be nil."
      [elem-count-or-seq]
      (if (number? elem-count-or-seq)
        [(long elem-count-or-seq) nil]
        (let [array-data (new js/Array)]
          (doseq [item elem-count-or-seq]
            (.push array-data item))
          [(count array-data) array-data])))

    (defn- setup-array
      [elem-count-or-seq constructor dtype]
      (let [[data-len data-buf] (normalize-elem-count-or-seq elem-count-or-seq)
            buffer (alloc-buffer dtype data-len)
            retval (constructor buffer)]
        (when data-buf
          (loop [idx 0]
            (when (< idx data-len)
              (aset retval idx (aget data-buf idx))
              (recur (inc idx)))))
        retval))

    (defn byte-array
      [elem-count-or-seq]
      (setup-array elem-count-or-seq #(new js/Int8Array %) :byte))

    (defn short-array
      [elem-count-or-seq]
      (setup-array elem-count-or-seq #(new js/Int16Array %) :short))

    (defn int-array
      [elem-count-or-seq]
      (setup-array elem-count-or-seq #(new js/Int32Array %) :int))

    (defn long-array
      [elem-count-or-seq]
      (throw (ex-info "No int64 support in js" {})))

    (defn float-array
      [elem-count-or-seq]
      (setup-array elem-count-or-seq #(new js/Float32Array %) :float))

    (defn double-array
      [elem-count-or-seq]
      (setup-array elem-count-or-seq #(new js/Float64Array %) :double))))


(defn make-array-of-type
  [datatype elem-count-or-seq]
  (dt-macros/try-catch-any
    (cond
      (= datatype :byte) (byte-array elem-count-or-seq)
      (= datatype :short) (short-array elem-count-or-seq)
      (= datatype :int) (int-array elem-count-or-seq)
      (= datatype :long) (long-array elem-count-or-seq)
      (= datatype :float) (float-array elem-count-or-seq)
      (= datatype :double) (double-array elem-count-or-seq)
      :else
      (throw (ex-info "Unknown datatype in make-array-of-type"
                      {:datatype datatype})))

    e (ex-info "make-array-of-type failed"
                {:datatype datatype
                 :elem-count-or-seq elem-count-or-seq
                 :error e})))
