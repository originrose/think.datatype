(ns think.datatype.base
  "Datatype library primitives shared between clojurescript and clojure.
Contains:
 - base protocols
  - functions that operate purely at the protocol level."
  (:require [clojure.core.matrix :as m])
  #?(:clj (:require [think.datatype.base-macros :as base-macros]
                    [clojure.core.matrix.macros :refer [c-for]]
                    [think.datatype.shared-macros :as shared-macros])
     :cljs (:require-macros [think.datatype.base-macros :as base-macros]
                            [clojure.core.matrix.macros :refer [c-for]]
                            [think.datatype.shared-macros :as shared-macros])))


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
  (base-macros/try-catch-any
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


(defprotocol PAccess
  (set-value! [item offset value])
  (set-constant! [item offset value elem-count])
  (get-value [item offset]))


(defprotocol PView
  (->view-impl [item offset elem-count]))


(defn ->view
  ([item ^long offset ^long elem-count]
   (let [item-ecount (long (m/ecount item))]
     (when-not (>= (- item-ecount offset) elem-count)
       (throw (ex-info "View out of range" {:required-count (+ offset elem-count)
                                            :actual-count item-ecount})))
     (->view-impl item offset elem-count)))
  ([item]
   (->view item 0 (m/ecount item))))


(defn make-view
  [datatype item-count-or-seq]
  (->view (make-array-of-type datatype item-count-or-seq)))


(defn generic-indexed-copy!
  [src src-offset ^ints src-indexes dest dest-offset ^ints dest-indexes n-elems-per-idx]
  (let [elem-count (alength src-indexes)
        src-offset (long src-offset)
        dest-offset (long dest-offset)
        n-elems-per-idx (long n-elems-per-idx)]
    (if (= 1 n-elems-per-idx)
      (c-for [idx 0 (< idx elem-count) (inc idx)]
             (set-value! dest (+ dest-offset (aget dest-indexes idx))
                         (get-value src (+ src-offset (aget src-indexes idx))))))
    dest))


(defn ecount
  ^long [item]
  (m/ecount item))


(defprotocol PCopyQueryIndirect
  "Copy protocol when the types do not match"
  (get-indirect-copy-fn [dest destoffset]))


(defn copy!
  "copy elem-count src items to dest items"
  ([src src-offset dest dest-offset elem-count]
   (let [src-dtype (get-datatype src)
         src-offset (long src-offset)
         dest-dtype (get-datatype dest)
         dest-offset (long dest-offset)
         elem-count (long elem-count)]
     (shared-macros/check-range src src-offset elem-count)
     (shared-macros/check-range dest dest-offset elem-count)
     ((get-indirect-copy-fn dest dest-offset) src src-offset elem-count)
     dest))
  ([src dest]
   (copy! src 0 dest 0 (min (ecount dest) (ecount src)))))
