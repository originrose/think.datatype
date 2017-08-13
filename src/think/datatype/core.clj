(ns think.datatype.core
  "Generalized efficient manipulations of sequences of primitive datatype.
Includes specializations for java arrays, array views (subsection of an array)
and nio buffers.  There are specializations to allow implementations to provide
efficient full typed copy functions when the types can be ascertained.  Usually
this involves a double-dispatch on both the src and dest arguments:

  https://en.wikipedia.org/wiki/Double_dispatch.

  Generic operations include:
  1. datatype of this sequence.
  2. Writing to, reading from.
  3. Construction.
  4. Efficient mutable copy from one sequence to another."

  (:require [clojure.core.matrix.macros :refer [c-for]]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix :as m]
            [think.datatype.marshal :as marshal]
            [think.datatype.base :as base])
  (:import [java.nio ByteBuffer ShortBuffer IntBuffer LongBuffer
            FloatBuffer DoubleBuffer Buffer]
           [mikera.arrayz INDArray]
           [think.datatype DoubleArrayView FloatArrayView
            LongArrayView IntArrayView ShortArrayView ByteArrayView]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defn get-datatype
  [item]
  (base/get-datatype item))


(defn make-array-of-type
  [datatype elem-count-or-seq]
  (base/make-array-of-type datatype elem-count-or-seq))


(defn ecount
  [item]
  (base/ecount item))


(defn ->view
  [& args]
  (apply base/->view args))


(defn make-view
  [datatype item-count-or-seq]
  (base/make-view datatype item-count-or-seq))


(defn copy!
  [& args]
  (apply base/copy! args))


(defn generic-indexed-copy!
  [& args]
  (apply base/generic-indexed-copy! args))


(extend-protocol base/PDatatype
  ByteBuffer
  (get-datatype [item] :byte)
  ShortBuffer
  (get-datatype [item] :short)
  IntBuffer
  (get-datatype [item] :int)
  LongBuffer
  (get-datatype [item] :long)
  FloatBuffer
  (get-datatype [item] :float)
  LongBuffer
  (get-datatype [item] :long)
  Byte
  (get-datatype [item] :byte)
  Short
  (get-datatype [item] :short)
  Integer
  (get-datatype [item] :int)
  Long
  (get-datatype [item] :long)
  Float
  (get-datatype [item] :float)
  Double
  (get-datatype [item] :double))

(def datatype->primitive-type-map
  {:byte Byte/TYPE
   :short Short/TYPE
   :int Integer/TYPE
   :long Long/TYPE
   :float Float/TYPE
   :double Double/TYPE})

(defn datatype->primitive-type
  [datatype]
  (get datatype->primitive-type-map datatype))


(defprotocol PBufferWrap ;;Conversion to nio buffer sharing underlying data
  (buffer-wrap-impl [item offset length]))

(defprotocol PArrayInfo
  (is-primitive-array? [item]))

(defprotocol PCopyToItemDirect
  "Fast paths for when the types match.  When they don't the slower
get/set value path is used."
  (copy-to-buffer-direct! [item item-offset dest dest-offset elem-count])
  (copy-to-array-direct! [item item-offset dest dest-offset elem-count]))

(defprotocol PCopyQueryDirect
  "Return a direct copy function for this dest datatype.
The function signature will be:
(copy-fn! item item-offset elem-count)."
  (get-direct-copy-fn [dest dest-offset]))


(defmacro copy-array-to-array-impl
  [item item-offset dest dest-offset elem-count]
  `(let [~item-offset (long ~item-offset)
         ~dest-offset (long ~dest-offset)
         ~elem-count (long ~elem-count)]
     (loop [~'idx 0]
       (when (< ~'idx ~elem-count)
         (aset ~dest (+ ~'idx ~dest-offset) (aget ~item (+ ~'idx ~item-offset)))
         (recur (inc ~'idx))))
     ~dest))


(defmacro copy-array-to-buffer-impl
  [item item-offset dest dest-offset elem-count]
  `(let [~item-offset (long ~item-offset)
         ~dest-offset (long ~dest-offset)
         ~elem-count (long ~elem-count)
         dest# (.duplicate ~dest)]
     (.position dest# ~dest-offset)
     (.put dest# ~item ~item-offset ~elem-count)
     ~dest))

(defmacro set-array-constant-impl
  [item offset value cast-fn elem-count]
  `(let [~value (~cast-fn ~value)
         elem-count# (long ~elem-count)
         offset# (long ~offset)]
     (c-for [idx# 0 (< idx# elem-count#) (inc idx#)]
            (aset ~item (+ offset# idx#) ~value))))

(defmacro array-copy-query-impl
  [dest dest-offset]
  `(fn [item# item-offset# elem-count#]
     (copy-to-array-direct! item# item-offset#
                            ~dest ~dest-offset
                            elem-count#)))


(extend-type ByteArrayView
  base/PDatatype
  (get-datatype [item] :byte)
  PBufferWrap
  (buffer-wrap-impl [item n-offset n-length] (ByteBuffer/wrap (.data item)
                                                           (+ (.offset item) (int n-offset))
                                                           (int n-length)))
  PCopyQueryDirect
  (get-direct-copy-fn [dest dest-offset]
    (array-copy-query-impl (.data dest) (+ (long dest-offset) (.offset dest))))
  base/PCopyQueryIndirect
  (get-indirect-copy-fn [dest dest-offset]
    (marshal/get-copy-to-fn (.data dest) (+ (long dest-offset) (.offset dest))))
  base/PAccess
  (set-value! [item in-offset value] (aset (.data item) (+ (long in-offset)
                                                           (.offset item)) (byte value)))
  (set-constant! [item in-offset value elem-count]
    (set-array-constant-impl (.data item) (+ (long in-offset) (.offset item))
                             value byte elem-count))
  (get-value [item in-offset] (aget (.data item) (+ (.offset item) (long in-offset))))
  mp/PElementCount
  (element-count [item] (.length item))
  base/PView
  (->view-impl [item n-offset length] (ByteArrayView. (.data item)
                                                      (+ (.offset item) (long n-offset))
                                                      length)))

(extend-type ShortArrayView
  base/PDatatype
  (get-datatype [item] :short)
  PBufferWrap
  (buffer-wrap-impl [item n-offset len] (ShortBuffer/wrap (.data item) (+ (.offset item)
                                                                          (long n-offset)) len))
  PCopyQueryDirect
  (get-direct-copy-fn [item dest-offset]
    (array-copy-query-impl (.data item) (+ (long dest-offset) (.offset item))))
  base/PCopyQueryIndirect
  (get-indirect-copy-fn [dest dest-offset]
    (marshal/get-copy-to-fn (.data dest) (+ (long dest-offset) (.offset dest))))
  base/PAccess
  (set-value! [item in-offset value] (aset (.data item) (+ (long in-offset) (.offset item))
                                           (short value)))
  (set-constant! [item in-offset value elem-count]
    (set-array-constant-impl (.data item) (+ (long in-offset) (.offset item))
                             value short elem-count))
  (get-value [item in-offset] (aget (.data item) (+ (.offset item) (long in-offset))))
  mp/PElementCount
  (element-count [item] (.length item))
  base/PView
  (->view-impl [item n-offset length] (ShortArrayView. (.data item) (+ (.offset item)
                                                                       (long n-offset))
                                                       length)))

(extend-type IntArrayView
  base/PDatatype
  (get-datatype [item] :int)
  PBufferWrap
  (buffer-wrap-impl [item n-offset len] (IntBuffer/wrap (.data item) (+ (long n-offset)
                                                                        (.offset item)) len))
  PCopyQueryDirect
  (get-direct-copy-fn [item dest-offset]
    (array-copy-query-impl (.data item) (+ (long dest-offset) (.offset item))))
  base/PCopyQueryIndirect
  (get-indirect-copy-fn [dest dest-offset]
    (marshal/get-copy-to-fn (.data dest) (+ (long dest-offset) (.offset dest))))
  base/PAccess
  (set-value! [item in-offset value] (aset (.data item) (+ (long in-offset) (.offset item))
                                           (int value)))
  (set-constant! [item in-offset value elem-count]
    (set-array-constant-impl (.data item) (+ (long in-offset) (.offset item))
                             value int elem-count))
  (get-value [item in-offset] (aget (.data item) (+ (.offset item) (long in-offset))))
  mp/PElementCount
  (element-count [item] (.length item))
  base/PView
  (->view-impl [item n-offset length] (IntArrayView. (.data item) (+ (.offset item)
                                                                     (long n-offset)) length)))

(extend-type LongArrayView
  base/PDatatype
  (get-datatype [item] :long)
  PBufferWrap
  (buffer-wrap-impl [item n-offset len] (LongBuffer/wrap (.data item) (+ (.offset item)
                                                                         (long n-offset)) len))
  PCopyQueryDirect
  (get-direct-copy-fn [item dest-offset]
    (array-copy-query-impl (.data item) (+ (long dest-offset) (.offset item))))
  base/PCopyQueryIndirect
  (get-indirect-copy-fn [dest dest-offset]
    (marshal/get-copy-to-fn (.data dest) (+ (long dest-offset) (.offset dest))))
  base/PAccess
  (set-value! [item in-offset value] (aset (.data item) (+ (long in-offset) (.offset item))
                                           (long value)))
  (set-constant! [item in-offset value elem-count]
    (set-array-constant-impl (.data item) (+ (long in-offset) (.offset item)) value
                             long elem-count))
  (get-value [item in-offset] (aget (.data item) (+ (.offset item) (long in-offset))))
  mp/PElementCount
  (element-count [item] (.length item))
  base/PView
  (->view-impl [item n-offset length] (LongArrayView. (.data item) (+ (.offset item)
                                                                      (long n-offset)) length)))

(extend-type FloatArrayView
  base/PDatatype
  (get-datatype [item] :float)
  PBufferWrap
  (buffer-wrap-impl [item n-offset len] (FloatBuffer/wrap (.data item) (+ (.offset item)
                                                                          (long n-offset))
                                                          len))
  PCopyQueryDirect
  (get-direct-copy-fn [item dest-offset]
    (array-copy-query-impl (.data item) (+ (long dest-offset) (.offset item))))
  base/PCopyQueryIndirect
  (get-indirect-copy-fn [dest dest-offset]
    (marshal/get-copy-to-fn (.data dest) (+ (long dest-offset) (.offset dest))))
  base/PAccess
  (set-value! [item in-offset value] (aset (.data item) (+ (long in-offset) (.offset item))
                                           (float value)))
  (set-constant! [item in-offset value elem-count]
    (set-array-constant-impl (.data item) (+ (long in-offset) (.offset item)) value
                             float elem-count))
  (get-value [item in-offset] (aget (.data item) (+ (.offset item) (long in-offset))))
  mp/PElementCount
  (element-count [item] (.length item))
  base/PView
  (->view-impl [item n-offset length] (FloatArrayView. (.data item) (+ (.offset item)
                                                                       (long n-offset)) length)))

(extend-type DoubleArrayView
  base/PDatatype
  (get-datatype [item] :double)
  PBufferWrap
  (buffer-wrap-impl [item n-offset len] (DoubleBuffer/wrap (.data item) (+ (long n-offset)
                                                                           (.offset item)) len))
  PCopyQueryDirect
  (get-direct-copy-fn [item dest-offset]
    (array-copy-query-impl (.data item) (+ (long dest-offset) (.offset item))))
  base/PCopyQueryIndirect
  (get-indirect-copy-fn [dest dest-offset]
    (marshal/get-copy-to-fn (.data dest) (+ (long dest-offset) (.offset dest))))
  base/PAccess
  (set-value! [item in-offset value] (aset (.data item) (+ (long in-offset) (.offset item))
                                           (double value)))
  (set-constant! [item in-offset value elem-count]
    (set-array-constant-impl (.data item) (+ (long in-offset) (.offset item)) value
                             double elem-count))
  (get-value [item in-offset] (aget (.data item) (+ (.offset item) (long in-offset))))
  mp/PElementCount
  (element-count [item] (.length item))
  base/PView
  (->view-impl [item n-offset n-length] (DoubleArrayView. (.data item) (+ (.offset item)
                                                                          (long n-offset))
                                                          n-length)))


;;Macros to use to use the sub views as efficiently as one uses arrays.
(defmacro v-aset
  [array-view item-offset value]
  `(.set ~array-view ~item-offset ~value))

(defmacro v-aget
  [array-view item-offset]
  `(.get ~array-view ~item-offset))

(defmacro v-aset-rem
  [array-view item-offset value]
  `(.set ~array-view
         (rem ~item-offset (.length ~array-view))
         ~value))

(defmacro v-aget-rem
  [array-view item-offset]
  `(aget (.data ~array-view)
         (+ (.offset ~array-view)
            (rem ~item-offset
                 (.length ~array-view)))))

(defmacro v-alength
  [array-view]
  `(.length ~array-view))

(extend-type (Class/forName "[B")
  base/PDatatype
  (get-datatype [item] :byte)
  PBufferWrap
  (buffer-wrap-impl [ary offset len] (ByteBuffer/wrap ^bytes ary (int offset) (int len)))
  PArrayInfo
  (is-primitive-array? [ary] true)
  PCopyQueryDirect
  (get-direct-copy-fn [dest dest-offset]
    (array-copy-query-impl dest dest-offset))
  base/PCopyQueryIndirect
  (get-indirect-copy-fn [dest dest-offset]
    (marshal/get-copy-to-fn dest dest-offset))
  PCopyToItemDirect
  (copy-to-array-direct! [item item-offset ^bytes dest dest-offset elem-count]
    (let [^bytes item item]
      (copy-array-to-array-impl item item-offset dest dest-offset elem-count)))
  (copy-to-buffer-direct! [item item-offset ^ByteBuffer dest dest-offset elem-count]
    (let [^bytes item item]
      (copy-array-to-buffer-impl item item-offset dest dest-offset elem-count)))
  base/PAccess
  (set-value! [item ^long offset value] (aset ^bytes item offset (byte value)))
  (set-constant! [item ^long offset value ^long elem-count]
    (let [^bytes item item]
      (set-array-constant-impl item offset value byte elem-count)))
  (get-value [item ^long offset] (aget ^bytes item offset))
  base/PView
  (->view-impl [item offset length] (ByteArrayView. item offset length)))

(extend-type (Class/forName "[S")
  base/PDatatype
  (get-datatype [item] :short)
  PBufferWrap
  (buffer-wrap-impl [ary offset len] (ShortBuffer/wrap ^shorts ary (int offset) (int len)))
  PArrayInfo
  (is-primitive-array? [ary] true)
  PCopyQueryDirect
  (get-direct-copy-fn [dest dest-offset]
    (array-copy-query-impl dest dest-offset))
  base/PCopyQueryIndirect
  (get-indirect-copy-fn [dest dest-offset]
    (marshal/get-copy-to-fn dest dest-offset))
  PCopyToItemDirect
  (copy-to-array-direct! [item item-offset ^shorts dest dest-offset elem-count]
    (let [^shorts item item]
      (copy-array-to-array-impl item item-offset dest dest-offset elem-count)))
  (copy-to-buffer-direct! [item item-offset ^ShortBuffer dest dest-offset elem-count]
    (let [^shorts item item]
      (copy-array-to-buffer-impl item item-offset dest dest-offset elem-count)))
  base/PAccess
  (set-value! [item ^long offset value] (aset ^shorts item offset (short value)))
  (set-constant! [item ^long offset value ^long elem-count]
    (let [^shorts item item]
      (set-array-constant-impl item offset value short elem-count)))
  (get-value [item ^long offset] (aget ^shorts item offset))
  base/PView
  (->view-impl [item offset length] (ShortArrayView. item offset length)))

(extend-type (Class/forName "[I")
  base/PDatatype
  (get-datatype [item] :int)
  PBufferWrap
  (buffer-wrap-impl [ary offset len] (IntBuffer/wrap ^ints ary (int offset) (int len)))
  PArrayInfo
  (is-primitive-array? [ary] true)
  PCopyQueryDirect
  (get-direct-copy-fn [dest dest-offset]
    (array-copy-query-impl dest dest-offset))
  base/PCopyQueryIndirect
  (get-indirect-copy-fn [dest dest-offset]
    (marshal/get-copy-to-fn dest dest-offset))
  PCopyToItemDirect
  (copy-to-array-direct! [item item-offset ^ints dest dest-offset elem-count]
    (let [^ints item item]
      (copy-array-to-array-impl item item-offset dest dest-offset elem-count)))
  (copy-to-buffer-direct! [item item-offset ^IntBuffer dest dest-offset elem-count]
    (let [^ints item item]
      (copy-array-to-buffer-impl item item-offset dest dest-offset elem-count)))
  base/PAccess
  (set-value! [item ^long offset value] (aset ^ints item offset (int value)))
  (set-constant! [item ^long offset value ^long elem-count]
    (let [^ints item item]
      (set-array-constant-impl item offset value int elem-count)))
  (get-value [item ^long offset] (aget ^ints item offset))
  base/PView
  (->view-impl [item offset length] (IntArrayView. item offset length)))

(extend-type (Class/forName "[J")
  base/PDatatype
  (get-datatype [item] :long)
  PBufferWrap
  (buffer-wrap-impl [ary] (LongBuffer/wrap ^long ary))
  PArrayInfo
  (is-primitive-array? [ary] true)
  PCopyQueryDirect
  (get-direct-copy-fn [dest dest-offset]
    (array-copy-query-impl dest dest-offset))
  base/PCopyQueryIndirect
  (get-indirect-copy-fn [dest dest-offset]
    (marshal/get-copy-to-fn dest dest-offset))
  PCopyToItemDirect
  (copy-to-array-direct! [item item-offset ^longs dest dest-offset elem-count]
    (let [^longs item item]
      (copy-array-to-array-impl item item-offset dest dest-offset elem-count)))
  (copy-to-buffer-direct! [item item-offset ^LongBuffer dest dest-offset elem-count]
    (let [^longs item item]
      (copy-array-to-buffer-impl item item-offset dest dest-offset elem-count)))
  base/PAccess
  (set-value! [item ^long offset value] (aset ^longs item offset (long value)))
  (set-constant! [item ^long offset value ^long elem-count]
    (let [^longs item item]
      (set-array-constant-impl item offset value long elem-count)))
  (get-value [item ^long offset] (aget ^longs item offset))
  base/PView
  (->view-impl [item offset length] (LongArrayView. item offset length)))

(extend-type (Class/forName "[F")
  base/PDatatype
  (get-datatype [item] :float)
  PBufferWrap
  (buffer-wrap-impl [ary offset len] (FloatBuffer/wrap ^floats ary (int offset) (int len)))
  PArrayInfo
  (is-primitive-array? [ary] true)
  PCopyQueryDirect
  (get-direct-copy-fn [dest dest-offset]
    (array-copy-query-impl dest dest-offset))
  base/PCopyQueryIndirect
  (get-indirect-copy-fn [dest dest-offset]
    (marshal/get-copy-to-fn dest dest-offset))
  PCopyToItemDirect
  (copy-to-array-direct! [item item-offset ^floats dest dest-offset elem-count]
    (let [^floats item item]
      (copy-array-to-array-impl item item-offset dest dest-offset elem-count)))
  (copy-to-buffer-direct! [item item-offset ^FloatBuffer dest dest-offset elem-count]
    (let [^floats item item]
      (copy-array-to-buffer-impl item item-offset dest dest-offset elem-count)))
  base/PAccess
  (set-value! [item ^long offset value] (aset ^floats item offset (float value)))
  (set-constant! [item ^long offset value ^long elem-count]
    (let [^floats item item]
      (set-array-constant-impl item offset value float elem-count)))
  (get-value [item ^long offset] (aget ^floats item offset))
  base/PView
  (->view-impl [item offset length] (FloatArrayView. item offset length)))

(extend-type (Class/forName "[D")
  base/PDatatype
  (get-datatype [item] :double)
  PBufferWrap
  (buffer-wrap-impl [ary offset len] (DoubleBuffer/wrap ^doubles ary (int offset) (int len)))
  PArrayInfo
  (is-primitive-array? [ary] true)
  PCopyQueryDirect
  (get-direct-copy-fn [dest dest-offset]
    (array-copy-query-impl dest dest-offset))
  base/PCopyQueryIndirect
  (get-indirect-copy-fn [dest dest-offset]
    (marshal/get-copy-to-fn dest dest-offset))
  PCopyToItemDirect
  (copy-to-array-direct! [item item-offset ^doubles dest dest-offset elem-count]
    (let [^doubles item item]
      (copy-array-to-array-impl item item-offset dest dest-offset elem-count)))
  (copy-to-buffer-direct! [item item-offset ^DoubleBuffer dest dest-offset elem-count]
    (let [^doubles item item]
      (copy-array-to-buffer-impl item item-offset dest dest-offset elem-count)))
  base/PAccess
  (set-value! [item ^long offset value] (aset ^doubles item offset (double value)))
  (set-constant! [item ^long offset value ^long elem-count]
    (let [^doubles item item]
      (set-array-constant-impl item offset value double elem-count)))
  (get-value [item ^long offset] (aget ^doubles item offset))
  base/PView
  (->view-impl [item offset length] (DoubleArrayView. item offset length)))

(defn make-buffer
  ([datatype elem-count-or-seq]
   (let [retval-ary (base/make-array-of-type datatype elem-count-or-seq)]
    (buffer-wrap-impl retval-ary 0 (m/ecount retval-ary)))))

(defn datatype->cast-fn
  [datatype]
  (cond
    (= datatype :byte) byte
    (= datatype :short) short
    (= datatype :int) int
    (= datatype :long) long
    (= datatype :float) float
    (= datatype :double) double))

(defn cast-to
  "cast to. boxes object if datatype is runtime variable"
  [value datatype]
  ((datatype->cast-fn datatype) value))

(defmacro cast-to-m
  "cast to, potentially keep unboxed if datatype is known at compile time"
  [value datatype]
  (cond
    (= datatype :byte) `(byte ~value)
    (= datatype :short) `(short ~value)
    (= datatype :int) `(int ~value)
    (= datatype :long) `(long ~value)
    (= datatype :float) `(float ~value)
    (= datatype :double) `(double ~value)))

(defmacro set-buffer-constant-impl
  [buffer offset value cast-fn elem-count]
  `(let [~value (~cast-fn ~value)]
     (loop [idx# 0]
       (when (< idx# ~elem-count)
         (.put ~buffer (+ ~offset idx#) ~value)
         (recur (inc idx#))))))

(defprotocol PBufferInfo
  (is-nio-buffer? [item]))

(defmacro copy-buffer-to-array-impl
  [item item-offset dest dest-offset elem-count]
  `(let [~item-offset (long ~item-offset)
         ~dest-offset (long ~dest-offset)
         ~elem-count (long ~elem-count)]
     (c-for [idx# 0 (< idx# ~elem-count) (inc idx#)]
            (aset ~dest (+ ~dest-offset idx#)
                  (.get ~item (+ ~item-offset idx#))))
     ~dest))


(defmacro copy-buffer-to-buffer-impl
  [item item-offset dest dest-offset elem-count]
  `(let [~item-offset (long ~item-offset)
         ~dest-offset (long ~dest-offset)
         ~elem-count (long ~elem-count)]
     (c-for [idx# 0 (< idx# ~elem-count) (inc idx#)]
            (.put ~dest (+ ~dest-offset idx#)
                  (.get ~item (+ ~item-offset idx#))))
     ~dest))

(defmacro buffer-copy-query-impl
  [dest dest-offset]
  `(fn [item# item-offset# elem-count#]
     (copy-to-buffer-direct! item# item-offset#
                             ~dest ~dest-offset
                             elem-count#)))

(defmacro nio-offset-impl
  [buffer start-offset length]
  `(let [temp# (.slice ~buffer)
         start-offset# (+ (long ~start-offset) (.position ~buffer))
         length# (long ~length)]
      (.position temp# start-offset#)
      (.limit temp# (+ start-offset# length#))
      (.slice temp#)))


(defprotocol PBufferOffset
  (offset-buffer [buffer start-offset length]))


(extend-type Buffer
  base/PCopyQueryIndirect
  (get-indirect-copy-fn [dest dest-offset]
    (marshal/get-copy-to-fn dest dest-offset)))


(extend-type ByteBuffer
  PBufferInfo
  (is-nio-buffer? [item] true)
  base/PDatatype
  (get-datatype [item] :byte)
  base/PAccess
  (set-value! [item ^long offset value] (.put ^ByteBuffer item offset (byte value)))
  (set-constant! [item ^long offset value ^long elem-count]
    (let [^ByteBuffer item item]
      (set-buffer-constant-impl item offset value byte elem-count)))
  (get-value [item ^long offset] (.get ^ByteBuffer item offset))
  PCopyQueryDirect
  (get-direct-copy-fn [dest dest-offset]
    (buffer-copy-query-impl dest dest-offset))
  PCopyToItemDirect
  (copy-to-array-direct! [item item-offset ^bytes dest dest-offset elem-count]
    (copy-buffer-to-array-impl item item-offset dest dest-offset elem-count))
  (copy-to-buffer-direct! [item item-offset ^ByteBuffer dest dest-offset elem-count]
    (copy-buffer-to-buffer-impl item item-offset dest dest-offset elem-count))
  PBufferOffset
  (offset-buffer [buffer start-offset length]
    (nio-offset-impl buffer start-offset length)))

(extend-type ShortBuffer
  PBufferInfo
  (is-nio-buffer? [item] true)
  base/PDatatype
  (get-datatype [item] :short)
  base/PAccess
  (set-value! [item ^long offset value] (.put ^ShortBuffer item offset (short value)))
  (set-constant! [item ^long offset value ^long elem-count]
    (let [^ShortBuffer item item]
      (set-buffer-constant-impl item offset value short elem-count)))
  (get-value [item ^long offset] (.get ^ShortBuffer item offset))
  PCopyQueryDirect
  (get-direct-copy-fn [dest dest-offset]
    (buffer-copy-query-impl dest dest-offset))
  PCopyToItemDirect
  (copy-to-array-direct! [item item-offset ^shorts dest dest-offset elem-count]
    (copy-buffer-to-array-impl item item-offset dest dest-offset elem-count))
  (copy-to-buffer-direct! [item item-offset ^ShortBuffer dest dest-offset elem-count]
    (copy-buffer-to-buffer-impl item item-offset dest dest-offset elem-count))
  PBufferOffset
  (offset-buffer [buffer start-offset length]
    (nio-offset-impl buffer start-offset length)))

(extend-type IntBuffer
  PBufferInfo
  (is-nio-buffer? [item] true)
  base/PDatatype
  (get-datatype [item] :int)
  base/PAccess
  (set-value! [item ^long offset value] (.put ^IntBuffer item offset (int value)))
  (set-constant! [item ^long offset value ^long elem-count]
    (let [^IntBuffer item item]
      (set-buffer-constant-impl item offset value int elem-count)))
  (get-value [item ^long offset] (.get ^IntBuffer item offset))
  PCopyQueryDirect
  (get-direct-copy-fn [dest dest-offset]
    (buffer-copy-query-impl dest dest-offset))
  PCopyToItemDirect
  (copy-to-array-direct! [item item-offset ^ints dest dest-offset elem-count]
    (copy-buffer-to-array-impl item item-offset dest dest-offset elem-count))
  (copy-to-buffer-direct! [item item-offset ^IntBuffer dest dest-offset elem-count]
    (copy-buffer-to-buffer-impl item item-offset dest dest-offset elem-count))
  PBufferOffset
  (offset-buffer [buffer start-offset length]
    (nio-offset-impl buffer start-offset length)))


(extend-type LongBuffer
  PBufferInfo
  (is-nio-buffer? [item] true)
  base/PDatatype
  (get-datatype [item] :long)
  base/PAccess
  (set-value! [item ^long offset value] (.put ^LongBuffer item offset (long value)))
  (set-constant! [item ^long offset value ^long elem-count]
    (let [^LongBuffer item item]
      (set-buffer-constant-impl item offset value long elem-count)))
  (get-value [item ^long offset] (.get ^LongBuffer item offset))
  PCopyQueryDirect
  (get-direct-copy-fn [dest dest-offset]
    (buffer-copy-query-impl dest dest-offset))
  PCopyToItemDirect
  (copy-to-array-direct! [item item-offset ^longs dest dest-offset elem-count]
    (copy-buffer-to-array-impl item item-offset dest dest-offset elem-count))
  (copy-to-buffer-direct! [item item-offset ^LongBuffer dest dest-offset elem-count]
    (copy-buffer-to-buffer-impl item item-offset dest dest-offset elem-count))
  PBufferOffset
  (offset-buffer [buffer start-offset length]
    (nio-offset-impl buffer start-offset length)))


(extend-type FloatBuffer
  PBufferInfo
  (is-nio-buffer? [item] true)
  base/PDatatype
  (get-datatype [item] :float)
  base/PAccess
  (set-value! [item ^long offset value] (.put ^FloatBuffer item offset (float value)))
  (set-constant! [item ^long offset value ^long elem-count]
    (let [^FloatBuffer item item]
      (set-buffer-constant-impl item offset value float elem-count)))
  (get-value [item ^long offset] (.get ^FloatBuffer item offset))
  PCopyQueryDirect
  (get-direct-copy-fn [dest dest-offset]
    (buffer-copy-query-impl dest dest-offset))
  PCopyToItemDirect
  (copy-to-array-direct! [item item-offset ^floats dest dest-offset elem-count]
    (copy-buffer-to-array-impl item item-offset dest dest-offset elem-count))
  (copy-to-buffer-direct! [item item-offset ^FloatBuffer dest dest-offset elem-count]
    (copy-buffer-to-buffer-impl item item-offset dest dest-offset elem-count))
  PBufferOffset
  (offset-buffer [buffer start-offset length]
    (nio-offset-impl buffer start-offset length)))


(extend-type DoubleBuffer
  PBufferInfo
  (is-nio-buffer? [item] true)
  base/PDatatype
  (get-datatype [item] :double)
  base/PAccess
  (set-value! [item ^long offset value] (.put ^DoubleBuffer item offset (double value)))
  (set-constant! [item ^long offset value ^long elem-count]
    (let [^DoubleBuffer item item]
      (set-buffer-constant-impl item offset value double elem-count)))
  (get-value [item ^long offset] (.get ^DoubleBuffer item offset))
  PCopyQueryDirect
  (get-direct-copy-fn [dest dest-offset]
    (buffer-copy-query-impl dest dest-offset))
  PCopyToItemDirect
  (copy-to-array-direct! [item item-offset ^doubles dest dest-offset elem-count]
    (copy-buffer-to-array-impl item item-offset dest dest-offset elem-count))
  (copy-to-buffer-direct! [item item-offset ^DoubleBuffer dest dest-offset elem-count]
    (copy-buffer-to-buffer-impl item item-offset dest dest-offset elem-count))
  PBufferOffset
  (offset-buffer [buffer start-offset length]
    (nio-offset-impl buffer start-offset length)))


(extend-type Buffer
  mp/PElementCount
  (element-count [item] (.remaining item)))


(defn generic-copy!
  [item item-offset dest dest-offset elem-count]
  (let [item-offset (long item-offset)
        dest-offset (long dest-offset)
        elem-count (long elem-count)]
    (c-for [idx 0 (< idx elem-count) (inc idx)]
           (base/set-value! dest (+ dest-offset idx)
                            (base/get-value item (+ item-offset idx))))
    dest))


(extend-type Object
  PArrayInfo
  (is-primitive-array? [ary] false)
  PBufferInfo
  (is-nio-buffer? [item] false)
  PCopyQueryDirect
  (get-direct-copy-fn [dest dest-offset] #(generic-copy! %1 %2 dest dest-offset %3))
  base/PCopyQueryIndirect
  (get-indirect-copy-fn [dest dest-offset] #(generic-copy! %1 %2 dest dest-offset %3))
  PCopyToItemDirect
  (copy-to-array-direct! [item item-offset dest dest-offset elem-count]
    (generic-copy! item item-offset dest dest-offset elem-count))
  (copy-to-buffer-direct! [item item-offset dest dest-offset elem-count]
    (generic-copy! item item-offset dest dest-offset elem-count)))


(defn copy->array
  ([src src-offset elem-count]
   (let [retval (base/make-array-of-type (get-datatype src) elem-count)]
     (base/copy! src src-offset retval 0 elem-count)))
  ([src]
   (copy->array src 0 (m/ecount src))))

(defn copy->buffer
  ([src src-offset elem-count]
   (let [retval (make-buffer (get-datatype src) (m/ecount src))]
     (base/copy! src src-offset retval 0 elem-count)))
  ([src]
   (copy->buffer src 0 (ecount src))))


(defprotocol PCopyRawDataToArray
  "Given a sequence of data copy it as fast as possible into a target item."
  (copy-raw->item! [raw-data ary-target target-offset]))

(defn copy-raw-seq->item!
  [raw-data-seq ary-target target-offset]
  (reduce (fn [[ary-target target-offset] new-raw-data]
            (copy-raw->item! new-raw-data ary-target target-offset))
          [ary-target target-offset]
          raw-data-seq))


(defn- raw-dtype-copy!
  [raw-data ary-target ^long target-offset]
  (base/copy! raw-data 0 ary-target target-offset (ecount raw-data))
  [ary-target (+ target-offset ^long (ecount raw-data))])


(extend-protocol PCopyRawDataToArray
  Number
  (copy-raw->item! [raw-data ary-target ^long target-offset]
    (base/set-value! ary-target target-offset raw-data)
    [ary-target (+ target-offset 1)])
  clojure.lang.PersistentVector
  (copy-raw->item! [raw-data ary-target ^long target-offset]
    (let [num-elems (count raw-data)]
     (if (= 0 num-elems)
       [ary-target target-offset]
       (if (number? (raw-data 0))
         (do
          (c-for [idx 0 (< idx num-elems) (inc idx)]
                 (base/set-value! ary-target (+ idx target-offset) (raw-data idx)))
          [ary-target (+ target-offset num-elems)])
         (copy-raw-seq->item! raw-data ary-target target-offset)))))
  clojure.lang.ISeq
  (copy-raw->item! [raw-data ary-target target-offset]
    (copy-raw-seq->item! raw-data ary-target target-offset))
  Buffer
  (copy-raw->item! [raw-data ary-target target-offset]
    (raw-dtype-copy! raw-data ary-target target-offset))
  INDArray
  (copy-raw->item! [raw-data ary-target target-offset]
    (let [^doubles item-data (or (mp/as-double-array raw-data)
                                 (mp/to-double-array raw-data))]
      (copy-raw->item! item-data ary-target target-offset))))


(extend-type (Class/forName "[D")
  PCopyRawDataToArray
  (copy-raw->item! [raw-data ary-target target-offset]
    (raw-dtype-copy! raw-data ary-target target-offset)))


(extend-type (Class/forName "[F")
  PCopyRawDataToArray
  (copy-raw->item! [raw-data ary-target target-offset]
    (raw-dtype-copy! raw-data ary-target target-offset)))


(extend-type (Class/forName "[J")
  PCopyRawDataToArray
  (copy-raw->item! [raw-data ary-target target-offset]
    (raw-dtype-copy! raw-data ary-target target-offset)))


(extend-type (Class/forName "[I")
  PCopyRawDataToArray
  (copy-raw->item! [raw-data ary-target target-offset]
    (raw-dtype-copy! raw-data ary-target target-offset)))


(extend-type (Class/forName "[S")
  PCopyRawDataToArray
  (copy-raw->item! [raw-data ary-target target-offset]
    (raw-dtype-copy! raw-data ary-target target-offset)))


(extend-type (Class/forName "[B")
  PCopyRawDataToArray
  (copy-raw->item! [raw-data ary-target target-offset]
    (raw-dtype-copy! raw-data ary-target target-offset)))


;;Add the overloads for object for marshal-type copies.
(defmacro generic-copy-impl
  [dest-type cast-type-fn copy-to-dest-fn cast-fn]
  `[(keyword (name ~copy-to-dest-fn)) generic-copy!])


(defmacro generic-indexed-copy-impl
  [dest-type cast-type-fn copy-to-dest-fn cast-fn]
  `[(keyword (name ~copy-to-dest-fn)) base/generic-indexed-copy!])


(extend Object
  marshal/PCopyToArray
  (->> (marshal/array-type-iterator generic-copy-impl)
       (into {}))
  marshal/PCopyToBuffer
  (->> (marshal/buffer-type-iterator generic-copy-impl)
       (into {}))
  marshal/PIndexedTypeToCopyToFn
  {:get-indexed-copy-to-fn (fn [dest dest-offset]
                             #(base/generic-indexed-copy! %1 %2 %3 dest dest-offset %4))}
  marshal/PIndexedCopyToArray
  (->> (marshal/indexed-array-type-iterator generic-indexed-copy-impl)
       (into {}))
  marshal/PIndexedCopyToBuffer
  (->> (marshal/indexed-buffer-type-iterator generic-indexed-copy-impl)
       (into {})))


(defn ->int-buffer
  "As efficiently as possible, ensure data is an int buffer."
  ^ints [data]
  (if (instance? (Class/forName "[I") data)
    data
    (int-array data)))


(defn indexed-copy!
  "Indirect copy function where src and dest indexes are provided."
  ([src src-offset src-indexes dest dest-offset dest-indexes n-elems-per-idx]
   (let [src-indexes (->int-buffer src-indexes)
         dest-indexes (->int-buffer dest-indexes)
         n-elems (alength src-indexes)
         src-offset (long src-offset)
         dest-offset (long dest-offset)
         n-elems-per-idx (long n-elems-per-idx)]
     (when-not (= (alength src-indexes)
                  (alength dest-indexes))
       (throw (ex-info "indexed-copy! src and dest index size mismatch"
                       {:src-index-count (alength src-indexes)
                        :dst-index-count (alength dest-indexes)})))
     (if (= 1 n-elems-per-idx)
       ((marshal/get-indexed-copy-to-fn dest dest-offset)
        src src-offset src-indexes dest-indexes)
       (c-for [idx 0 (< idx n-elems) (inc idx)]
              (base/copy! src (+ src-offset (* (aget src-indexes idx) n-elems-per-idx))
                          dest (+ dest-offset (* (aget dest-indexes idx) n-elems-per-idx))
                          n-elems-per-idx)))))
  ([src src-indexes src-offset dest dest-indexes dest-offset]
   (indexed-copy! src src-indexes src-offset dest dest-indexes dest-offset 1))
  ([src src-indexes dest dest-indexes]
   (indexed-copy! src 0 src-indexes dest 0 dest-indexes)))
