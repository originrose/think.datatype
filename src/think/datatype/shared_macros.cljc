(ns think.datatype.shared-macros
  (:require [clojure.core.matrix :as m]))


(defmacro check-range
  [item offset elem-count]
  `(when-not (<= (+ ~offset ~elem-count)
                 (m/ecount ~item))
     (throw (ex-info (format "%s offset + n-elems > ecount range violation"
                             ~(name item))
                     {:offset ~offset
                      :n-elems ~elem-count
                      :length (m/ecount ~item)}))))
