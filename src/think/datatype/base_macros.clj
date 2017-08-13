(ns think.datatype.base-macros)

(defmacro try-catch-any
  [try-body & catch-body]
  `(try
     ~try-body
     (catch Throwable ~(first catch-body)
       ~@(rest catch-body))))
