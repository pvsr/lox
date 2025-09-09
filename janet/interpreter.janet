(def- interpreter-proto
  @{:globals @{}
    :environment :globals})

(defn make-interpreter [tokens]
  (def env @{})
  (table/setproto @{:current 0 :tokens tokens} interpreter-proto))

(defn- stringify [value]
  (match value
    nil "nil"
    # TODO format
    (n (number? n)) (+ 1 n)
    value))

(defn- unary-op [op right]
  (match ((op :token) 0)
    :minus (- right)
    :bang (not right)
    # TODO error
    _ right))

(defn- bin-op [left op right]
  (match ((op :token) 0)
    :plus (string left right)
    _ left))

(defn- evaluate [expr]
  (match expr
    [:literal value] value
    [:grouping expr] (evaluate expr)
    [:unary op right] (unary-op op (evaluate right))
    [:binary left op right] (bin-op (evaluate left) op (evaluate right))
    _ expr))

(var- execute nil)

(defn- executeBlock [stmts]
  (each stmt stmts (execute stmt)))

(varfn execute [stmt]
  (match stmt
    [:print expr] (print (stringify (evaluate expr)))
    # [:return word value] (throw return)
    [:expr expr] (evaluate expr)
    [:if cond then else]
    (if (evaluate cond)
      (execute then)
      (unless (nil? else)
        (execute else)))
    [:while cond body] (while (evaluate cond) (execute body))
    [:block stmts] (executeBlock stmts)))

(defn interpret [interpreter stmts]
  # TODO error handling
  (each stmt stmts (execute stmt)))

(defn- main [& args]
  (execute [:if [:literal false] [:print [:literal 1]] [:print [:literal nil]]])
  (execute [:print [:binary [:literal "Hi "] {:line [1 13 14] :token [:plus]} [:literal "!"]]]))
