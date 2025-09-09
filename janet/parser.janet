(import ./scanner)

(defn- token-type [token] ((token :token) 0))

(def- parser-proto
  @{:peek |(($ :tokens) ($ :current))
    :prev |(($ :tokens) (- ($ :current) 1))
    :eof? |(= (length ($ :tokens)) ($ :current))
    :advance |(++ ($ :current))
    :check (fn [self ty]
             (= ty (token-type (:peek self))))
    :match (fn [self & types]
             (unless (:eof? self)
               (def token (:peek self))
               (when (has-value? types (token-type token))
                 (:advance self)
                 token)))
    :consume (fn [self ty msg]
               (if-let [token (:match self ty)]
                 token (error msg)))})

(defn make-parser [tokens]
  (table/setproto @{:current 0 :tokens tokens} parser-proto))

# fns used out of declaration order
(var- expression nil)
(var- block nil)
(var- expression-statement nil)
(var- statement nil)
(var- declaration nil)

(defn- primary [parser]
  (def token (:match parser :false :true :nil :num :str :ident :left-paren))
  (unless token (errorf "Expect expression: %q" (:peek parser)))
  (match (token :token)
    [:false] [:literal false]
    [:true] [:literal true]
    [:nil] [:literal nil]
    [:num val] [:literal val]
    [:str val] [:literal val]
    [:ident name] [:variable name]
    [:left-paren] (do (def expr (expression parser))
                    (:consume parser :right-paren "Expect ')' after expression.")
                    [:grouping expr])))

(defn- doCall [parser callee]
  (def args @[])
  (unless (:check parser :right-paren)
    (array/push args (expression parser))
    (while (:match parser :comma)
      (when (>= (length args) 255)
        (error "Can't have more than 255 arguments."))
      (array/push args (expression parser))))
  (def paren (:consume parser :right-paren "Expect ')' after arguments."))
  [:call callee paren args])

(defn- call [parser]
  (var expr (primary parser))
  (while (:match parser :left-paren)
    (set expr (doCall parser expr)))
  expr)

(defn- unary [parser]
  (if-let [token (:match parser :bang :minus)]
    (let [op token
          right (unary parser)]
      [:unary op right])
    (call parser)))

(defmacro bin-parse [ty name f & token-types]
  ~(defn- ,name [parser]
     (var expr (,f parser))
     (while (def op (:match parser ,;token-types))
       (def right (,f parser))
       (set expr [,ty expr op right]))
     expr))
(defmacro binary [& args] ~(bin-parse :binary ,;args))
(defmacro logical [& args] ~(bin-parse :logical ,;args))
(binary factor unary :slash :star)
(binary term factor :plus :minus)
(binary comparison term :greater :greater-eq :less :less-eq)
(binary equality comparison :bang-eq :eq-eq)
(logical op-and equality :and)
(logical op-or op-and :or)

(defn- assignment [parser]
  (var expr (op-or parser))
  (when (def equals (:match parser :eq))
    (def value (assignment parser))
    (match expr
      [:variable name] (set expr [:assignment name value])
      # TODO put equals token in error?
      _ (errorf "Invalid assignment target: %M" expr)))
  expr)

(varfn expression [parser] (assignment parser))

(defn- function [parser kind]
  (def name (:consume parser :ident (string/format "Expect %s name" kind)))
  (:consume parser :left-paren (string/format "Expect '(' after %s name" kind))
  (def params @[])
  (unless (:check parser :right-paren)
    (array/push params (:consume parser :ident "Expect parameter name."))
    (while (:match parser :comma)
      (when (>= (length params) 255)
        # TODO token in error
        (error "Can't have more than 255 parameters."))
      (array/push params (:consume parser :ident "Expect parameter name."))))
  (:consume parser :right-paren "Expect ')' after parameters.")
  (:consume parser :left-brace (string/format "Expect '{' before %s body" kind))
  [:fun name params (block parser)])

(defn- var-decl [parser]
  (def name (:consume parser :ident "Expect variable name"))
  (def init (if (:match parser :eq) (expression parser) nil))
  (:consume parser :semicolon "Expect ';' after variable declaration.")
  [:var name init])

(defn- for-statement [parser]
  (:consume parser :left-paren "Expect '(' after 'for'.")
  (def ty (if-let [token (:match parser :semicolon :var)] (token-type token)))
  (def init (case ty
              :semicolon nil
              :var (var-decl parser)
              (expression-statement parser)))
  (def cond (if (:check parser :semicolon) [:literal true] (expression parser)))
  (:consume parser :semicolon "Expect ';' after loop condition.")
  (def incr (unless (:check parser :right-paren) (expression parser)))
  (:consume parser :right-paren "Expect ')' after for clauses.")
  (var body (statement parser))
  (when incr (set body [:block [body [:expr incr]]]))
  (set body [:while cond body])
  (if init [:block [init body]] body))

(defn- if-statement [parser]
  (:consume parser :left-paren "Expect '(' after 'if'.")
  (def cond (expression parser))
  (:consume parser :right-paren "Expect ')' after if condition.")
  (def then (statement parser))
  (def else (if (:match parser :else) (statement parser) nil))
  [:if cond then else])

(defn- print-statement [parser]
  (def value (expression parser))
  (:consume parser :semicolon "Expect ';' after value.")
  [:print value])

(defn- return-statement [parser]
  (def kw (:prev parser))
  (def value (if (:check parser :semicolon)
               nil
               (expression parser)))
  (:consume parser :semicolon "Expect ';' after return value.")
  [:return kw value])

(defn- while-statement [parser]
  (:consume parser :left-paren "Expect '(' after 'while'.")
  (def cond (expression parser))
  (:consume parser :right-paren "Expect ')' after while condition.")
  (def body (statement parser))
  [:while cond body])

(varfn block [parser]
  (def stmts @[])
  (while (not (or (:check parser :right-brace) (:eof? parser)))
    (do (array/push stmts (declaration parser))))
  (:consume parser :right-brace "Expect '}' after block.")
  stmts)

(varfn expression-statement [parser]
  (def expr (expression parser))
  (:consume parser :semicolon "Expect ';' after value.")
  [:expr expr])

(varfn statement [parser]
  (def token (:match parser :for :if :print :return :while :left-brace))
  (def ty (if token (token-type token)))
  (case ty
    :for (for-statement parser)
    :if (if-statement parser)
    :print (print-statement parser)
    :return (return-statement parser)
    :while (while-statement parser)
    :left-brace [:block (block parser)]
    (expression-statement parser)))

(varfn declaration [parser]
  # TODO catch parse errors and synchronize
  (def token (:match parser :fun :var))
  (def ty (if token (token-type token)))
  (match ty
    :fun (function parser :function)
    :var (var-decl parser)
    (statement parser)))

(defn parse [parser]
  (def stmts @[])
  (while (not (:eof? parser))
    (array/push stmts (declaration parser)))
  stmts)
