(def- keywords (struct ;(mapcat
                          |[$ (keyword $)]
                          ["and" "class" "else" "false"
                           "for" "fun" "if" "nil"
                           "or" "print" "return" "super"
                           "this" "true" "var" "while"])))

(def- double-operators {"!=" :bang-eq "==" :eq-eq
                        "<=" :less-eq ">=" :greater-eq})

(def- single-operators {"(" :left-paren ")" :right-paren
                        "{" :left-brace "}" :right-brace
                        "-" :minus "+" :plus "*" :star "/" :slash
                        "!" :bang "=" :eq "<" :less ">" :greater
                        "," :comma "." :dot ";" :semicolon})

(defn- get-keyword [word]
  (if-let [kw (get keywords word)] [kw] [:ident word]))

(defn- token [inner make-token]
  ~(/ (* (line) ,inner) ,|{:token (make-token $&) :line $0}))

(defn- token0 [inner tok]
  (token inner (fn [&] [tok])))

(defn- token1 [inner make-token]
  (token inner (fn [[val]] (make-token val))))

(def- grammar
  ~{:keyword-or-ident (<- (* :a :w*))
    :number (<- (* :d+ (? (* "." :d+))))
    :string (* `"` (+ (<- (to `"`)) (error (constant "unterminated string"))) `"`)
    :main (any (+ :s
                  (* "//" (to "\n"))
                  ,;(seq [[str tok] :pairs double-operators] (token0 str tok))
                  ,;(seq [[str tok] :pairs single-operators] (token0 str tok))
                  ,(token1 :keyword-or-ident get-keyword)
                  ,(token1 :number |[:num (scan-number $)])
                  ,(token1 :string |[:str $])))})

(def- peg (peg/compile grammar))

(defn scan [source]
  (peg/match peg source))
