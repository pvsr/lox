(import ./scanner)
(import ./parser)
(import ./repl)

(defn- process [contents]
  (def tokens (scanner/scan contents))
  (pp tokens)
  (def parser (parser/make-parser tokens))
  (def stmts (parser/parse parser))
  (pp stmts))

(defn main [_ &opt path & args]
  (unless (empty? args) (error "expected 0 or 1 args"))
  (if (nil? path)
    (repl/run process)
    (process (slurp path))))
