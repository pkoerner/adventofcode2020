(ns adventofcode.day18
  (:require [clojure.string]))

(def input18 (clojure.string/split-lines (slurp "input18")))

(defn clojurify [s] (read-string (str \( s \))))
(defn evali [expr]
  (cond (number? expr) expr
        (and (list? expr) (= 1 (count expr))) (evali (first expr))
        :otherwise (let [[a op b & r] expr] (evali (cons ((resolve op) (evali a) (evali b)) r)))))

(reduce + (map (comp evali clojurify) input18))

;; just insert parenthesis where necessary to avoid clever parsing :-)
;; would have used instaparse otherwise
(defn preprocess [expr]
  (cond (number? expr) expr
        (and (list? expr) (= 1 (count expr))) (list (preprocess (first expr)))
        :otherwise (let [[a op b & r] expr] 
                     (if (seq r)
                       (if (= op '+)
                         (preprocess (cons (list a op b) r))
                         (cons (preprocess a) (cons op (preprocess (cons b r)))))
                       (if (= op '+)
                         (list (list (preprocess a) op (preprocess b)))
                         (list (preprocess a) op (preprocess b)))))))

;; debugging code because I messed something up
; (defn spy [x] (prn x) x)
; (defn newl [x] (newline) x)
; 
; (reduce + (map (comp evali newl spy preprocess spy clojurify) input18))

(evali (preprocess (clojurify "1 + (2 * 3) + (4 * (5 + 6))")))
