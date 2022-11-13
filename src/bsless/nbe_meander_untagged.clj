(ns bsless.nbe-meander-untagged
  (:require
   [meander.epsilon :as m]))

(defn lookup
  [env k]
  (or (get env k)
      (throw (ex-info "Var not in env" {:var k :env env}))))

(defn ext-env [env k v] (assoc env k v))

(declare -apply)

(defn -eval
  [expr env]
  (m/match expr
    (m/pred symbol? ?x) (lookup env ?x)
    (?f ?x) (-apply (-eval ?f env) (-eval ?x env))
    ('fn [(m/pred symbol? ?x)] ?b) `(~'clo ~env ~?x ~?b)))

(defn -apply
  [f v]
  (m/match f
    ('clo ?env ?x ?b) (-eval ?b (ext-env ?env ?x v))
    ('N ?n) (list 'N (list 'NApp ?n v))))

(declare -eval -apply)

(defn fresh
  [env x]
  (update env x (fnil inc -1)))

(defn get-fresh
  [env x]
  (symbol (str x (get env x))))

(declare uneval uneval-neutral)

(defn uneval-neutral
  [n uenv]
  (m/match n
    ('NVar ?x) ?x
    ('NApp ?n ?v) (list (uneval-neutral ?n uenv) (uneval ?v uenv))))

(defn uneval
  [v uenv]
  (m/match v
    ('clo ?env ?x ?b)
    (let [uenv (fresh uenv ?x)
          x' (get-fresh uenv ?x)]
      (list 'fn [x'] (-> ?b
                        (-eval (ext-env ?env ?x (list 'N (list 'NVar x'))))
                        (uneval uenv))))
    ('N ?n) (uneval-neutral ?n uenv)))

(defn nf
  ([expr] (nf expr {}))
  ([expr env] (uneval (-eval expr env) {})))

(let [one (-eval (quote (fn [f] (fn [x] (f x)))) {})
      succ (-eval (quote (fn [n] (fn [f] (fn [x] (f ((n f) x)))))) {})
      pred (-eval (quote (fn [n] (fn [f] (fn [x] (((n (fn [g] (fn [h] (h (g f))))) (fn [u] x)) (fn [u] u)))))) {})
      plus (-eval (quote (fn [m] (fn [n] ((m succ) n)))) {'succ succ})
      mult (-eval (quote (fn [m] (fn [n] (fn [f] (m (n f)))))) {})]
  (println (str (-eval (quote (succ one)) {'one one 'succ succ})))
  (println (str (nf (quote (succ one)) {'one one 'succ succ})))
  (println (str (nf (quote (pred (succ one))) {'one one 'succ succ 'pred pred})))
  (println (str (nf (quote ((plus one) one)) {'one one 'plus plus})))
  )
