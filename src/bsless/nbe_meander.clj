(ns bsless.nbe-meander
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
    ('Var ?x) (lookup env ?x)
    ('App ?f ?x) (-apply (-eval ?f env) (-eval ?x env))
    ('Lam ?x ?b) `(~'Closure ~env ~?x ~?b)))

(defn -apply
  [f v]
  (m/match f
    ('Closure ?env ?x ?b) (-eval ?b (ext-env ?env ?x v))
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
    ('NVar ?x) (list 'Var ?x)
    ('NApp ?n ?v) (list 'App (uneval-neutral ?n uenv) (uneval ?v uenv))))

(defn uneval
  [v uenv]
  (m/match v
    ('Closure ?env ?x ?b)
    (let [uenv (fresh uenv ?x)
          x' (get-fresh uenv ?x)]
      (list 'Lam x' (-> ?b
                        (-eval (ext-env ?env ?x (list 'N (list 'NVar x'))))
                        (uneval uenv))))
    ('N ?n) (uneval-neutral ?n uenv)))

(defn nf
  ([expr] (nf expr {}))
  ([expr env] (uneval (-eval expr env) {})))

(let [one (-eval (quote (Lam f (Lam x (App (Var f) (Var x))))) {})
      succ (-eval (quote (Lam n (Lam f (Lam x (App (Var f) (App (App (Var n) (Var f)) (Var x))))))) {})
      pred (-eval (quote (Lam n (Lam f (Lam x (App (App (App (Var n) (Lam g (Lam h (App (Var h) (App (Var g) (Var f)))))) (Lam u (Var x))) (Lam u (Var u))))))) {})
      plus (-eval (quote (Lam m (Lam n (App (App (Var m) (Var succ)) (Var n))))) {'succ succ})
      mult (-eval (quote (Lam m (Lam n (Lam f (App (Var m) (App (Var n) (Var f))))))) {})]
  (println (str (-eval (quote (App (Var succ) (Var one))) {'one one 'succ succ})))
  (println (str (nf (quote (App (Var succ) (Var one))) {'one one 'succ succ})))
  (println (str (nf (quote (App (Var pred) (App (Var succ) (Var one)))) {'one one 'succ succ 'pred pred})))
  (println (str (nf (quote (App (App (Var plus) (Var one)) (Var one))) {'one one 'plus plus})))
  )
