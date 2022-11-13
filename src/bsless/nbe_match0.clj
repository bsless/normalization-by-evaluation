(ns bsless.nbe-match0
  (:require
   [clojure.core.match :refer [match]]))

(declare -eval -apply)

(defn lookup [env x]
  (or (get env x)
      (throw (ex-info "Unbound variable" {:var x}))))

(defn ext-env
  [env k v]
  (assoc env k v))

(defn -apply
  [f v]
  (match f
   (['Closure env x b] :seq) (-eval b (ext-env env x v))
   (['N n] :seq) (list 'N (list 'NApp n v))))

(defn -eval
  [expr env]
  (match expr
   (['Var x] :seq) (lookup env x)
   (['App f x] :seq) (-apply (-eval f env) (-eval x env))
   (['Lam x b] :seq) (list 'Closure env x b)))

(defn fresh
  [env x]
  (update env x (fnil inc -1)))

(defn get-fresh
  [env x]
  (symbol (str x (get env x))))

(declare uneval uneval-neutral)

(defn uneval-neutral
  [n uenv]
  (match n
   (['NVar x] :seq) (list 'Var x)
   (['NApp u v] :seq) (list 'App (uneval-neutral u uenv) (uneval v uenv))))

(defn uneval
  [v uenv]
  (match v
   (['Closure env x b] :seq)
   (let [uenv (fresh uenv x)
         x' (get-fresh uenv x)]
     (list 'Lam x' (-> b
                       (-eval (ext-env env x (list 'N (list 'NVar x'))))
                       (uneval uenv))))
   (['N n] :seq) (uneval-neutral n uenv)))

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
