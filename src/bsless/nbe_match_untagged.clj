(ns bsless.nbe-match-untagged
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
   (['clo env x b] :seq) (-eval b (ext-env env x v))
   (['N n] :seq) (list 'N (list 'NApp n v))))

(defn -eval
  [expr env]
  (match expr
   (x :guard symbol?) (lookup env x)
   ([f x] :seq) (-apply (-eval f env) (-eval x env))
   (['fn [x] b] :seq) (list 'clo env x b)))

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
   (['NVar x] :seq) x
   (['NApp u v] :seq) (list (uneval-neutral u uenv) (uneval v uenv))))

(defn uneval
  [v uenv]
  (match v
   (['clo env x b] :seq)
   (let [uenv (fresh uenv x)
         x' (get-fresh uenv x)]
     (list 'fn [x'] (-> b
                       (-eval (ext-env env x (list 'N (list 'NVar x'))))
                       (uneval uenv))))
   (['N n] :seq) (uneval-neutral n uenv)))

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
