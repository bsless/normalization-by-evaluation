(ns bsless.nbc-protocols)

(defn lookup
  [env k]
  (or (get env k)
      (throw (ex-info "Var not in env" {:var k :env env}))))

(defn ext-env [env k v] (assoc env k v))

(defprotocol Eval
  (-eval [expr env]))

(defprotocol Apply
  (-apply [rator rand]))

(defprotocol UnEval
  (-uneval [value uenv]))

;;; Expressions

(defrecord Var [x])

(defrecord App [f x])

(defrecord Lam [x b])

;;; Values

(defrecord Clo [env x b])

;;; Neutrals

(defrecord N [n])

(defrecord NVar [v])

(defrecord NApp [n v])

(extend-protocol Eval
  Var
  (-eval [v env] (lookup env (.-x v)))
  App
  (-eval [app env]
    (-apply (-eval (.-f app) env)
            (-eval (.-x app) env)))
  Lam
  (-eval [lam env]
    (new Clo env (.-x lam) (.-b lam))))

(extend-protocol Apply
  Clo
  (-apply [clo arg]
    (-eval (.-b clo) (ext-env (.-env clo) (.-x clo) arg)))
  N
  (-apply [o v] (new N (new NApp (.-n o) v))))

(defn fresh
  [env x]
  (update env x (fnil inc -1)))

(defn get-fresh
  [env x]
  (symbol (str x (get env x))))

(extend-protocol UnEval
  Clo
  (-uneval [clo uenv]
    (let [uenv (fresh uenv (.-x clo))
          x' (get-fresh uenv (.-x clo))]
      (new Lam x' (-> (.-b clo)
                      (-eval (ext-env (.-env clo) (.-x clo) (new N (new NVar x'))))
                      (-uneval uenv)))))
  N
  (-uneval [o uenv] (-uneval (.-n o) uenv))
  NVar
  (-uneval [v uenv] (new Var (.-v v)))
  NApp
  (-uneval [app uenv]
    (new App (-uneval (.-n app) uenv) (-uneval (.-v app) uenv))))

(defn nf
  ([expr] (nf expr {}))
  ([expr env] (-uneval (-eval expr env) {})))

(def constructors
  {
   'Lam #'->Lam
   'App #'->App
   'Var #'->Var
   })

(defn ana
  [x]
  (cond
    (list? x) (let [[h & t] x]
                (apply (get constructors h) (map ana t)))
    :else x))

(defprotocol UnAna
  (-unana [expr]))

(extend-protocol UnAna
  Lam
  (-unana [lam] (list 'Lam (.-x lam) (-unana (.-b lam))))
  Var
  (-unana [v] (list 'Var (.-x v)))
  App
  (-unana [app]
    (list 'App (-unana (.-f app)) (-unana (.-x app)))))

(let [one (-eval (ana (quote (Lam f (Lam x (App (Var f) (Var x)))))) {})
      succ (-eval (ana (quote (Lam n (Lam f (Lam x (App (Var f) (App (App (Var n) (Var f)) (Var x)))))))) {})
      pred (-eval (ana (quote (Lam n (Lam f (Lam x (App (App (App (Var n) (Lam g (Lam h (App (Var h) (App (Var g) (Var f)))))) (Lam u (Var x))) (Lam u (Var u)))))))) {})
      plus (-eval (ana (quote (Lam m (Lam n (App (App (Var m) (Var succ)) (Var n)))))) {'succ succ})
      mult (-eval (ana (quote (Lam m (Lam n (Lam f (App (Var m) (App (Var n) (Var f)))))))) {})]
  ;; (println (-unana (-eval (ana (quote (App (Var succ) (Var one)))) {'one one 'succ succ})))
  (println (-unana (nf (ana (quote (App (Var succ) (Var one)))) {'one one 'succ succ})))
  (println (-unana (nf (ana (quote (App (Var pred) (App (Var succ) (Var one))))) {'one one 'succ succ 'pred pred})))
  (println (-unana (nf (ana (quote (App (App (Var plus) (Var one)) (Var one)))) {'one one 'plus plus})))
  )
