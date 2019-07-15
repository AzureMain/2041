(defn morph [f l]
  (loop [x l k '()]
    (if (empty? x)
      k
      (recur (drop-last x) (cons (f (last x)) k)))))

(morph (fn [n] (+ n 1)) '(1 2 3))

(defn morph
  ([f l]
      (morph f (drop-last l) (cons (f (last l)) '())))
  ([f l k]
     (if (empty? l)
       k
       (recur f (drop-last l) (cons (f (last l)) k)))))

(morph (fn [n] (+ n 1)) '(1 2 3))


(defn iffy [x y]
  (if (symbol? x)
    (get y x)
    (if (iffy (second x) y)
        (iffy (second (rest x)) y)
        (iffy (second (rest (rest x))) y))))


(iffy '(if (if a s d) (if s a d) (if d s a)) {'a false 's :true 'd false})

(defmacro od [& f]
  (let [g (gensym)]
    `(let [~g ~(first f)]
       (do ~@(rest f) ~g))))
(macroexpand (od '1 '2 '3))
