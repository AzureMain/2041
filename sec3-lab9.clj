(defn error [msg]
  (throw (Exception. msg)))

(def Queue
  (fn []
    (letfn
      [(make [top]
        (fn [method & args]
          (letfn
            [(empty? []
                     (= top '()))
             (enqueue [obj]
                   (make (cons obj top)))
             (dequeue []
                      (if (empty?)
                          (error "Queue is empty.")
                          (make (drop-last top))))
             (front []
                    (if (empty?)
                      (error "Queue is empty.")
                      (last top)))]
            (cond
              (= method :empty?)
              (empty?)
              (= method :enqueue)
              (enqueue (first args))
              (= method :dequeue)
              (dequeue)
              (= method :front)
              (front)
              true
              (error "No such method.")))))]
       (make '()))))

(def s0 (Queue))
(println (s0 :empty?))

(def s1 (s0 :enqueue "A"))
(println (s1 :empty?))
(println (s1 :dequeue))

(def s2 (s1 :enqueue "B"))
(println (s2 :empty?))
(println (s2 :front))

(def s3 (s2 :enqueue "C"))
(println (s3 :empty?))
(println (s3 :front))

(def s4 (s3 :dequeue))
(println (s4 :empty?))
(println (s4 :front))

(def s5 (s4 :dequeue))
(println (s5 :empty?))
(println (s5 :front))