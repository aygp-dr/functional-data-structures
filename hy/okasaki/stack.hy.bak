;; stack.hy - Persistent Stack implementation

(setv empty-stack [])

(defn empty-stack? [stack]
  (= (len stack) 0))

(defn push [item stack]
  (+ [item] stack))

(defn top [stack]
  (if (empty-stack? stack)
      (raise (Exception "Cannot get top of empty stack"))
      (get stack 0)))

(defn pop [stack]
  (if (empty-stack? stack)
      (raise (Exception "Cannot pop empty stack"))
      (cut stack 1 None)))
