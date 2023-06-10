(define dl_edb (make-hashmap))
(define dl_idb (make-hashmap))
(define dl_rdb (quote ()))

(define dl_idx_entity (make-hashmap))
(define dl_idx_attr (make-hashmap))

(define dl_counter 0)
(define dl_nextID (lambda () (begin
   (set! dl_counter (+ dl_counter 1))
   dl_counter)))

(define dl_assert (lambda (entity attr value) (begin
  (hashmap-set! dl_edb (list entity attr value) #t)
  (dl_update_indices (list entity attr value)))))

(define dl_update_indices (lambda (tuple)
   (let ((entity (car tuple))
         (attr (car (cdr tuple))))
     (let ((m (hashmap-ref dl_idx_entity entity #f)))
       (if m (hashmap-set! m tuple #t)
         (let ((new (make-hashmap)))
           (hashmap-set! dl_idx_entity entity new)
           (hashmap-set! new tuple #t))))
     (let ((m (hashmap-ref dl_idx_attr attr #f)))
       (if m (hashmap-set! m tuple #t)
         (let ((new (make-hashmap)))
           (hashmap-set! dl_idx_attr attr new)
           (hashmap-set! new tuple #t)))))))

(define-syntax dl_record
   (syntax-rules (list let dl_nextID dl_assert)
     ((_ type (attr value) ...) (let ((id (dl_nextID)))
       (dl_assert id (list type attr) value) ... id))))

(define-syntax dl_find
   (syntax-rules (where run* equalo dl_edb dl_idb dl_vars let list->set cons eval hashmap-keys fresh dl_findo)
     ((_ x where ( match ... ))
      (let ((vars (list->set (dl_vars (quote (x match ...)))))
            (edb (hashmap-keys dl_edb))
            (idb (hashmap-keys dl_idb)))
        (run* (eval (cons 'fresh (cons (cons 'q vars) (quote ((equalo q (quasiquote x)) (dl_findo (quasiquote match) edb idb) ...))))))))))

(define dl_findo (lambda (m edb idb)
   (fresh (x y entity attr db)
   (conso entity x m)
   (conso attr y x)
     (conde
       [(boundo entity) (lookupo dl_idx_entity entity db) (membero m db)]
       [(unboundo entity) (boundo attr) (lookupo dl_idx_attr attr db) (membero m db)] ))))

(define dl_var? (lambda (s) (if (symbol? s) (prefix? (symbol->string s) "?"))))

(define dl_vars (lambda (l)
   (list->set (foldl (lambda (x acc)
     (cond
       [(dl_var? x) (cons x acc)]
       [(pair? x) (append (dl_vars x) acc)]
       [else acc])) l (quote ())))))

(define-syntax dl_rule
   (syntax-rules (list dl_assert_rule :-)
     ((_ (head hx hy) :- (body bx by) ...)
      (dl_assert_rule (quote (hx head hy)) (list (quote (bx body by)) ...)))))

(define dl_assert_rule (lambda (head body)
  (set! dl_rdb (cons (cons head body) dl_rdb))))

(define dl_fixpoint (lambda () (begin
    (set! dl_idb (make-hashmap))
    (dl_fixpoint_iterate))))

(define dl_fixpoint_iterate (lambda ()
   (let ((new (hashmap-keys (set_difference (foldl (lambda (x y) (set-extend! y x)) (map dl_apply_rule dl_rdb) (make-hashmap)) dl_idb))))
     (set-extend! dl_idb new)
     (map dl_update_indices new)
     (if (not (null? new)) (dl_fixpoint_iterate)))))

