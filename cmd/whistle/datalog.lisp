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

