
(define dl_counter 0)
(define dl_nextID (lambda () (begin
   (set! dl_counter (+ dl_counter 1))
   dl_counter)))

(define dl_assert (lambda (entity attr value) (begin
  (hashmap-set! dl_edb (list entity attr value) #t)
  (dl_update_indices (list entity attr value)))))

(define-syntax dl_record
   (syntax-rules (list let dl_nextID dl_assert)
     ((_ type (attr value) ...) (let ((id (dl_nextID)))
       (dl_assert id (list type attr) value) ... id))))

