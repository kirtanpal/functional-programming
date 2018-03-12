(defun all-length (listt)
  (setq test 1)
  (if (eq (car listt) nil)
  (progn
    (if (eq (cdr listt) nil)
      (return-from all-length 0)
      (progn
        (setq test 0)
        (setq test (+ test (all-length (cdr listt))) )
      )
    )
  )

  (progn
  (if (listp (car listt))
    (progn
      (if (eq (cdr listt) nil)
        (setq test (all-length (car listt)))
        (setq test (+ (all-length (car listt)) (all-length (cdr listt))))
      )
    )
    (progn
      (if (eq (cdr listt) nil)
        1
        (setq test (+ test (all-length (cdr listt))))
      )
    )
  )
  )
  )
  ;(format t "~% end ~d " test)
  (return-from all-length test)
)


;(format t "value ~A~2% " (eval (all-length '(G NIL H (X Y Z NIL))) ))
