;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Your solution goes in this file ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  ;; t "~% end ~d " test)
  (return-from all-length test)
)

(defun range (listt)
  (setq ret (list (apply 'min listt) (apply 'max listt) ))
  (return-from range ret)
)

(defun before (a b listt)
  (setq flist (list nil))
  (if(equal (car listt) nil)
  ;if
    (progn
      ;(print "nill " )
      (if (equal (cdr listt) nil)
        (return-from before nil)
        (progn
          ;(print "nill else " )
          (setq flist (before a b (cdr listt) ))
          ;; t "~A~%" flist)
          (return-from before flist)
          )
      )
    )

    ;else
    (progn
      ;(print "else1 " )
      (if (equal (car listt) a)
        ;if
        (progn
          ;(print "a is found")
          (if (equal (car (cdr listt)) b) ;(eq (car (cdr (cdr listt))) nil))
            ;if
            (progn
              ;(print "b is next")
              (setq flistt (list (car listt) (car (cdr listt))) )
              ;; t "~A~%" flist)
              (return-from before flistt )
            )
            ;else
            (progn
              ;(print "print next")
              (setq flist (list nil (car listt)))
              (setq flist (append flist (before  (car (cdr listt)) b (cdr listt)) ) )
              ;; t "~A~%" (cdr flist))
              (return-from before (cdr flist))
            )
          )
          )
          ;else
          (progn
          ;(print "else2 " )
          (setq flist (before a b (cdr listt) ))
          ;; t "~A~%" flist)
          (return-from before flist)
          )
        )

      )
    )
)

(defun split-if (func list1)
  (if (and (eq (car list1) nil) (eq (cdr list1) nil))
    (return-from split-if (list nil nil))
    (return-from split-if (split func list1))
)
)

(defun split (func listt)
  (if (equal (cdr listt) nil)
    ;if
    (progn
      ;(print "nill")
      (setq alistt (list (list ) (list )) )
      ;(print "nill")
      (if (funcall func (car listt))
        ;if
        (progn
          ;(print "nill1")
          (setq alistt (addtolist (car listt) alistt 1) )
          (return-from split alistt)
        )
        ;else
        (progn
          ;(print "nill2")
          (setq alistt (addtolist (car listt) alistt 2) )
          (return-from split alistt)
        )
      )
    )
    ;else
    (progn
      ;(print "else1")
      ;(format t "~%~A~%" (funcall func 82) )
      (setq alistt (split func (cdr listt)) )
      ;(print "else1")
      (if (funcall func (car listt))
      ;if
        (progn
          (setq alistt (addtolist (car listt) alistt 1) )
          (return-from split alistt)
        )
        ;else
        (progn
          (setq alistt (addtolist (car listt) alistt 2) )
          (return-from split alistt)
        )
      )
    )
  )
)
;(setq alistt (list (list ) (list )) )
(defun addtolist (listt alistt sel)
  (if (eq sel 1)
  (progn
    (setq templist2 (car (cdr alistt)))
    ;(format t "add ~A~2%" listt)
    ;(format t "else2.0 ~A~2%" templist2)
    (setq templist2 (append (list listt) templist2 ) )
    ;(format t "else2.1 ~A~2%" templist2)
    (setq templist1 (car alistt))
    ;(format t "else2.1 list1 ~A~2%" templist1)
    (setq alistt (list templist1 templist2 ))
    (setq templist2 (cdr alistt))
    ;(format t "else2.2 ~A~2%" templist2)
    ;(format t "alistt ~A~2%" alistt)
    (return-from addtolist alistt)
  )
    (progn
      (setq templist1 (car alistt))
      ;(format t "else1.0 ~A~2%" templist1)
      (setq templist1 (append (list listt) templist1 ))
      ;(format t "else1.1 ~A~2%" templist1)
      (setq templist2 (car (cdr alistt)))
      ;(format t "else1.0 list2 ~A~2%" templist2)
      ;(format t "else1 ~A~2%" templist1)
      (setq alistt (list templist1 templist2) )
      ;(format t "else1.0 alist ~A~2%" alistt)

      ;(setq (car alistt) (append (list (car alistt)) (list listt)))
      (return-from addtolist alistt)
    )
  )
)

(defun extractelements (listt count)
  (setq flist (list nil))
  (if(eq (car listt) nil)
    (if(eq (cdr listt) nil)
      (return-from extractelements)
      ;else part, means we have reached end
      (progn
        (setq list2 (car listt))
        (setq flist (append (list list2) (extractelements (cdr listt) (- count 1)) ))
        ;; t "~A~2%" flist)
        (return-from extractelements flist ) ;count is not 0 yet
      )
    )
    ;else part start is not nil
    (progn
      (if (eq count 1)
        (progn
          ;(print "suspected problem")
          (setq flist (car listt))
          ;(print "suspected problem2 ")
          ;; t "~A~2%"flist)
          (return-from extractelements (list flist))
        )
        (progn
          (setq list2 (car listt))
          ;(print "Entered else")
          ;; t "~A~2%" list2)
          (setq flist (append (list list2) (extractelements (cdr listt) (- count 1)) ))
          ;(print "suspected problem2 ")
          ;; t "~A~2%" flist)
          (return-from extractelements flist ) ;count is not 0 yet
        )
      )
    )
  );done with outer if loop
) ;done with function


(defun extractlist (listt count)
  (setq flist (list nil))
  (if(eq (car listt) nil)
    (if(eq (cdr listt) nil)
      (return-from extractlist)
      ;else part, means we have reached end
      (progn
          (if(eq count 0)
          (progn
            (return-from extractlist (cdr listt))
          )
          (progn
            (setq flist (extractlist (cdr listt) (- count 1)))
            ;; t "~A~2%" flist)
            (return-from  extractlist flist)
          )
          ) ;count is not 0 yet
      )
    )

    ;else part start is not nil
    (progn
      (if(eq count 1)
      (progn
        ;; t "~A~2%" (cdr listt))
        (return-from extractlist (cdr listt))
      )
      (progn
        (setq flist (extractlist (cdr listt) (- count 1)))
        ;; t "~A~2%" flist)
        (return-from  extractlist flist)
      )
      ) ;count is not 0 yet
    )
  ) ;done with outer if loop
) ;done with function



(defun group (listt division)
  ;calling function grouping that keeps track of how
  ;many divisions were there originally
  (if (eq (car listt) nil)
    (return-from group nil)
  (progn
    (setq flist (list nil))
  (if(eq (extractlist listt division) nil)
    (progn
      (setq flist  (list (extractelements listt division)))
      (return-from group flist)
    )
    (progn
      (setq flist (append (list (extractelements listt division)) (group (extractlist listt division) division ) ) )
      (return-from group flist)
    )

  ;(progn
;    (setq flist (list (list (extractelements listt division)) (list (group (extractlist listt division) division )) ) )
;    (return-from group flist)
;  )
)
)
)
)


(defun calculatevalue (func listt)
  (setq value (funcall func (car listt)))
  (return-from calculatevalue (list value))
)

(defun mostn (func listt)
  (setq listCapture (mostn2 func listt))
  ;; t "f2 simer ~A~2%" listCapture)
  (setq listCapture (append (list listCapture) (calculatevalue func listcapture)))
  (return-from mostn listCapture)
)
(defun mostn2 (func listt)
;(print "enter")
  (if (equal (cdr listt) nil )
    (return-from mostn2 listt)
    (progn
      ;; t "f1.0 ~A~2%" (car listt))
      (setq nlist (mostn2 func (cdr listt)))
      ;; t "f1.0 ~A~2%" (car listt))
      (if (> (funcall func (car listt)) (funcall func (car nlist)))
        (progn
          ;; t "f1 ~A~2%" (car listt))
          (return-from mostn2 (list (car listt)))
          )
        (progn
          (if (equal (funcall func (car listt)) (funcall func (car nlist)))
            (progn
              ;; t "f2 ~A~2%" (car listt))
              (setq nlist (append (list (car listt)) nlist))
            ;  ; t "f2 ~A~2%" nlist)
              (return-from mostn2 nlist)
            )
            (progn
              ;; t "f3 ~A~2%" (car listt))
              (return-from mostn2 nlist)
            )
          )
        )
      )
    )
  )
)


(defun match (list1 list2)
;; t "enter1 ~A~2%" list1)
;; t "enter2 ~A~2%" list2)
  (cond
    ( (and (eq (car list1) nil) (eq (car list2) nil));list is terminating
        ;(print "Entered terminating")
        (if (not (eq (length list1) (length list2) ))
          (return-from match nil)
        (progn
          (if (and (eq (cdr list1) nil) (eq (cdr list2) nil))
          (return-from match t)
          (progn
            (if (or (eq (cdr list1) nil) (eq (cdr list2) nil))
            (return-from match nil)
            (progn
              ;; t "nil1 ~A~2%" (cdr list1))
              ;; t "nil2 ~A~2%" (cdr list2))
              (return-from match (match (cdr list1) (cdr list2)))
            )
            )
          )
        )
        )
        )
    ) ;done with first condition
    ( (or (eq (car list1) (car list2)) (eq (car list1) '?))
    ;    ; t "or1 ~A~2%" list1)
      ;  ; t "or2 ~A~2%" list2)
      (if (and (eq (cdr list2) nil) (not (eq (cdr list1) nil)) )
        (return-from match nil)
        (progn

      (if (and (and (eq (car (cdr list1)) nil) (eq (car (cdr list2)) nil)) (not (eq (length list1) (length list2))))
        (return-from match nil)
        (return-from match (match (cdr list1) (cdr list2)))
      )
      )
      )
    ) ;done with second condition(checking if ? or same numbers)
    ( (eq (car list1) '!)
      (if (eq (car (cdr list1)) '!)
        (progn
          (if (and (eq (cdr list2) nil) (not (eq (cdr list1) nil)))
            (return-from match nil)
            (return-from match (match (cdr list1) (cdr list2)))
        )
        )
        (progn
          (if (and (eq (cdr list2) nil) (eq (cdr list1) nil) )
            (return-from match t)
            (progn
              (if (or (eq (car (cdr list1)) (car (cdr list2))) (eq (car (cdr list1)) '?) )
                (return-from match (match (cdr list1) (cdr list2)))
                (progn
                  (setq list2 (append (list (car list2)) (cdr (cdr list2))))
                  (return-from match (match list1 list2))
              )
              )
            )
            )
        )
        )
      ) ;done with condition for !


    ((or 1 1)
      ;; t "else part1  ~A~2%" list1)
    ;  ; t "else part2  ~A~2%" list2)
    ;  (print "test")
      (return-from match nil)
    );else condition means not matched so return false
  )
)





(defun match-var (list1 list2)
  (if (eq (match-check list1 list2) nil)
  (progn
    (garbage-collector list1 list2)
    (return-from match-var nil)
  )
  (return-from match-var t)
  )
)

(defun match-check (list1 list2)
;; t "enter1 ~A~2%" list1)
;; t "enter2 ~A~2%" list2)
  (cond
    ( (and (and (eq (car list1) nil) (eq (car list2) nil)) (and (eq (cdr list1) nil) (eq (cdr list2) nil) )) ;list is terminating
        ;(print "Entered terminating")
        (if (not (eq (length list1) (length list2) ))
          (return-from match-check nil)
        (progn
          (if (and (eq (cdr list1) nil) (eq (cdr list2) nil))
          (return-from match-check t)
          (progn
            (if (or (eq (cdr list1) nil) (eq (cdr list2) nil))
            (return-from match-check nil)
            (progn
              ;; t "nil1 ~A~2%" (cdr list1))
              ;; t "nil2 ~A~2%" (cdr list2))
              (return-from match-check (match-check (cdr list1) (cdr list2)))
            )
            )
          )
        )
        )
        )
    ) ;done with first condition
    ;(plus (? a) (? b)) '(plus 2 3)
    ( (and (listp (car list1) ) (eq (car (car list1)) '?))
       ; t "?1 ~A~2%" list1)
       ; t "?2 ~A~2%" list2)
      (if (and (eq (cdr list2) nil) (not (eq (cdr list1) nil)) )
        (progn
          (if (eq (car (car list1)) '?)
          (progn
            ; t "?1.1 ~A~2%" list1)
            ; t "?2.1 ~A~2%" list2)
            (setf (symbol-value (car (cdr (car list1)))) (car list2))
            (return-from match-check nil)
          )
          (progn
            ; t "?1.2 ~A~2%" list1)
            ; t "?2.2 ~A~2%" list2)
            (return-from match-check nil)
          )
          )
        )
        (progn
          (if (and (and (eq (cdr list1) nil) (eq (cdr list2) nil) ) (and (and (eq (car (cdr list1)) nil) (eq (car (cdr list2)) nil)) (not (eq (length list1) (length list2)))))
            (progn
              (if (eq (car (car list1)) '?)
              (progn
                ; t "?1.3 ~A~2%" list1)
                ; t "?2.3 ~A~2%" list2)
                (setf (symbol-value (car (cdr (car list1)))) (car list2))
                (return-from match-check nil)
              )
              (progn
                ; t "?1.4 ~A~2%" list1)
                ; t "?2.4 ~A~2%" list2)
                (return-from match-check nil)
                )
              )
            )
            (progn
              (if (eq (car (car list1)) '?)
              (progn
                ; t "?1.5 ~A~2%" list1)
                ; t "?2.5 ~A~2%" list2)
                (setf (symbol-value (car (cdr (car list1)))) (car list2))
                (return-from match-check (match-check (cdr list1) (cdr list2)))
              )
              (progn
                ; t "?1.6 ~A~2%" list1)
                ; t "?2.6 ~A~2%" list2)
                (return-from match-check (match-check (cdr list1) (cdr list2)))
                )
              )
            )
      )
      )
      )
    )
    ( (eq (car list1) (car list2))
       ; t "or1 ~A~2%" list1)
       ; t "or2 ~A~2%" list2)
       (if (and (eq (cdr list2) nil) (not (eq (cdr list1) nil)) )
         (return-from match-check nil)
         (progn
           (if (and (and (eq (car (cdr list1)) nil) (eq (car (cdr list2)) nil)) (not (eq (length list1) (length list2))))
            (return-from match-check nil)
            (progn
              ; t "or1.1 ~A~2%" (cdr list1))
              ; t "or2.1 ~A~2%" (cdr list2))
              (return-from match-check (match-check (cdr list1) (cdr list2)))
            )
          )
        )
       )
    ) ;done with second condition(checking if ? or same numbers)

    ( (and (listp (car list1)) (eq (car (car list1)) '!) )
      (if (and (listp (car (cdr list1))) (eq (car (car (cdr list1))) '!))
        (progn
          (if (and (eq (cdr list2) nil) (not (eq (cdr list1) nil)))
            (progn
              (setq newlist (list (car (cdr (car list1))) (car list2) ))
              (setf (symbol-value (car (cdr (car list1)))) (cdr newlist))
              (return-from match-check nil)
            )
            (progn
              (setq newlist (list (car (cdr (car list1))) (car list2) ))
              (setf (symbol-value (car (cdr (car list1)))) (cdr newlist))
              ; t "!1.0 ~A~2%" list1)
              ; t "!2.0 ~A~2%" list2)
              (return-from match-check (match-check (cdr list1) (cdr list2)))
            )
          )
        )
        ;if next one is not !
        (progn
          ;if both list are ending
          (if (and (eq (cdr list2) nil) (eq (cdr list1) nil) )
            (progn
              ; t "!1.1 ~A~2%" list1)
              ; t "!2.1 ~A~2%" list2)
              (setq newlist (list (car (cdr (car list1))) (car list2) ))
              (setf (symbol-value (car (cdr (car list1)))) (cdr newlist))
              (return-from match-check t)
            )
            ;else if list1 next is a ? (?,a), cehck if next is a list
            (progn
              (if (and (listp (car (cdr list1))) (eq (car (car (cdr list1))) '?))
                (progn
                  ; t "!1.2 ~A~2%" list1)
                  ; t "!2.2 ~A~2%" list2)
                  (setq newlist (list  (car (cdr (car list1))) (car list2) ))
                  (setf (symbol-value (car (cdr (car list1)))) (cdr newlist))
                  (return-from match-check (match-check (cdr list1) (cdr list2)))
                )
                (progn
                  ;if both next match and they are not lists
                  (if (eq (car (cdr list1)) (car (cdr list2)))
                    (progn
                      ; t "!1.4 ~A~2%" list1)
                      ; t "!2.4 ~A~2%" list2)
                      (setq b 2)
                      ;(setf (symbol-value 'b) (symbol-value (car (cdr (car list1)))) )
                      ; t "!3.4 ~A~2%" b)

                      (if (boundp (car (cdr (car list1))))
                        (progn
                          (setq newlist (append  (symbol-value (car (cdr (car list1)))) (list (car list2)) ))
                          ; t "!newlist1 ~A~2%" newlist)
                        )
                        (progn
                          (setq newlist (list (car (cdr list2))) )
                          ; t "!newlist2 ~A~2%" newlist)
                        )
                      )
                      (setf (symbol-value (car (cdr (car list1)))) newlist)
                      (setf (symbol-value 'b) (symbol-value (car (cdr (car list1)))) )
                      ; t "!4.4 ~A~2%" b)
                      (if (eq (car (cddr list1)) (cadr (cddr list2)))
                      (progn
                        (setq newlist (append  (symbol-value (car (cdr (car list1)))) (list (cadr list2) (car (cddr list2))) ))
                        (setf (symbol-value (car (cdr (car list1)))) newlist)
                        ; t "!1.55 ~A~2%" newlist)
                        ; t "!2.55 ~A~2%" list2)
                        (return-from match-check (match-check (cdr list1) (cdr (cddr list2)) ))
                      )
                      (progn
                        ; t "!1.56 ~A~2%" newlist)
                        ; t "!2.56 ~A~2%" list2)
                        (if (not (eq (car (cdr list1)) (car (cddr list2))))
                          (return-from match-check (match-check (cdr list1) (cdr list2)))
                          (return-from match-check (match-check list1 (cdr list2)))
                        )
                      )
                      )
                    )
                    ;if next one does not match
                    (progn
                      ; t "!1.5 ~A~2%" list1)
                      ; t "!2.5 ~A~2%" list2)
                      ; t "!3.5 ~A~2%" (car (cdr (car list1))))
                      (if (boundp (car (cdr (car list1))))
                        (progn
                          (setq newlist (append  (symbol-value (car (cdr (car list1)))) (list (car list2)) ))
                          ; t "!newlist11 ~A~2%" newlist)
                        )
                        (progn
                          (setq newlist (list (car list2)) )
                          ; t "!newlist12 ~A~2%" newlist)
                        )
                      )
                      ;(setq newlist (list  (car (cdr (car list1))) (car list2) (car (cdr list2))))
                      (setf (symbol-value (car (cdr (car list1)))) newlist)
                      (setf (symbol-value 'b) (symbol-value (car (cdr (car list1)))) )
                      ; t "!4.5 ~A~2%" b)

                      ;(setq list2 (append (list (car list2)) (cdr (cdr list2))))
                      (return-from match-check (match-check list1 (cdr list2)))
                    )
                  )
                )
              )
            )
          )
        )
      )
    ) ;done with condition for !


    ((or 1 1)
      ;; t "else part1  ~A~2%" list1)
    ;  ; t "else part2  ~A~2%" list2)
    ;  (print "test")
      (return-from match-check nil)
    );else condition means not matched so return false
  )
)

(defun garbage-collector (list1 list2)
;; t "enter1 ~A~2%" list1)
;; t "enter2 ~A~2%" list2)
  (cond
    ( (and (and (eq (car list1) nil) (eq (car list2) nil)) (and (eq (cdr list1) nil) (eq (cdr list2) nil) )) ;list is terminating
        ;(print "Entered terminating")
        (if (not (eq (length list1) (length list2) ))
          (return-from garbage-collector nil)
        (progn
          (if (and (eq (cdr list1) nil) (eq (cdr list2) nil))
          (return-from garbage-collector t)
          (progn
            (if (or (eq (cdr list1) nil) (eq (cdr list2) nil))
            (return-from garbage-collector nil)
            (progn
              ;; t "nil1 ~A~2%" (cdr list1))
              ;; t "nil2 ~A~2%" (cdr list2))
              (return-from garbage-collector (garbage-collector (cdr list1) (cdr list2)))
            )
            )
          )
        )
        )
        )
    ) ;done with first condition
    ;(plus (? a) (? b)) '(plus 2 3)
    ( (and (listp (car list1) ) (eq (car (car list1)) '?))
       ; t "?1 ~A~2%" list1)
       ; t "?2 ~A~2%" list2)
      (if (and (eq (cdr list2) nil) (not (eq (cdr list1) nil)) )
        (progn
          (if (eq (car (car list1)) '?)
          (progn
            ; t "?1.1 ~A~2%" list1)
            ; t "?2.1 ~A~2%" list2)
            (setf (symbol-value (car (cdr (car list1)))) nil)
            (return-from garbage-collector nil)
          )
          (progn
            ; t "?1.2 ~A~2%" list1)
            ; t "?2.2 ~A~2%" list2)
            (return-from garbage-collector nil)
          )
          )
        )
        (progn
          (if (and (and (eq (cdr list1) nil) (eq (cdr list2) nil) ) (and (and (eq (car (cdr list1)) nil) (eq (car (cdr list2)) nil)) (not (eq (length list1) (length list2)))))
            (progn
              (if (eq (car (car list1)) '?)
              (progn
                ; t "?1.3 ~A~2%" list1)
                ; t "?2.3 ~A~2%" list2)
                (setf (symbol-value (car (cdr (car list1)))) nil)
                (return-from garbage-collector nil)
              )
              (progn
                ; t "?1.4 ~A~2%" list1)
                ; t "?2.4 ~A~2%" list2)
                (return-from garbage-collector nil)
                )
              )
            )
            (progn
              (if (eq (car (car list1)) '?)
              (progn
                ; t "?1.5 ~A~2%" list1)
                ; t "?2.5 ~A~2%" list2)
                (setf (symbol-value (car (cdr (car list1)))) nil)
                (return-from garbage-collector (garbage-collector (cdr list1) (cdr list2)))
              )
              (progn
                ; t "?1.6 ~A~2%" list1)
                ; t "?2.6 ~A~2%" list2)
                (return-from garbage-collector (garbage-collector (cdr list1) (cdr list2)))
                )
              )
            )
      )
      )
      )
    )
    ( (eq (car list1) (car list2))
       ; t "or1 ~A~2%" list1)
       ; t "or2 ~A~2%" list2)
       (if (and (eq (cdr list2) nil) (not (eq (cdr list1) nil)) )
         (return-from garbage-collector nil)
         (progn
           (if (and (and (eq (car (cdr list1)) nil) (eq (car (cdr list2)) nil)) (not (eq (length list1) (length list2))))
            (return-from garbage-collector nil)
            (progn
              ; t "or1.1 ~A~2%" (cdr list1))
              ; t "or2.1 ~A~2%" (cdr list2))
              (return-from garbage-collector (garbage-collector (cdr list1) (cdr list2)))
            )
          )
        )
       )
    ) ;done with second condition(checking if ? or same numbers)

    ( (and (listp (car list1)) (eq (car (car list1)) '!) )
      (if (and (listp (car (cdr list1))) (eq (car (car (cdr list1))) '!))
        (progn
          (if (and (eq (cdr list2) nil) (not (eq (cdr list1) nil)))
            (progn
              (setq newlist (list (car (cdr (car list1))) (car list2) ))
              (setf (symbol-value (car (cdr (car list1)))) nil)
              (return-from garbage-collector nil)
            )
            (progn
              (setq newlist (list (car (cdr (car list1))) (car list2) ))
              (setf (symbol-value (car (cdr (car list1)))) nil)
              ; t "!1.0 ~A~2%" list1)
              ; t "!2.0 ~A~2%" list2)
              (return-from garbage-collector (garbage-collector (cdr list1) (cdr list2)))
            )
          )
        )
        ;if next one is not !
        (progn
          ;if both list are ending
          (if (and (eq (cdr list2) nil) (eq (cdr list1) nil) )
            (progn
              ; t "!1.1 ~A~2%" list1)
              ; t "!2.1 ~A~2%" list2)
              (setq newlist (list (car (cdr (car list1))) (car list2) ))
              (setf (symbol-value (car (cdr (car list1)))) nil)
              (return-from garbage-collector t)
            )
            ;else if list1 next is a ? (?,a), cehck if next is a list
            (progn
              (if (and (listp (car (cdr list1))) (eq (car (car (cdr list1))) '?))
                (progn
                  ; t "!1.2 ~A~2%" list1)
                  ; t "!2.2 ~A~2%" list2)
                  (setq newlist (list  (car (cdr (car list1))) (car list2) ))
                  (setf (symbol-value (car (cdr (car list1)))) nil)
                  (return-from garbage-collector (garbage-collector (cdr list1) (cdr list2)))
                )
                (progn
                  ;if both next match and they are not lists
                  (if (eq (car (cdr list1)) (car (cdr list2)))
                    (progn
                      ; t "!1.4 ~A~2%" list1)
                      ; t "!2.4 ~A~2%" list2)
                      (setq b 2)
                      ;(setf (symbol-value 'b) (symbol-value (car (cdr (car list1)))) )
                      ; t "!3.4 ~A~2%" b)

                      (if (boundp (car (cdr (car list1))))
                        (progn
                          (setq newlist (append  (symbol-value (car (cdr (car list1)))) nil ))
                          ; t "!newlist1 ~A~2%" newlist)
                        )
                        (progn
                          (setq newlist (list (car (cdr list2))) )
                          ; t "!newlist2 ~A~2%" newlist)
                        )
                      )
                      (setf (symbol-value (car (cdr (car list1)))) nil)
                      (setf (symbol-value 'b) (symbol-value (car (cdr (car list1)))) )
                      ; t "!4.4 ~A~2%" b)
                      (if (eq (car (cddr list1)) (cadr (cddr list2)))
                      (progn
                        (setq newlist (append  (symbol-value (car (cdr (car list1)))) (list (cadr list2) (car (cddr list2))) ))
                        (setf (symbol-value (car (cdr (car list1)))) nil)
                        ; t "!1.55 ~A~2%" newlist)
                        ; t "!2.55 ~A~2%" list2)
                        (return-from garbage-collector (garbage-collector (cdr list1) (cdr (cddr list2)) ))
                      )
                      (progn
                        ; t "!1.56 ~A~2%" newlist)
                        ; t "!2.56 ~A~2%" list2)
                        (if (not (eq (car (cdr list1)) (car (cddr list2))))
                          (return-from garbage-collector (garbage-collector (cdr list1) (cdr list2)))
                          (return-from garbage-collector (garbage-collector list1 (cdr list2)))
                        )
                      )
                      )
                    )
                    ;if next one does not match
                    (progn
                      ; t "!1.5 ~A~2%" list1)
                      ; t "!2.5 ~A~2%" list2)
                      ; t "!3.5 ~A~2%" (car (cdr (car list1))))
                      (if (boundp (car (cdr (car list1))))
                        (progn
                          (setq newlist (append  (symbol-value (car (cdr (car list1)))) (list (car list2)) ))
                          ; t "!newlist11 ~A~2%" newlist)
                        )
                        (progn
                          (setq newlist (list (car list2)) )
                          ; t "!newlist12 ~A~2%" newlist)
                        )
                      )
                      ;(setq newlist (list  (car (cdr (car list1))) (car list2) (car (cdr list2))))
                      (setf (symbol-value (car (cdr (car list1)))) nil)
                      (setf (symbol-value 'b) (symbol-value (car (cdr (car list1)))) )
                      ; t "!4.5 ~A~2%" b)

                      ;(setq list2 (append (list (car list2)) (cdr (cdr list2))))
                      (return-from garbage-collector (garbage-collector list1 (cdr list2)))
                    )
                  )
                )
              )
            )
          )
        )
      )
    ) ;done with condition for !


    ((or 1 1)
      ;; t "else part1  ~A~2%" list1)
    ;  ; t "else part2  ~A~2%" list2)
    ;  (print "test")
      (return-from garbage-collector nil)
    );else condition means not matched so return false
  )
)
