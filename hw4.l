
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
          ;(format t "~A~%" flist)
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
              ;(format t "~A~%" flist)
              (return-from before flistt )
            )
            ;else
            (progn
              ;(print "print next")
              (setq flist (list nil (car listt)))
              (setq flist (append flist (before  (car (cdr listt)) b (cdr listt)) ) )
              ;(format t "~A~%" (cdr flist))
              (return-from before (cdr flist))
            )
          )
          )
          ;else
          (progn
          ;(print "else2 " )
          (setq flist (before a b (cdr listt) ))
          ;(format t "~A~%" flist)
          (return-from before flist)
          )
        )

      )
    )
)

(defun split-if (func listt)
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
          (return-from split-if alistt)
        )
        ;else
        (progn
          ;(print "nill2")
          (setq alistt (addtolist (car listt) alistt 2) )
          (return-from split-if alistt)
        )
      )
    )
    ;else
    (progn
      ;(print "else1")
      ;(format t "~%~A~%" (funcall func 82) )
      (setq alistt (split-if func (cdr listt)) )
      ;(print "else1")
      (if (funcall func (car listt))
      ;if
        (progn
          (setq alistt (addtolist (car listt) alistt 1) )
          (return-from split-if alistt)
        )
        ;else
        (progn
          (setq alistt (addtolist (car listt) alistt 2) )
          (return-from split-if alistt)
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
        ;(format t "~A~2%" flist)
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
          ;(format t "~A~2%"flist)
          (return-from extractelements (list flist))
        )
        (progn
          (setq list2 (car listt))
          ;(print "Entered else")
          ;(format t "~A~2%" list2)
          (setq flist (append (list list2) (extractelements (cdr listt) (- count 1)) ))
          ;(print "suspected problem2 ")
          ;(format t "~A~2%" flist)
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
            ;(format t "~A~2%" flist)
            (return-from  extractlist flist)
          )
          ) ;count is not 0 yet
      )
    )

    ;else part start is not nil
    (progn
      (if(eq count 1)
      (progn
        ;(format t "~A~2%" (cdr listt))
        (return-from extractlist (cdr listt))
      )
      (progn
        (setq flist (extractlist (cdr listt) (- count 1)))
        ;(format t "~A~2%" flist)
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
  ;(format t "f2 simer ~A~2%" listCapture)
  (setq listCapture (append (list listCapture) (calculatevalue func listcapture)))
  (return-from mostn listCapture)
)
(defun mostn2 (func listt)
;(print "enter")
  (if (equal (cdr listt) nil )
    (return-from mostn2 listt)
    (progn
      ;(format t "f1.0 ~A~2%" (car listt))
      (setq nlist (mostn2 func (cdr listt)))
      ;(format t "f1.0 ~A~2%" (car listt))
      (if (> (funcall func (car listt)) (funcall func (car nlist)))
        (progn
          ;(format t "f1 ~A~2%" (car listt))
          (return-from mostn2 (list (car listt)))
          )
        (progn
          (if (equal (funcall func (car listt)) (funcall func (car nlist)))
            (progn
              ;(format t "f2 ~A~2%" (car listt))
              (setq nlist (append (list (car listt)) nlist))
            ;  (format t "f2 ~A~2%" nlist)
              (return-from mostn2 nlist)
            )
            (progn
              ;(format t "f3 ~A~2%" (car listt))
              (return-from mostn2 nlist)
            )
          )
        )
      )
    )
  )
)


(defun match (list1 list2)
;(format t "enter1 ~A~2%" list1)
;(format t "enter2 ~A~2%" list2)
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
              ;(format t "nil1 ~A~2%" (cdr list1))
              ;(format t "nil2 ~A~2%" (cdr list2))
              (return-from match (match (cdr list1) (cdr list2)))
            )
            )
          )
        )
        )
        )
    ) ;done with first condition
    ( (or (eq (car list1) (car list2)) (eq (car list1) '?))
    ;    (format t "or1 ~A~2%" list1)
      ;  (format t "or2 ~A~2%" list2)
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
      ;(format t "else part1  ~A~2%" list1)
    ;  (format t "else part2  ~A~2%" list2)
    ;  (print "test")
      (return-from match nil)
    );else condition means not matched so return false
  )
)
