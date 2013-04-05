
(defun mflex-all-combinations (list num)
  "let LENGTH = (length list)

return all combinations of sequences of NUM elements from list.

The length of the resulting list is ("
  (let ((length (length list))
        (head (car list))
        (rest (cdr list)))
    (cond ((= 0 num)
           (list nil))
          ((= length num)
           (list list))
          (t
           (append (mapcar
                    (lambda (list)
                      (cons head list))
                    (mflex-all-combinations rest (1- num)))
                   (mflex-all-combinations rest num))))))

(provide 'mflex)