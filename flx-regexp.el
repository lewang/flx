
;;; I'm stopping the regexp investigation because this cannot yield the
;;; correct answer for cases when preferring BoW leads us down the wrong path
;;; (see test "mflex-filename-not-always-prefer-bounds").
;;;
;;;
;;; This regexp implementation is incorrect, and unoptimized.
;;;
;;;   1. (insert (format "%S"(let* ((case-fold-search nil)
;;;                            (mflex-max-groups 1)
;;;                            (mflex-max-chars 1)
;;;                            (regexp (mflex-get-query-regexp "a")))
;;;                       regexp)))
;;;
;;;       "\\(?:\\(\\<a\\|A\\)\\)"
;;;
;;;      But it should also match just the a character.
;;;
;;;  2. The regexp generated is suboptimal, because it repeats the characters
;;;     after `mflex-max-chars' for each grouping, when this could be pulled out.
;;;
;;;
;;;



(eval-when-compile
  (require 'cl))

(defvar mflex-max-groups 2
  "Maximum number of word boundaries to look for")

(defvar mflex-max-chars 4
  "The first `mflex-max-chars' characters will be
  looked at as possible word starters.")

;;; TODO: use this?
(defvar mflex-syntax-table
  (let ((table (make-syntax-table)))
    ;; ...
    ()))

(defmacro mflex-map-inject (list separator)
  "Inject separator between elements of list `mapconcat' for lists"
  `(let ((l ,list))
     (apply 'nconc
            (loop for x in l
                  for index from 0
                  with last-index = (1- (length l))
                  collect (if (< index last-index)
                              (list x ,separator)
                            (list x))))))

(defun mflex-all-combinations (list num)
  "return all combinations of sequences of NUM elements from list.

The length of the resulting list is nCr(length-of-list, num)."
  (if (= num 0)
      (list nil)
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
                     (mflex-all-combinations rest num)))))))

(defun mflex-query-to-positions (query)
  "Return list of index into query used to create the flex
  regexp."
  (let ((count 0)
        (push-count 0)
        res)
    (dolist (char (string-to-list query))
      (when (and (>= char ?a)
                 (<= char ?z ))
        (push count res)
        (incf push-count))
      (if (> push-count mflex-max-chars)
          (return)
        (incf count)))
    (reverse res)))


(defun mflex-position-to-regexp-list (positions query)
  "Convert list of positions wrt query into regexp"
  (apply 'nconc
         (mflex-map-inject
          (loop with next-pos = (pop positions)
                for index from 0
                for char across query
                collect (nconc
                         (list "\\(")
                         (if (and next-pos
                                  (= index next-pos))
                             (progn
                               (setq next-pos (pop positions))
                               (list "\\<"
                                     (string char)
                                     "\\|"
                                     (string (upcase char))))
                           (list "[" (string char (upcase char))"]"))
                         (list "\\)")))
          (list ".*?"))))

(defun mflex-combo-to-regexp-list (query combo)
  (destructuring-bind (flex-part rest)
      (if (> (length query) mflex-max-chars)
          (list (substring query 0 mflex-max-chars)
                (substring query mflex-max-chars))
        (list query nil))
    (list
     "\\(?:"
     (mflex-position-to-regexp-list combo flex-part)
     (when rest
       (list ".*?"
             (mflex-position-to-regexp-list nil rest)))
     "\\)")))

(defun mflex-flatten (l)
  "flatten list"
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun mflex-get-query-regexp (query)
  "Return regexp to do flex searching"
  (let ((positions (mflex-query-to-positions query)))
    (apply 'concat
           (mflex-flatten
            (list
             (loop for count from (min (length positions) mflex-max-groups) downto 1
                   collect (let ((combos (mflex-all-combinations positions count)))
                             (mflex-map-inject
                              (mapcar
                               (apply-partially 'mflex-combo-to-regexp-list query)
                               combos)
                              "\\|"))))))))


(provide 'mflex-regexp)
