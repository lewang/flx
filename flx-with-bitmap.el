
;;; what if get-bitmask didn't convert string to list, but used indexes???
;;;
;;;   - Using bitmasks still slower than not

;;; consing things is really slow but computation is fast.
;;; credit note: Daniel Skarda ido-speed-hack for bitmap idea

;;; Use defsubst instead of defun

;;; Should use bitmap optimizations to filter out strings that can't possible
;;; match.
;;;
;;;     Also use bitmaps to record substring after char will match?
;;;


(eval-when-compile
  (require 'cl)
  (defmacro mflex-64-eval (form-64 &rest forms-32)
    "Expand to FORM-64 on 64 bit build otherwise FORMS-32."
    (declare (indent 1))
    (if (not (= -536870912 (1+ 536870911)))
        form-64
      (cons 'progn forms-32))))

(defvar mflex-bitmap-table
  (eval-when-compile
    (let ((table (make-hash-table :size 64 :test 'eq))
          (i -1))
      (loop for upper from ?A upto ?Z
            for lower from ?a upto ?z
            do (progn
                 (incf i)
                 (puthash upper i table)
                 (puthash lower i table)))
      (puthash ?\/ (incf i) table)
      (puthash ?\  (incf i) table)
      (incf i)
      (loop for num from ?0 upto ?9
            do (puthash num i table))
      (mflex-64-eval
          (loop for char in '(?\~
                              ?\}
                              ?\|
                              ?\{
                              ?\`
                              ?\_
                              ?^
                              ?\]
                              ?\\
                              ?\[
                              ?\@
                              ?\?
                              ?\>
                              ?\=
                              ?\<
                              ?\;
                              ?\:
                              ?\.
                              ?\-
                              ?\,
                              ?\+
                              ?\*
                              ?\)
                              ?\(
                              ?\'
                              ?\&
                              ?\%
                              ?\$
                              ?\#
                              ?\"
                              ?\!)
                do (puthash char (incf i) table)))
      table))
    "Extra characters to insert into the bitmap.

The bitmah shoud consist of

for 32-bit systems:

  - one bit for each character a-z
  - any number
  - slash
  - space

for 64-bit systems:
  - add a bunch of extra characters
")

(defun mflex-get-bitmask (obj &optional beg end)
  "Compute string bitmask "
  (if (characterp obj)
      (lsh 1 (gethash obj mflex-bitmap-table))
    (let ((beg (or beg 0))
          (end (or end (length obj))))
      (loop with res = 0
            for index from beg below end
            do (let* ((char (aref obj index))
                      (char-hash (lsh 1 (gethash char mflex-bitmap-table))))
                 (setq res (logior res char-hash)))
            finally return res))))

(defun mflex-get-hash-for-string (str)
  "Return hash-table for string where keys are characters value
  is a sorted list of indexes for character occurrences."
  (let* ((res (make-hash-table :test 'eq :size 32))
         (str-len (length str))
         (last-mask 0)
         (mask-vector (make-vector str-len nil))
         char)
    (loop for index from (1- str-len) downto 0
          do (progn
               (setq char (aref str index))
               (push index (gethash char res))
               (setq last-mask (logior
                                last-mask (mflex-get-bitmask char)))
               (aset mask-vector index last-mask)))
    (puthash 'mask-vector mask-vector res)
    res))

(defsubst mflex-unroll-heat (rolled)
  "unroll ROLLED into

|------restbits-score------|--12bits-group-index--|--1bit-boost--|

1bit-boost decides to boost the basepath part of the path
group-index is index of character within group
score is score bro.
"
  (list (lsh rolled -13)
        (logand (lsh rolled -1) #xfff)
        (logand 1 rolled)))

(defsubst mflex-roll-heat (score cont boost)
  "roll SCORE and CONTINUITY into one fixnum.  see `mflex-unroll-heat'"
  (+ (lsh score 13)
     (lsh (min #xfff cont) 1)
     (logand 1 boost)))

;;; do we need more word separators than Sublime Text?
(defsubst mflex-is-word (char)
  "returns t if char is word"
  (and char
       (not (memq char '(?\  ?- ?_ ?. ?/)))))

(defsubst mflex-is-capital (char)
  "returns t if char is word"
  (and char
       (and (<= char ?Z)
            (<= ?A char))))

(defsubst mflex-is-boundary (last-char char)
  (cond ((mflex-is-capital char)
         t)
        ((and (not (mflex-is-word last-char))
              (mflex-is-word char))
         t)))

(defsubst mflex-inc-vec (vec &optional inc beg end)
  "increment each element of vectory by INC(default=1)
from BEG (inclusive) to end (not inclusive).
"
  (or inc
      (setq inc 1))
  (or beg
      (setq beg 0))
  (or end
      (setq end (length vec)))
  (while (< beg end)
    (incf (aref vec beg) inc)
    (incf beg))
  vec)

;;; So we store one fixnum per character.  Is this too memory inefficient?
(defun mflex-get-heatmap-vector (str)
  "Generate heat map vector of string.

See documentation for logic."
  (let* ((str-len (length str))
         (str-last-index (1- str-len))
         ;; ++++ base
         (scores (make-vector str-len -35))
         (group-separator ?/)
         (penalty-lead ?.)
         (groups-alist (list (cons -1 nil))))
    ;; ++++ final char bonus
    (incf (aref scores str-last-index) 1)
    ;; Establish baseline mapping
    (loop for char across str
          for index from 0
          with last-char = nil
          do (progn
               (when (mflex-is-boundary last-char char)
                 (setcdr (car groups-alist) (cons index (cdar groups-alist))))
               ;; ++++ -45 penalize extension
               (when (eq last-char penalty-lead)
                 (incf (aref scores index) -45))
               (when (= group-separator char)
                 (push (cons index nil) groups-alist))
               (setq last-char char)))
    (let* ((group-count (length groups-alist))
           (separator-count (1- group-count)))
      ;; ++++ slash group-count penalty
      (unless (zerop separator-count)
        (mflex-inc-vec scores (* -2 group-count)))
      ;; score each group further
      (loop for group in groups-alist
            for index from separator-count downto 0
            with last-group-limit = nil
            do (let ((group-start (car group))
                     (words-length (length (cdr group)))
                     (basepath-p (not last-group-limit)))
                 (let (num)
                   (setq num
                         (if basepath-p
                             (+ 35
                                ;; ++++ basepath separator-count boosts
                                (if (> separator-count 1)
                                    (1- separator-count)
                                  0)
                                ;; ++++ basepath word count penalty
                                (- words-length))
                           ;; ++++ non-basepath penalties
                           (if (= index 0)
                               -3
                             (+ -5 (1- index)))))
                   (mflex-inc-vec scores num (1+ group-start) last-group-limit))
                 (loop for word in (cdr group)
                       for word-index from (1- words-length) downto 0
                       with last-word = (or last-group-limit
                                            str-len)
                       do (progn
                            (incf (aref scores word)
                                  ;; ++++  beg word bonus AND
                                  85)
                            (loop for index from word below last-word
                                  for char-i from 0
                                  do (incf (aref scores index)
                                           (-
                                            ;; ++++ word order penalty
                                            (* -3 word-index)
                                            ;; ++++ char order penalty
                                            char-i)))
                            (setq last-word word)))
                 (setq last-group-limit (1+ group-start)))))
    scores))


(defsubst mflex-bigger-sublist (sorted-list val)
  "return sublist bigger than VAL from sorted SORTED-LIST

  if VAL is nil, return entire list."
  (if val
      (loop for sub on sorted-list
            do (when (> (car sub) val)
                 (return sub)))
      sorted-list))

(defun* mflex-matches (str query &optional hash greater-than q-index)
  "Return list of all unique indexes into str where query can match.

That is all character sequences of query that occur in str are returned.

HASH accept as the cached analysis of str.
sstr
e.g. (\"aab\" \"ab\") returns
       '((0 2) (1 2)
"

  (unless hash
    (setq hash (mflex-get-hash-for-string str)))
  (setq q-index (or q-index 0))
  (let ((mask (mflex-get-bitmask query q-index)))
    (unless (= mask
               (logand mask
                       (aref (gethash 'mask-vector hash)
                             (if greater-than
                                 (1+ greater-than)
                               0))))
      (return-from mflex-matches nil)))
  (let* ((q-char (aref query q-index))
         (indexes (mflex-bigger-sublist
                   (gethash q-char hash) greater-than)))
    (if (< q-index (1- (length query)))
        (apply                          ; `mapcan'
         'nconc
         (mapcar
          (lambda (index)
            (let ((next-matches-for-rest (mflex-matches str query hash index (1+ q-index))))
              (when next-matches-for-rest
                (mapcar (lambda (match)
                          (cons index match))
                        next-matches-for-rest))))
          indexes))
      (mapcar 'list indexes))))


;; (defun* mflex-matches-pure-recurse (str query)
;;   "Naive pure recursive implementation of `mflex-matches'."
;;   (unless (listp str)
;;     (setq str (string-to-list str)))
;;   (unless (listp query)
;;     (setq query (string-to-list query)))
;;   (let (index)
;;     (setq matched-str
;;           (loop for chars on str
;;                 for char in str
;;                 for i from 0
;;                 with q = (car query)
;;                 do (when (= char q)
;;                      (setq index i)
;;                      (return chars))))
;;     (when matched-str
;;       (let ((q-rest (cdr query))
;;             (str-rest (nthcdr (1+ index) str)))
;;         (nconc
;;          (if q-rest
;;              (mapcar (lambda (match)
;;                        (cons index
;;                              (mapcar (lambda (i)
;;                                        (and i (+ 1 index i)))
;;                                      match)))
;;                      ;; match rest of query
;;                      (mflex-matches-pure-recurse str-rest q-rest))
;;            (list (list index)))
;;          (when str-rest
;;            (mapcar (lambda (match)
;;                      (mapcar (lambda (i)
;;                                (and i (+ 1 index i)))
;;                              match))
;;                    ;; match query again
;;                    (mflex-matches2 str-rest query))))))))

(provide 'mflex)


;;; macro expanded


