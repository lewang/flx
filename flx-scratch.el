"\\(?:\\(\\<a\\|A\\).*?\\(\\<b\\|B\\)\\)\\(?:\\(\\<a\\|A\\).*?\\(b\\)\\)\\|\\(?:\\(a\\).*?\\(\\<b\\|B\\)\\)"

aadf ffasdfbsdf c


(setq gc-cons-threshold 8000000)
(insert (format "%S"(let* ((case-fold-search nil)
                           (mflex-max-groups 1)
                           (mflex-max-chars 1)
                           (regexp (mflex-get-query-regexp "a")))
                      regexp)))
"\\(?:\\(\\<a\\|A\\)\\)\\(?:\\([aA]\\)\\)"
"\\(?:\\(\\<a\\|A\\).*?\\([bB]\\).*?\\([cC]\\).*?\\([dD]\\)\\)
\\|\\(?:\\([aA]\\).*?\\([bB]\\).*?\\([cC]\\).*?\\([dD]\\)\\)"

"\\(?:\\(\\<a\\|A\\).*?\\([bB]\\).*?\\([cC]\\).*?\\([dD]\\)\\)
\\|\\(?:\\([aA]\\).*?\\(\\<b\\|B\\).*?\\([cC]\\).*?\\([dD]\\)\\)
\\|\\(?:\\([aA]\\).*?\\([bB]\\).*?\\(\\<c\\|C\\).*?\\([dD]\\)\\)
\\|\\(?:\\([aA]\\).*?\\([bB]\\).*?\\([cC]\\).*?\\([dD]\\)\\)"


;;; downto 0
"  \\(?:\\(\\<a\\|A\\).*?\\([bB]\\).*?\\([cC]\\).*?\\([dD]\\)\\)
\\|\\(?:\\([aA]\\).*?\\(\\<b\\|B\\).*?\\([cC]\\).*?\\([dD]\\)\\)
\\|\\(?:\\([aA]\\).*?\\([bB]\\).*?\\(\\<c\\|C\\).*?\\([dD]\\)\\)
\\|\\(?:\\([aA]\\).*?\\([bB]\\).*?\\([cC]\\).*?\\([dD]\\)\\)
   \\(?:\\([aA]\\).*?\\([bB]\\).*?\\([cC]\\).*?\\([dD]\\)\\)"

(mflex-get-query-regexp "abcdefghij")
(benchmark-run 100
  (apply 'concat (make-list 1000 "a")))

(benchmark-run 100
  (let (res
        (list (make-list 1000 "a")))
    (dolist (item list)
      (setq res (concat res item)))))



(benchmark-run 100
  (dotimes (i 100)
    (let ((h (make-hash-table :test 'eq)))
      (puthash ?a (list 1 2 3 4 5 6 7 8 9 10) h)
      (puthash ?b (list 1 2 3 4 5 6 7 8 9 10) h)
      (puthash ?c (list 1 2 3 4 5 6 7 8 9 10) h)
      (puthash ?d (list 1 2 3 4 5 6 7 8 9 10) h)
      (puthash ?e (list 1 2 3 4 5 6 7 8 9 10) h)
      (puthash ?f (list 1 2 3 4 5 6 7 8 9 10) h)
      (puthash ?g (list 1 2 3 4 5 6 7 8 9 10) h)
      (puthash ?h (list 1 2 3 4 5 6 7 8 9 10) h)
      (puthash ?i (list 1 2 3 4 5 6 7 8 9 10) h)
      (puthash ?j (list 1 2 3 4 5 6 7 8 9 10) h))))

(benchmark-run 100000
  (let ((h (make-hash-table :test 'eq)))
    (puthash ?a (vector 1 2 3 4 5 6 7 8 9 10) h)
    (puthash ?b (vector 1 2 3 4 5 6 7 8 9 10) h)
    (puthash ?c (vector 1 2 3 4 5 6 7 8 9 10) h)
    (puthash ?d (vector 1 2 3 4 5 6 7 8 9 10) h)
    (puthash ?e (vector 1 2 3 4 5 6 7 8 9 10) h)
    (puthash ?f (vector 1 2 3 4 5 6 7 8 9 10) h)
    (puthash ?g (vector 1 2 3 4 5 6 7 8 9 10) h)
    (puthash ?h (vector 1 2 3 4 5 6 7 8 9 10) h)
    (puthash ?i (vector 1 2 3 4 5 6 7 8 9 10) h)
    (puthash ?j (vector 1 2 3 4 5 6 7 8 9 10) h)))(6.410875999999999 35 5.82594299999937)(6.98802 40 6.481155000000172)


(setq alist nil)
(benchmark-run 100000
  (push (cons ?a (vector 1 2 3 4 5 6 7 8 9 10)) alist)
  (push (cons ?b (vector 1 2 3 4 5 6 7 8 9 10)) alist)
  (push (cons ?c (vector 1 2 3 4 5 6 7 8 9 10)) alist)
  (push (cons ?d (vector 1 2 3 4 5 6 7 8 9 10)) alist)
  (push (cons ?e (vector 1 2 3 4 5 6 7 8 9 10)) alist)
  (push (cons ?f (vector 1 2 3 4 5 6 7 8 9 10)) alist)
  (push (cons ?g (vector 1 2 3 4 5 6 7 8 9 10)) alist)
  (push (cons ?h (vector 1 2 3 4 5 6 7 8 9 10)) alist)
  (push (cons ?i (vector 1 2 3 4 5 6 7 8 9 10)) alist)
  (push (cons ?j (vector 1 2 3 4 5 6 7 8 9 10)) alist))

(vector)
(4.045558 2 2.576531000000159)

(list)
(5.741084 4 4.371603999999934)

(list (list))
(5.662621 5 4.207565999999815)

(setq alist nil)

(setq foo-hash (make-hash-table))

(push 'f (gethash 'foo foo-hash))

(gethash 'foo foo-hash)


(mflex-matches2 "aa" "a")
(mflex-matches2 "f a fbcd/fabcd/z" "abcd")
(benchmark-run 10000
  (let* ((str "f a fbcd/fabcd/z")
         (h (mflex-get-hash-for-string str)))
    (mflex-matches str "a" h)))


(let* ((str "f  abcd/agggfbcd/z")
       (h (mflex-get-hash-for-string str)))
  (benchmark-run 10000
    (mflex-matches str "gggg" h)))
;;; gggg against *gggf* without bitmask
(0.080481 0 0.0)
(0.081875 0 0.0)
(0.110706 1 0.031450999999999896)
;;; gggg against *gggf* with bitmask
(0.216329 3 0.05498800000000004)
(0.233953 4 0.07267499999999993)
(0.236138 4 0.07617499999999999)
;;; gggg without bitmask
(0.027082000000000002 1 0.018973000000000018)
(0.010993 0 0.0)
(0.026293999999999998 1 0.018352000000000035)
;;; gggg with bitmask
(0.042047999999999995 1 0.018706)
(0.037953 1 0.01822299999999999)

(let* ((str "f a fbcd/fabcd/z")
       (h (mflex-get-hash-for-string str)))
  (gethash 'mask-vector h))[234881071 234881071 234881071 234881071 100663343 100663343 100663343 100663343 100663343 100663343 100663311 100663310 ...]


(benchmark-run 100000
  (make-hash-table :size 64 :test 'eq))




(let* ((str "f  abcd/agggfbcd/z")
       (h (mflex-get-hash-for-string str)))
  (mflex-matches str "g" h))

(mflex-get-bitmask "gggg" 3)


(let* ((str "f  abcd/agggfbcd/z")
       (h (mflex-get-hash-for-string str)))
  (benchmark-run 1000000
    (mflex-matches str "y" h)))

;;; with bit-mask.  bit-mask just seems to slow it down.
(1.285536 0 0.0)

;;; without bitmask
(0.463834 0 0.0)




(length foo-list)

;;;;;;;;;
;; ido ;;
;;;;;;;;;

(require 'flx-test-list)
(completing-read ": " foo-list)