;;;
;;; credit to scott frazer's blog entry here:http://scottfrazersblog.blogspot.com.au/2009/12/emacs-better-ido-flex-matching.html
;;;

(require 'ido)
(require 'flx)

;;; dynamically bound by ido
(defvar hist)

(defun flx-ido-match (query items)
  "Better sorting for flx ido matching."
  (if (zerop (length query))
      items
    (let ((cache flx-file-cache)
          matches)
      (mapc (lambda (item)
              (let ((score (flx-score item query cache)))
                (when score
                  (push (cons item (car score)) matches))))
            items)
      (mapcar 'car (if ido-rotate
                       matches
                     (sort matches (lambda (x y) (> (cdr x) (cdr y)))))))))

(defvar flx-ido-use t
  "*Use flx matching for ido.")

(defadvice ido-set-matches-1 (around flx-ido-set-matches-1 activate)
  "Choose between the regular ido-set-matches-1 and my-ido-fuzzy-match"
  (if flx-ido-use
      (setq ad-return-value (flx-ido-match ido-text (ad-get-arg 0)))
    ad-do-it))


(setq ido-enable-flex-matching t)

(defun ido-demo ()
  (interactive)
  (require 'flx-test-list)
  (ido-completing-read ": " foo-list))


;;;;;;;;;;;;;;;;;;;;;;;;; testing

;; (defvar ido-enable-replace-completing-read t
;;   "If t, use ido-completing-read instead of completing-read if possible.

;;     Set it to nil using let in around-advice for functions where the
;;     original completing-read is required.  For example, if a function
;;     foo absolutely must use the original completing-read, define some
;;     advice like this:

;;     (defadvice foo (around original-completing-read-only activate)
;;       (let (ido-enable-replace-completing-read) ad-do-it))")

;; ;; Replace completing-read wherever possible, unless directed otherwise
;; (defadvice completing-read
;;   (around use-ido-when-possible activate)
;;   (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
;;           (and (boundp 'ido-cur-list)
;;                ido-cur-list)) ; Avoid infinite loop from ido calling this
;;       ad-do-it
;;     (let ((allcomp (all-completions "" collection predicate)))
;;       (if allcomp
;;           (setq ad-return-value
;;                 (ido-completing-read prompt
;;                                      allcomp
;;                                      nil require-match initial-input hist def))
;;         ad-do-it))))

;; (ido-everywhere t)

(provide 'flx-ido)
