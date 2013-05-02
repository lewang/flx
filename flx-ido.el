;;; flx-ido.el --- flx integration for ido

;; this file is not part of Emacs

;; Copyright (C) 2013 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: flx integration for ido
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Sun Apr 21 20:38:36 2013 (+0800)
;; Version: 0.1
;; Last-Updated:
;;           By:
;;     Update #: 8
;; URL:
;; Keywords:
;; Compatibility:

;;; Installation:

;; Add to your init file:
;;
;;     (require 'flx-ido)
;;     (setq ido-enable-flex-matchint t
;;           flx-ido-use              t)
;;
;;

;;; Commentary:

;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; credit to Scott Frazer's blog entry here:http://scottfrazersblog.blogspot.com.au/2009/12/emacs-better-ido-flex-matching.html
;;;


;;; Code:

(eval-when-compile (require 'cl))
(require 'ido)
(require 'flx)

(defvar flx-ido-narrowed-matches-hash (make-hash-table :test 'equal))

(defun flx-ido-narrowed (query items)
  "Get the value from `flx-ido-narrowed-matches-hash' with the
  longest prefix match."
  (let (best-match
        exact
        res)
    (loop for key being the hash-key of flx-ido-narrowed-matches-hash
          do (when (and (>= (length query) (length key))
                        (eq t
                            (compare-strings query 0 (min (length query)
                                                          (length key))
                                             key 0 nil))
                        (or (null best-match)
                            (> (length key) (length best-match))))
               (setq best-match key)
               (when (= (length key)
                        (length query))
                 (setq exact t)
                 (return))))
    (setq res (cond (exact
                     (gethash best-match flx-ido-narrowed-matches-hash))
                    (best-match
                     (flx-ido-undecorate (gethash best-match flx-ido-narrowed-matches-hash)))
                    (t
                     (flx-ido-undecorate items))))
    (list exact res)))

(defun flx-ido-undecorate (strings)
  (flx-ido-decorate strings t))


(defun flx-ido-decorate (things &optional clear)
  (let ((decorate-count (min ido-max-prospects
                             (length things))))
    (nconc
     (loop for thing in things
           for i from 0 below decorate-count
           collect (if clear
                       (substring-no-properties thing)
                     ;; copy the string in case it's "pure"
                     (flx-propertize (copy-sequence (car thing)) (cdr thing))))
     (if clear
         (nthcdr decorate-count things)
       (mapcar 'car (nthcdr decorate-count things))))))

(defun flx-ido-match (query items)
  "Better sorting for flx ido matching."
  (when (and (equal "" query)
             (not (gethash query flx-ido-narrowed-matches-hash)))
    ;; original function reverses list.
    (setq items (nreverse items))
    (puthash query items flx-ido-narrowed-matches-hash))
  (destructuring-bind (exact items)
      (flx-ido-narrowed query items)
    (if exact                         ; `ido-rotate' case is covered by exact match
        items
      (let* ((matches (loop for item in items
                            for score = (flx-score item query flx-file-cache)
                            if score
                            collect (cons item score)
                            into matches
                            finally return matches))
             res)
        (setq res (flx-ido-decorate (sort matches
                                          (lambda (x y) (> (cadr x) (cadr y))))))
        (puthash query res flx-ido-narrowed-matches-hash)))))

(defvar flx-ido-use t
  "Use flx matching for ido.")

(defadvice ido-read-internal (before flx-ido-reset-hash activate)
  "clear our narrowed hash."
  (clrhash flx-ido-narrowed-matches-hash))

(defadvice ido-set-matches-1 (around flx-ido-set-matches-1 activate)
  "Choose between the regular ido-set-matches-1 and my-ido-fuzzy-match"
  (if (and flx-ido-use
           ido-enable-flex-matching)
      (setq ad-return-value (flx-ido-match ido-text (ad-get-arg 0)))
    ad-do-it))

(provide 'flx-ido)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flx-ido.el ends here

