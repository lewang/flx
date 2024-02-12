;; Copyright © 2013-2022 Le Wang -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'flx)
(require 'flx-test-list)

(defun flx-helm-candidate-transformer (candidates)
  "We score candidate and add the score info for later use.

The score info we add here is later removed with another filter."
  (if (zerop (length helm-pattern))
      candidates
    (let* ((mp-3-patterns (helm-mp-3-get-patterns helm-pattern))
           (flx-pattern (cdar mp-3-patterns))
           (patterns (cons (cons 'identity
                                 (mapconcat
                                  #'regexp-quote
                                  (split-string flx-pattern "" t)
                                  ".*"))
                           (cdr mp-3-patterns)))
           res)
      (setq res (loop for candidate in candidates
                      for matched = (loop for (predicate . regexp) in patterns
                                          always (funcall predicate (string-match regexp (helm-candidate-get-display candidate))))
                      if matched
                      collect (let ((score (flx-score candidate flx-pattern flx-file-cache)))
                                (unless (consp candidate)
                                  (setq candidate (cons (copy-sequence candidate) candidate)))
                                (setcdr candidate (cons (cdr candidate) score))
                                candidate)))
      (setq res (sort res
                      (lambda (a b)
                        (> (caddr a) (caddr b)))))
      (loop for item in res
            for index from 0
            for score = (cddr item)
            do (progn
                 ;; highlight first 20 matches
                 (when (and (< index 20) (> (car score) 0))
                   (setcar item (flx-propertize (car item) score 'add-score)))
                 (setcdr item (cadr item))))
      res)))

(defun flx-helm-test-candidates ()
  foo-list)

(defvar flx-helm-candidate-list-test
  '((name . "flx candidate-list-test")
    (init . (lambda ()
              (helm-init-candidates-in-buffer
               'global (flx-helm-test-candidates))))
    (candidates-in-buffer)
    (candidate-transformer flx-helm-candidate-transformer)
    (volatile)
    (match-strict identity)))

(defun flx-helm-demo ()
  (interactive)
  (helm :sources '(flx-helm-candidate-list-test)))

(defvar flx-helm-no-flx
  '((name . "flx no flx")
    (candidates . flx-helm-test-candidates)
    (volatile)))

(defun flx-helm-no-flx ()
  "Test Helm's volatile performance without flx."
  (interactive)
  (helm :sources '(flx-helm-no-flx)))
