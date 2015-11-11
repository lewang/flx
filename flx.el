;;; flx.el --- fuzzy matching with good sorting -*- lexical-binding: t -*-

;; Copyright © 2013, 2015 Le Wang

;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: fuzzy matching with good sorting
;; Created: Wed Apr 17 01:01:41 2013 (+0800)
;; Version: 0.6.1
;; Package-Requires: ((cl-lib "0.3"))
;; URL: https://github.com/lewang/flx

;; This file is NOT part of GNU Emacs.

;;; License

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

;;; Commentary:

;; Implementation notes
;; --------------------
;;
;; Use defsubst instead of defun
;;
;; * Using bitmaps to check for matches worked out to be SLOWER than just
;;   scanning the string and using `flx-get-matches'.
;;
;; * Consing causes GC, which can often slowdown Emacs more than the benefits
;;   of an optimization.

;;; Acknowledgments

;; Scott Frazer's blog entry http://scottfrazersblog.blogspot.com.au/2009/12/emacs-better-ido-flex-matching.html
;; provided a lot of inspiration.
;; ido-hacks was helpful for ido optimization

;;; Code:

(require 'cl-lib)

(defgroup flx nil
  "Fuzzy matching with good sorting"
  :group 'convenience
  :prefix "flx-")

(defcustom flx-word-separators '(?\  ?- ?_ ?: ?. ?/ ?\\)
  "List of characters that act as word separators in flx"
  :type '(repeat character)
  :group 'flx)

(defface flx-highlight-face  '((t (:inherit font-lock-variable-name-face :bold t :underline t)))
  "Face used by flx for highlighting flx match characters."
  :group 'flx)

;;; Do we need more word separators than ST?
(defsubst flx-separator-p (char)
  "Check if CHAR is a word character."
  (memq char flx-word-separators))

(defsubst flx-capital-p (char)
  "Check if CHAR is an uppercase character."
  (= char (upcase char)))

(defsubst flx-boundary-p (last-char char)
  "Check if LAST-CHAR is the end of a word and CHAR the start of the next.

This function is camel-case aware."
  ;; The following is logically equivalent to:
  ;; (or (and (not (and (not (flx-separator-p last-char))
  ;;                    (flx-capital-p last-char)))
  ;;          (and (not (flx-separator-p char))
  ;;               (flx-capital-p char)))
  ;;     (and (flx-separator-p last-char)
  ;;          (not (flx-separator-p char))))
  (and (not (flx-separator-p char))
       (or (flx-separator-p last-char)
           (and (not (flx-capital-p last-char))
             (flx-capital-p char)))))

(defsubst flx-inc-vec (vec inc beg end)
  "Increment each element of vectory by INC(default=1)
from BEG (inclusive) to END (not inclusive)."
  (while (< beg end)
    (cl-incf (aref vec beg) inc)
    (cl-incf beg))
  vec)

(defun flx-get-hash-for-string (str heatmap-func)
  "Return hash-table for string where keys are characters.
Value is a sorted list of indexes for character occurrences."
  (let* ((res (make-hash-table :test 'eq :size (length str)))
         (str-len (length str))
         down-char)
    (cl-loop for index from (1- str-len) downto 0
             for char = (aref str index)
          do (progn
               ;; simulate `case-fold-search'
               (if (and (not (flx-separator-p char))
                        (flx-capital-p char))
                   (progn
                     (push index (gethash char res))
                     (setq down-char (downcase char)))
                 (setq down-char char))
               (push index (gethash down-char res))))
    (puthash 'heatmap (funcall heatmap-func str) res)
    res))

;; So we store one fixnum per character.  Is this too memory inefficient?
(defun flx-get-heatmap-str (str &optional group-separator)
  "Generate the heatmap vector of string.

See documentation for logic."
  (let* ((str-len (length str))
         (str-last-index (1- str-len))
         ;; ++++ base
         (scores (make-vector str-len -35))
         (penalty-lead ?.)
         (groups-alist (list (list -1 0))))
    ;; ++++ final char bonus
    (cl-incf (aref scores str-last-index) 1)
    ;; Establish baseline mapping
    (cl-loop for char across str
          for index from 0
          with last-char = nil
          with group-word-count = 0
          do (progn
               (let ((effective-last-char
                      ;; before we find any words, all separaters are
                      ;; considered words of length 1.  This is so "foo/__ab"
                      ;; gets penalized compared to "foo/ab".
                      (if (zerop group-word-count) nil last-char)))
                 (when (flx-boundary-p effective-last-char char)
                   (setcdr (cdar groups-alist) (cons index (cl-cddar groups-alist))))
                 (when (and (not (flx-word-p last-char))
                            (flx-word-p char))
                   (cl-incf group-word-count)))
               ;; ++++ -45 penalize extension
               (when (eq last-char penalty-lead)
                 (cl-incf (aref scores index) -45))
               (when (eq group-separator char)
                 (setcar (cdar groups-alist) group-word-count)
                 (setq group-word-count 0)
                 (push (nconc (list index group-word-count)) groups-alist))
               (if (= index str-last-index)
                   (setcar (cdar groups-alist) group-word-count)
                 (setq last-char char))))
    (let* ((group-count (length groups-alist))
           (separator-count (1- group-count)))
      ;; ++++ slash group-count penalty
      (unless (zerop separator-count)
        (flx-inc-vec scores (* -2 group-count) 0 (length scores)))
      ;; score each group further
      (cl-loop for group in groups-alist
            for index from separator-count downto 0
            with last-group-limit = nil
            with basepath-found = nil
            do (let ((group-start (car group))
                     (word-count (cadr group))
                     ;; this is the number of effective word groups
                     (words-length (length (cddr group)))
                     basepath-p)
                 (when (not (or (zerop words-length)
                                basepath-found))
                   (setq basepath-found t
                         basepath-p t))
                 (let (num)
                   (setq num
                         (if basepath-p
                             (+ 35
                                ;; ++++ basepath separator-count boosts
                                (max (1- separator-count)
                                     0)
                                ;; ++++ basepath word count penalty
                                (- word-count))
                           ;; ++++ non-basepath penalties
                           (if (= index 0)
                               -3
                             (- index 6))))
                   (flx-inc-vec scores num (1+ group-start)
                                (or last-group-limit (length scores))))
                 (cl-loop for word in (cddr group)
                       for word-index from (1- words-length) downto 0
                       with last-word = (or last-group-limit
                                            str-len)
                       do (progn
                            (cl-incf (aref scores word)
                                  ;; ++++  beg word bonus AND
                                  85)
                            (cl-loop for index from word below last-word
                                  for char-i from 0
                                  do (cl-incf (aref scores index)
                                           (-
                                            ;; ++++ word order penalty
                                            (* -3 word-index)
                                            ;; ++++ char order penalty
                                            char-i)))
                            (setq last-word word)))
                 (setq last-group-limit (1+ group-start)))))
    scores))

(defun flx-get-heatmap-file (filename)
  "Return heatmap vector for filename."
  (flx-get-heatmap-str filename ?/))


(defsubst flx-bigger-sublist (sorted-list val)
  "Return sublist bigger than VAL from sorted SORTED-LIST

  if VAL is nil, return entire list."
  (if (zerop val)
      sorted-list
    (catch 'return
      (while sorted-list
        (if (> (car sorted-list) val)
            (throw 'return sorted-list)
          (setq sorted-list (cdr sorted-list)))))))

(defun flx-make-filename-cache ()
  "Return cache hashtable appropraite for storing filenames."
  (flx-make-string-cache 'flx-get-heatmap-file))

(defun flx-make-string-cache (&optional heat-func)
  "Return cache hashtable appropraite for storing strings."
  (let ((hash (make-hash-table :test 'equal
                               :size 4096)))
    (puthash 'heatmap-func (or heat-func 'flx-get-heatmap-str) hash)
    hash))

(defsubst flx-process-cache (str &optional cache)
  "Get calculated heatmap from cache, add it if necessary."
  (if cache
      (or (gethash str cache)
          (puthash str (flx-get-hash-for-string
                        str
                        (gethash 'heatmap-func cache))
                   cache))
    (flx-get-hash-for-string str 'flx-get-heatmap-str)))

(defun flx-find-best-match (str-info
                            heatmap
                            greater-than
                            query
                            query-length
                            q-index
                            match-cache)
  "Recursively compute the best match for a string, passed as STR-INFO and
HEATMAP, according to QUERY.

This function uses MATCH-CACHE to memoize its return values.
For other parameters, see `flx-score'"

  ;; Here, we use a simple N'ary hashing scheme
  ;; You could use (/ hash-key query-length) to get greater-than
  ;; Or, (mod hash-key query-length) to get q-index
  ;; We use this instead of a cons key for the sake of efficiency
  (let* ((hash-key (+ q-index
                      (* greater-than
                         query-length)))
         (hash-value (gethash hash-key match-cache)))
    (if hash-value
        ;; Here, we use the value 'no-match to distinguish a cache miss
        ;; from a nil (i.e. non-matching) return value
        (if (eq hash-value 0)
            nil
          hash-value)
      (let ((indexes (flx-bigger-sublist
                       (gethash (aref query q-index) str-info)
                       greater-than))
            (match)
            (temp-score)
            (best-score most-negative-fixnum))

        ;; Matches are of the form:
        ;; ((match_indexes) . (score . contiguous-count))
        (if (>= q-index (1- query-length))
            ;; At the tail end of the recursion, simply
            ;; generate all possible matches with their scores
            ;; and return the list to parent.
            (setq match (mapcar (lambda (index)
                                  (cons (list index)
                                        (cons (aref heatmap index) 0)))
                                indexes))
          (dolist (index indexes)
            (dolist (elem (flx-find-best-match str-info
                                               heatmap
                                               index
                                               query
                                               query-length
                                               (1+ q-index)
                                               match-cache))
              (setq temp-score
                    (if (= (1- (caar elem)) index)
                        (+ (cadr elem)
                           (aref heatmap index)

                           ;; boost contiguous matches
                           (* (min (cddr elem)
                                   3)
                              15)
                           60)
                      (+ (cadr elem)
                         (aref heatmap index))))

              ;; We only care about the optimal match, so only
              ;; forward the match with the best score to parent
              (when (> temp-score best-score)
                (setq best-score temp-score
                      match (list (cons (cons index (car elem))
                                        (cons temp-score
                                              (if (= (1- (caar elem))
                                                     index)
                                                  (1+ (cddr elem))
                                                0)))))))))

        ;; Calls are cached to avoid exponential time complexity
        (puthash hash-key
                 (or match 0)
                 match-cache)
        match))))

(defun flx-score (str query &optional cache)
  "Return best score matching QUERY against STR"
  (unless (or (zerop (length query))
              (zerop (length str)))
    (let ((str-info (flx-process-cache str cache))
          (query-length (length query))

          ;; Raise recursion limit
          (max-lisp-eval-depth 5000)
          (max-specpdl-size 10000)

          (optimal-match))

      (setq optimal-match (flx-find-best-match
                           str-info
                           ;; heatmap
                           (gethash 'heatmap str-info)
                           0
                           query
                           query-length
                           0
                           ;; Dynamic Programming table for memoizing
                           (make-hash-table :test 'eql
                                            :size 10)))
      ;; Postprocess candidate
      (and optimal-match
           (cons
            ;; This is the computed score, adjusted to boost the scores
            ;; of exact matches for short queries. It's assumed that longer
            ;; queries will get large scores anyway.
            (if (and (< 1 query-length)
                     (< query-length 5)
                     (=  (length (caar optimal-match))
                         (length str)))
                (+ (cl-cadar optimal-match) 10000)
              (cl-cadar optimal-match))

            ;; This is the list of match positions
            (caar optimal-match))))))

(defun flx-propertize (obj score &optional add-score)
  "Return propertized copy of obj according to score.

SCORE of nil means to clear the properties."
  (let ((block-started (cadr score))
        (last-char nil)
        (str (if (consp obj)
                 (substring-no-properties (car obj))
               (substring-no-properties obj))))

    (when score
      (dolist (char (cdr score))
        (when (and last-char
                   (not (= (1+ last-char) char)))
          (put-text-property block-started  (1+ last-char) 'face 'flx-highlight-face str)
          (setq block-started char))
        (setq last-char char))
      (put-text-property block-started  (1+ last-char) 'face 'flx-highlight-face str)
      (when add-score
        (setq str (format "%s [%s]" str (car score)))))
    (if (consp obj)
        (cons str (cdr obj))
      str)))



(defvar flx-file-cache nil
  "Cached heatmap info about strings.")

;;; reset value on every file load.
(setq flx-file-cache (flx-make-filename-cache))

(defvar flx-strings-cache nil
  "Cached heatmap info about filenames.")

;;; reset value on every file load.
(setq flx-strings-cache (flx-make-string-cache))


(provide 'flx)

;;; flx.el ends here
