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

(require 'flx-ido)
(require 'flx-test-list)

(setq ido-enable-flex-matching t
      flx-ido-use t)

(defun flx-ido-demo ()
  (interactive)
  (require 'flx-test-list)
  (ido-completing-read ": " foo-list))

(defun flx-ido-big-demo (max)
  (interactive "P")
  (setq max (or max
                most-positive-fixnum))
  (let* ((names (loop for i in (ucs-names)
                      for stop below max
                      collect (car i)))
         (names-length (length names)))
    (ido-completing-read (format "ucs (%s total): " names-length)
                         names)))

(provide 'flx-ido-demo)
