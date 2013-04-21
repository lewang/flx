(require 'flx-ido)
(require 'flx-test-list)

(defun ido-demo ()
  (interactive)
  (require 'flx-test-list)
  (ido-completing-read ": " foo-list))

(defun ido-big-demo (max)
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