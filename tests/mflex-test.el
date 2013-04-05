(require 'ert)
(require 'mflex)


;; for "every" function
(require 'cl)

(ert-deftest mflex-test-sanity ()
  "sanity check."
  (should (= 1 1)))


(ert-deftest mflex-all-combinations ()
  "making combinations."
  (should (equal '((1) (2) (3))
                 (mflex-all-combinations '(1 2 3) 1)))
  (should (= 210
             (length (mflex-all-combinations '(1 2 3 4 5 6 7 8 9 10) 4)))))

(ert-deftest mflex-basename-basic ()
  "basic scoring -- matches get number, non-matches get nil"
  ;; matches
  (mapc (lambda (str)
          (should (mflex-score "a" str 'file)))
        '("a"
          "ba"
          "ab"
          ".a"
          "aaaa"
          "foo.bra"
          "a/foo"
          "b/a/foo"
          "b/.a/foo"
          "b/.a./foo"))
  ;; empty string should match everything
  (mapc (lambda (str)
          (should (mflex-score "" str 'file)))
        '(""
          "zz"
          "."))
  ;; non-matches
  (mapc (lambda (str)
          (should-not (mflex-score "a" str 'file)))
        '(""
          "zz"
          ".")))


(ert-deftest mflex-basename-entire ()
  "whole match is preferred"
  (let* ((query "a")
         (higher (mflex-score "a" query 'file))
         (lower (mflex-score "ab" query 'file)))
    (should (> higher lowercase))))

(ert-deftest mflex-basename-order ()
  ""
  (let* ((query "a")
         (higher (mflex-score "a_b_c" query 'file))
         (lower (mflex-score "b_a_c" query 'file)))
    (should (> higher lowercase))))


