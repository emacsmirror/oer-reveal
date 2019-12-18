;;; oer-reveal-ert-tests.el --- Tests for oer-reveal  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2019 Jens Lechtenbörger

;;; Commentary:
;; Run tests interactively or as follows in batch mode:
;; emacs -batch -L [path to your org-re-reveal directory] -L . -l ert -l oer-reveal-ert-tests.el -f ert-run-tests-batch-and-exit

;;; Code:
(require 'oer-reveal)

(ert-deftest parse-external-plugins ()
  "Test parsing of external plugin configuration."
  (let* ((tmp-file (make-temp-file "ert"))
         (test-cases `((nil . nil)
                       ("nil" . nil)
                       ("()" . nil)
                       ("((p1 . \"one\"))" . ((p1 . "one")))
                       (((p1 . "one")) . ((p1 . "one")))
                       ("((p1 . \"one\") (p2 . \"two\"))" . ((p1 . "one") (p2 . "two")))
                       (((p1 . "one") (p2 . "two")) . ((p1 . "one") (p2 . "two")))
                       (,tmp-file . ((dummy . "one") (dummy . "two"))))))
    (unwind-protect
        (progn
          (write-region "one\ntwo" nil tmp-file)
          (dolist (test-case test-cases nil)
            (should (equal (oer-reveal--plugin-dependencies
                            `(:oer-reveal-plugins ()
                              :reveal-external-plugins ,(car test-case)
                              :oer-reveal-audio-slideshow-dependency "d1"
                              :oer-reveal-anything-dependency "d2"
                              :oer-reveal-audio-slideshow-config "c1"
                              :oer-reveal-anything-config "c2"))
                           (cdr test-case)))
            (should (equal (oer-reveal--plugin-dependencies
                            `(:oer-reveal-plugins ("reveal.js-plugins")
                              :reveal-external-plugins ,(car test-case)
                              :oer-reveal-audio-slideshow-dependency "d1"
                              :oer-reveal-anything-dependency "d2"
                              :oer-reveal-audio-slideshow-config "c1"
                              :oer-reveal-anything-config "c2"))
                           (append (cdr test-case)
                                   '((dummy . "d1") (dummy . "d2")))))
            (let ((oer-reveal-plugin-config '(("rp1" ("dp1" "dp2")))))
              (should (equal (oer-reveal--plugin-dependencies
                              `(:oer-reveal-plugins ("rp1")
                                :reveal-external-plugins ,(car test-case)))
                             (append (cdr test-case)
                                     '((dummy . "dp1") (dummy . "dp2"))))))))
      (delete-file tmp-file))))

(ert-deftest parse-init-script ()
  "Test parsing of external plugin configuration."
  (should (equal (oer-reveal--plugin-config
                  '(:oer-reveal-plugins ()
                    :reveal-init-script nil
                    :oer-reveal-audio-slideshow-dependency "d1"
                    :oer-reveal-anything-dependency "d2"
                    :oer-reveal-audio-slideshow-config "c1"
                    :oer-reveal-anything-config "c2"))
                   nil))
  (should (equal (oer-reveal--plugin-config
                  '(:oer-reveal-plugins ()
                    :reveal-init-script "init"
                    :oer-reveal-audio-slideshow-dependency "d1"
                    :oer-reveal-anything-dependency "d2"
                    :oer-reveal-audio-slideshow-config "c1"
                    :oer-reveal-anything-config "c2"))
                 (format oer-reveal-plugin-config-fmt "init")))
  (should (equal (oer-reveal--plugin-config
                  '(:oer-reveal-plugins ("reveal.js-plugins")
                    :reveal-init-script nil
                    :oer-reveal-audio-slideshow-dependency "d1"
                    :oer-reveal-anything-dependency "d2"
                    :oer-reveal-audio-slideshow-config "c1"
                    :oer-reveal-anything-config "c2"))
                 (concat
                  (format oer-reveal-plugin-config-fmt "c1")
                  (format oer-reveal-plugin-config-fmt "c2"))))
  (should (equal (oer-reveal--plugin-config
                  '(:oer-reveal-plugins ("reveal.js-plugins")
                    :reveal-init-script "c0"
                    :oer-reveal-audio-slideshow-dependency "d1"
                    :oer-reveal-anything-dependency "d2"
                    :oer-reveal-audio-slideshow-config "c1"
                    :oer-reveal-anything-config "c2"))
                 (concat
                  (format oer-reveal-plugin-config-fmt "c0")
                  (format oer-reveal-plugin-config-fmt "c1")
                  (format oer-reveal-plugin-config-fmt "c2")))))

(ert-deftest test-alternate-types ()
  "Test generation of alternate type information."
  (let ((oer-reveal-alternate-types
         '(("org" "text/org")
           ("pdf" "application/pdf"))))
    ;; Links without title attribute.
    (should (equal (oer-reveal-add-alternate-types
                    '("org") "git" "example.org/" "presentation")
                   "#+HTML_HEAD: <link rel=\"alternate\" type=\"text/org\" href=\"git/blob/master/presentation.org\"/>
#+TITLE: @@latex:\\footnote{This PDF document is an inferior version of an \\href{example.org/presentation.html}{OER HTML presentation}; free/libre \\href{git}{Org mode source repository}.}@@
"))
    (should (equal (oer-reveal-add-alternate-types
                    '("pdf") "git" "example.org/" "presentation")
                   "#+HTML_HEAD: <link rel=\"alternate\" type=\"application/pdf\" href=\"presentation.pdf\"/>
#+TITLE: @@latex:\\footnote{This PDF document is an inferior version of an \\href{example.org/presentation.html}{OER HTML presentation}; free/libre \\href{git}{Org mode source repository}.}@@
")))
  ;; Default value for oer-reveal-alternate-types, with link titles.
  (should (equal (oer-reveal-add-alternate-types
                  '("org" "pdf") "git" "example.org/" "presentation")
                 "#+HTML_HEAD: <link rel=\"alternate\" type=\"text/org\" href=\"git/blob/master/presentation.org\" title=\"Org mode source code of HTML presentation\"/>
#+HTML_HEAD: <link rel=\"alternate\" type=\"application/pdf\" href=\"presentation.pdf\" title=\"Concise PDF version of HTML presentation\"/>
#+TITLE: @@latex:\\footnote{This PDF document is an inferior version of an \\href{example.org/presentation.html}{OER HTML presentation}; free/libre \\href{git}{Org mode source repository}.}@@
")))
;;; oer-reveal-ert-tests.el ends here
