;;; oer-reveal-ert-tests.el --- Tests for oer-reveal  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2019 Jens Lechtenbörger

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

(defvar oer-metadata "((filename . \"./doesnotexist.png\")
 (licenseurl . \"https://creativecommons.org/publicdomain/zero/1.0/\")
 (licensetext . \"CC0 1.0\")
 (dc:source . \"https://example.org/\")
 (sourcetext . \"Sample source\")
 (cc:attributionName . \"Jens Lechtenboerger\")
 (cc:attributionURL . \"https://gitlab.com/lechten\"))")
(defvar oer-metadata-gif "((filename . \"./doesnotexist.gif\")
 (licenseurl . \"https://creativecommons.org/publicdomain/zero/1.0/\")
 (licensetext . \"CC0 1.0\")
 (dc:source . \"https://example.org/\")
 (sourcetext . \"Sample source\")
 (cc:attributionName . \"Jens Lechtenboerger\")
 (dcmitype . \"Image\"))")
(ert-deftest test-license-info ()
  "Tests for RDFa license information."
  (let ((meta (make-temp-file "oer.meta"))
        (meta-gif (make-temp-file "oer-gif.meta")))
    (with-temp-file meta (insert oer-metadata))
    (with-temp-file meta-gif (insert oer-metadata-gif))
    (let ((result
           (oer-reveal--attribution-strings meta))
          (result-gif
           (oer-reveal--attribution-strings meta-gif))
          (result-short
           (oer-reveal--attribution-strings meta nil nil nil t))
          (result-full
           (oer-reveal--attribution-strings meta "A caption" "50vh" "figure fragment appear" nil nil "data-fragment-index=\"1\""))
          )
      (should (equal (car result)
                     "<div about=\"./doesnotexist.png\" typeof=\"dcmitype:StillImage\" class=\"figure\"><p><img data-src=\"./doesnotexist.png\" alt=\"Figure\" /></p><p></p><p>&ldquo;<span property=\"dc:title\">Figure</span>&rdquo; by <a rel=\"cc:attributionURL dc:creator\" href=\"https://gitlab.com/lechten\" property=\"cc:attributionName\">Jens Lechtenboerger</a> under <a rel=\"license\" href=\"https://creativecommons.org/publicdomain/zero/1.0/\">CC0 1.0</a>; from <a rel=\"dc:source\" href=\"https://example.org/\">Sample source</a></p></div>"))
      (should (equal (car result-gif)
                     "<div about=\"./doesnotexist.gif\" typeof=\"dcmitype:Image\" class=\"figure\"><p><img data-src=\"./doesnotexist.gif\" alt=\"Figure\" /></p><p></p><p>&ldquo;<span property=\"dc:title\">Figure</span>&rdquo; by <span property=\"cc:attributionName\">Jens Lechtenboerger</span> under <a rel=\"license\" href=\"https://creativecommons.org/publicdomain/zero/1.0/\">CC0 1.0</a>; from <a rel=\"dc:source\" href=\"https://example.org/\">Sample source</a></p></div>"))
      (should (equal (car result-short)
                     "<div about=\"./doesnotexist.png\" typeof=\"dcmitype:StillImage\" class=\"figure\"><p><img data-src=\"./doesnotexist.png\" alt=\"Figure\" /></p><p></p><p><a rel=\"dc:source\" href=\"https://example.org/\">Figure</a> under <a rel=\"license\" href=\"https://creativecommons.org/publicdomain/zero/1.0/\">CC0 1.0</a></p></div>"))
      (should (equal (car result-full)
                     "<div about=\"./doesnotexist.png\" typeof=\"dcmitype:StillImage\" class=\"figure fragment appear\" data-fragment-index=\"1\"><p><img data-src=\"./doesnotexist.png\" alt=\"Figure\" style=\"max-height:50vh\" /></p><p>A caption</p><p style=\"max-width:50vh\">&ldquo;<span property=\"dc:title\">Figure</span>&rdquo; by <a rel=\"cc:attributionURL dc:creator\" href=\"https://gitlab.com/lechten\" property=\"cc:attributionName\">Jens Lechtenboerger</a> under <a rel=\"license\" href=\"https://creativecommons.org/publicdomain/zero/1.0/\">CC0 1.0</a>; from <a rel=\"dc:source\" href=\"https://example.org/\">Sample source</a></p></div>"))
  )))
;;; oer-reveal-ert-tests.el ends here
