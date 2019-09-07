<!--- Local IspellDict: en -->

This repository provides *oer-reveal*, a package to extend
[org-re-reveal](https://gitlab.com/oer/org-re-reveal)
with resources and functionality that aim to simplify the creation of
[Open Educational Resources (OER)](https://en.wikipedia.org/wiki/Open_educational_resources).
This package forms the basis of [emacs-reveal](https://gitlab.com/oer/emacs-reveal).

More specifically, *oer-reveal* provides:
- Installation and configuration of reveal.js with several plugins,
  e.g., for audio explanations, for quizzes, to see notes, to display
  a hyperlinked table of contents as footer
- An Org export backend that extends *org-re-reveal*
  - With keys `C-c C-e w w` and `C-c C-e w b`
- Org macros to embed OER figures with proper license attribution (in
  machine-readable RDFa format for HTML export)

[Sample Org files](https://gitlab.com/oer/oer-reveal/tree/master/examples) for *oer-reveal* are available in its repository.
Besides, there is a [howto for emacs-reveal](https://oer.gitlab.io/emacs-reveal-howto/howto.html).
