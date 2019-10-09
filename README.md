<!--- Local IspellDict: en -->
<!--- Copyright (C) 2018-2019 Jens Lechtenbörger -->
<!--- SPDX-License-Identifier: CC0-1.0 -->

# Relationships between projects

| Package                                                       | Description                                                                                                                                      |
|---------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------|
| [org-reveal](https://github.com/yjwen/org-reveal)             | Origin of org-re-reveal                                                                                                                          |
| [org-re-reveal](https://gitlab.com/oer/org-re-reveal)         | Fork of org-reveal, initially to add audio fragments, now with [various changes](https://gitlab.com/oer/org-re-reveal/blob/master/CHANGELOG.org) |
| [org-re-reveal-ref](https://gitlab.com/oer/org-re-reveal-ref) | Addon to org-re-reveal for bibliography slide based on [org-ref](https://github.com/jkitchin/org-ref)                                            |
| [oer-reveal](https://gitlab.com/oer/oer-reveal)               | Export backend derived from org-re-reveal; functionality for installation of reveal.js and plugins; simplification of licensing for OER          |
| [emacs-reveal](https://gitlab.com/oer/emacs-reveal/)          | Bundling of org-re-reveal, org-re-reveal-ref, and oer-reveal                                                                                     |

Originally, *emacs-reveal* was created to enhance *org-reveal*, and it
contained the code of what is now maintained separately as
*org-re-reveal-ref* and *oer-reveal*.  Those packages were separated
to make as much of *emacs-reveal* available on MELPA as possible.
Whether anyone wants to use those package in isolation is up to them.
I recommend to go for *emacs-reveal*, for which a
[howto](https://oer.gitlab.io/emacs-reveal-howto/howto.html) is
available.  Note that as of October 2019 the submodules installed by
*emacs-reveal* have a size of 226 MB; hence, initial installation
takes some time.

The package *oer-reveal* was created for use in conjunction with
*emacs-reveal* (available at https://gitlab.com/oer/emacs-reveal, not
on MELPA) but can also be used individually if you know what to
expect:

1. Submodules (for reveal.js and plugins) are *not* installed
   automatically, stable paths to contained resources are *not*
   created automatically.  See file
   [emacs-reveal.el](https://gitlab.com/oer/emacs-reveal/blob/master/emacs-reveal.el)
   for sample initialization code.
   A howto for emacs-reveal is available, which serves as sample
   emacs-reveal presentation:
   - [Org source](https://gitlab.com/oer/emacs-reveal-howto)
   - [Reveal.js presentation](https://oer.gitlab.io/emacs-reveal-howto/howto.html)
2. Publishing of Org *projects* is different from export of
   individual Org *files*.  Export can be triggered with the usual
   key binding `C-c C-e` (see below), while publishing is performed
   with `oer-reveal-publish-all`, which requires setup as described
   under 1.  In particular, `oer-reveal-publish-all` publishes
   entire projects (with all necessary resources) based on
   `org-publish-project-alist`, while interactive export of
   individual Org files only creates an HTML file, for which
   necessary resources must be available locally.

# Functionality

This repository provides *oer-reveal*, a package to extend
[org-re-reveal](https://gitlab.com/oer/org-re-reveal)
with resources and functionality that aim to simplify the creation of
[Open Educational Resources (OER)](https://en.wikipedia.org/wiki/Open_educational_resources).
This package defines an Org mode export backend derived from
*org-re-reveal* for export to HTML with reveal.js.  It provides help
in installing and configuring reveal.js and several of its plugins.
This package forms the basis of [emacs-reveal](https://gitlab.com/oer/emacs-reveal).

More specifically, *oer-reveal* provides:
- Installation and configuration of reveal.js with several plugins,
  e.g., for audio explanations, for quizzes, to see notes, to display
  a hyperlinked table of contents as footer
- An Org export backend that extends *org-re-reveal*
  - With keys `C-c C-e w w` and `C-c C-e w b`
- Org macros to embed OER figures with proper license attribution (in
  machine-readable RDFa format for HTML export)

[Sample Org files](https://gitlab.com/oer/oer-reveal/tree/master/examples)
for *oer-reveal* are available in its repository.
Besides, there is a [howto for emacs-reveal](https://oer.gitlab.io/emacs-reveal-howto/howto.html).

As usual for Org export, use `C-c C-e` to start an export, followed by
backend specific key bindings.  With `oer-reveal`, the default
bindings are `C-c C-e w w` and `C-c C-e w b`, which can be customized
with `oer-reveal-keys`.  (Actually, "ö" seems preferable to "w", if it
exists on your keyboard.)

Notably, `oer-reveal` simplifies one traditionally cumbersome task
for OER creators, namely the re-use of figures under free licenses
that require proper attribution.  Towards that end, macros
`revealimg`, `reveallicense`, and `revealgrid` are defined and
documented in file [org/config.org](org/config.org).

# Usage

Variable `oer-reveal-dir` points to the directory of oer-reveal and
its embedded resources.  You may want to use that variable in your
own publication code, for which some pointers are provided in
function `oer-reveal-publish-all` of file
[oer-reveal-publish.el](oer-reveal-publish.el).
Note that subdirectory "title-slide" contains some variants for
title slides of presentations, and subdirectory "css" contains
sample CSS.  Subdirectory "org" contains Org files to embed in
presentations.  Please be warned that included resources, in
particular CSS files, may change in incompatible ways.  You may
want to work with your own copies.

Function `oer-reveal-setup-submodules` downloads and installs
reveal.js and some of its plugins into the directory
`oer-reveal-submodules-dir`.  Function
`oer-reveal-generate-include-files` generates Org files under
`oer-reveal-org-includes-dir`, which include Org files coming with
`oer-reveal`; when installing `oer-reveal` from MELPA (with
changing directories upon updates) you can include those generated
files at stable locations in your own Org files.

Function `oer-reveal-publish-setq-defaults` changes variables from
other packages, which may offer some suggestions what to adapt in
your own configuration.

Again, note that the file
[emacs-reveal.el](https://gitlab.com/oer/emacs-reveal/blob/master/emacs-reveal.el),
hosted at https://gitlab.com/oer/emacs-reveal, provides the following sample
initialization code for *oer-reveal*, and the howto at
https://gitlab.com/oer/emacs-reveal-howto offers a sample presentation
using this code.

```
(require 'oer-reveal-publish)
(oer-reveal-setup-submodules t)
(oer-reveal-generate-include-files t)
(oer-reveal-publish-setq-defaults)
```

# Customizable options

Variable `oer-reveal-script-files` lists JavaScript files to load
when initializing reveal.js.  If you use the version of reveal.js
coming with oer-reveal, you may want to assign the value of
`oer-reveal-script-files` to `org-re-reveal-script-files`.  This
also happens in `oer-reveal-publish-setq-defaults`.

Variable `oer-reveal-plugins` lists reveal.js plugins to be
activated.  To configure those plugins, customize
`oer-reveal-plugin-config`, which in turn points to customizable
variables for individual plugins.

When generating image grids, `oer-reveal-export-dir` specifies
the directory into which to generate CSS code.  This should
probably be the directory into which you publish your HTML code.
I set this to "./" before exporting with `C-c C-e w b`.
The names of generated CSS files for image grids are determined by
`oer-reveal-css-filename-template`.

To see all customizable variables, check out customization group
`oer-reveal`: `M-x customize-group` followed by `oer-reveal`.
In particular, `oer-reveal-publish-babel-languages` can be used to
activate Babel languages during HTML (and PDF) export, e.g., to
generate figures from embedded sources (e.g., dot/graphviz or ditaa).
