#+BEGIN_HTML
<a href="https://elpa.gnu.org/packages/dired-git-info.html"><img alt="GNU ELPA" src="https://elpa.gnu.org/favicon.png"/></a>
#+END_HTML

* Description

This Emacs packages provides a minor mode which shows git information inside
the dired buffer:

[[./images/screenshot2.png]]

* Installation

For manual installation, clone the repository and call:

#+BEGIN_SRC elisp
(package-install-file "/path/to/dired-git-info.el")
#+END_SRC

* Config

Bind the minor mode command in dired:

#+BEGIN_SRC elisp
(with-eval-after-load 'dired
  (define-key dired-mode-map ")" 'dired-git-info-mode))
#+END_SRC
