;;; dired-git-info.el --- Show git info in dired -*- lexical-binding: t; -*-
;; Copyright (C) 2019  Clemens Radermacher

;; Author: Clemens Radermacher <clemera@posteo.net>
;; Package-Requires: ((emacs "25"))
;; Version: 0.1
;; Keywords: files
;; URL: https://github.com/clemera/dired-git-info

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

;;; Commentary:
;;
;; Command to show last commit message and date (info shown is configurable)
;; of files in dired buffer of a git project.
;;

;;; Code:



(defface dgi-commit-message-face
  '((t (:inherit font-lock-comment-face)))
  "Face for commit message overlays.")

(defvar dgi-commit-message-format "%s\t%cr"
  "Format of the commit messages.

Entries separated by tabs are aligned. The placeholders are (from
git-log PRETTY FORMATS):

           · %H: commit hash

           · %h: abbreviated commit hash

           · %T: tree hash

           · %t: abbreviated tree hash

           · %P: parent hashes

           · %p: abbreviated parent hashes

           · %an: author name

           · %aN: author name (respecting .mailmap, see git-shortlog(1) or
                git-blame(1))

           · %ae: author email

           · %aE: author email (respecting .mailmap, see git-shortlog(1) or
                git-blame(1))

           · %ad: author date (format respects --date= option)

           · %aD: author date, RFC2822 style

           · %ar: author date, relative

           · %at: author date, UNIX timestamp

           · %ai: author date, ISO 8601-like format

           · %aI: author date, strict ISO 8601 format

           · %cn: committer name

           · %cN: committer name (respecting .mailmap, see git-shortlog(1) or
               git- blame(1))

           · %ce: committer email

           · %cE: committer email (respecting .mailmap, see git-shortlog(1) or
               git- blame(1))

           · %cd: committer date (format respects --date= option)

           · %cD: committer date, RFC2822 style

           · %cr: committer date, relative

           · %ct: committer date, UNIX timestamp

           · %ci: committer date, ISO 8601-like format

           · %cI: committer date, strict ISO 8601 format

           · %d: ref names, like the --decorate option of git-log(1)

           · %D: ref names without the\" (\", \")\" wrapping.

           · %e: encoding

           · %s: subject

           · %f: sanitized subject line, suitable for a filename

           · %b: body

           · %B: raw body (unwrapped subject and body)

           · %N: commit notes

           · %GG: raw verification message from GPG for a signed commit

           · %G?: show \"G\" for a good (valid) signature, \"B\" for
               a bad signature, \"U\" for a good signature with
               unknown validity, \"X\" for a good signature that
               has expired, \"Y\" for a good signature made by an
               expired key, \"R\" for a good signature made by a
               revoked key, \"E\" if the signature cannot be
               checked (e.g. missing key) and \"N\" for no
               signature

           · %GS: show the name of the signer for a signed commit

           · %GK: show the key used to sign a signed commit

           · %gD: reflog selector, e.g., refs/stash@{1} or
               refs/stash@{2 minutes ago}; the format follows the
               rules described for the -g option. The portion
               before the @ is the refname as given on the
               command line (so git log -g refs/heads/master
               would yield refs/heads/master@{0}).

           · %gd: shortened reflog selector; same as %gD, but the
               refname portion is shortened for human
               readability (so refs/heads/master becomes just
               master).

           · %gn: reflog identity name

           · %gN: reflog identity name (respecting .mailmap, see
               git-shortlog(1) or git- blame(1))

           · %ge: reflog identity email

           · %gE: reflog identity email (respecting .mailmap, see
               git-shortlog(1) or git- blame(1))

           · %gs: reflog subject

           · %Cred: switch color to red

           · %Cgreen: switch color to green

           · %Cblue: switch color to blue

           · %Creset: reset color

           · %C(...): color specification, as described under
               Values in the \"CONFIGURATION FILE\" section of
               git-config(1). By default, colors are shown only
               when enabled for log output (by color.diff,
               color.ui, or --color, and respecting the auto
               settings of the former if we are going to a
               terminal). %C(auto,...) is accepted as a
               historical synonym for the default (e.g.,
               %C(auto,red)). Specifying %C(always,...) will show
               the colors even when color is not otherwise
               enabled (though consider just using
               `--color=always to enable color for the whole
               output, including this format and anything else
               git might color). auto alone (i.e. %C(auto)) will
               turn on auto coloring on the next placeholders
               until the color is switched again.

           · %m: left (<), right (>) or boundary (-) mark

           · %n: newline

           · %%: a raw %

           · %x00: print a byte from a hex code

           · %w([<w>[,<i1>[,<i2>]]]): switch line wrapping, like the -w option of git-
               shortlog(1).

           · %<(<N>[,trunc|ltrunc|mtrunc]): make the next
               placeholder take at least N columns, padding
               spaces on the right if necessary. Optionally
               truncate at the beginning (ltrunc), the
               middle (mtrunc) or the end (trunc) if the output
               is longer than N columns. Note that truncating
               only works correctly with N >= 2.

           · %<|(<N>): make the next placeholder take at least
               until Nth columns, padding spaces on the right if
               necessary

           · %>(<N>), %>|(<N>): similar to %<(<N>), %<|(<N>)
               respectively, but padding spaces on the left

           · %>>(<N>), %>>|(<N>): similar to %>(<N>), %>|(<N>)
               respectively, except that if the next placeholder
               takes more spaces than given and there are spaces
               on its left, use those spaces


           · %><(<N>), %><|(<N>): similar to %<(<N>), %<|(<N>)
               respectively, but padding both sides (i.e. the
               text is centered)

           · %(trailers[:options]): display the trailers of the
               body as interpreted by git-interpret-trailers(1).
               The trailers string may be followed by a colon and
               zero or more comma-separated options. If the only
               option is given, omit non-trailer lines from the
               trailer block. If the unfold option is given,
               behave as if interpret-trailer’s --unfold option
               was given. E.g., %(trailers:only,unfold) to do
               both.

           Note Some placeholders may depend on other options
           given to the revision traversal engine. For example,
           the %g* reflog options will insert an empty string
           unless we are traversing reflog entries (e.g., by git
           log -g). The %d and %D placeholders will use
           the \"short\" decoration format if --decorate was not
           already provided on the command line.

       If you add a + (plus sign) after % of a placeholder, a
       line-feed is inserted immediately before the expansion if
       and only if the placeholder expands to a non-empty string.

       If you add a - (minus sign) after % of a placeholder, all
       consecutive line-feeds immediately preceding the expansion
       are deleted if and only if the placeholder expands to an
       empty string.

       If you add a ` ` (space) after % of a placeholder, a space
       is inserted immediately before the expansion if and only
       if the placeholder expands to a non-empty string.")

(defvar dgi--commit-ovs nil
  "Overlays which show the commit messages.")

(defun dgi--command-to-string (program &rest args)
  "Execute PROGRAM with arguments ARGS and return output string."
  (with-output-to-string
    (with-current-buffer standard-output
      (apply #'process-file program nil t nil args))))

(defun dgi--get-commit-info (&optional file gitf)
  "Get commit message info.

FILE default to current dired file. GITF determines the commit
info format and defaults to `dgi-commit-message-format'."
  (if (not (locate-dominating-file "." ".git"))
      (user-error "Not inside a git repo")
    (let* ((file (or file (dired-get-file-for-visit)))
           (lfile (and file
                       ;; get the actual displayed name, to make it work with
                       ;; dired collapse for example
                       (save-excursion
                         (dired-goto-file file)
                         (buffer-substring (point) (line-end-position))))))
      (when (and lfile
                 (not (member lfile '(".." "."))))
        (let ((msg (dgi--command-to-string
                    "git" "log" "-1"
                    (concat "--pretty="
                            (or gitf dgi-commit-message-format))
                    lfile)))
          (when (and msg (not (string= "" msg)))
            (substring msg
                       ;; skip newline
                       0 -1)))))))

(defmacro dgi--save-marked (&rest body)
  "Execute BODY and restore marks afterwards."
  `(let ((marked (save-excursion
                   (goto-char (point-min))
                   (dired-get-marked-files)))
         (inhibit-message t))
     (save-excursion
       (unwind-protect
           (progn ,@body)
         (dired-unmark-all-marks)
           (dolist (file marked)
             (dired-goto-file file)
             (dired-mark 1))))))

(defun dgi--cleanup ()
  (dolist (ov dgi--commit-ovs)
    (delete-overlay ov))
  (setq dgi--commit-ovs nil))


(defun dgi--get-dired-files-length (files)
  (let ((dnames ()))
    (dolist (file files (nreverse dnames))
      (push (dgi--get-dired-file-length file)
            dnames))))

(defun dgi--get-dired-file-length (file)
  (save-excursion
    (dired-goto-file file)
    (length (buffer-substring (point)
                              (line-end-position)))))


(defun dgi--get-commit-messages (files)
  (let ((messages ()))
    (setq messages
          (dolist (file files (nreverse messages))
            (push (dgi--get-commit-info file)
                  messages)))
    (with-current-buffer (get-buffer-create " *dired-git-info*")
      (let ((inhibit-read-only nil))
        (erase-buffer)
        (dolist (message messages)
          (insert (or message "") "\n"))
        (align-regexp (point-min)
                      (point-max)
                      "\\(\\s-*\\)\t" nil nil t)
        (goto-char (point-min))
        (while (search-forward "\t" nil t)
          (replace-match " "))
        (split-string (buffer-string) "\n")))))


;;;###autoload
(defun dgi-toggle-git-info ()
  "Toggle git message info in current dired buffer."
  (interactive)
  (if dgi--commit-ovs
      (dgi--cleanup)
    (let* ((files (dgi--save-marked
                   (dired-unmark-all-marks)
                   (dired-toggle-marks)
                   (dired-get-marked-files)))
           (minspc  (1+ (apply #'max  (dgi--get-dired-files-length files))))
           (messages (dgi--get-commit-messages files)))
      (save-excursion
        (dolist (file files)
          (let ((msg (pop messages)))
            (when msg
              (dired-goto-file file)
              (let ((spc (make-string
                          (- minspc (dgi--get-dired-file-length file))
                          ?\s)))
                (goto-char (line-end-position))
                (let ((ov (make-overlay (point) (1+ (point))))
                      (ovs (concat spc
                                   (propertize
                                    msg 'face 'dgi-commit-message-face)
                                   "\n")))
                  (push ov dgi--commit-ovs)
                  ;; I don't use after-string because I didn't get it to work
                  ;; in combination with hl-line-mode overlay
                  (overlay-put ov 'display ovs)
                  ;; hl line mode should have priority
                  (overlay-put ov 'priority -60))))))))))


(provide 'dired-git-info)
;;; dired-git-info.el ends here
