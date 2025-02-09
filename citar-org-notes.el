;;; citar-org-notes.el --- Minor mode integrating Citar and Org mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; Maintainer: Peter Prevos <peter@prevos.net>
;; Homepage: https://github.com/pprevos/citar-org-notes
;; Version: 0.1
;; Package-Requires: ((emacs "29.4") (citar "1.4") (org "9.6") (dash "2.19.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A minor-mode integrating 'citar' and 'org' to create and manage
;; bibliographic notes.
;;
;; Citar lets you browse and act on bibliographic data in BibTeX, BibLaTeX or
;; JSON format (CSL).  Combining Citar and Org enables linking Org files to
;; your bibliography, providing a complete solution for literature reviews.
;;
;; Read the manual with `C-h R citar-org-notes`.

;;; Code:

(require 'org)
(require 'dash)
(require 'citar)
(eval-when-compile
  (require 'subr-x))

;; Customisable variables

(defgroup citar-org-notes ()
  "Creating and accessing bibliography files with Citar and Org mode."
  :group 'files
  :link  '(url-link :tag "Homepage" "https://github.com/pprevos/citar-org-notes/"))

(defcustom citar-org-notes-directory "~/citar-org-notes"
  "Directory where Org mode files used by citar are stored."
  :type 'directory
  :group 'citar-org-notes)

(defcustom citar-org-notes-title-format "title"
  "Title format for new bibliographic notes.

- `title': Extract title (or short title) from bibliographic entry
- `author-year': Author-year citation style, e.g. Stallman (1981)
- `author-year-title': Combine author, year and title
- `full': Full citation
- nil: User-entry

For `author-year' and `author-year-title' you can configure:
- `citar-org-notes-title-format-authors'
- `citar-org-notes-title-format-andstr'."
  :group 'citar-org-notes
  :type  '(choice
           (const :tag "Title" "title")
           (const :tag "Author (Year)" "author-year")
           (const :tag "Author (Year). Title" "author-year-title")
           (const :tag "Full citation" "full")
           (const :tag "Citekey" nil)))

(defcustom citar-org-notes-title-format-authors 1
  "Maximum number of authors befor et al. in `citar-org-notes--format-author-editor'."
  :group 'citar-org-notes
  :type  'integer)

(defcustom citar-org-notes-title-format-andstr "and"
  "Connecting word for last two authors `citar-org-notes--format-author-editor'."
  :group 'citar-org-notes
  :type  'string)

(defcustom citar-org-notes-file-name nil
  "Separator to use for file names.

Options:
- As typed (no modification)
- kebab-case: dash-separated, lowercase
- Snake-case: Underscore_separated (casing as typed)

The file name is based on the title, as defined by `citar-org-notes-title-format'."
  :group 'citar-org-notes
  :type '(choice
	  (const :tag "As-is" nil)
	  (const :tag "kebab-case (lowercase)" 'kebab)
	  (const :tag "Snake_case" 'snake)))

(defcustom citar-org-notes-use-bib-keywords nil
  "Extract keywords from bibliography and use as file tags.
Extracts keywords from comma-separated list in the `keywords' BibTeX field."
  :group 'citar-org-notes
  :type  'boolean)

(defcustom citar-org-notes-open-attachment t
  "Open the first defined attachment of the bibliographic entry when creating a new note."
  :group 'citar-org-notes
  :type 'boolean)

;; Operational variables

(defvar citar-org-notes-reference-format "#+reference: %s\n"
  "Template for the reference line.")

(defvar citar-org-notes-reference-regex "^#\\+reference:"
  "Regular expression to extract references from Org mode files.")

;; Auxiliary functions

(defun citar-org-notes--directory-files ()
  "List all Org mode files in `citar-org-notes-directory'."
  (if (file-directory-p citar-org-notes-directory)
      (cl-remove-if
       (lambda (file)
	 (or (string-match-p "/\\.#" file)   ;; Lock files
	     (string-match-p "~\\'" file)    ;; Backup files
	     (string-match-p "/#.*#\\'" file))) ;; Auto-save files
       (directory-files citar-org-notes-directory t "\\.org\\'"))
    (user-error "No notes directory defined. Customise `citar-org-notes-directory'")))

(defun citar-org-notes--retrieve-references (file)
  "Return reference citekey(s) from FILE front matter."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((trims "[ \t\n\r]+"))
      (when (re-search-forward citar-org-notes-reference-regex nil t 1)
        (split-string
         (string-trim
          (buffer-substring-no-properties (point) (line-end-position))
          trims trims)
	 ";")))))

(defun citar-org-notes--get-notes (&optional citekeys)
  "Generate hash table of Org files associated with CITEKEYS.

If CITEKEYS is omitted, return all Org files with a reference line."
  (let ((notes (make-hash-table :test 'equal))
	(files (citar-org-notes--directory-files)))
    (prog1 notes
      ;; Find org files
      (dolist (file files)
        (let ((keys-in-file (citar-org-notes--retrieve-references file)))
          (dolist (key keys-in-file)
            (if citekeys
                (dolist (k citekeys)
                  (when (string= k key)
                    (push file (gethash key notes))))
              (push file (gethash key notes))))))
      (maphash
       (lambda (key filelist)
         (puthash key (nreverse filelist) notes))
       notes))))

(defun citar-org-notes--has-notes ()
  "Return a list of all citekeys referenced in an Org file.

See documentation for `citar-has-notes'."
  (let ((notes (citar-org-notes--get-notes)))
    (unless (hash-table-empty-p notes)
      (lambda (citekey) (and (gethash citekey notes) t)))))

(defun citar-org-notes--has-citekeys (citekeys)
  "Return hash table of citar entries with associated CITEKEYS."
  (let ((citar-entries (citar-get-entries))
        (citekey-entries (make-hash-table :test 'equal)))
    (mapc (lambda (key)
            (puthash key (gethash key citar-entries) citekey-entries))
          citekeys)
    (unless (hash-table-empty-p citekey-entries)
      (lambda (citekey) (and (gethash citekey citekey-entries) t)))))

(defun citar-org-notes--format-author-editor (citekey)
  "Extract author or editor from CITEKEY.
Format author(s) or editor(s) in accordance with:
- `citar-org-notes-title-format-authors': Number of authors before et al.
- `citar-org-notes-title-format-andstr': Connecting string between last two authors."
  (let ((author-names (or (citar-get-value "author" citekey)
                          (citar-get-value "editor" citekey))))
    (citar--shorten-names
     author-names
     citar-org-notes-title-format-authors
     citar-org-notes-title-format-andstr)))

(defun citar-org-notes--generate-title (citekey)
  "Generate title for new bibliographic note using CITEKEY.

Either title, author/year, full reference or citation key.
Based on the `citar-org-notes-title-format' variable."
  (let ((author-editor (citar-org-notes--format-author-editor citekey))
        (title (citar-get-value "title" citekey))
        (year (or (citar-get-value "year" citekey)
                  (citar-get-value "date" citekey)
                  (citar-get-value "issued" citekey))))
    (cond ((equal citar-org-notes-title-format "title")
           title)
          ((equal citar-org-notes-title-format "author-year")
           (concat author-editor " (" year ")"))
          ((equal citar-org-notes-title-format "author-year-title")
           (let* ((citar-org-notes-title-format "author-year")
                  (author-year (citar-org-notes--generate-title citekey))
                  (title (citar-get-value "title" citekey)))
             (concat author-year " " title)))
          ((equal citar-org-notes-title-format "full")
           (let ((ref (citar-format-reference (list citekey))))
             (substring ref 0 (- (length ref) 2))))
          (t citekey))))

(defun citar-org-notes--generate-file-name (title)
  "Generate a unique file name from TITLE using `citar-org-notes-file-name'.
If a file already exists in `citar-org-notes-directory', a numeric suffix is added."
  (let* ((separator (cond ((eq citar-org-notes-file-name 'kebab) "-")
			  ((eq citar-org-notes-file-name 'snake) "_")
			  (t " ")))
	 (base-name (cond ((eq citar-org-notes-file-name 'kebab)
                           (let* ((lower (downcase title))
                                  (no-punct (replace-regexp-in-string "[[:punct:]]" "" lower))
                                  (trimmed (string-trim no-punct))
                                  (kebab (replace-regexp-in-string "[[:space:]]+" separator trimmed)))
                             kebab))
                          ((eq citar-org-notes-file-name 'snake)
                           (let* ((no-punct (replace-regexp-in-string "[[:punct:]]" "" title))
                                  (trimmed (string-trim no-punct))
                                  (snake (replace-regexp-in-string "[[:space:]]+" separator trimmed)))
                             snake))
                          (t title)))
         (file-name (expand-file-name (concat base-name ".org") citar-org-notes-directory))
         (counter 1))
    (while (file-exists-p file-name)
      (setq file-name (expand-file-name (concat base-name separator (number-to-string counter) ".org")
                                        citar-org-notes-directory))
      (setq counter (1+ counter)))
    file-name))

(defun citar-org-notes--add-new-reference-line (citekeys)
  "Add a new reference line with CITEKEYS to buffer of FILETYPE."
  (save-excursion
    (goto-char (point-min))
    (while (not (looking-at "^$")) ;; Search for the first empty line
      (forward-line 1))
    (insert (format citar-org-notes-reference-format
		    (mapconcat #'identity citekeys ";")))))

(defun citar-org-notes--add-reference (citekeys)
  "Add CITEKEYS to the front matter of the current buffer.
`citar-org-notes-add-reference' is the interactive version of this function."
  (let* ((file (buffer-file-name))
         (new-references (mapconcat #'identity citekeys ";"))
         (existing-references (citar-org-notes--retrieve-references file)))
    (if existing-references
        ;; Add to existing
        (save-excursion
          (goto-char (point-min))
          (re-search-forward citar-org-notes-reference-regex)
          (end-of-line)
          (insert (concat ";" new-references))
          (save-buffer))
      ;; New reference line
      (citar-org-notes--add-new-reference-line citekeys))))

(defun citar-org-notes--create-note (citekey &optional _entry)
  "Create a bibliographic note for CITEKEY with properties ENTRY.

The title format is set by `citar-org-notes-title-format'."
  ;; Create empty literature note org buffer
  (let* ((title (citar-org-notes--generate-title citekey))
	 (file-name (citar-org-notes--generate-file-name title)))
    (switch-to-buffer (get-buffer-create file-name))
    (org-mode)
    (insert (format "#+title:     %s\n#+date:      %s\n#+author:    %s\n"
                      title
                      (format-time-string "[%Y-%m-%d %a %H:%M]")
                      (user-full-name)))
    (set-visited-file-name file-name))
  ;; insert reference line
  (citar-org-notes--add-new-reference-line (list citekey))
  (goto-char (point-max))
  ;; Open available atachment in other window
  (when (and citar-org-notes-open-attachment (citar-get-value "file" citekey) (one-window-p))
    (split-window-right)
    (other-window 1)
    (citar-open-files citekey)
    (other-window -1)))

;; Interactive functions

;;;###autoload
(defun citar-org-notes-open-note ()
  "Open a bibliographic note using Citar.

Provides a selection list of all bibliographic entries with notes."
  (interactive)
  (let* ((citekeys (citar-select-refs :filter (citar-org-notes--has-notes)))
         (file (citar--select-resource
                citekeys :notes t :create-notes t)))
    (find-file (cdr file))))

;; Citar integration

(defconst citar-org-notes-config
  (list :name "Org mode file"
        :category 'citar-org-notes-mode
        :items #'citar-org-notes--get-notes
        :hasitems #'citar-org-notes--has-notes
        :open #'find-file
        :create #'citar-org-notes--create-note)
  "Instructing citar to use citar-org-notes functions.")

(defvar citar-org-notes--orig-source
  citar-notes-source
  "Store the `citar-notes-source' value prior to enabling `citar-org-notes-mode'.")

;; Initialise minor mode

(defun citar-org-notes--setup ()
  "Setup `citar-org-notes-mode'."
  (citar-register-notes-source
   'citar-org-notes citar-org-notes-config)
  (setq citar-notes-source 'citar-org-notes))

(defun citar-org-notes--reset ()
  "Reset citar to default values."
  (setq citar-notes-source citar-org-notes--orig-source)
  (citar-remove-notes-source 'citar-org-notes))

;;;###autoload
(define-minor-mode citar-org-notes-mode
  "Toggle integration between Citar and Denote."
  :global t
  :group 'citar
  :lighter nil
  (if citar-org-notes-mode
      (citar-org-notes--setup)
    (citar-org-notes--reset)))

(provide 'citar-org-notes)
;;; citar-org-notes.el ends here
