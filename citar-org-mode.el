;;; citar-org-mode.el --- Minor mode integrating Citar and Org mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; Maintainer: Peter Prevos <peter@prevos.net>
;; Homepage: https://github.com/pprevos/citar-org-mode
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
;;
;;; Commentary:
;;
;; A minor-mode integrating 'citar' and 'org' to create and manage
;; bibliographic notes.
;;
;; Citar lets you browse and act on bibliographic data in BibTeX, BibLaTeX or
;; JSON format (CSL).  Combining Citar and Org enables linking Org files to
;; your bibliography, providing a complete solution for literature reviews.
;;

;;; Code:

(require 'org)
(require 'dash)
(require 'citar)
(eval-when-compile
  (require 'subr-x))

;; Customisable variables

(defgroup citar-org-mode ()
  "Creating and accessing bibliography files with Citar and Org mode."
  :group 'files
  :link  '(url-link :tag "Homepage" "https://github.com/pprevos/citar-org-mode/"))

(defcustom citar-org-mode-directory "~/citar-org-mode"
  "Directory where Org mode files used by citar are stored."
  :type 'directory
  :group 'citar-org-mode)

(defcustom citar-org-mode-title-format "title"
  "Title format for new bibliographic notes.

- `title': Extract title (or short title) from bibliographic entry
- `author-year': Author-year citation style, e.g. Stallman (1981)
- `author-year-title': Combine author, year and title
- `full': Full citation
- nil: Citation key

For `author-year' and `author-year-title' you can configure:
- `citar-org-mode-title-format-authors'
- `citar-org-mode-title-format-andstr'."
  :group 'citar-org-mode
  :type  '(choice
           (const :tag "Title" "title")
           (const :tag "Author (Year)" "author-year")
           (const :tag "Author (Year). Title" "author-year-title")
           (const :tag "Full citation" "full")
           (const :tag "Citekey" nil)))

(defcustom citar-org-mode-title-format-authors 1
  "Maximum number of authors befor et al. in `citar-org-mode--format-author-editor'."
  :group 'citar-org-mode
  :type  'integer)

(defcustom citar-org-mode-title-format-andstr "and"
  "Connecting word for last two authors `citar-org-mode--format-author-editor'."
  :group 'citar-org-mode
  :type  'string)

(defcustom citar-org-mode-file-name-case nil
  "casing to use for file names.

Options:
- As-is (no modification)
- kebab-case: dash-separated, lowercase
- Snake-case: Underscore_separated (casing as typed)

The file name is based on the title, as defined by `citar-org-mode-title-format'."
  :group 'citar-org-mode
  :type '(choice
	  (const :tag "As-is" nil)
	  (const :tag "kebab-case (lowercase)" kebab)
	  (const :tag "Snake_case" snake)))

(defcustom citar-org-mode-open-attachment t
  "Open the first defined attachment of the bibliographic entry when creating a new note."
  :group 'citar-org-mode
  :type 'boolean)

;; Operational variables

(defvar citar-org-mode-reference-format "#+reference: %s\n"
  "Template for the reference line.")

(defvar citar-org-mode-reference-regex "^#\\+reference:"
  "Regular expression to extract references from Org mode files.")

;; Auxiliary functions

(defun citar-org-mode--directory-files ()
  "List all Org mode files in `citar-org-mode-directory'."
  (mapcar
   #'expand-file-name
   (seq-filter
    (lambda (file)
      (and (file-regular-p file)
           (not (backup-file-name-p file))))
    (directory-files citar-org-mode-directory t "\\.org\\'"))))

(defun citar-org-mode-file-p ()
  "Current buffer file is an Org mode file in `citar-org-mode-directory'."
    (and (eq major-mode 'org-mode)
    (string-equal (file-name-as-directory (file-name-directory (buffer-file-name)))
              (file-name-as-directory (expand-file-name citar-org-mode-directory)))))

(defun citar-org-mode--retrieve-references (file)
  "Return reference citekey(s) from FILE front matter."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((trims "[ \t\n\r]+"))
      (when (re-search-forward citar-org-mode-reference-regex nil t 1)
        (split-string
         (string-trim
          (buffer-substring-no-properties (point) (line-end-position))
          trims trims)
	 ";")))))

(defun citar-org-mode--get-notes (&optional citekeys)
  "Generate hash table of Org files associated with CITEKEYS.

If CITEKEYS is omitted, return all Org files with a reference line."
  (let ((notes (make-hash-table :test 'equal))
	(files (citar-org-mode--directory-files)))
    (prog1 notes
      ;; Find org files
      (dolist (file files)
        (let ((keys-in-file (citar-org-mode--retrieve-references file)))
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

(defun citar-org-mode--has-notes ()
  "Return a list of all citekeys referenced in an Org file.

See documentation for `citar-has-notes'."
  (let ((notes (citar-org-mode--get-notes)))
    (unless (hash-table-empty-p notes)
      (lambda (citekey) (and (gethash citekey notes) t)))))

(defun citar-org-mode--has-citekeys (citekeys)
  "Return hash table of citar entries with associated CITEKEYS."
  (let ((citar-entries (citar-get-entries))
        (citekey-entries (make-hash-table :test 'equal)))
    (mapc (lambda (key)
            (puthash key (gethash key citar-entries) citekey-entries))
          citekeys)
    (unless (hash-table-empty-p citekey-entries)
      (lambda (citekey) (and (gethash citekey citekey-entries) t)))))

(defun citar-org-mode--format-author-editor (citekey)
  "Extract author or editor from CITEKEY.
Format author(s) or editor(s) in accordance with:
- `citar-org-mode-title-format-authors': Number of authors before et al.
- `citar-org-mode-title-format-andstr': Connecting string between last two authors."
  (let ((author-names (or (citar-get-value "author" citekey)
                          (citar-get-value "editor" citekey))))
    (citar--shorten-names
     author-names
     citar-org-mode-title-format-authors
     citar-org-mode-title-format-andstr)))

(defun citar-org-mode--generate-title (citekey)
  "Generate title for new bibliographic note using CITEKEY.

Either title, author/year, full reference or citation key.
Based on the `citar-org-mode-title-format' variable."
  (let ((author-editor (citar-org-mode--format-author-editor citekey))
        (title (citar-get-value "title" citekey))
        (year (or (citar-get-value "year" citekey)
                  (citar-get-value "date" citekey)
                  (citar-get-value "issued" citekey))))
    (cond ((equal citar-org-mode-title-format "title")
           title)
          ((equal citar-org-mode-title-format "author-year")
           (concat author-editor " (" year ")"))
          ((equal citar-org-mode-title-format "author-year-title")
           (let* ((citar-org-mode-title-format "author-year")
                  (author-year (citar-org-mode--generate-title citekey))
                  (title (citar-get-value "title" citekey)))
             (concat author-year " " title)))
          ((equal citar-org-mode-title-format "full")
           (let ((ref (citar-format-reference (list citekey))))
             (substring ref 0 (- (length ref) 2))))
          (t citekey))))

(defun citar-org-mode--generate-file-name (title)
  "Generate a unique file name from TITLE using `citar-org-mode-file-name-case'.
If a file already exists in `citar-org-mode-directory', a numeric suffix is added."
  (let* ((separator (cond ((eq citar-org-mode-file-name-case 'kebab) "-")
			  ((eq citar-org-mode-file-name-case 'snake) "_")
			  (t " ")))
	 (base-name (cond ((eq citar-org-mode-file-name-case 'kebab)
                           (let* ((lower (downcase title))
                                  (no-punct (replace-regexp-in-string "[[:punct:]]" "" lower))
                                  (trimmed (string-trim no-punct))
                                  (kebab (replace-regexp-in-string "[[:space:]]+" separator trimmed)))
                             kebab))
                          ((eq citar-org-mode-file-name-case 'snake)
                           (let* ((no-punct (replace-regexp-in-string "[[:punct:]]" "" title))
                                  (trimmed (string-trim no-punct))
                                  (snake (replace-regexp-in-string "[[:space:]]+" separator trimmed)))
                             snake))
                          (t title)))
         (file-name (expand-file-name (concat base-name ".org") citar-org-mode-directory))
         (counter 1))
    (while (file-exists-p file-name)
      (setq file-name (expand-file-name (concat base-name separator (number-to-string counter) ".org")
                                        citar-org-mode-directory))
      (setq counter (1+ counter)))
    file-name))

(defun citar-org-mode--add-new-reference-line (citekeys)
  "Add a new reference line with CITEKEYS to buffer of FILETYPE."
  (save-excursion
    (goto-char (point-min))
    (while (not (looking-at "^$")) ;; Search for the first empty line
      (forward-line 1))
    (insert (format citar-org-mode-reference-format
		    (mapconcat #'identity citekeys ";")))))

(defun citar-org-mode--create-note (citekey &optional _entry)
  "Create a bibliographic note for CITEKEY with properties ENTRY.

The title format is set by `citar-org-mode-title-format'."
  ;; Create empty literature note org buffer
  (let* ((default-title (citar-org-mode--generate-title citekey))
	 (title (read-string "Edit title: " default-title nil default-title))
	 (file-name (citar-org-mode--generate-file-name title)))
    (switch-to-buffer (get-buffer-create file-name))
    (org-mode)
    (insert (format "#+title:     %s\n#+date:      %s\n#+author:    %s\n"
                      title
                      (format-time-string "[%Y-%m-%d %a %H:%M]")
                      (user-full-name)))
    (set-visited-file-name file-name))
  ;; insert reference line
  (citar-org-mode--add-new-reference-line (list citekey))
  (goto-char (point-max))
  ;; Open available atachment in other window
  (when (and citar-org-mode-open-attachment
	     (citar-get-value "file" citekey)
	     (one-window-p))
    (split-window-right)
    (other-window 1)
    (citar-open-files citekey)
    (other-window -1)))

(defun citar-org-mode--get-non-referenced (file)
  "Select Citar entry not already referenced in FILE."
  (let* ((bibliography (hash-table-keys (citar-get-entries)))
         (references (citar-org-mode--retrieve-references file))
         (unused (-difference bibliography references)))
    (when unused (citar-select-refs
                  :multiple t
                  :filter (citar-org-mode--has-citekeys unused)))))

(defun citar-org-mode--add-reference (citekeys)
  "Add CITEKEYS to the front matter of the current buffer.
`citar-org-mode-add-reference' is the interactive version of this function."
  (let* ((file (buffer-file-name))
         (new-references (mapconcat #'identity citekeys ";"))
         (existing-references (citar-org-mode--retrieve-references file)))
    (if existing-references
        ;; Add to existing
        (save-excursion
          (goto-char (point-min))
          (re-search-forward citar-org-mode-reference-regex)
          (end-of-line)
          (insert (concat ";" new-references))
          (save-buffer))
      ;; New reference line
      (citar-org-mode--add-new-reference-line citekeys))))

(defun citar-org-mode--get-nocite ()
  "Select item(s) from Citar entries not cited or referenced in Org mode files."
  (let* ((bibliography (hash-table-keys (citar-get-entries)))
         (references (hash-table-keys (citar-org-mode--get-notes)))
	 (nocite (cl-set-difference bibliography references :test #'string=)))
    (when nocite (citar-select-refs
                  :multiple t
                  :filter (citar-org-mode--has-citekeys nocite)))))

;;; Interactive functions

;;;###autoload
(defun citar-org-mode-open-note ()
  "Open a bibliographic note using Citar.

Provides a selection list of all bibliographic entries with notes."
  (interactive)
  (let* ((citekeys (citar-select-refs :filter (citar-org-mode--has-notes)))
         (file (citar--select-resource
                citekeys :notes t :create-notes t)))
    (find-file (cdr file))))

;;;###autoload
(defun citar-org-mode-dwim ()
  "Access attachments, notes and links of a bibliographic reference.

When more than one bibliographic item is referenced, select item first."
  (interactive)
  ;; Any citation keys in the note?
  (if-let* ((references (citar-org-mode--retrieve-references (buffer-file-name)))
            (key (if (= (length references) 1)
                     (car references)
                   (citar-select-ref :filter (citar-org-mode--has-citekeys references)))))
      (citar-open (list key))
    (if (citar-org-mode-file-p)
	(when (yes-or-no-p "Current buffer does not reference a citation key.
Add a reference? ")
	  (citar-org-add-citekey)
	  (citar-org-dwim))
      (user-error "Buffer is not an Org mode file or not stored in %s" citar-org-mode-directory))))

;;;###autoload
(defun citar-org-mode-open-reference-entry ()
  "Open Bib(La)TeX or JSON file associated with a bibliographic reference.

When more than one bibliographic item is referenced, select item first."
  (interactive)
  (if-let* ((keys (citar-org-mode--retrieve-references (buffer-file-name)))
            (key (if (= (length keys) 1)
                     (car keys)
                   (citar-select-ref
                    :filter (citar-org-mode--has-citekeys keys)))))
      (citar-open-entry key)
    (if (citar-org-mode-file-p)
        (when (yes-or-no-p "Current buffer does not reference a citation key.
Add a reference? ")
          (citar-org-add-citekey)
          (citar-org-open-reference-entry))
      (message "Buffer is not an Org mode file or not stored in %s" citar-org-mode-directory))))

;;;###autoload
(defun citar-org-mode-add-reference (&optional nocite)
  "Add citation key(s) to existing note.
With universal argument choose from entries not yet used in any notes (NOCITE)."
  (interactive "P")
  (if-let* ((citar-org-mode-file (citar-org-mode-file-p))
	    (file (buffer-file-name))
            (citekeys (if nocite
                          (citar-org-mode--get-nocite)
                        (citar-org-mode--get-non-referenced file))))
      (citar-org-mode--add-reference citekeys)
    (message "Buffer is not an Org mode file or not stored in %s" citar-org-mode-directory)))

;;;###autoload
(defun citar-org-mode-remove-reference ()
  "Remove a reference from a bibliographic note."
  (interactive)
  (if-let* ((file (buffer-file-name))
            (citekeys (citar-org-mode--retrieve-references file))
            (selected (if (< (length citekeys) 2)
                          (car citekeys)
                        (citar-select-ref
                         :filter
                         (citar-org-mode--has-citekeys citekeys)))))
      (let ((new-citekeys (delete selected citekeys)))
        (save-excursion
          ;; Remove references line
          (goto-char (point-min))
          (re-search-forward citar-org-mode-reference-regex)
          (move-beginning-of-line nil)
          (kill-line 1)
          ;; Add new line when applicable
          (when (> (length new-citekeys) 0)
            (citar-org-mode--add-new-reference-line new-citekeys))))
    (message "No references in this buffer, or not an Org file stored in %s"
	     citar-org-mode-directory)))

;;;###autoload
(defun citar-org-mode-noref ()
    "Create a new note from any literature that is not yet referneced in `citar-org-mode-directory'."
  (interactive)
  (if-let ((citekeys (citar-org-mode--get-nocite)))
      (citar-open citekeys)
    (message "All items in the bibliography have been referenced")))

;;;###autoload
(defun citar-org-mode-check ()
  "List all files in `citar-org-mode-directory' without reference."
  (interactive)
  (let ((files (citar-org-mode--directory-files))
        (no-reference-files '()))
    (dolist (file files)
      (let ((reference-p (with-temp-buffer
                            (insert-file-contents file)
                            (re-search-forward citar-org-mode-reference-regex nil t))))
        (unless reference-p
          (push file no-reference-files))))
    (if no-reference-files
        (let ((selected-file (completing-read "Open file without reference: " no-reference-files nil t)))
          (find-file selected-file))
      (message "All files contain references."))))

;;; Citar integration

(defconst citar-org-mode-config
  (list :name "Org mode file"
        :category 'citar-org-mode
        :items #'citar-org-mode--get-notes
        :hasitems #'citar-org-mode--has-notes
        :open #'find-file
        :create #'citar-org-mode--create-note)
  "Instructing citar to use citar-org-mode functions.")

(defvar citar-org-mode--orig-source
  citar-notes-source
  "Store the `citar-notes-source' value prior to enabling `citar-org-mode-mode'.")

;; Initialise minor mode

(defun citar-org-mode--setup ()
  "Setup `citar-org-mode'."
  (citar-register-notes-source
   'citar-org-mode citar-org-mode-config)
  (setq citar-notes-source 'citar-org-mode))

(defun citar-org-mode--reset ()
  "Reset citar to default values."
  (setq citar-notes-source citar-org-mode--orig-source)
  (citar-remove-notes-source 'citar-org-mode))

;;;###autoload
(define-minor-mode citar-org-mode
  "Toggle integration between Citar and Org mode."
  :global t
  :group 'citar
  :lighter nil
  (if citar-org-mode
      (citar-org-mode--setup)
    (citar-org-mode--reset)))

(provide 'citar-org-mode)
;;; citar-org-mode.el ends here
