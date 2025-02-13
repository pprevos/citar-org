;; Citar-Denote minimum configuration

;; Configure package manager and use-package
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Mini Buffer Completion
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Search for partial matches in any order
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))

;; Citar bibliography management
(use-package citar
  :ensure t
  :custom
  ;; set bibliography's location
  (citar-bibliography '("~/documents/library/horizonofreason.bib"))
  ;; Allow multiple notes per bibliographic entry
  (citar-open-always-create-notes nil))

;; Org cite minimal setup
(setq org-cite-global-bibliography citar-bibliography
      org-cite-insert-processor 'citar
      org-cite-follow-processor 'citar
      org-cite-activate-processor 'citar)

(use-package citar-org-mode
  :load-path "~/documents/projects/citar-org-mode/"
  :demand t ;; Ensure minor mode loads
  :after (:any citar org)
  :custom
  ;; Location of literature notes
  (citar-org-mode-directory "~/documents/projects/citar-org-mode/notes")
  :config
  (citar-org-mode)
  ;; Bind all available commands
  :bind (("C-c w c" . citar-create-note)
	 ("C-c w n" . citar-open-notes)
	 :map org-mode-map
	 ("C-c w d" . citar-org-mode-dwim)
	 ("C-c w e" . citar-org-mode-open-reference-entry)
	 ("C-c w a" . citar-org-mode-add-citekey)
	 ("C-c w k" . citar-org-mode-remove-citekey)
	 ("C-c w l" . citar-org-mode-link-reference)
	 ("C-c w x" . citar-org-mode-noref)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(orderless vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight regular :height 157 :width normal)))))
