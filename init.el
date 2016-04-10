;;; package --- Summary:
;;; Commentary:
;; This is my configuration of Emacs

;;; Code:
(setq user-full-name "Joe Crawford")
(setq user-mail-address "jc423@buffalo.edu")

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; autosave
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

 (setq auto-save-file-name-transforms
       `((".*" ,temporary-file-directory t)))

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;;configure fly-check
(require 'flycheck)
(global-flycheck-mode 1)

;;configure autopair
(require 'autopair)
(autopair-global-mode)

;;config switch-window
(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)

;;configure magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;;configure elpy
(require 'elpy)
(elpy-enable)

;;configure company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;;(add-to-list 'company-backends 'company-tern)
;;key binding for company code
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

;;configure js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;configure itern
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

;;configure for web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("/some/react/path/.*\\.jsx?\\'" . web-mode))
(setq web-mode-content-types-alist
      '(("json" . "\\.api\\'")
    ("xml"  . "\\.api\\'")
    ("jsx"  . "\\.jsx\\'")))
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-pairing t)

;;configure org-mode
(require 'org)
(require 'org-trello)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/org/work.org"
			     "~/org/tutorial.org"
			     "~/org/work.org"))

;;configure helm
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)
(helm-mode 1)

(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/org/work.org" "~/org/tutorial.org" "~/org/home.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'ein)

