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

;;configure projectile
(require 'projectile)
(projectile-global-mode)

;;configure fly-check
(require 'flycheck)
(global-flycheck-mode 1)
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
	  '(json-jsonlist)))

;;configure autopair
(require 'autopair)
(autopair-global-mode)

;;config switch-window
(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)

;;configure magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c g p") 'magit-pull)
;; make status buffer open in current window
(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer (if (and (derived-mode-p 'magit-mode)
                         (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode)))
                    nil
                  '(display-buffer-same-window)))))

;;configure elpy
(require 'elpy)
(elpy-enable)
(elpy-use-ipython)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

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
;(custom-set-variables  
(setq-default js2-basic-offset 2)  
(setq-default js2-bounce-indent-p t)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)

;;configure hs-minor-mode
(load-library "hideshow")
(global-set-key (kbd "C-+") 'hs-toggle-hiding)
(add-hook 'js2-mode-hook 'hs-minor-mode)

;;configure itern
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

;;configure for web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;;(setq web-mode-content-types-alist
;;      '(("json" . "\\.api\\'")
;;    ("xml"  . "\\.api\\'")
;;    ("jsx"  . "\\.jsx\\'")))

(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-pairing t)

;;configure org-mode
(require 'org)
(require 'org-trello)
(require 'org-habit)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cb" 'org-iswitchb)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Me/org/notes.org" "Tasks")
	 "* TODO %?\n %i\n %a")
	("j" "Journal" entry (file+datetree "~/Me/org/journal.org")
	 "* %?\n Entered on %U\n %i\n %a")))
    

;;(setq org-todo-keyword-faces
;;      (quote (("TODO" :foreground "red" :weight bold)
;;              ("DONE" :foreground "forest green" :weight bold)
;;              ("WAITING" :foreground "orange" :weight bold)
;;              ("CANCELLED" :foreground "forest green" :weight bold))))

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
 '(flycheck-eslintrc nil)
 '(initial-buffer-choice "~/Me/org/personal.org")
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(json-reformat:indent-width 2)
 '(org-agenda-files (quote ("~/Me/org")))
 '(org-capture-templates nil)
 '(org-default-notes-file "~/Me/org/notes.org")
 '(org-directory "~/Me/org")
 '(org-enforce-todo-dependencies t)
 '(org-log-done (quote time))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'jedi)
(add-to-list 'ac-sources 'ac-source-jedi-direct)
(add-hook 'python-mode-hook 'jedi:setup)

