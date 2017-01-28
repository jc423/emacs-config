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

;;backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
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
(global-set-key (kbd "C-c g o") 'magit-push)
(global-set-key (kbd "C-c g c") 'magit-checkout)
(global-set-key (kbd "C-c g b") 'magit-branch-and-checkout)
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

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;(custom-set-variables  
(setq-default js2-basic-offset 2)  
(setq-default js2-bounce-indent-p t)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)

;;configure hs-minor-mode
(load-library "hideshow")
(global-set-key (kbd "C-T") 'hs-toggle-hiding)
(global-set-key (kbd "C--") 'hs-hide-all)
(global-set-key (kbd "C-+") 'hs-show-all)
;(global-set-key (kbd "C-\S-h") 'hs-hide-block)
;(global-set-key (kbd "C-\S-s") 'hs-show-block)

(add-hook 'js2-mode-hook 'hs-minor-mode)

;;configure itern
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

;;configure for web-mode
(require 'web-mode)
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-pairing t)

;;configure org-mode
(require 'org)
(require 'org-trello)
(require 'org-habit)
(require 'ox-md)
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(global-set-key (kbd "C-c t")
                (lambda() (interactive) (find-file "~/Me/org/gtd.org")))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cb" 'org-iswitchb)
(setq org-todo-keywords
      '((sequence "TODO(t)" "BLOCKED(b)" "SOMEDAY(s)" "|" "READ(r)" "DONE(d)" "CANCELLED(c)")))
(setq org-capture-templates
      '(("W" "Wedding" entry (file+headline "~/Me/org/gtd.org" "Wedding")
         "*** TODO %?")
        ("f" "Finance" entry (file+headline "~/Me/org/gtd.org" "Finance")
         "*** TODO %?")
        ("s" "Study" entry (file+headline "~/Me/org/gtd.org" "Study")
         "*** TODO %?")
        ("v" "Violet" entry (file+headline "~/Me/org/gtd.org" "Violet")
         "*** TODO %?")
        ("w" "Work" entry (file+headline "~/Me/org/work.org" "Work")
         "*** TODO %?\n %i\n %a")))

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
 '(exec-path
   (quote
    ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Users/crawfoj0/Downloads/emacs-24.5/nextstep/Emacs.app/Contents/MacOS/libexec" "/Users/crawfoj0/Downloads/emacs-24.5/nextstep/Emacs.app/Contents/MacOS/bin" "/usr/local/bin" "/usr/local/bin" "/Library/TeX/texbin")))
 '(flycheck-eslintrc nil)
 '(haskell-tags-on-save t)
 '(indent-tabs-mode nil)
 '(initial-buffer-choice "~/Me/org/gtd.org")
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(json-reformat:indent-width 2)
 '(jsx-indent-level 2)
 '(org-agenda-files (quote ("~/Me/org/gtd.org" "~/Me/org/work.org")))
 '(org-capture-templates nil)
 '(org-default-notes-file "~/Me/org/notes.org")
 '(org-directory "~/Me/org")
 '(org-enforce-todo-dependencies t)
 '(org-log-done (quote time))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)")
     (sequence "TODO(t)" "IP(p)" "BLOCKED(b)" "|" "MERGING(m)" "DONE(d)" "CANCELLED(c)"))))
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(tab-width 2)
 '(typescript-indent-level 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'jedi)
(add-to-list 'ac-sources 'ac-source-jedi-direct)
(add-hook 'python-mode-hook 'jedi:setup)

(put 'erase-buffer 'disabled nil)


(global-set-key (kbd "C-c h -") (kbd "C-u - 1 6 C-x }"))
(global-set-key (kbd "C-c h +") (kbd "C-u 1 6 C-x }"))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
(windmove-default-keybindings)

;; haskell
(eval-after-load 'haskell-mode
          '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))


;; Install Intero
(package-install 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)

;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;; Move more quickly
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

;; Function to create new functions that look for a specific pattern
(defun ffip-create-pattern-file-finder (&rest patterns)
  (lexical-let ((patterns patterns))
    (lambda ()
      (interactive)
      (let ((ffip-patterns patterns))
        (find-file-in-project)))))

;; Find file in project, with specific patterns
(global-unset-key (kbd "C-x C-o"))
(global-set-key (kbd "C-x C-o ja")
                (ffip-create-pattern-file-finder "*.java"))
(global-set-key (kbd "C-x C-o js")
                (ffip-create-pattern-file-finder "*.js*"))
(global-set-key (kbd "C-x C-o ts")
                (ffip-create-pattern-file-finder "*.ts"))
(global-set-key (kbd "C-x C-o a")
                (ffip-create-pattern-file-finder "*"))
