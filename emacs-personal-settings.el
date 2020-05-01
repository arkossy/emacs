(setq inhibit-startup-screen t)

(use-package emacs
  :config
  (defun apollo/delete-emacs-init ()
    (interactive)
    (let ((configs "~/.emacs.d/emacs-personal-settings.el"))
      (when configs
        (delete-file configs))))
  :hook (kill-emacs . apollo/delete-emacs-init))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(global-set-key (kbd "TAB") 'self-insert-command)

(setq sentence-end-double-space nil)

(blink-cursor-mode 0)

(global-visual-line-mode t)

(show-paren-mode 1)

(setq ring-bell-function 'ignore)

(setq sentence-end-double-space nil)

(defvar backup-dir (expand-file-name "/Users/apollo/.emacs.d/emacs-backups/"))
(defvar autosave-dir (expand-file-name "/Users/apollo/.emacs.d/emacs-autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(setq default-directory "~/Documents/project-org")

(require 'ido)
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-enable-last-directory-history t)

(add-to-list 'ido-ignore-files "emacs-personal-settings.el")
(add-to-list 'ido-ignore-files ".pia_manager_crash.log")

;Avoid certain directories:
;(setq ido-ignore-directories '("Applications/" "Library/" "Movies/" "Music/" "Pictures/" "Public/"))

(set-face-attribute 'fringe nil :background nil)

(require 'theme-looper)
(global-set-key (kbd "C-}") 'theme-looper-enable-next-theme)
(theme-looper-set-favorite-themes '(modus-operandi modus-vivendi))

(desktop-save-mode 1)

;; Use CMD+arrows
(windmove-default-keybindings 'super)
;; Don't cycle around at edges (nil), enabled (t)
(setq windmove-wrap-around nil)

(require 'olivetti)
(global-set-key (kbd "C-{") 'olivetti-mode)

;; on error delete the content of: .mc-lists.el
(require 'multiple-cursors)
(global-set-key (kbd "M-m") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(ac-config-default)

(setq org-list-description-max-indent 5)

(setq org-adapt-indentation nil)

(global-set-key (kbd "<f5>") 'org-id-get-create)

(defun my/copy-id-to-clipboard() "Copy the ID property value to killring,
if no ID is there then create a new unique ID. 
This function works only in org-mode buffers.

The purpose of this function is to easily construct id:-links to 
org-mode items. If its assigned to a key it saves you marking the
text and copying to the killring."
       (interactive)
       (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
	 (setq mytmpid (funcall 'org-id-get-create))
	 (kill-new mytmpid)
	 (message "Copied %s to killring (clipboard)" mytmpid)
       ))

(global-set-key (kbd "<f6>") 'my/copy-id-to-clipboard)

(setq org-directory "~/Documents/project-org")

(add-to-list 'auto-mode-alist '("\\.\\(org\\)$" . org-mode))
(require 'org)

(global-set-key "\C-cl" 'org-store-link)

(global-set-key "\C-ca" 'org-agenda)

(setq org-agenda-files (list "~/Documents/project-org"))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "#3a70af" :weight bold)
              ("NEXT" :foreground "#cc0000" :weight bold)
              ("DONE" :foreground "#00994d" :weight bold)
              ("WAITING" :foreground "#ff8833" :weight bold)
              ("HOLD" :foreground "#ff8833" :weight bold)
              ("CANCELLED" :foreground "#177a21" :weight bold))))

(setq org-use-fast-todo-selection t)

(global-set-key (kbd "C-c c") 'org-capture)
