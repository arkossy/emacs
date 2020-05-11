(setq user-full-name "Zsolt Arkossy"
      user-mail-address "zsolt@arkossy.com")

(setq inhibit-startup-screen t) ; No Splash Screen
(menu-bar-mode 1) ; No Menu Mar
(tool-bar-mode -1) ; No Tool Bar
(toggle-scroll-bar -1) ; No Scroll Bar
(setq ring-bell-function 'ignore) ; No Bell Sound
(blink-cursor-mode 0) ; No Cursor Blink
(fset 'yes-or-no-p 'y-or-n-p) ; Set Yes/No Action
(column-number-mode 1) ; Show Column Number in Mode Line
(show-paren-mode 1) ; Set Matching Parenthesis

(use-package emacs
  :config
  (defun apollo/delete-emacs-init ()
    (interactive)
    (let ((configs "~/.emacs.d/emacs-personal-settings.el"))
      (when configs
	(delete-file configs))))
  :hook (kill-emacs . apollo/delete-emacs-init))

(setq backup-directory-alist '(("." . "~/.emacs.d/emacs-backups")))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/emacs-autosave/" t)))

(setq savehist-file "~/.emacs.d/emacs-save-history")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
	search-ring
	regexp-search-ring))

(require 'theme-looper)
(theme-looper-reset-themes-selection)
(global-set-key (kbd "C-}") 'theme-looper-enable-next-theme)
(theme-looper-set-favorite-themes '(modus-vivendi modus-operandi))

; Set 24h format
(setq display-time-24hr-format t)

; Set display format (to show date: %Y-%m-%d)
(setq display-time-format "[%H:%M]")

; Don't show the load time (x.x format)
(setq display-time-default-load-average nil)

; Show time in mode line
(display-time-mode 1)

; Align time display to the right side of the mode line
   (setq global-mode-string (remove 'display-time-string global-mode-string))
   (setq mode-line-end-spaces
         (list (propertize " " 'display '(space :align-to (- right 8)))
               'display-time-string))

(set-face-attribute 'fringe nil :background nil)

;; `line-spacing' is nil by default, I change it from time to time
(setq-default line-spacing 4)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;(global-set-key (kbd "C-=") 'text-scale-adjust)

(use-package moody
  :ensure)

(use-package keycast
  :ensure
  :after moody
  :commands keycast-mode
  :config
  (setq keycast-window-predicate 'moody-window-active-p)
  (setq keycast-separator-width 1)
  (setq keycast-insert-after 'mode-line-buffer-identification)
  (setq keycast-remove-tail-elements nil))

(keycast-mode 1)

;; on error delete the content of: .mc-lists.el
(require 'multiple-cursors)
(global-set-key (kbd "M-m") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
;(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(use-package olivetti
  :ensure
  :diminish
  :config
  (setq olivetti-body-width 100)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t)

  (defun prot/olivetti-mode ()
    "Toggle `olivetti-mode' with additional parameters Fringes
are disabled for the current window.  For the font-related
changes see `prot/variable-pitch-mode'."
    (interactive)
    (if (bound-and-true-p olivetti-mode)
	(progn
	  (olivetti-mode -1)
	  (set-window-fringes (selected-window) nil) ; Use default width
	  (prot/variable-pitch-mode))
      (olivetti-mode 1)
      (set-window-fringes (selected-window) 0 0)
      (prot/variable-pitch-mode (prot/reading-fonts))))
  :bind ("C-{" . prot/olivetti-mode))

(setq echo-keystrokes 0.1)

(global-visual-line-mode t)

(global-set-key (kbd "C-c w") 'auto-fill-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(setq-default fill-column 80)

(setq sentence-end-double-space nil)

(setq sentence-end-double-space nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(bind-key "M-SPC" 'cycle-spacing)

(add-to-list 'load-path "~/.emacs.d/additional-packages/")

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(setq default-directory "~/Documents/project-emacs")

(require 'ido)
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-enable-last-directory-history t)

(add-to-list 'ido-ignore-files "emacs-personal-settings.el")
(add-to-list 'ido-ignore-files ".pia_manager_crash.log")
(add-to-list 'ido-ignore-files "archive-todo.org")
(add-to-list 'ido-ignore-files "archive-day.org")
;Avoid certain directories:
;(setq ido-ignore-directories '("Applications/" "Library/" "Movies/" "Music/" "Pictures/"))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(desktop-save-mode 1)

;; Use CMD+arrows
(windmove-default-keybindings 'super)
;; Don't cycle around at edges (nil), enabled (t)
(setq windmove-wrap-around nil)

(global-set-key (kbd "<s-f1>") 'split-window-below)
(global-set-key (kbd "<s-f2>") 'split-window-right)
(global-set-key (kbd "<s-f3>") 'balance-windows)
(global-set-key (kbd "<s-f4>") 'delete-other-windows)
(global-set-key (kbd "<s-f5>") 'delete-window)

(require 'rotate)
(global-set-key (kbd "<s-f12>") 'rotate-window)
(global-set-key (kbd "<s-f11>") 'rotate:even-horizontal)
(global-set-key (kbd "<s-f10>") 'rotate-layout)

(setq org-directory "~/Documents/project-emacs")

(setq org-agenda-start-on-weekday 1)

(defun my/org-add-ids-to-headlines-in-file ()
  "Add ID properties to all headlines in the current file which
do not already have one."
  (interactive)
  (org-map-entries 'org-id-get-create))

(add-hook 'org-mode-hook
	  (lambda ()
	    (add-hook 'before-save-hook 'my/org-add-ids-to-headlines-in-file nil 'local)))


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

(global-set-key (kbd "<s-f6>") 'org-id-update-id-locations)

(setq calendar-week-start-day 1)

(add-to-list 'auto-mode-alist '("\\.\\(org\\)$" . org-mode))
(require 'org)

(global-set-key "\C-ca" 'org-agenda)

(setq org-hierarchical-todo-statistics nil)

;Demote sequence for list bullets
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))

;; Increase sub-item indentation
(setq org-list-indent-offset 1)

(setq org-catch-invisible-edits 'show-and-error)

(setq org-use-fast-todo-selection t)

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-agenda-files (list "~/Documents/project-emacs"))

(global-set-key "\C-cl" 'org-store-link)

(setq initial-major-mode 'org-mode)

(setq org-priority-faces '((?A . (:foreground "red" :weight 'bold))
			   (?B . (:foreground "orange"))
			   (?C . (:foreground "blue"))))

(setq org-list-description-max-indent 5)

(setq org-adapt-indentation nil)
