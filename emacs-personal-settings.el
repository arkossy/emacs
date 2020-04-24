;; emacs-personal-settings.el 

;; ====  Personal GNU Emacs configuration file.

;; Copyright (c) 2020 Zsolt Arkossy <zsolt@arkossy.com>
;;
;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is the personal settings initialisation file for Emacs. 
;; The reason I created this file is that I wanted to separate the 
;; compulsory boot code from the custom settings and personal settings.
;; My emacs code structure has been influenced by learning from
;; Protesilaos Stavrou <info@protesilaos.com>.

;; See my dotfiles: https://https://github.com/arkossy

;; Code:


;; ==== No Initial Splash Screen
(setq inhibit-startup-screen t)


;; ==== Backup and autosave
(defvar backup-dir (expand-file-name "/Users/apollo/.emacs.d/emacs-backups/"))
(defvar autosave-dir (expand-file-name "/Users/apollo/.emacs.d/emacs-autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))


;; ==== C-u C-l Command Enablement
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; ==== Screen Position
(setq default-frame-alist '((left . 82) (top . 38) (width . 100) (height . 70)))


;; ==== Set Cursor Blink Status 0 no, 1 yes
(blink-cursor-mode 0)


;; ==== Set Text Wrapping 
(global-visual-line-mode t)


;; ==== Show Relative Number Lines
(setq display-line-numbers 'relative)


;; ==== Enable Save Window Settings
;(desktop-save-mode 1)



;; ==== Add Matching Parenthesis
(show-paren-mode 1)


;; ==== Sentences end with a single space
(setq sentence-end-double-space nil)



;; ==== Theme Looper
;; ==== https://github.com/myTerminal/theme-looper
(require 'theme-looper)
(global-set-key (kbd "C-}") 'theme-looper-enable-next-theme)
(theme-looper-set-favorite-themes '(afternoon zenburn modus-operandi modus-vivendi))


;; ==== Olivetti Mode
;; ==== https://github.com/myTerminal/theme-looper
(require 'olivetti)
(global-set-key (kbd "C-{") 'olivetti-mode)


;; ==== Cycle through buffers
(global-set-key [C-tab] 'next-buffer)
(global-set-key [C-S-tab] 'previous-buffer)


;; ==== Set TAB settings
(global-set-key (kbd "TAB") 'self-insert-command)


;; ==== Default Startup Folder for C-x C-f
(setq default-directory "~/Documents/project-unix" )


;; ==== Add theme directory
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")


;; ==== Keybinding Agenda
(global-set-key (kbd "C-c a") 'org-agenda)


;; ==== Set maximum indentation for description lists
(setq org-list-description-max-indent 5)


;; ==== Prevent demoting heading also shifting text inside sections
(setq org-adapt-indentation nil)


;; ==== No bell sound
(setq ring-bell-function 'ignore)


;; ==== Fill paragraphs with a single space after each period
(setq sentence-end-double-space nil)


;; ==== Add folder to org agenda scanning
(setq org-agenda-files '("~/Documents/project-unix"))

;; ==== Add rainbow delimiter - parenthesis colors
;; ==== https://github.com/Fanael/rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; ==== Prot Theme Settings
;;(load-theme 'modus-operandi t)
;;(setq modus-operandi-theme-rainbow-headings t)
;;(setq modus-operandi-theme-section-headings t)

