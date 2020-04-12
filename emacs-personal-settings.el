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
(defvar backup-dir (expand-file-name "/Users/apollo/Documents/emacs/_backup-emacs/"))
(defvar autosave-dir (expand-file-name "/Users/apollo/Documents/emacs/_autosave-emacs/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))


;; ==== C-u C-l Command Enablement
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; ==== Screen Position
(setq default-frame-alist '((left . 883) (top . 60) (width . 117) (height . 60) ))


;; ==== Set Cursor to No Blink Status
(blink-cursor-mode 0)


;; ==== Set Text Wrapping 
(global-visual-line-mode t)


;; ==== Show Relative Number Lines
(setq display-line-numbers 'relative)

