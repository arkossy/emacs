;; .emacs 

;; ====  Main Boot GNU Emacs configuration file.

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

;; This file is the main initialisation file for Emacs. The reason I
;; created this file is that I wanted to separate the compulsory boot
;; code from the custom settings and personal settings.
;; My emacs code structure has been influenced by learning from
;; Protesilaos Stavrou <info@protesilaos.com>.

;; See my dotfiles: https://https://github.com/arkossy

;;; Code:

;; Mandatory code by emacs
(require 'package)


;; Refresh Melpa packages on startup
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))


;; Initialise the packages, avoiding a re-initialisation.
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))


;; Make sure `use-package' is available.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))



(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))


(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))



;; Create a separate custom file for system settings
;; This file needs to be created separately by the user
(setq custom-file "~/.emacs.d/emacs-custom-settings.el")
(load-file custom-file)


;; Babel translates the emacs-personal-settings.org to emacs-personal-settings.el
;(require 'org)
(setq vc-follow-symlinks t)
(org-babel-load-file (expand-file-name "~/.emacs.d/emacs-personal-settings.org"))
