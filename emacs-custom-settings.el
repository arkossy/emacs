;; emacs-custom-settings.el

;; ==== Custom GNU Emacs configuration file.

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

;; This file is the custom initialisation file for Emacs. The reason I
;; created this file is that I wanted to separate the compulsory boot
;; code from the custom settings and personal settings.
;; My emacs code structure has been influenced by learning from
;; Protesilaos Stavrou <info@protesilaos.com>.

;; See my dotfiles: https://https://github.com/arkossy
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (modus-operandi)))
 '(custom-safe-themes
   (quote
    ("d2af374d72f412b04bbad533072f1987674453ce8efae8c88725fcd9c246327b" "fb213c95bb4c2d8d9259580ae8063ce06a7094a0ccf7a85b1a53b56c5b4d9f1c" default)))
 '(package-selected-packages
   (quote
    (which-key zenburn-theme use-package theme-looper sublime-themes solarized-theme rainbow-delimiters olivetti multiple-cursors multi-term modus-vivendi-theme modus-operandi-theme leuven-theme ivy autothemer auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "nil" :family "Hack")))))
