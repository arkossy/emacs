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

;;; Code:


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (misterioso)))
 '(custom-safe-themes
   (quote
    ("d2af374d72f412b04bbad533072f1987674453ce8efae8c88725fcd9c246327b" "eac734e23475656ca76306044b8c97537d0745e41908baf295d63cd8fd551c47" "9163deb96c234c979ebf3995e5d486b092fc4d3360eedd866c7a494584f1b6dc" default)))
 '(flymake-error-bitmap
   (quote
    (flymake-double-exclamation-mark modus-theme-fringe-red)))
 '(flymake-note-bitmap (quote (exclamation-mark modus-theme-fringe-cyan)))
 '(flymake-warning-bitmap (quote (exclamation-mark modus-theme-fringe-yellow)))
 '(frame-background-mode (quote light))
 '(hl-todo-keyword-faces
   (quote
    (("HOLD" . "#714900")
     ("TODO" . "#721045")
     ("NEXT" . "#5317ac")
     ("THEM" . "#8f0075")
     ("PROG" . "#005589")
     ("OKAY" . "#185870")
     ("DONT" . "#4a5700")
     ("FAIL" . "#a80000")
     ("DONE" . "#005200")
     ("NOTE" . "#804000")
     ("KLUDGE" . "#8b3800")
     ("HACK" . "#8b3800")
     ("TEMP" . "#4d0006")
     ("FIXME" . "#9d2020")
     ("XXX+" . "#880000")
     ("REVIEW" . "#005a68")
     ("DEPRECATED" . "#001170"))))
 '(ibuffer-deletion-face (quote dired-flagged))
 '(ibuffer-filter-group-name-face (quote dired-mark))
 '(ibuffer-marked-face (quote dired-marked))
 '(ibuffer-title-face (quote dired-header))
 '(package-selected-packages
   (quote
    (ivy theme-looper modus-vivendi-theme modus-operandi-theme multi-term use-package)))
 '(scroll-bar-mode nil)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#a80000")
     (40 . "#721045")
     (60 . "#8f0075")
     (80 . "#880000")
     (100 . "#8b3800")
     (120 . "#714900")
     (140 . "#5d3026")
     (160 . "#184034")
     (180 . "#005200")
     (200 . "#4a5700")
     (220 . "#005a68")
     (240 . "#185870")
     (260 . "#005589")
     (280 . "#093060")
     (300 . "#0030a6")
     (320 . "#223fbf")
     (340 . "#0000bb")
     (360 . "#5317ac"))))
 '(vc-annotate-very-old-color nil)
 '(xterm-color-names
   ["#000000" "#a80000" "#005200" "#8b3800" "#0030a6" "#721045" "#005589" "#f3f1f3"])
 '(xterm-color-names-bright
   ["#505050" "#880000" "#4a5700" "#714900" "#223fbf" "#8f0075" "#185870" "#ffffff"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "nil" :family "Hack")))))
