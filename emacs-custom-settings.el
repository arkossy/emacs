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
   [default bold shadow italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#ff6188" "#fcfcfa"])
 '(ansi-term-color-vector
   [unspecified "#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#ff6188" "#fcfcfa"])
 '(beacon-color "#d33682")
 '(blink-cursor-mode nil)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (afternoon)))
 '(custom-safe-themes
   (quote
    ("73c69e346ec1cb3d1508c2447f6518a6e582851792a8c0e57a22d6b9948071b4" "3f5f69bfa958dcf04066ab2661eb2698252c0e40b8e61104e3162e341cee1eb9" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "8587afa8626fd22782049193a9e9bcd0463dc0528393bfedd489260519673ead" "cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2" "d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c" "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae" "ffba0482d3548c9494e84c1324d527f73ea4e43fff8dfd0e48faa8fc6d5c2bc7" "bcd0237b2a5b7897e482458cc62c4f3fa3d9d7f9a9667338e67d4c7a8e009819" "454c1c9ce70f7d807c51c890910365fd3c64a9e63f596511e9ff57dd97bbeea8" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" "76c5b2592c62f6b48923c00f97f74bcb7ddb741618283bdb2be35f3c0e1030e3" "aded61687237d1dff6325edb492bde536f40b048eab7246c61d5c6643c696b7f" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "2074a98e21377af1c50897d4330caca2b719542bcdf9618ed3c1575c99b41363" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "3f9e0224e15bd698eda9931920221a90acf6889ca9553d3171a2dc4206700274" "5bb4521035977989a56a2ce66079746dfef98058823893e561ec0deb978a9654" "40536857ab8070b45a57ae915fe7744b2fd2f175d0dd30e7bf03cc730f3c037a" "6b9613ba6393af7f9617599a8bc1190316335bd490f6a4c86a129f00fd6515de" "468e20feb719ea313d8b071b50c50e0f704cd2da0b406a1eadea838d57f90088" "63f3e8e89267c2e01f2cdef951fb2b594e51d09ed150cbb3dba94630cfd5dffa" "f9aede508e587fe21bcfc0a85e1ec7d27312d9587e686a6f5afdbb0d220eab50" "983eb22dae24cab2ce86ac26700accbf615a3f41fef164085d829fe0bcd3c236" "b89ae2d35d2e18e4286c8be8aaecb41022c1a306070f64a66fd114310ade88aa" "939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "d1ede12c09296a84d007ef121cd72061c2c6722fcb02cb50a77d9eae4138a3ff" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d2af374d72f412b04bbad533072f1987674453ce8efae8c88725fcd9c246327b" "a432dc46f3dc96346ccdf9a6a538b3e2db843b16824108c0265ca44391966f6b" default)))
 '(fci-rule-color "#383838")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(flymake-error-bitmap
   (quote
    (flymake-double-exclamation-mark modus-theme-fringe-red)))
 '(flymake-note-bitmap (quote (exclamation-mark modus-theme-fringe-cyan)))
 '(flymake-warning-bitmap (quote (exclamation-mark modus-theme-fringe-yellow)))
 '(frame-background-mode (quote light))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (quote
    ("#3b6b40f432d6" "#07b9463c4d36" "#47a3341e358a" "#1d873c3f56d5" "#2d86441c3361" "#43b7362d3199" "#061d417f59d7")))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#5b7300" . 20)
     ("#007d76" . 30)
     ("#0061a8" . 50)
     ("#866300" . 60)
     ("#992700" . 70)
     ("#a00559" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#866300" "#992700" "#a7020a" "#a00559" "#243e9b" "#0061a8" "#007d76" "#5b7300")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(hl-sexp-background-color "#33323e")
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(ibuffer-deletion-face (quote dired-flagged))
 '(ibuffer-filter-group-name-face (quote dired-mark))
 '(ibuffer-marked-face (quote dired-marked))
 '(ibuffer-title-face (quote dired-header))
 '(linum-format " %7i ")
 '(lsp-ui-doc-border "#93a1a1")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files (quote ("~/Desktop/test.org")) t)
 '(org-log-done (quote note))
 '(org-log-into-drawer t)
 '(org-log-reschedule (quote note))
 '(org-support-shift-select t)
 '(package-selected-packages
   (quote
    (rainbow-delimiters sublime-themes leuven-theme color-theme-sanityinc-tomorrow afternoon-theme zenburn-theme use-package theme-looper olivetti multi-term modus-vivendi-theme modus-operandi-theme ivy)))
 '(pdf-view-midnight-colors (quote ("#fdf4c1" . "#32302f")))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
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
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#000000" "#a80000" "#005200" "#8b3800" "#0030a6" "#721045" "#005589" "#f3f1f3"])
 '(xterm-color-names-bright
   ["#505050" "#880000" "#4a5700" "#714900" "#223fbf" "#8f0075" "#185870" "#ffffff"]))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "nil" :family "Hack")))))
