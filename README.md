# Personal GNU Emacs configuration files

These files are my settings initialisation files for Emacs. 
The reason I created this file is that I wanted to separate the 
compulsory boot code from the custom settings and personal settings.
My emacs code structure has been influenced by learning from
Protesilaos Stavrou <info@protesilaos.com>.


The 'emacs-custom-settings.el' and 'emacs-personal-settings.el' files are
automatically called by '.emacs' with the following code snippets:

(setq custom-file "~/.emacs.d/emacs-custom-settings.el")

(load-file custom-file)

;; Babel translates the emacs-personal-settings.org to emacs-personal-settings.el
(require 'org)

(setq vc-follow-symlinks t)

(org-babel-load-file (expand-file-name "~/.emacs.d/emacs-personal-settings.org"))

By using 'babel' I can use literate programming technique by combining org comments
with embeded source codes snippets.

With this approach I can separate my own settings from the ones created 
automatically by emacs (or by M-x customize command)
