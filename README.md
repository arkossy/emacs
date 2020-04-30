# GNU Emacs configuration files

These files are my main initialisation files for Emacs. My objective was to separate the compulsory boot code from the custom settings and personal settings.

By implementing 'babel' I can use literate programming technique by combining org comments with embeded source codes snippets. With this approach I can separate my own settings from the ones created automatically by emacs (or by M-x customize command). I use the 'emacs-personal-settings.org' file as main source, and the '.el' version is generated automatically at every startup.

The 'emacs-custom-settings.el' and 'emacs-personal-settings.el' files are automatically called by '.emacs' with the following code snippets:

<pre><code>
(setq custom-file "~/.emacs.d/emacs-custom-settings.el")
(load-file custom-file)
</code></pre>

<pre><code>
;; Babel translates the emacs-personal-settings.org to emacs-personal-settings.el
(require 'org)
(setq vc-follow-symlinks t)
(org-babel-load-file (expand-file-name "~/.emacs.d/emacs-personal-settings.org"))
</code></pre>

*Note: My emacs code structure has been influenced by:
- Protesilaos Stavrou https://protesilaos.com
- Bernt Hansen http://doc.norang.ca/org-mode.html#HowToUseThisDocument
- Rainer König https://www.youtube.com/watch?v=sQS06Qjnkcc&list=PLVtKhBrRV_ZkPnBtt_TD1Cs9PJlU0IIdE*
