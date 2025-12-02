# Colourful

Semantic Common Lisp & Elisp syntax highlight in Emacs.

![screenshot](./screenshot.png)

## Usage

Load `colourful.el`, then enable `colourful` minor mode under `emacs-lisp-mode` or `lisp-mode`.

Example configuration:

``` emacs-lisp
(use-package colourful
  :vc (:url "https://github.com/calsys456/colorful"
	        :rev :newest
	        :branch "main")
  :config
  (add-hook 'emacs-lisp-mode-hook 'colourful-mode)
  (add-hook 'lisp-mode-hook 'colourful-mode))
```

# Acknowledgements

Ported from same-named [LispWorks plugin by ourselves](https://github.com/calsys456/lw-plugins).

Support Neurodivisity & Transgender & Plurality!
