;;; monokai-pro-ristretto-theme.el --- A monokai pro colorscheme

;;; Code:

(require 'monokai-pro-theme)

(defvar monokai-pro-machine-theme-colors
  '(;; Background and foreground colors
    :bg     "#273136"
    :bg+1   "#3a4449"
    :bg+2   "#545f62"
    :fg-4   "#6b7678"
    :fg-3   "#8b9798"
    :fg-2   "#b4c3c4"
    :fg-1   "#f2fffc"
    :fg     "#f9fffe"

    ;; General colors
    :white  "#ffffff"
    :red    "#ff6d7e"
    :orange "#ffb270"
    :yellow "#ffed72"
    :green  "#a2e57b"
    :blue   "#7cd5f1"
    :purple "#baa0f8"
    :pink   "#ff6d7e"

    ;; Colors from the original Monokai colorschemes. Some of these are used
    ;; rarely as highlight colors. They should be avoided if possible.
    :orig-red     "#f92672"
    :orig-orange  "#fd971f"
    :orig-yellow  "#e6db74"
    :orig-green   "#a6e22e"
    :orig-cyan    "#a1efe4"
    :orig-blue    "#66d9ef"
    :orig-violet  "#ae81ff"
    :orig-magenta "#fd5ff0"))

(deftheme monokai-pro-machine)
(monokai-pro-theme-define 'monokai-pro-machine monokai-pro-machine-theme-colors)
(provide-theme 'monokai-pro-machine)

(provide 'monokai-pro-machine-theme)

;;; monokai-pro-machine-theme.el ends here
