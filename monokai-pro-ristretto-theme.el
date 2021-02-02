;;; monokai-pro-ristretto-theme.el --- A monokai pro colorscheme

;;; Code:

(require 'monokai-pro-theme)

(defvar monokai-pro-ristretto-theme-colors
  '(;; Background and foreground colors
    :bg     "#2C2525"
    :bg+1   "#403838"
    :bg+2   "#5B5353"
    :fg-4   "#72696A"
    :fg-3   "#948A8B"
    :fg-2   "#B5A9AA"
    :fg-1   "#FFF1F3"
    :fg     "#FFF8F9"

    ;; General colors
    :white  "#ffffff"
    :red    "#fd6883"
    :orange "#f38d70"
    :yellow "#f9cc6c"
    :green  "#adda78"
    :blue   "#85dacc"
    :purple "#a8a9eb"
    :pink   "#fd6883"

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

(deftheme monokai-pro-ristretto)
(monokai-pro-theme-define 'monokai-pro-ristretto monokai-pro-ristretto-theme-colors)
(provide-theme 'monokai-pro-ristretto)

(provide 'monokai-pro-ristretto-theme)

;;; monokai-pro-ristretto-theme.el ends here
