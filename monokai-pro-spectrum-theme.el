;;; monokai-pro-ristretto-theme.el --- A monokai pro colorscheme

;;; Code:

(require 'monokai-pro-theme)

(defvar monokai-pro-spectrum-theme-colors
  '(;; Background and foreground colors
    :bg     "#222222"
    :bg+1   "#363537"
    :bg+2   "#525053"
    :fg-4   "#69676c"
    :fg-3   "#8b888f"
    :fg-2   "#bab6bf"
    :fg-1   "#f7f1ff"
    :fg     "#fbf8ff"

    ;; General colors
    :white  "#ffffff"
    :red    "#fc618d"
    :orange "#fd9353"
    :yellow "#fce566"
    :green  "#7bd88f"
    :blue   "#5ad4e6"
    :purple "#948ae3"
    :pink   "#fc618d"

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

(deftheme monokai-pro-spectrum)
(monokai-pro-theme-define 'monokai-pro-spectrum monokai-pro-spectrum-theme-colors)
(provide-theme 'monokai-pro-spectrum)

(provide 'monokai-pro-spectrum-theme)

;;; monokai-pro-spectrum-theme.el ends here
