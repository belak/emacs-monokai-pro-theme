;;; monokai-pro-ristretto-theme.el --- A monokai pro colorscheme

;;; Code:

(require 'monokai-pro-theme)

(defvar monokai-pro-octagon-theme-colors
  '(;; Background and foreground colors
    :bg     "#282a3a"
    :bg+1   "#3a3d4b"
    :bg+2   "#535763"
    :fg-4   "#696d77"
    :fg-3   "#888d94"
    :fg-2   "#a0a5ae"
    :fg-1   "#eaf2f1"
    :fg     "#f5f9f8"

    ;; General colors
    :white  "#ffffff"
    :red    "#ff657a"
    :orange "#ff9b5e"
    :yellow "#ffd76d"
    :green  "#bad761"
    :blue   "#9cd1bb"
    :purple "#c39ac9"
    :pink   "#ff657a"

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

(deftheme monokai-pro-octagon)
(monokai-pro-theme-define 'monokai-pro-octagon monokai-pro-octagon-theme-colors)
(provide-theme 'monokai-pro-octagon)

(provide 'monokai-pro-octagon-theme)

;;; monokai-pro-octagon-theme.el ends here
