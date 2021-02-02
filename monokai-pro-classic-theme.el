;;; monokai-pro-classic-theme.el --- A monokai pro colorscheme

;;; Code:

(require 'monokai-pro-theme)

(defvar monokai-pro-classic-theme-colors
  '(;; Background and foreground colors
    :bg     "#272821"
    :bg+1   "#3b3c35"
    :bg+2   "#57584f"
    :fg-4   "#6e7066"
    :fg-3   "#919288"
    :fg-2   "#abaca0"
    :fg-1   "#fdfff1"
    :fg     "#fefff8"

    ;; General colors
    :white  "#ffffff"
    :red    "#f82570"
    :orange "#fc961f"
    :yellow "#e4db73"
    :green  "#a6e12d"
    :blue   "#66d9ee"
    :purple "#ae81ff"
    :pink   "#f82570"

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

(deftheme monokai-pro-classic)
(monokai-pro-theme-define 'monokai-pro-classic monokai-pro-classic-theme-colors)
(provide-theme 'monokai-pro-classic)

(provide 'monokai-pro-classic-theme)

;;; monokai-pro-classic-theme.el ends here
