;;; monokai-pro-theme.el --- A simple theme based on the Monokai Pro Sublime color schemes

;; Copyright (C) 2019  Kaleb Elwert

;; Author: Kaleb Elwert <kaleb@coded.io>
;; Maintainer: Kaleb Elwert <kaleb@coded.io>
;; Version: 0.1
;; Package-Version: 20190924.2152
;; URL: https://github.com/belak/emacs-monokai-pro-theme

;;; Commentary:

;; This theme is a simple theme based on the monokai-pro colors.

;;; Code:

(defvar monokai-pro-theme-colors
  '(;; Background and foreground colors
    :bg     "#2d2a2e"
    :bg+1   "#353236"
    :bg+2   "#403e41"
    :fg-4   "#5b595c"
    :fg-3   "#727072"
    :fg-2   "#939293"
    :fg-1   "#c1c0c0"
    :fg     "#fcfcfa"

    ;; General colors
    :white  "#ffffff"
    :red    "#ff6188"
    :orange "#fc9867"
    :yellow "#ffd866"
    :green  "#a9dc76"
    :blue   "#78dce8"
    :purple "#ab9df2"
    :pink   "#ff6188"))

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
    :red    "#FD6883"
    :orange "#F38D70"
    :yellow "#F9CC6C"
    :green  "#ADDA78"
    :blue   "#85DACC"
    :purple "#A8A9EB"
    :pink   "#FD6883"))

(defvar monokai-pro-spectrum-theme-colors
  '(;; Background and foreground colors
    :bg     "#222222"
    :bg+1   "#363537"
    :bg+2   "#525053"
    :fg-4   "#69676C"
    :fg-3   "#8B888F"
    :fg-2   "#BAB6BF"
    :fg-1   "#F7F1FF"
    :fg     "#FBF8FF"

    ;; General colors
    :white  "#ffffff"
    :red    "#FC618D"
    :orange "#FD9353"
    :yellow "#FCE566"
    :green  "#7BD88F"
    :blue   "#5AD4E6"
    :purple "#948AE3"
    :pink   "#FC618D"))

(defvar monokai-pro-machine-theme-colors
  '(;; Background and foreground colors
    :bg     "#273136"
    :bg+1   "#3A4449"
    :bg+2   "#545F62"
    :fg-4   "#6B7678"
    :fg-3   "#8B9798"
    :fg-2   "#B4C3C4"
    :fg-1   "#F2FFFC"
    :fg     "#F9FFFE"

    ;; General colors
    :white  "#ffffff"
    :red    "#FF6D7E"
    :orange "#FFB270"
    :yellow "#FFED72"
    :green  "#A2E57B"
    :blue   "#7CD5F1"
    :purple "#BAA0F8"
    :pink   "#FF6D7E"))


(defvar monokai-pro-octagon-theme-colors
  '(;; Background and foreground colors
    :bg     "#282A3A"
    :bg+1   "#3A3D4B"
    :bg+2   "#535763"
    :fg-4   "#696D77"
    :fg-3   "#888D94"
    :fg-2   "#A0A5AE"
    :fg-1   "#EAF2F1"
    :fg     "#F5F9F8"

    ;; General colors
    :white  "#ffffff"
    :red    "#FF657A"
    :orange "#FF9B5E"
    :yellow "#FFD76D"
    :green  "#BAD761"
    :blue   "#9CD1BB"
    :purple "#C39AC9"
    :pink   "#FF657A"))


(defvar monokai-pro-classic-theme-colors
  '(;; Background and foreground colors
    :bg     "#272821"
    :bg+1   "#3B3C35"
    :bg+2   "#57584F"
    :fg-4   "#6E7066"
    :fg-3   "#919288"
    :fg-2   "#ABACA0"
    :fg-1   "#FDFFF1"
    :fg     "#FEFFF8"

    ;; General colors
    :white  "#ffffff"
    :red    "#F82570"
    :orange "#FC961F"
    :yellow "#E4DB73"
    :green  "#A6E12D"
    :blue   "#66D9EE"
    :purple "#AE81FF"
    :pink   "#F82570"))

(defun monokai-pro-theme-transform-spec (spec colors)
  "Transform a theme `SPEC' into a face spec using `COLORS'."
  (let ((output))
    (while spec
      (let* ((key       (car  spec))
             (value     (cadr spec))
             (color-key (if (symbolp value) (intern (concat ":" (symbol-name value))) nil))
             (color     (plist-get colors color-key)))

        ;; Prepend the transformed element
        (cond
         ((and (memq key '(:box :underline)) (listp value))
          (push (monokai-pro-theme-transform-spec value colors) output))
         (color
          (push color output))
         (t
          (push value output)))

      ;; Prepend the actual key
      (push key output)

      ;; Go to the next element in the list
      (setq spec (cddr spec))))

    ;; Return the transformed spec
    output))

(defun monokai-pro-theme-transform-face (spec colors)
  "Transform a face `SPEC' into an Emacs theme face definition using `COLORS'."
  (let* ((face             (car spec))
         (definition       (cdr spec)))

    (list face `((t ,(monokai-pro-theme-transform-spec definition colors))))))

(defun monokai-pro-theme-set-faces (theme-name colors faces)
  "Define the important part of `THEME-NAME' using `COLORS' to map the `FACES' to actual colors."
  (apply 'custom-theme-set-faces theme-name
         (mapcar #'(lambda (face)
                     (monokai-pro-theme-transform-face face colors))
                 faces)))

(defvar monokai-pro-faces
  '(
;;; Built-in

;;;; basic colors
   ;;(border                                       :background bg+2)
   (cursor                                       :background fg)
   (default                                      :foreground fg :background bg)

   ;; TODO: bg matches what's in the sublime theme here, not bg+2
   (fringe                                       :background bg+2)

   ;;(gui-element                                  :background bg+1)
   (header-line                                  :background nil :inherit mode-line)

   ;; TODO: This matches highlight and findHighlight, but we may want
   ;; to look at findHighlightForeground which is simply bg.
   (highlight                                    :foreground fg-3 :background bg+1)

   (link                                         :foreground blue :underline t)
   (link-visited                                 :foreground purple :underline t)

   (minibuffer-prompt                            :foreground fg)
   (region                                       :background bg+2)
   (secondary-selection                          :background bg+2)
   (trailing-whitespace                          :foreground fg :background red)
   (widget-button                                :underline t)
   (widget-field                                 :background fg-1 :box (:line-width 1 :color bg+2))

   (error                                        :foreground red    :weight bold)
   (warning                                      :foreground orange :weight bold)
   (success                                      :foreground green  :weight bold)

;;;; font-lock
   (font-lock-builtin-face                       :foreground purple)
   (font-lock-comment-delimiter-face             :foreground fg-3)
   (font-lock-comment-face                       :foreground fg-3 :slant italic)
   (font-lock-constant-face                      :foreground purple)
   (font-lock-doc-face                           :foreground fg-3)
   (font-lock-doc-string-face                    :foreground fg-3)
   (font-lock-function-name-face                 :foreground green)
   (font-lock-keyword-face                       :foreground pink)
   ;;(font-lock-negation-char-face                 :foreground fg-1)
   ;;(font-lock-preprocessor-face                  :foreground fg-1)
   ;;(font-lock-regexp-grouping-backslash          :foreground fg-1)
   ;;(font-lock-regexp-grouping-construct          :foreground fg)
   (font-lock-string-face                        :foreground yellow)
   (font-lock-type-face                          :foreground blue)
   (font-lock-variable-name-face                 :foreground fg)
   (font-lock-warning-face                       :foreground orange)

;;;; isearch
   (match                                        :foreground yellow :background bg :inverse-video t)

   ;; TODO: Revisit this - doesn't seem to map properly onto tmThemes
   (isearch                                      :foreground bg :background yellow :weight bold)
   (lazy-highlight                               :foreground fg-1 :inverse-video t)
   (isearch-fail                                 :foreground red :background fg)

;;;; line-numbers
   (line-number                                  :foreground fg-2)
   (line-number-current-line                     :foreground bg+2 :inverse-video t :inherit line-number)

;;;; linum-mode
   (linum                                        :foreground fg-3 :inherit fringe)
   (linum-highlight-face                         :foreground bg+2 :background fg-2)

;;;; mode-line
   (mode-line                                    :foreground fg-2 :background bg+1)
   (mode-line-buffer-id                          :foreground yellow :background nil)
   (mode-line-emphasis                           :foreground fg-1 :slant italic)
   (mode-line-highlight                          :foreground fg :box nil :weight bold)
   (mode-line-inactive                           :foreground fg-2 :background bg+2)

;;; Third-party

;;;; php-mode
   (php-function-name				 :foreground green)
   (php-function-call				 :foreground green)
   (php-string 					 :foreground yellow)
   (php-keyword 				 :foreground blue)
   (php-builtin 				 :foreground purple)
   (php-method-call 				 :foreground green)
   (php-static-method-call 			 :foreground green)
   (php-variable-name 				 :foreground fg)
   (php-property-name 				 :foreground fg)
   (php-variable-sigil 				 :foreground fg-2)
   (php-operator                                 :foreground red)
   (php-paamayim-nekudotayim 			 :foreground red)
   (php-type 					 :foreground blue :slant italic)
   (php-class 					 :foreground blue)
   (php-constant 				 :foreground purple)
   (php-constant-assign 			 :foreground blue)
   (php-magical-constant 			 :foreground purple)
   (php-$this 					 :foreground fg-2)
   (php-$this-sigil				 :foreground fg-2)
   (php-errorcontrol-op 			 :foreground red)
   (php-doc-annotation-tag 			 :foreground blue)
   (php-doc-variable-sigil 			 :foreground fg-4)
   (php-doc-$this 				 :foreground fg-4)
   (php-doc-$this-sigil 			 :foreground fg-4)
   (php-doc-class-name 				 :foreground fg-4)


;;;; anzu-mode
   ;;    (anzu-mode-line                               :foreground yellow)

;;;; company-mode
   ;; TODO: These don't feel quite right
   (company-tooltip                              :background bg+2 :inherit default)
   (company-scrollbar-bg                         :background bg+1)
   (company-scrollbar-fg                         :background fg-1)
   (company-tooltip-annotation                   :foreground red)
   (company-tooltip-common                       :foreground yellow)
   (company-tooltip-selection                    :background bg+1)
   (company-preview-common                       :foreground fg-1 :background blue)


;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face		 :foreground red)
   (rainbow-delimiters-depth-2-face		 :foreground pink)
   (rainbow-delimiters-depth-3-face		 :foreground orange)
   (rainbow-delimiters-depth-4-face		 :foreground yellow)
   (rainbow-delimiters-depth-5-face		 :foreground green)
   (rainbow-delimiters-depth-6-face		 :foreground blue)
   (rainbow-delimiters-depth-7-face		 :foreground purple)
   (rainbow-delimiters-depth-8-face		 :foreground fg-1)
   (rainbow-delimiters-depth-9-face		 :foreground fg)

;;;; popup
   (popup-menu-selection-face			 :background bg+1)
   (popup-summary-face				 :background bg+2 :inherit default)
   (popup-menu-face                              :background bg+2)
   (popup-menu-mouse-face			 :background bg+1)
   (popup-tip-face				 :background bg+2)
   (popup-scroll-bar-background-face             :background bg+1)
   (popup-scroll-bar-foreground-face             :background fg-1)
   
;;;; diff-hl-mode
   (diff-hl-change                               :foreground blue)
   (diff-hl-delete                               :foreground red)
   (diff-hl-insert                               :foreground green)

;;;; diff-mode
   (diff-added                                   :foreground green)
   (diff-changed                                 :foreground purple)
   (diff-removed                                 :foreground red)
   (diff-header                                  :background bg)
   (diff-file-header                             :background bg+1)
   (diff-hunk-header                             :foreground pink :background bg)

;;;; flycheck-mode
   (flycheck-error                               :underline (:style wave :color red))
   (flycheck-info                                :underline (:style wave :color yellow))
   (flycheck-warning                             :underline (:style wave :color orange))

;;;; flyspell-mode
   (flyspell-duplicate                           :underline (:style wave :color orange))
   (flyspell-incorrect                           :underline (:style wave :color red))

;;;; hl-line-mode
   (hl-line                                      :background bg+1)

;;;; ido-mode
   ;; TODO: These don't feel quite right
   (ido-subdir                                   :foreground fg-2)
   (ido-first-match                              :foreground orange)
   (ido-only-match                               :foreground green)
   (ido-indicator                                :foreground red :background bg+2)
   (ido-virtual                                  :foreground fg-2)

;;;; ido-vertical-mode
   (ido-vertical-match-face                      :foreground fg-1)

;;;; show-paren-mode
   (show-paren-match                             :foreground fg :background blue)
   (show-paren-mismatch                          :background red :inverse-video t)

   ))

(deftheme monokai-pro)
(monokai-pro-theme-set-faces
 'monokai-pro
 monokai-pro-theme-colors
 monokai-pro-faces
 )
;; Anything leftover that doesn't fall neatly into a face goes here.
(let ((bg      (plist-get monokai-pro-theme-colors :bg))
      (fg      (plist-get monokai-pro-theme-colors :fg))
      (red     (plist-get monokai-pro-theme-colors :red))
      (green   (plist-get monokai-pro-theme-colors :green))
      (yellow  (plist-get monokai-pro-theme-colors :yellow))
      (blue    (plist-get monokai-pro-theme-colors :blue))
      (magenta (plist-get monokai-pro-theme-colors :purple))
      (cyan    (plist-get monokai-pro-theme-colors :pink)))
  (custom-theme-set-variables
   'monokai-pro
   `(ansi-color-names-vector
     ;; black, base08, base0B, base0A, base0D, magenta, cyan, white
     [,bg ,red ,green ,yellow ,blue ,magenta ,cyan ,fg])
   `(ansi-term-color-vector
     ;; black, base08, base0B, base0A, base0D, magenta, cyan, white
     [unspecified ,bg ,red ,green ,yellow ,blue ,magenta ,cyan ,fg])))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'monokai-pro)

(provide 'monokai-pro-theme)

;;; monokai-pro-theme.el ends here
