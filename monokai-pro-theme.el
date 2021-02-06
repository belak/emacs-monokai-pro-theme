;;; monokai-pro-theme.el --- A simple theme based on the Monokai Pro Sublime color schemes

;; Copyright (C) 2019-2020  Kaleb Elwert

;; Author: Kaleb Elwert <belak@coded.io>
;; Maintainer: Kaleb Elwert <belak@coded.io>
;; Version: 0.2
;; URL: https://github.com/belak/emacs-monokai-pro-theme

;;; Commentary:

;; This theme is a simple theme based on the monokai-pro colors.

;;; Code:

(defvar monokai-pro-theme-default-colors
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
    :pink   "#ff6188"

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

(defun monokai-pro-theme-define (theme-name theme-colors)
  "Define the faces for a monokai-pro colorscheme given a `THEME-NAME' and a plist of `THEME-COLORS'."
  (monokai-pro-theme-set-faces
   theme-name
   theme-colors
   '(
;;; Built-in

;;;; basic colors
     ;;(border                                       :background bg+2)
     (cursor                                       :background fg)
     (default                                      :foreground fg :background bg)

     (fringe                                       :background bg)

     ;;(gui-element                                  :background bg+1)
     (header-line                                  :background nil :inherit mode-line)

     ;; TODO: This matches highlight and findHighlight, but we may want
     ;; to look at findHighlightForeground which is simply bg.
     (highlight                                    :foreground fg-3 :background bg+1)

     (link                                         :foreground blue :underline t)
     (link-visited                                 :foreground purple :underline t)

     (minibuffer-prompt                            :foreground fg)
     (region                                       :background bg+2)
     (shadow                                       :foreground fg-2)
     (secondary-selection                          :background bg+2)
     (trailing-whitespace                          :foreground fg :background red)
     (vertical-border                              :foreground bg+1)
     (whitespace-trailing                          :inherit trailing-whitespace)
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
     (line-number-current-line                     :foreground fg :background bg+2)

;;;; linum-mode
     (linum                                        :foreground fg-3 :inherit fringe)
     (linum-highlight-face                         :foreground bg+2 :background fg-2)

;;;; mode-line
     (mode-line                                    :foreground fg-2 :background bg+1)
     (mode-line-buffer-id                          :foreground yellow :background nil)
     (mode-line-emphasis                           :foreground fg-1)
     (mode-line-highlight                          :foreground fg :box nil :weight bold)
     (mode-line-inactive                           :foreground fg-2 :background bg+2)

;;;; tab-bar
     (tab-bar                                      :background bg)
     (tab-bar-tab                                  :foreground yellow :underline t :weight semi-bold :inherit tab-bar)
     (tab-bar-tab-inactive                         :foreground fg-1 :inherit tab-bar)

;;;; term
     (term-color-black                             :foreground bg+1)
     (term-color-blue                              :foreground blue)
     (term-color-cyan                              :foreground purple)
     (term-color-green                             :foreground green)
     (term-color-magenta                           :foreground pink)
     (term-color-red                               :foreground red)
     (term-color-white                             :foreground fg)
     (term-color-yellow                            :foreground yellow)

;;; Third-party

;;;; all-the-icons
     (all-the-icons-red                            :foreground red)
     (all-the-icons-blue                           :foreground blue)
     (all-the-icons-green                          :foreground green)
     (all-the-icons-yellow                         :foreground yellow)
     (all-the-icons-purple                         :foreground purple)
     (all-the-icons-pink                           :foreground pink)
     (all-the-icons-orange                         :foreground orange)

;;;; anzu-mode
     ;;    (anzu-mode-line                               :foreground yellow)

;;;; company-mode
     (company-box-selection                        :foreground fg-1 :background bg+2)
     (company-tooltip                              :background bg+1 :inherit default)
     (company-scrollbar-bg                         :background bg+1)
     (company-scrollbar-fg                         :background fg-1)
     (company-tooltip-annotation                   :foreground red)
     (company-tooltip-common                       :foreground yellow)
     (company-tooltip-selection                    :background bg+2)
     (company-preview-common                       :foreground blue :background bg+2)

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
     (flycheck-fringe-error                        :foreground red)
     (flycheck-fringe-info                         :foreground yellow)
     (flycheck-fringe-warning                      :foreground orange)

;;;; flymake-mode
     (flymake-error                                :underline (:style wave :color red))
     (flymake-note                                 :underline (:style wave :color yellow))
     (flymake-warning                              :underline (:style wave :color orange))

;;;; flyspell-mode
     (flyspell-duplicate                           :underline (:style wave :color orange))
     (flyspell-incorrect                           :underline (:style wave :color red))

;;;; git-gutter-mode
     (git-gutter:added                             :foreground green)
     (git-gutter:deleted                           :foreground red)
     (git-gutter:modified                          :foreground purple)
     (git-gutter:separator                         :foreground blue)
     (git-gutter:unchanged                         :background yellow)

;;;; hl-line-mode
     (hl-line                                      :background bg+1)

;;;; hl-todo-mode
     (hl-todo                                      :slant italic :weight bold)

;;;; info
     (info-menu-star                               :foreground red)

;;;; ido-mode
     ;; TODO: These don't feel quite right
     (ido-subdir                                   :foreground fg-2)
     (ido-first-match                              :foreground orange)
     (ido-only-match                               :foreground green)
     (ido-indicator                                :foreground red :background bg+2)
     (ido-virtual                                  :foreground fg-2)

;;;; ido-vertical-mode
     (ido-vertical-match-face                      :foreground fg-1)

;;;; ivy
     (ivy-action                                   :foreground purple)
     (ivy-confirm-face                             :foreground green)
     (ivy-current-match                            :foreground bg :background yellow)
     (ivy-cursor                                   :foreground bg :background fg)
     (ivy-grep-info                                :foreground pink)
     (ivy-grep-line-number                         :foreground red)
     (ivy-highlight-face                           :background bg+1 :foreground fg-1)
     (ivy-match-required-face                      :foreground red)
     (ivy-minibuffer-match-face-1                  :foreground fg :background bg+1)
     (ivy-minibuffer-match-face-2                  :foreground fg :background bg+2)
     (ivy-minibuffer-match-face-3                  :foreground fg :background purple)
     (ivy-minibuffer-match-face-4                  :foreground fg :background pink)
     (ivy-minibuffer-match-highlight               :foreground fg-4 :background bg+1)
     (ivy-modified-buffer                          :foreground fg)
     (ivy-modified-outside-buffer                  :foreground fg)
     (ivy-org                                      :foreground bg+1 :slant italic)
     (ivy-prompt-match                             :foreground bg :background yellow)
     (ivy-remote                                   :foreground purple)
     (ivy-separator                                :foreground bg+1)
     (ivy-subdir                                   :foreground green)
     (ivy-virtual                                  :foreground purple)
     (ivy-yanked-word                              :foreground fg-4 :background bg+1)
     (swiper-background-match-face-1               :foreground bg :background fg)
     (swiper-background-match-face-2               :foreground bg :background yellow)
     (swiper-background-match-face-3               :foreground bg :background yellow :weight bold)
     (swiper-background-match-face-4               :foreground red :background fg)
     (swiper-match-face-1                          :foreground bg :background fg)
     (swiper-match-face-2                          :foreground bg :background yellow)
     (swiper-match-face-3                          :foreground bg :background yellow :weight bold)
     (swiper-match-face-4                          :foreground red :background fg)
     (swiper-line-face                             :foreground bg+2 :background bg+1)

;;; lsp-ui
     (lsp-ui-doc-background                        :background bg+1)
     (lsp-ui-peek-filename                         :foreground orange)
     (lsp-ui-peek-header                           :foreground bg :background fg)
     (lsp-ui-peek-footer                           :foreground bg :background fg)
     (lsp-ui-peek-peek                             :foreground fg :background bg+1)
     (lsp-ui-peek-highlight                        :foreground fg-1)
     (lsp-ui-peek-selection                        :foreground fg-1 :background bg+2)
     (lsp-ui-peek-list                             :background bg+1)
     (lsp-ui-peek-line-number                      :foreground red)

;;; org-mode
     (org-level-1                                  :foreground orange)
     (org-level-2                                  :foreground green)
     (org-level-3                                  :foreground blue)
     (org-level-4                                  :foreground yellow)
     (org-level-5                                  :foreground orig-cyan)
     (org-level-6                                  :foreground green)
     (org-level-7                                  :foreground red)
     (org-level-8                                  :foreground blue)
     (org-document-info                            :foreground fg)
     (org-document-title                           :weight bold :inherit org-document-info)

;;;; php-mode
     (php-block-delimiter                          :foreground fg-3)
     (php-block-statement                          :foreground red)
     (php-class-declaration-spec                   :foreground red)
     (php-class-modifiers                          :foreground red)
     (php-constant-assign                          :foreground red)
     (php-constant-keyword                         :foreground purple)
     (php-flow-control-statement                   :foreground red)
     (php-import-declaration                       :foreground red)
     (php-include-statement                        :foreground green)
     (php-method-access                            :foreground red :slant italic)
     (php-method-modifiers                         :foreground red :slant italic)
     (php-method-static                            :foreground red :slant italic)
     (php-namespace-declaration                    :foreground red)
     (php-number                                   :foreground purple)
     (php-php-tag                                  :foreground orange)
     (php-print-statement                          :foreground green)
     (php-property-access                          :foreground red :slant italic)
     (php-property-const                           :foreground red :slant italic)
     (php-property-static                          :foreground red :slant italic)
     (php-string-quote                             :foreground fg-3)
     (php-type-operator                            :foreground red)
     (php-function-keyword                         :foreground blue :slant italic)

     (php-function-name                            :foreground green)
     (php-function-call                            :foreground green)
     (php-string                                   :foreground yellow)
     (php-keyword                                  :foreground blue)
     (php-builtin                                  :foreground purple)
     (php-method-call                              :foreground green)
     (php-static-method-call                       :foreground green)
     (php-variable-name                            :foreground fg)
     (php-property-name                            :foreground fg)
     (php-variable-sigil                           :foreground fg-2)
     (php-operator                                 :foreground red)
     (php-paamayim-nekudotayim                     :foreground red)
     (php-type                                     :foreground blue :slant italic)
     (php-class                                    :foreground red)
     (php-constant                                 :foreground purple)
     (php-constant-assign                          :foreground blue)
     (php-magical-constant                         :foreground purple)
     (php-$this                                    :foreground fg-2 :slant italic)
     (php-$this-sigil                              :foreground fg-2 :slant italic)
     (php-errorcontrol-op                          :foreground red)
     (php-doc-annotation-tag                       :foreground blue)
     (php-doc-variable-sigil                       :foreground fg-4)
     (php-doc-$this                                :foreground fg-4)
     (php-doc-$this-sigil                          :foreground fg-4)
     (php-doc-class-name                           :foreground fg-4)

;;;; popup
     (popup-menu-selection-face                    :background bg+1)
     (popup-summary-face                           :background bg+2 :inherit default)
     (popup-menu-face                              :background bg+2)
     (popup-menu-mouse-face                        :background bg+1)
     (popup-tip-face                               :background bg+2)
     (popup-scroll-bar-background-face             :background bg+1)
     (popup-scroll-bar-foreground-face             :background fg-1)

;;;; rainbow-delimiters
     (rainbow-delimiters-depth-1-face              :inherit rainbow-delimiters-base-face :foreground red)
     (rainbow-delimiters-depth-2-face              :inherit rainbow-delimiters-base-face :foreground pink)
     (rainbow-delimiters-depth-3-face              :inherit rainbow-delimiters-base-face :foreground orange)
     (rainbow-delimiters-depth-4-face              :inherit rainbow-delimiters-base-face :foreground yellow)
     (rainbow-delimiters-depth-5-face              :inherit rainbow-delimiters-base-face :foreground green)
     (rainbow-delimiters-depth-6-face              :inherit rainbow-delimiters-base-face :foreground blue)
     (rainbow-delimiters-depth-7-face              :inherit rainbow-delimiters-base-face :foreground purple)
     (rainbow-delimiters-depth-8-face              :inherit rainbow-delimiters-base-face :foreground fg-1)
     (rainbow-delimiters-depth-9-face              :inherit rainbow-delimiters-base-face :foreground fg)
     (rainbow-delimiters-unmatched-face            :inherit (show-paren-mismatch rainbow-delimiters-base-face))

;;;; show-paren-mode
     (show-paren-match                             :underline t :weight bold)
     (show-paren-mismatch                          :foreground fg :background orig-red :weight bold)

;;;; selectrum
     (selectrum-current-candidate                  :foreground orange)
     (selectrum-completion-annotation              :foreground blue)

     ))

  ;; Anything leftover that doesn't fall neatly into a face goes here.
  (let ((bg      (plist-get theme-colors :bg))
        (fg      (plist-get theme-colors :fg))
        (red     (plist-get theme-colors :red))
        (green   (plist-get theme-colors :green))
        (yellow  (plist-get theme-colors :yellow))
        (blue    (plist-get theme-colors :blue))
        (magenta (plist-get theme-colors :purple))
        (cyan    (plist-get theme-colors :orig-cyan)))
    (custom-theme-set-variables
     'monokai-pro
     `(ansi-color-names-vector
       ;; black, base08, base0B, base0A, base0D, magenta, cyan, white
       [,bg ,red ,green ,yellow ,blue ,magenta ,cyan ,fg])
     `(ansi-term-color-vector
       ;; black, base08, base0B, base0A, base0D, magenta, cyan, white
       [unspecified ,bg ,red ,green ,yellow ,blue ,magenta ,cyan ,fg]))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(deftheme monokai-pro)
(monokai-pro-theme-define 'monokai-pro monokai-pro-theme-default-colors)
(provide-theme 'monokai-pro)

(provide 'monokai-pro-theme)

;;; monokai-pro-theme.el ends here
