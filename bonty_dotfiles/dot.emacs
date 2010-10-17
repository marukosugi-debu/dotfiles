;; -* Mode: Emacs-Lisp ; Coding: utf-8 -*-

;;; my .emacs settings

;; TODO
;; setting: .skk, wl, w3m, navi2ch(install to .emacs.d/elisp)
;; skype.el, redo, eshell, keychord.el, vi-mode, php-mode, mmm-mode, objc-mode

;; Common Lisp exstensions for Emacs
(require 'cl nil t)


;;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; eval-safe
;; usage: (eval-safe (some-suspicious-condition)) #nesting available
;; @see http://www.sodan.org/~knagano/emacs/dotemacs.html
(defmacro eval-safe (&rest body)
  `(condition-case err
       (progn ,@body)
     (error (message "[eval-safe] %s" err))))


;;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; system-type predicates
;; @see http://github.com/elim/dotemacs/blob/master/init.el
(defun x->bool (elt) (not (not elt)))
(setq darwin-p  (eq system-type 'darwin)
      ns-p      (featurep 'ns)
      carbon-p  (eq window-system 'mac)
      linux-p   (eq system-type 'gnu/linux)
      colinux-p (when linux-p
                  (let ((file "/proc/modules"))
                    (and
                     (file-readable-p file)
                     (x->bool
                      (with-temp-buffer
                        (insert-file-contents file)
                        (goto-char (point-min))
                        (re-search-forward "^cofuse\.+" nil t))))))
      cygwin-p  (eq system-type 'cygwin)
      nt-p      (eq system-type 'windows-nt)
      meadow-p  (featurep 'meadow)
      windows-p (or cygwin-p nt-p meadow-p))

;; autoload-if-found
;; @see http://www.sodan.org/~knagano/emacs/dotemacs.html
(defun autoload-if-found (function file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (and (locate-library file)
       (autoload function file docstring interactive type)))


;;; load-path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; System
(let ((default-directory "~/.emacs.d/elisp/"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))
;; (setq load-path (cons "/usr/local/share/emacs/site-lisp" load-path))


;;; exec-path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq exec-path (cons "~/.emacs.d/bin" exec-path))

;; add shell $PATH to exec-path
;; @see http://d.hatena.ne.jp/sugyan/20100704/1278212225
(loop for x in (reverse
                (split-string (substring (shell-command-to-string "echo $PATH") 0 -1) ":"))
      do (add-to-list 'exec-path x))

;; image-load-path
;; @see http://www.mail-archive.com/emacs-devel@gnu.org/msg10703.html
(when (eq window-system nil)
  (defvar image-load-path
    (list (file-name-as-directory (expand-file-name "images" data-directory))
          'data-directory 'load-path)
    "List of locations in which to search for image files.
    If an element is a string, it defines a directory to search.
    If an element is a variable symbol whose value is a string, that
    value defines a directory to search.
    If an element is a variable symbol whose value is a list, the
    value is used as a list of directories to search.")
  )



;;; Language Environment (default utf8)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-language-environment "Japanese")
(set-language-environment-coding-systems "Japanese")
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8)
(setq local-coding-system 'utf-8)
(cond
 (windows-p ; local variables for windows (shift-jis)
  (setq file-name-coding-system 'sjis-dos)
  (setq default-file-name-coding-system 'sjis-dos))
 (t         ; default setting
  (setq file-name-coding-system 'utf-8)
  (setq default-file-name-coding-system 'utf-8)))

;; Mapping wide character correctly (for Emacs22.x)
(if (< emacs-major-version 23)
    (utf-translate-cjk-set-unicode-range
     '((#x00a2 . #x00a3) (#x00a7 . #x00a8) (#x00ac . #x00ac)
       (#x00b0 . #x00b1) (#x00b4 . #x00b4) (#x00b6 . #x00b6)
       (#x00d7 . #x00d7) (#X00f7 . #x00f7) (#x0370 . #x03ff)
       (#x0400 . #x04FF) (#x2000 . #x206F) (#x2100 . #x214F)
       (#x2190 . #x21FF) (#x2200 . #x22FF) (#x2300 . #x23FF)
       (#x2500 . #x257F) (#x25A0 . #x25FF) (#x2600 . #x26FF)
       (#x2e80 . #xd7a3) (#xff00 . #xffef)
       )))


;;; Global Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hide startup message
(setq inhibit-startup-message t)

;; hide menubar and toolbar (eval-safe)
(when (eq window-system nil)
  (eval-safe (menu-bar-mode nil))
  (eval-safe (tool-bar-mode nil))
  )

;; disable visible bell
(setq visible-bell nil)

;; disable beep
(setq ring-bell-funciton '(lambda ()))

;; disable backup
(setq make-backup-files nil)

;; delete auto save file when quit
(setq delete-auto-save-files t)

;; show line num and column num
(line-number-mode t)
(column-number-mode t)

;; show file directory in mode line
(add-to-list 'global-mode-string '("" default-directory ""))

;; yes/no -> y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; delete region when input something
(delete-selection-mode t)

;; set space indent (not tab)
(setq-default indent-tabs-mode nil)

;; set deafault tab width
;; (setq-default tab-width 4)

;; auto relative indent
(setq indent-line-function 'indent-relative-maybe)

;; hide password
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; enable iswitchb mode
(iswitchb-mode t)

;; show newline code
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; Command-Key and Option-Key (for Mac)
(when darwin-p
  (when ns-p
    (setq ns-command-modifier (quote meta))
    (setq ns-alternate-modifier (quote super))))

;; open file when file dragged
(when ns-p
  (define-key global-map [ns-drag-file] 'ns-find-file))

;; display filepath on title bar
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

;; enable rectanble region
(setq cua-enable-cua-keys nil)
(cua-mode t)


;;; Highlight
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-font-lock-mode t)
(show-paren-mode t)
(setq show-paren-ring-bell-on-mismatch t)
(setq transient-mark-mode t)
(setq search-highlight t)
(setq isearch-lazy-highlight-initial-delay 0)
(setq query-replace-highlight t)

;; emphasis fullwidth space and tab
(defface my-face-b-1 '((t (:background "medium aquamarine"))) nil)
(defface my-face-b-2 '((t (:background "medium aquamarine"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(;("\t" 0 my-face-b-2 append)      ; tab
     ("　" 0 my-face-b-1 append)      ; fullwidth space
     ("[ \t]+$" 0 my-face-u-1 append) ; following space
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)


;;; Color settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://hie.s64.xrea.com/xyzzy/note/colors.html
;; xyzzy Part7 374
(when window-system
  (set-background-color "#313e32")
  (set-foreground-color "#ffffff")
  (set-cursor-color "#84ff84")
  (set-face-background 'region "#ffffff")
  (set-face-foreground 'region "#000000")
  (set-face-foreground 'font-lock-string-face  "#ffa2ff")
  (set-face-foreground 'font-lock-comment-face "#ecc142")
  (set-face-foreground 'font-lock-keyword-face "SandyBrown")
  (set-face-foreground 'font-lock-function-name-face "SkyBlue")
  (set-face-foreground 'font-lock-variable-name-face "LimeGreen")
  (set-face-foreground 'font-lock-type-face "SkyBlue1")
  (set-face-foreground 'font-lock-warning-face "Yellow")
  (set-face-foreground 'font-lock-builtin-face "Goldenrod")
  (set-face-foreground 'font-lock-constant-face "Tomato")
  (set-face-background 'highlight "Gray")
  (set-face-foreground 'highlight "Black")
  (set-face-foreground 'modeline "Black")
  (set-face-background 'modeline "White")
  (set-face-foreground 'mode-line-inactive "Gray")
  (set-face-background 'mode-line-inactive "Black")
  (set-face-foreground 'minibuffer-prompt "Green"))

;; Color Scheme
;; Black   #000000(0,0,0)       #000000(85,85,85)
;; Red     #ff6347(255,99,71)   #ff0000(255,0,0)
;; Green   #32cd32(50,205,50)   #00ff00(0,255,0)
;; Yellow  #ecc142(236,193,66)  #ffff00(255,255,0)
;; Blue    #87ceff(135,206,255) #00adff(0,173,255)
;; Magenta #ffa2ff(255,162,255) #ff00ff(255,0,255)
;; Cyan    #66cdaa(102,205,170) #00ffff(0,255,255)
;; White   #f5f5f5(245,245,245) #ffffff(255,255,255)

(when (eq window-system nil)
  ;; (set-background-color "Black")
  ;; (set-foreground-color "White")
  (set-cursor-color "Green")
  (set-face-background 'region "White")
  (set-face-foreground 'region "Black")
  (set-face-foreground 'font-lock-string-face  "Magenta")
  (set-face-foreground 'font-lock-comment-face "Yellow")
  (set-face-foreground 'font-lock-keyword-face "Yellow")
  (set-face-foreground 'font-lock-function-name-face "Blue")
  (set-face-foreground 'font-lock-variable-name-face "Green")
  (set-face-foreground 'font-lock-type-face "Blue")
  (set-face-foreground 'font-lock-warning-face "Yellow")
  (set-face-foreground 'font-lock-builtin-face "Yellow")
  (set-face-foreground 'font-lock-constant-face "Red")
  (set-face-background 'highlight "Gray")
  (set-face-foreground 'highlight "Black")
  (set-face-foreground 'modeline "Black")
  (set-face-background 'modeline "White")
  (set-face-foreground 'mode-line-inactive "Gray")
  (set-face-background 'mode-line-inactive "Black")
  (set-face-foreground 'minibuffer-prompt "Green"))

;; transition when use window-system
(when window-system
  (progn
    (set-frame-parameter nil 'alpha 80)))

(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray")
     )
    (((class color)
      (background light))
     (:background "ForestGreen"))
    (t
     ()))
  "*Face used by hl-line.")

(when window-system
  (setq hl-line-face 'hlline-face))
(when (eq window-system nil)
  (setq hl-line-face 'underline))
(global-hl-line-mode)


;;; Font
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; @see http//d.hatena.ne.jp/antipop/20100126/1264493522
(when window-system
  (when (>= emacs-major-version 23)
    (set-face-attribute 'default nil
                        :family "monaco"
                        :height 120)
    (set-fontset-font
     (frame-parameter nil 'font)
     'japanese-jisx0208
     '("Hiragino Maru Gothic Pro" . "iso10646-1"))
    (set-fontset-font
     (frame-parameter nil 'font)
     'japanese-jisx0212
     '("Hiragino Maru Gothic Pro" . "iso10646-1"))
    (set-fontset-font
     (frame-parameter nil 'font)
     'mule-unicode-0100-24ff
     '("monaco" . "iso10646-1"))

    ;; for halfwidth kana
    (set-fontset-font
     (frame-parameter nil 'font)
     'katakana-jisx0201
     '("Hiragino Maru Gothic Pro" . "iso10646-1"))

    (setq face-font-rescale-alist
          '(("^-apple-hiragino.*" . 1.2)
            (".*osaka-bold.*" . 1.2)
            (".*osaka-medium.*" . 1.2)
            (".courier-bold-.*-mac-roman" . 1.0)
            (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
            (".*monaco-bold-.*-mac-roman" . 0.9)
            ("-cdac$" . 1.3)))))


;;; Keybind
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key isearch-mode-map "\C-k" 'isearch-edit-string)
(define-key global-map "\C-cc" 'comment-region)
(define-key global-map "\C-cu" 'uncomment-region)
(define-key global-map "\C-cr" 'replace-string)
(define-key global-map "\C-ci" 'indent-region)
(define-key global-map "\C-\\" 'undo)
(define-key global-map "\C-h" 'delete-backward-char)
(define-key global-map "\C-x\C-b" 'electric-buffer-list)
(define-key global-map "\C-cg" 'goto-line)
(define-key function-key-map [delete] "\C-d")
(define-key global-map "\C-m" 'newline-and-indent)
(define-key global-map "\C-a" 'beginning-of-indented-line)

;; move beginning of indented line
;; @see http://d.hatena.ne.jp/gifnksm/20100131/1264956220
(defun beginning-of-indented-line (current-point)
  "move beginning of indented line"
  (interactive "d")
  (when (>= emacs-major-version 23)
      (let ((vhead-pos (save-excursion (progn (beginning-of-visual-line) (point))))
            (head-pos (save-excursion (progn (beginning-of-line) (point)))))
        (cond
         ;; when current pos is first visual line
         ((eq vhead-pos head-pos)
          (if (string-match
               "^[ \t]+$"
               (buffer-substring-no-properties vhead-pos current-point))
              (beginning-of-visual-line)
            (back-to-indentation)))
         ;; when current pos is head of second visual line
         ((eq vhead-pos current-point)
          (backward-char)
          (beginning-of-visual-indented-line (point)))
         ;; when current pos is second visual line
     (t (beginning-of-visual-line)))))

  (when (< emacs-major-version 23)
    (if (string-match
         "^[ \t]+$"
         (save-excursion
           (buffer-substring-no-properties
            (progn (beginning-of-line) (point))
            current-point)))
        (beginning-of-line)
      (back-to-indentation)))
  )


;;; auto-install.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))


;;; auto-async-byte-complie.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'auto-async-byte-complie nil t)
  (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-complie-mode))

;;; anything.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "TEMPDIR" "~/.emacs.d/tmp")

;; load anything.el settings
(when (require 'anything nil t)
  (when (executable-find "w3m")
    (require 'anything-config nil t))
  (require 'anything-startup nil t)

  (when (require 'anything-complete nil t)
    (anything-lisp-complete-symbol-set-timer 150)

    (setq anything-enable-shortcuts 'alphabet
          anything-sources
          (list anything-c-source-buffers
                anything-c-source-file-name-history
                anything-c-source-info-pages
                anything-c-source-man-pages
                anything-c-source-locate
                anything-c-source-emacs-commands
                anything-c-source-complete-shell-history
                ))

    (define-key ctl-x-map "\C-y" 'anything-show-kill-ring)

    (mapc '(lambda (key)
             (global-set-key key 'anything))
          (list
           [(control ?:)]
           [(control \;)]
           [(control x)(b)]
           [(control x)(control :)]
           [(control x)(control \;)])))

  ;; anything-dabbrev-expand
  (when (require 'anything-dabbrev-expand nil t)
    (global-set-key "\C-o" 'anything-dabbrev-expand)
    (define-key anything-dabbrev-map "\C-o" 'anything-dabbrev-find-all-buffers))
  )


;;; load plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; align
(require 'align nil t)

;; linum
(when (require 'linum nil t)
  ;; (global-linum-mode t)

  ;; define linum enable mode
  ;; @see http://macemacsjp.sourceforge.jp/index.php?CocoaEmacs#aae602ba
  (dolist (hook (list
                 'cperl-mode-hook
                 'c-mode-hook
                 'c++-mode-hook
                 'emacs-lisp-mode-hook
                 'objc-mode-hook
                 'js2-mode-hook
                 'ruby-mode-hook
                 'lisp-interaction-mode-hook
                 'lisp-mode-hook
                 'sh-mode-hook
                 'text-mode-hook
                 ))
    (add-hook hook (lambda () (linum-mode t))))
  (setq linum-format "%5d ")
  )

;; recentf-ext.el
(require 'recentf-ext nil t)

;; bracket.el
(require 'brackets nil t)

;; auto-save-buffers.el
;; @see http://0xcc.net/misc/auto-save/
(when (require 'auto-save-buffers nil t)
  (run-with-idle-timer 0.5 t 'auto-save-buffers))

;; autosave and resume all buffers (revive.el)
;; @see http://www.hasta-pronto.org/archives/2008/01/30-0235.php
(when (autoload-if-found 'save-current-configuration "revive" "Save status" t)
  (when (autoload-if-found 'resume "revive" "Resume Emacs" t)
    (when (autoload-if-found 'wipe "revive" "Wipe emacs" t)
      (define-key ctl-x-map "R" 'resume)                        ; C-x R resume
      (define-key ctl-x-map "K" 'wipe)                          ; C-x K kill
      (add-hook 'kill-emacs-hook 'save-current-configuration)   ; autosave when quit
      )))

;; killring (kill-summary.el)
;; Usage:
;; p/n(or j/k)      : select kill-ring
;; C-p/C-n          : move kill-ring candidate
;; SPC              : select current line
;; C-v(scroll-down) : scroll
;; d                : delete kill-ring
(when (autoload-if-found 'kill-summary "kill-summary" nil t)
  (global-set-key "\M-y" 'kill-summary))

;; mic-paren.el
;; highlight characteres between parenthesis
(when (require 'mic-paren nil t)
  (paren-activate)
  (setq paren-match-face 'bold)
  (setq paren-sexp-mode t))


;;; Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; display escape
(autoload-if-found 'ansi-color-for-comint-mode-on "ansi-color"
                   "Set `ansi-color-for-comint-mode' to t." t)

;; shell-mode keybind
(add-hook 'shell-mode-hook
          (lambda ()
            ; M-p/n complete history
            (define-key comint-mode-map "\M-p" 'comint-previous-matching-input-from-input)
            (define-key comint-mode-map "\M-n" 'comint-next-matching-input-from-input)
            ; M-r search history
            (define-key comint-mode-map "\M-r" 'anything-complete-shell-history)
            (ansi-color-for-comint-mode-on)))

;; save shell history (shell-history.el)
(require 'shell-history nil t)


;;; auto-complete.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'auto-complete nil t)
  (global-auto-complete-mode t)

  ;; keybind in complete window
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-p") 'ac-previous)
  (define-key ac-completing-map (kbd "M-/") 'ac-stop)

  ;; stop auto launch
  ;; (setq ac-auto-start nil)

  ;; set trigger key
  (ac-set-trigger-key "TAB")

  ;; default max candidate
  (setq ac-candidate-max 20)
  )


;;; ac-company.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'ac-company nil t)
  ;; enable company-xcode in ac-company
  (ac-company-define-source ac-source-company-xcode company-xcode)

  ;; set objc-mode
  (setq ac-modes (append ac-modes '(objc-mode)))
)

;;; flymake.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'flymake nil t)

  ;; set warning face
  (set-face-background 'flymake-errline "Red")
  (set-face-foreground 'flymake-errline "Black")
  (set-face-background 'flymake-warnline "Yellow")
  (set-face-foreground 'flymake-warnline "Black")

  ;; display error in minibuffer
  (defun flymake-display-err-minibuf
    "Displays the error/warning for the current line in the minibuffer"
    (interactive)
    (let* ((line-no (flymake-current-line-no))
           (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
           (count (length line-err-info-list)))
      (while (> count 0)
        (let* ((file (flymake-ler-file (nth (1- count) line-erro)))
               (full-file (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line (flymake-ler-line (nth (1- count) line-err-info-list)))))
        (setq count (1- count)))))

  ;; C-c e => jump flymake error
  (defun next-flymake-error ()
    (interactive)
    (flymake-goto-next-error)
    (let ((err (get-char-property (point) 'help-echo)))
      (when err
        (message err))))
  (global-set-key "\C-ce" 'next-flymake-error)

  ;; setting for perl
  ;; @see http://unknownplace.org/memo/2007/12/21#e001

  ;; set-perl5lib
  ;; @see http://svn.coderepos.org/share/lang/elisp/set-perl5lib/set-perl5lib.el
  (require 'set-perl5lib nil t)

  (defvar flymake-perl-err-line-patterns
    '(("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)))

  (defconst flymake-allowed-perl-file-name-masks
    '(("\\.pl$" flymake-perl-init)
      ("\\.pm$" flymake-perl-init)
      ("\\.t$" flymake-perl-init)))

  (defun flymake-perl-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "perl" (list "-wc" local-file))))

  (defun flymake-perl-load ()
    (interactive)
    (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
      (setq flymake-check-was-interrupted t))
    (ad-activate 'flymake-post-syntax-check)
    (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-perl-file-name-masks))
    (setq flymake-err-line-patterns flymake-perl-err-line-patterns)
    (set-perl5lib)
    (flymake-mode t))

  (add-hook 'cperl-mode-hook '(lambda () (flymake-perl-load)))

  ;; setting for C++
  (defun flymake-cc-init ()
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
           (local-file  (file-relative-name
                         temp-file
                         (file-name-directory buffer-file-name))))
      (list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))

  (push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)

  (add-hook 'c++-mode-hook '(lambda () (flymake-mode t)))

  ;; setting for Objective-C
  ;; @see http://sakito.jp/emacs/emacsobjectivec.html
  (defvar xcode:gccver "4.0")
  (defvar xcode:sdkver "3.1.2")
  (defvar xcode:sdkpath "/Developer/Platforms/iPhoneSimulator.platform/Developer")
  (defvar xcode:sdk (concat xcode:sdkpath "/SDKs/iPhoneSimulator" xcode:sdkver ".sdk"))
  (defvar flymake-objc-compiler (concat xcode:sdkpath "/usr/bin/gcc-" xcode:gccver))
  (defvar flymake-objc-compile-default-options (list "-Wall" "-Wextra" "-fsyntax-only" "-ObjC" "-std=c99" "-isysroot" xcode:sdk))
  (defvar flymake-last-position nil)
  (defvar flymake-objc-compile-options '("-I."))
  (defun flymake-objc-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list flymake-objc-compiler (append flymake-objc-compile-default-options flymake-objc-compile-options (list local-file)))))

  (add-hook 'objc-mode-hook
            (lambda ()
              (push '("\\.m$" flymake-objc-init) flymake-allowed-file-name-masks)
              (push '("\\.h$" flymake-objc-init) flymake-allowed-file-name-masks)

              (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
                  (flymake-mode t))
              ))

  )


;;; zencoding-mode.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'zencoding-mode nil t)
  (add-hook 'sgml-mode-hook 'zencoding-mode)
  (add-hook 'html-mode-hook 'zencoding-mode)
  (add-hook 'text-mode-hook 'zencoding-mode)
  (define-key zencoding-mode-keymap "\C-z" 'zencoding-expand-line))


;;; yasnipet.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'yasnippet nil t)
  (yas/initialize)
  (yas/load-directory "~/.emacs.d/elisp/snippets")
  (setq yas/prompt-functions '(yas/dropdown-prompt)))


;;; smartchr.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'smartchr nil t)
  ;; (global-set-key (kbd "=") (smartchr '("=" " = " " == ")))
  (global-set-key (kbd ">") (smartchr '(">" " => " " => \'`!!'\'" " => \"`!!'\"")))
  )


;;; SKK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'skk-autoloads nil t)
  (global-set-key "\C-x\C-j" 'skk-mode)
  (global-set-key "\C-xj" nil)
  (global-set-key "\C-xt" nil)

  ;; (global-set-key "\C-xj" 'skk-auto-fill-mode)
  ;; (global-set-key "\C-xt" 'skk-tutorial)

  ;; Specify dictionary location
  (when darwin-p
    (setq skk-large-jisyo "/Users/kouno/Library/AquaSKK/SKK-JISYO.L"))
  (when linux-p
    (setq skk-large-jisyo "/usr/share/skk/SKK-JISYO.L"))

  ;; Specify tutorial location
  (setq skk-tut-file "~/.emacs.d/etc/skk/SKK.tut")

  ;; show vertical candidate
  (setq skk-show-inline 'vertical)
  
  ;; display multi candidate ing dinamic abbrev
  (setq skk-dcomp-multiple-activate t
              skk-dcomp-multiple-rows 20)

  ;; abbrev with ascii prefix
  (setq skk-comp-use-prefix t)

  ;; cycle abbrev
  (setq skk-comp-circulate t)
  
  ;; (add-hook 'isearch-mode-hook
  ;;           (function (lambda ()
  ;;                       (and (boundp 'skk-mode) skk-mode
  ;;                         (skk-isearch-mode-setup)))
  ;;           ))

  ;; (add-hook 'isearch-mode-end-hook
  ;;           (function (lambda ()
  ;;                       (and (boundp 'skk-mode) skk-mode
  ;;                         (skk-isearch-mode-cleanup))
  ;;                       (and (boundp 'skk-mode-invoked) skk-mode-invoked
  ;;                         (skk-set-cursor-properly)))
  ;;           ))
  )


;;; migemo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (when (require 'migemo nil t)
;;   (setq migemo-command "migemo")
;;   (setq migemo-options '("-t" "emacs" "-U"))
;;   (setq migemo-dictionary (expand-file-name "~/.emacs.d/share/migemo/migemo-dict"))
;;   (setq migemo-user-dictionary nil)
;;   (setq migemo-regex-dictionary nil)
;;   (setenv "RUBYLIB" "~/.emacs.d/lib/ruby/site_ruby/")
;;   )
(setq migemo-command "migemo")
(setq migemo-options '("-t" "emacs"))
(setq migemo-dictionary (expand-file-name "~/.emacs.d/share/migemo/migemo-dict"))
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setenv "RUBYLIB" "~/.emacs.d/lib/ruby/site_ruby/")
(require 'migemo nil t)


;;; w3m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; how to install emacs-w3m
;; @see http://homepage.mac.com/matsuan_tamachan/software/EmacsW3m.html
;; ./configure --with-emacs=/usr/local/bin/emacs --with-lispdir=~/.emacs.d/elisp/w3m --with-icondir=~/.emacs.d/elisp/w3m/icon
;; make
;; make install
;; make install-icons
(when (and
       (executable-find "w3m")
       (require 'w3m-load nil t))
  (autoload-if-found 'w3m "w3m" "Interface for w3m on Emacs." t)
  )


;;; navi2ch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (autoload-if-found 'navi2ch "navi2ch" "Navigator for 2ch for Emacs" t)
  (setq navi2ch-article-auto-range nil) 
  ;; (setq navi2ch-mona-enable t)
  ;; (setq navi2ch-mona-use-ipa-mona t)
  ;; (setq navi2ch-mona-ipa-mona-font-family-name "IPAMonaPGothic")
  )


;;; Wanderlust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload-if-found 'wl "wl" "Wanderlust" t)
(autoload-if-found 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload-if-found 'wl-draft "wl-draft" "Write draft with Wanderlust." t)


;;; Perl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cperl-mode
(when (require 'cperl-mode nil t)
  ;; apply cperl-mode
  (add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\|t\\|cgi\\)\\'" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

  ;; don't use perl-mode
  (defalias 'perl-mode 'cperl-mode)

  ;; coding style
  (setq cperl-close-paren-offset -4)
  (setq cperl-continued-statement-offset 4)
  (setq cperl-indent-level 4)
  (setq cperl-indent-parens-as-block t)
  (setq cperl-label-offset -4)
  (setq cperl-highlight-variables-indiscriminately t)
  (setq cperl-fontlock t)

  (add-hook 'cperl-mode-hook
            '(lambda()
               (progn
                 ;; auto insert brackets by brackets.el
                 (define-key cperl-mode-map "{" 'insert-braces)
                 (define-key cperl-mode-map "(" 'insert-parens)
                 (define-key cperl-mode-map "\"" 'insert-double-quotation)
                 (define-key cperl-mode-map "\`" 'insert-back-quotation)
                 (define-key cperl-mode-map "'" 'insert-single-quotation)
                 (define-key cperl-mode-map "[" 'insert-brackets)

                 ;; smartchr
                 (eval-safe
                  (define-key cperl-mode-map (kbd "F") (smartchr '("F" "$" "$_" "$_->" "@$")))
                  (define-key cperl-mode-map (kbd "M") (smartchr '("M" "my $`!!' = " "my @`!!' = " "my %`!!' = " "my ($self, $`!!') = @_;")))
                  )

                 ;; perl-completion
                 (when (require 'perl-completion nil t)
                   (add-to-list 'ac-sources 'ac-source-perl-completion)
                   (perl-completion-mode t))
                 )))
  )


;;; JavaScript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; js2-mode modified by mooz
;; @see http://d.hatena.ne.jp/mooz/20100315/p1
(when (require 'js2-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-hook 'js2-mode-hook
            '(lambda()
               (progn
                 (define-key js2-mode-map "{" 'insert-braces)
                 (define-key js2-mode-map "(" 'insert-parens)
                 (define-key js2-mode-map "\"" 'insert-double-quotation)
                 (define-key js2-mode-map "\`" 'insert-back-quotation)
                 (define-key js2-mode-map "'" 'insert-single-quotation)
                 (define-key js2-mode-map "[" 'insert-brackets)
                 )))
  )

;; ejacs
;; @see http://code/google.com/p/ejacs/
;; @see http://d.hatena.ne.jp/Rion778/20100925/1285419192
;; usage: C-c C-j => launch js-console
;;        C-c C-r => evaluate region and send interpreter
(when (autoload-if-found 'js-console "js-console" nil t)
  ;; launch js2-mode and js-console
  (defun js-console-other-window ()
    "Run JavaScript on other window"
    (interactive)
    (split-window)
    (js-console)
    (other-window t)
    )

  ;; evaluate region
  (defun js-execute-region ()
    "Execute region"
    (interactive)
    (let ((buf-name (buffer-name (current-buffer))))
      (copy-region-as-kill (point-min)(point-max))
      (let ((js-code (car kill-ring)))
        (switch-to-buffer-other-window "*js*")
        (js-console-exec-input (car kill-ring))
        (js-console-display-output (switch-to-buffer-other-window buf-name))
        )))

  (add-hook 'js2-mode-hook '(lambda ()
                              (local-set-key "\C-c\C-r" 'js-execute-region)
                              ))

  (define-key global-map "\C-c\C-j" 'js-console-other-window)
  )


;;; C
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-common-hook
          '(lambda()
             (progn
               ;; style
               (c-set-style "cc-mode")

               ;; auto insert brackets by brackets.el
               (define-key c-mode-map "{" 'insert-braces)
               (define-key c-mode-map "(" 'insert-parens)
               (define-key c-mode-map "\"" 'insert-double-quotation)
               (define-key c-mode-map "\`" 'insert-back-quotation)
               (define-key c-mode-map "'" 'insert-single-quotation)
               (define-key c-mode-map "[" 'insert-brackets)
               )))


;;; C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c++-mode-hook
          '(lambda()
             (progn
               ;; auto insert brackets by brackets.el
               (define-key c++-mode-map "{" 'insert-braces)
               (define-key c++-mode-map "(" 'insert-parens)
               (define-key c++-mode-map "\"" 'insert-double-quotation)
               (define-key c++-mode-map "\`" 'insert-back-quotation)
               (define-key c++-mode-map "'" 'insert-single-quotation)
               (define-key c++-mode-map "[" 'insert-brackets)
               )))


;;; Objective-C
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; @see http://sakito.jp/emacs/emacsobjectivec.html
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))

(ffap-bindings)
(setq ffap-newfile-prompt t) ; confirm when new file
(setq ffap-kpathsea-depth 5) ; expand path depth ffap-kpathsea-expand-path

(setq ff-other-file-alist
      '(("\\.mm?$" (".h"))
        ("\\.cc$"  (".hh" ".h"))
        ("\\.hh$"  (".cc" ".C"))

        ("\\.c$"   (".h"))
        ("\\.h$"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m" ".mm"))

        ("\\.C$"   (".H"  ".hh" ".h"))
        ("\\.H$"   (".C"  ".CC"))

        ("\\.CC$"  (".HH" ".H"  ".hh" ".h"))
        ("\\.HH$"  (".CC"))

        ("\\.cxx$" (".hh" ".h"))
        ("\\.cpp$" (".hpp" ".hh" ".h"))

        ("\\.hpp$" (".cpp" ".c"))))

(defun xcode:buildandrun ()
  (interactive)
  (do-applescript
   (format
    (concat
     "tell application \"Xcode\" to activate \r"
     "tell application \"System Events\" \r"
     "     tell process \"Xcode\" \r"
     "          key code 36 using {command down} \r"
     "    end tell \r"
     "end tell \r"
     ))))

(add-hook 'objc-mode-hook
          (lambda ()
            (progn
              (define-key c-mode-base-map (kbd "C-c o") 'ff-find-other-file)
              (define-key objc-mode-map (kbd "\t") 'ac-complete)
              (define-key objc-mode-map (kbd "C-c C-r") 'xcode:buildandrun)

              ;; enable complete using XCode
              (push 'ac-source-company-xcode ac-sources)
              ;; complete C++ keyword only comment out using Objective-C++
              ;; (push 'ac-source-c++-keywords ac-sources)

              ;; auto insert brackets by brackets.el
              (define-key objc-mode-map "{" 'insert-braces)
              (define-key objc-mode-map "(" 'insert-parens)
              (define-key objc-mode-map "\"" 'insert-double-quotation)
              (define-key objc-mode-map "\`" 'insert-back-quotation)
              (define-key objc-mode-map "'" 'insert-single-quotation)
              (define-key objc-mode-map "[" 'insert-brackets)

              ;; smartchr
              (eval-safe
               (local-set-key (kbd "@") (smartchr '("@\"`!!'\"" "@"))))
              )))


;;; PHP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'ruby-mode-hook
          '(lambda()
             (progn
               ;; auto insert brackets by brackets.el
               (define-key ruby-mode-map "{" 'insert-braces)
               (define-key ruby-mode-map "(" 'insert-parens)
               (define-key ruby-mode-map "\"" 'insert-double-quotation)
               (define-key ruby-mode-map "\`" 'insert-back-quotation)
               (define-key ruby-mode-map "'" 'insert-single-quotation)
               (define-key ruby-mode-map "[" 'insert-brackets)
               )))


;;; ELisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'emacs-lisp-mode-hook
          '(lambda()
             (progn
               ;; auto insert brackets by brackets.el
               (define-key emacs-lisp-mode-map "{" 'insert-braces)
               (define-key emacs-lisp-mode-map "(" 'insert-parens)
               (define-key emacs-lisp-mode-map "\"" 'insert-double-quotation)
               (define-key emacs-lisp-mode-map "\`" 'insert-back-quotation)
               (define-key emacs-lisp-mode-map "'" 'insert-single-quotation)
               (define-key emacs-lisp-mode-map "[" 'insert-brackets)
               )))


;;; YaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (autoload-if-found 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
  (setq load-path (cons (expand-file-name "/usr/share/emacs/site-lisp/yatex") load-path))
  (setq auto-mode-alist
        (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))

  (setq load-path (cons (expand-file-name "~/src/emacs/yatex") load-path))
  (setq tex-command "platex"
        dvi2-command "pxdvi"
        YaTeX-use-AMS-LaTeX t
        YaTeX-use-LaTeX2e t)

  (setq tex-command "platex"
        dviprint-command-format "pdvips -f %f %t %s | lpr"
        dviprint-from-format "-p %b"
        dviprint-to-format "-l %e"
        dvi2-command "pxdvi -geo +0+0 -s 4"
        section-name "documentclass")
  )


;; EOF ;;