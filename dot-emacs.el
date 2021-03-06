
(load "server")
(unless (server-running-p (server-start)))


(require 'package)

;; Fetch and install packaages
(setq package-list '(exec-path-from-shell expand-region magit ag scss-mode feature-mode string-inflection geben rainbow-identifiers dired+ js2-refactor ac-js2 json-reformat auto-complete php-mode php-refactor-mode iedit midnight powershell jinja2-mode wgrep-ag browse-kill-ring csharp-mode csv-mode flycheck json-mode))

(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'load-path (expand-file-name "~/elisp"))



(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


(require 'auto-install)

;; Settings
(add-to-list 'load-path (expand-file-name "~/elisp/settings"))

(require 'init-midnight)


(setq x-select-enable-clipboard t)

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(add-hook 'php-mode-hook 'php-refactor-mode)



(setq c-default-style "k&r"
          c-basic-offset 2)

(add-hook 'php-mode-hook
          (function (lambda ()
                      (setq php-indent-level 4
                            php-continued-statement-offset 4
                            php-continued-brace-offset 0
                            php-brace-offset 4
                            php-brace-imaginary-offset 0
                            php-label-offset -4
                            tab-width 4
                            c-basic-offset 4
                            indent-tabs-mode nil))))


(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'case-label '+)))

;; make C-M-forward jump between identifiers in python like it does for ruby and other languages.
(add-hook 'python-mode-hook
        (lambda () (setq forward-sexp-function nil)))

;; [del] key deletes region
(delete-selection-mode 1)

;; Don't do backups
(setq make-backup-files nil)

;; Use cperl-mode instead of the default perl-mode
(defalias 'perl-mode 'cperl-mode)

;; just spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq-default tab-width 2)
(setq js-indent-level 4)
(setq css-indent-offset 2)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-mode t t)
 '(c-offsets-alist (quote ((arglist-close . c-lineup-close-paren))))
 '(cperl-brace-offset -2)
 '(cperl-close-paren-offset -2)
 '(cperl-continued-statement-offset 2)
 '(cperl-electric-keywords t)
 '(cperl-electric-parens t)
 '(cperl-indent-level 2)
 '(cperl-indent-parens-as-block t)
 '(cperl-tab-always-indent nil)
 '(fill-column 100)
 '(flyspell-auto-correct-binding [67108990])
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p nil)
 '(magit-diff-arguments nil)
 '(org-support-shift-select (quote always))
 '(scss-compile-at-save nil)
 '(undo-limit 800000)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2))

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 4
                                  tab-width 4
                                  indent-tabs-mode nil)))

(add-hook 'html-mode-hook
        (lambda ()
          ;; Default indentation is usually 2 spaces, changing to 4.
          (set (make-local-variable 'sgml-basic-offset) 4)))

;; Use 2 space indents via cperl mode



;; Use classic style indentation
;;(global-set-key (kbd "<tab>") '(indent-rigidly 2))
;;(global-set-key (kbd "S-<tab>") '(indent-rigidly -2))

(ac-config-default)

;; make autocompletion case sensitive
(setq-default ac-ignore-case nil)


(global-set-key (kbd "C-%") 'insert-asp-tag)

;;rename active file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; sticky buffers
(defun toggle-window-dedicated ()

"Toggle whether the current active window is dedicated or not"

(interactive)

(message

 (if (let (window (get-buffer-window (current-buffer)))

       (set-window-dedicated-p window

        (not (window-dedicated-p window))))

    "Window '%s' is dedicated"

    "Window '%s' is normal")

 (current-buffer)))

(global-set-key [pause] 'toggle-window-dedicated)


;; Make ruby indent function calls properly
(setq ruby-deep-indent-paren nil)

(defun align-to-equals (begin end)
  "Align region to equal signs"
   (interactive "r")
   (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))

(defun align-to-colon (begin end)
  "Align region to the furthest out colon"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\):" 1 1 ))

;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; in ruby, indent multiline method calls like this:
;; foo(
;;  :a => :b,
;;  :c => :d
;; )
;; instead of
;; foo(
;;  :a => :b,
;;  :c => :d
;;  )
;; From: https://gist.github.com/dgutov/1274520
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

(defadvice php-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))


;; add sublime style file finding (Win/Cmd-f)
(require 'projectile)
(define-key projectile-mode-map [?\s-f] 'projectile-find-file)
(define-key projectile-mode-map [f6] 'projectile-find-file)
(projectile-global-mode)

;; remove trailing whitespace on save

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; only untabify if we're in a proper mode
(defun jlf-before-save-hook () (unless indent-tabs-mode (untabify (point-min) (point-max))))
(add-hook 'before-save-hook 'jlf-before-save-hook)

;; make font size changes global
(defadvice text-scale-increase (around all-buffers (arg) activate)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      ad-do-it)))

;; turn on line numbers
(global-linum-mode)
;; Make sure line numbers don't get corrupted
(setq linum-format "  %d ")

;; show ruby syntax errors
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; Enable Multe selection
(require 'iedit)

;; Select matching blocks in ruby
(require 'ruby-block)
(ruby-block-mode t)

;; Convert hashes between json and ruby

(defun test-outline ()
  (interactive)
  (occur " it\\| describe\\| context"))

;; Prevent the cursor from blinking
(blink-cursor-mode 0)
;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
;; Don't let Emacs hurt your ears, just change the mode line a bit
 (defun my-terminal-visible-bell ()
   "A friendlier visual bell effect."
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line))

 (setq visible-bell nil
       ring-bell-function 'my-terminal-visible-bell)

(if (display-graphic-p)
    (progn
     (scroll-bar-mode 0)
     (tool-bar-mode 0)))
(menu-bar-mode 0)
;; Turn out the lights
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "grey"))))
 '(fringe ((t (:background "black"))))
 '(js2-external-variable ((t (:underline t)))))
(put 'narrow-to-region 'disabled nil)

;; Always use the default for find-tag
(defun sm-find-tag ()
  (interactive)
  (find-tag (funcall (or find-tag-default-function
                         (get major-mode 'find-tag-default-function)
                         'find-tag-default))))
(global-set-key (kbd "M-.") 'sm-find-tag)

;; Update tags file for the current project
(defun update-ctags ()
  (interactive)
  (let ((project-root (expand-file-name(projectile-project-root))))
        (with-temp-buffer (shell-command (concat "cd " project-root " && ctags -e -R * -f " project-root "tags") t))))

;; for some reason the above doesn't work on mac, so just bind the keys directly
(global-set-key (kbd "M-<left>")  'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>")    'windmove-up)
(global-set-key (kbd "M-<down>")  'windmove-down)

;; make autoindent happen after newline
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Make mac paths carry over to emacs shells
;; (when (memq window-system '(mac ns))
;;  (exec-path-from-shell-initialize))

;; setup ag
(when (executable-find "ag")
  (require 'ag)
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "M-?") 'ag-project))

;; Undo for window rearranging
(winner-mode 1)
(put 'upcase-region 'disabled nil)

;; Magit shortcut
(global-set-key (kbd "C-M-g") 'magit-status)

;; Always use org-indent-mode
(add-hook 'org-mode-hook
          (function
           (lambda ()
             (org-indent-mode))))


(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
(setq rainbow-identifiers-choose-face-function
      'rainbow-identifiers-cie-l*a*b*-choose-face)
(setq rainbow-identifiers-cie-l*a*b*-lightness 85)
(setq rainbow-identifiers-cie-l*a*b*-saturation 35)
(setq rainbow-identifiers-faces-to-override
      '(font-lock-variable-name-face
        js2-function-call
        js2-external-variable
        js2-function-param
        js2-object-property
        default
        font-lock-function-name-face
        font-lock-type-face))

;; Add ksl ticket prefix
(defun ksl ()
  (interactive)
  (insert "kingstreetlabs/KingStreetLabs#"))

;; Add PHP filter_var to the current expression
(defun insert-filter-var ()
  "Inserts php code for filter_var.  Surrounds selected text if mark is set"
  (interactive)
  (let ((before (if (use-region-p) (region-beginning) (point)))
    (after (if (use-region-p) (region-end) (point))))
  (goto-char after)
  (insert ", FILTER_SANITIZE_SPECIAL_CHARS)")
  (goto-char before)
  (insert "filter_var(")))

;; When using dired, always open new folders in the same buffer
(diredp-toggle-find-file-reuse-dir 1)

(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(js2r-add-keybindings-with-prefix "C-c r")


;; add a function to kill all buffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))
;; save desktop on quit
(desktop-save-mode 1)


(defun insert-date ()
    "Inserts standard date time string."
    (interactive)
    (insert (format-time-string "%c")))


;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
;; (when (memq window-system '(mac ns))
;; (exec-path-from-shell-initialize))
