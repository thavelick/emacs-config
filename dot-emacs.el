(unless (server-running-p (server-start)))
(require 'package)

; Fetch and install packaages
(setq package-list '(exec-path-from-shell expand-region magit))

(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'load-path (expand-file-name "~/elisp"))
(add-to-list 'load-path (expand-file-name "~/elisp/iedit"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


(require 'auto-install)

(setq x-select-enable-clipboard t)

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(require 'php-mode)

(setq c-default-style "k&r"
          c-basic-offset 2)

(add-hook 'php-mode-hook
          (function (lambda ()
                      (setq php-indent-level 2
                            php-continued-statement-offset 2
                            php-continued-brace-offset 0
                            php-brace-offset 2
                            php-brace-imaginary-offset 0
                            php-label-offset -2))))


;; make speedbar show ruby files
(eval-after-load "speedbar" '(speedbar-add-supported-extension ".rb"))
(eval-after-load "speedbar" '(speedbar-add-supported-extension
                              ".feature"))
(eval-after-load "speedbar" '(speedbar-add-supported-extension
                              ".erb"))
(eval-after-load "speedbar" '(speedbar-add-supported-extension
                              ".ts"))
(eval-after-load "speedbar" '(speedbar-add-supported-extension
                              ".php"))
(eval-after-load "speedbar" '(speedbar-add-supported-extension
                              ".htm"))
(eval-after-load "speedbar" '(speedbar-add-supported-extension
                              ".htm"))
(eval-after-load "speedbar" '(speedbar-add-supported-extension
                              ".css"))
(eval-after-load "speedbar" '(speedbar-add-supported-extension
                              ".lua"))

;; [del] key deletes region
(delete-selection-mode 1)

;; Don't do backups
(setq make-backup-files nil)

;; Use cperl-mode instead of the default perl-mode
(defalias 'perl-mode 'cperl-mode)

;; just spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq js-indent-level 2)

;; Use 2 space indents via cperl mode

(custom-set-variables
  '(cperl-close-paren-offset -2)
  '(cperl-continued-statement-offset 2)
  '(cperl-indent-level 2)
  '(cperl-indent-parens-as-block t)
  '(cperl-tab-always-indent nil)
  '(cperl-electric-parens t)
  '(cperl-electric-keywords t)
  '(abbrev-mode t)
  '(cperl-brace-offset -2)
)

;; Use classic style indentation
;;(global-set-key (kbd "<tab>") '(indent-rigidly 2))
;;(global-set-key (kbd "S-<tab>") '(indent-rigidly -2))

;; Add auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
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

; sticky buffers
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


;; add sublime style file finding (Win/Cmd-f)
(require 'projectile)
(define-key projectile-mode-map [?\s-f] 'projectile-find-file)
(define-key projectile-mode-map [f6] 'projectile-find-file)
(projectile-global-mode)

;; remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

(defun ruby-hash-to-json (&optional b e )
  (interactive "r")
  (shell-command-on-region b e (concat "/Users/tristan/.rvm/bin/ruby-1.9.3-p547 " (expand-file-name "~/elisp/") "hash_convert.rb" " --to-json") t t "*Error*" t)
  (indent-region b e))

(defun json-to-ruby-hash (&optional b e )
  (interactive "r")
  (shell-command-on-region b e (concat "/Users/tristan/.rvm/bin/ruby-1.9.3-p547 " (expand-file-name "~/elisp/") "hash_convert.rb" " --from-json") t t "*Error*" t) ;
  (indent-region b e))

(defun test-outline ()
  (interactive)
  (occur " it\\| describe\\| context"))

;; Prevent the cursor from blinking
(blink-cursor-mode 0)
;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
;; Don't let Emacs hurt your ears
(setq visible-bell t)
(if (display-graphic-p)
    (progn
     (scroll-bar-mode 0)
     (tool-bar-mode 0)))
(menu-bar-mode 0)
;; Turn out the lights
(custom-set-faces
  '(default ((t (:background "black" :foreground "grey"))))
  '(fringe ((t (:background "black")))))
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

; Make mac paths carry over to emacs shells
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
