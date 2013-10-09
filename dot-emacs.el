

(server-start)
(setq x-select-enable-clipboard t)

(add-to-list 'load-path (expand-file-name "~/elisp"))

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(require 'auto-install)
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

(global-set-key (kbd "C-=") 'align-to-equals)

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

;; show ruby syntax errors
(require 'flymake-ruby)
