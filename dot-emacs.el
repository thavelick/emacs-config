(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;(server-start)
(setq x-select-enable-clipboard t)

(add-to-list 'load-path (expand-file-name "~/elisp"))
(add-to-list 'load-path (expand-file-name "~/elisp/iedit"))

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
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

;; move between windows with M-<arrow>
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(windmove-default-keybindings 'meta)

;; Use the mouse in command line mode
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

;; handle tmux's xterm-keys
;; put the following line in your ~/.tmux.conf:
;;   setw -g xterm-keys on
(if (getenv "TMUX")
    (progn
      (let ((x 2) (tkey ""))
        (while (<= x 8)
          ;; shift
          (if (= x 2)
              (setq tkey "S-"))
          ;; alt
          (if (= x 3)
              (setq tkey "M-"))
          ;; alt + shift
          (if (= x 4)
              (setq tkey "M-S-"))
          ;; ctrl
          (if (= x 5)
              (setq tkey "C-"))
          ;; ctrl + shift
          (if (= x 6)
              (setq tkey "C-S-"))
          ;; ctrl + alt
          (if (= x 7)
              (setq tkey "C-M-"))
          ;; ctrl + alt + shift
          (if (= x 8)
              (setq tkey "C-M-S-"))

          ;; arrows
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d A" x)) (kbd (format "%s<up>" tkey)))
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d B" x)) (kbd (format "%s<down>" tkey)))
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d C" x)) (kbd (format "%s<right>" tkey)))
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d D" x)) (kbd (format "%s<left>" tkey)))
          ;; home
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d H" x)) (kbd (format "%s<home>" tkey)))
          ;; end
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d F" x)) (kbd (format "%s<end>" tkey)))
          ;; page up
          (define-key key-translation-map (kbd (format "M-[ 5 ; %d ~" x)) (kbd (format "%s<prior>" tkey)))
          ;; page down
          (define-key key-translation-map (kbd (format "M-[ 6 ; %d ~" x)) (kbd (format "%s<next>" tkey)))
          ;; insert
          (define-key key-translation-map (kbd (format "M-[ 2 ; %d ~" x)) (kbd (format "%s<delete>" tkey)))
          ;; delete
          (define-key key-translation-map (kbd (format "M-[ 3 ; %d ~" x)) (kbd (format "%s<delete>" tkey)))
          ;; f1
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d P" x)) (kbd (format "%s<f1>" tkey)))
          ;; f2
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d Q" x)) (kbd (format "%s<f2>" tkey)))
          ;; f3
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d R" x)) (kbd (format "%s<f3>" tkey)))
          ;; f4
          (define-key key-translation-map (kbd (format "M-[ 1 ; %d S" x)) (kbd (format "%s<f4>" tkey)))
          ;; f5
          (define-key key-translation-map (kbd (format "M-[ 15 ; %d ~" x)) (kbd (format "%s<f5>" tkey)))
          ;; f6
          (define-key key-translation-map (kbd (format "M-[ 17 ; %d ~" x)) (kbd (format "%s<f6>" tkey)))
          ;; f7
          (define-key key-translation-map (kbd (format "M-[ 18 ; %d ~" x)) (kbd (format "%s<f7>" tkey)))
          ;; f8
          (define-key key-translation-map (kbd (format "M-[ 19 ; %d ~" x)) (kbd (format "%s<f8>" tkey)))
          ;; f9
          (define-key key-translation-map (kbd (format "M-[ 20 ; %d ~" x)) (kbd (format "%s<f9>" tkey)))
          ;; f10
          (define-key key-translation-map (kbd (format "M-[ 21 ; %d ~" x)) (kbd (format "%s<f10>" tkey)))
          ;; f11
          (define-key key-translation-map (kbd (format "M-[ 23 ; %d ~" x)) (kbd (format "%s<f11>" tkey)))
          ;; f12
          (define-key key-translation-map (kbd (format "M-[ 24 ; %d ~" x)) (kbd (format "%s<f12>" tkey)))
          ;; f13
          (define-key key-translation-map (kbd (format "M-[ 25 ; %d ~" x)) (kbd (format "%s<f13>" tkey)))
          ;; f14
          (define-key key-translation-map (kbd (format "M-[ 26 ; %d ~" x)) (kbd (format "%s<f14>" tkey)))
          ;; f15
          (define-key key-translation-map (kbd (format "M-[ 28 ; %d ~" x)) (kbd (format "%s<f15>" tkey)))
          ;; f16
          (define-key key-translation-map (kbd (format "M-[ 29 ; %d ~" x)) (kbd (format "%s<f16>" tkey)))
          ;; f17
          (define-key key-translation-map (kbd (format "M-[ 31 ; %d ~" x)) (kbd (format "%s<f17>" tkey)))
          ;; f18
          (define-key key-translation-map (kbd (format "M-[ 32 ; %d ~" x)) (kbd (format "%s<f18>" tkey)))
          ;; f19
          (define-key key-translation-map (kbd (format "M-[ 33 ; %d ~" x)) (kbd (format "%s<f19>" tkey)))
          ;; f20
          (define-key key-translation-map (kbd (format "M-[ 34 ; %d ~" x)) (kbd (format "%s<f20>" tkey)))

          (setq x (+ x 1))
          ))
      )
  )
