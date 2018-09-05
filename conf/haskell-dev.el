;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install required dependencies ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(defvar required-haskell-packages
  '(magit
    flycheck-haskell
    haskell-mode
    company
    hindent
    undo-tree
    hasky-stack
    hasky-extensions
    ) "a list of packages to ensure are installed at launch for haskell dev environment.")

(require 'cl)

;; method to check if all packages are installed
(defun haskell-packages-installed-p (required-packages)
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; if not all packages are installed, check one by one and install the missing ones.
(defun haskell-install-required-packages (required-packages)
  (unless (haskell-packages-installed-p required-packages) ; check for new packages (package versions)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.") ; install the missing packages
    (dolist (p required-packages)
      (when (not (package-installed-p p))
        (message "%s" p)
        (package-install p)))))

(haskell-install-required-packages required-haskell-packages)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure haskell-mode and others ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; undo-tree
(global-undo-tree-mode)


(defun haskell-custom-format-imports ()
  "Sort and format haskell module imports"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (progn (haskell-navigate-imports)
                  (looking-at "^import "))
      (haskell-align-imports)
      (haskell-sort-imports))))


(defun haskell-mode-setup ()
  "Configure my Haskell environment"
  (setq haskell-indentation-layout-offset 2)
  (setq haskell-indentation-starter-offset 2)
  (setq haskell-indentation-left-offset 2)
  ;;(setq haskell-indentation-ifte-offset 2)
  (setq haskell-indentation-where-pre-offset 2)
  (setq haskell-indentation-where-post-offset 2)
  (setq haskell-process-log t)
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-suggest-remove-import-lines nil)
  (setq haskell-process-suggest-restart nil)
  (setq haskell-process-type 'stack-ghci)
  ;;(setq haskell-process-args-stack-ghci '("--test" "--bench"))
  (setq haskell-process-args-ghci
        '("-ferror-spans" "-fshow-loaded-modules"))
  (setq haskell-process-args-cabal-repl
        '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
  (setq haskell-process-args-stack-ghci
        '("--with-ghc=ghci"
          "--ghci-options=-ferror-spans"
          ;; this should be added to ghci script: "-fshow-loaded-modules"
          "--no-build" "--no-load" "--test" "--bench"))
  (setq haskell-process-args-cabal-new-repl
        '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
  (setq haskell-stylish-on-save nil)
  (setq haskell-ask-also-kill-buffers nil)
  (setq haskell-tags-on-save t)
  (local-set-key (kbd "C-c C-d") #'haskell-process-load-file)
  (local-set-key (kbd "s-q h e") #'hasky-extensions)
  (local-set-key (kbd "s-q h s") #'hasky-stack-execute)
  (local-set-key (kbd "s-q h n") #'hasky-stack-new)
  (local-set-key (kbd "M-1") 'haskell-custom-format-imports)
  (local-set-key (kbd "M-2") 'haskell-mode-stylish-buffer)
  )

;; Repl
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(add-hook 'haskell-mode-hook 'haskell-mode-setup)

(require 'haskell-move-nested)
(add-hook 'haskell-mode-hook (lambda ()
  (define-key haskell-mode-map (kbd "M-<left>")
    (lambda ()
      (interactive)
      (haskell-move-nested -1)))))
(add-hook 'haskell-mode-hook (lambda ()
  (define-key haskell-mode-map (kbd "M-<right>")
    (lambda ()
      (interactive)
      (haskell-move-nested 1)))))

;; Quick access to import list and ability to move back
(require 'haskell-navigate-imports)
(add-hook 'haskell-mode-hook (lambda ()
  (local-set-key (kbd "M-[") 'haskell-navigate-imports)))
(add-hook 'haskell-mode-hook (lambda ()
  (local-set-key (kbd "M-]") 'haskell-navigate-imports-return)))

(add-hook 'haskell-mode-hook #'hindent-mode)
(setq hindent-style "johan-tibell")
