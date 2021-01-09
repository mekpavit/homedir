(setq user-full-name "Pavit Kiatkraipob"
      user-mail-address "mek.kiatkrai@shopee.com")

(setq doom-font (font-spec :family "Jetbrains Mono" :size 15 :weight 'semi-light))

(setq doom-theme 'doom-dracula)

(setq org-directory "~/org/")

(setq display-line-numbers-type 'relative)

(defun my/go-coverage-all ()
  "Show go coverage of current buffer."
  (interactive)
  (shell-command "go test -tags=integration -coverprofile=cover.out ./...")
  (go-coverage "cover.out"))

;; override Doom's function
(defun my/go-test-single ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
        (+go--run-tests (concat "-tags=integration " "-run" "='" (match-string-no-properties 2) "'")))
    (error "Must be in a _test.go file")))

;; override Doom's function
(defun my/go-test-all ()
  (interactive)
  (+go--run-tests "-tags=integration"))

(map! :map go-mode-map
      :localleader
      (:prefix ("c" . "coverage")
       "a" #'go-coverage-all
       :prefix ("t" . "test")
       "s" #'my/go-test-single
       "a" #'my/go-test-all))

(use-package! ox-hugo
	:after ox)
