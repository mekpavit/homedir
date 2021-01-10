(setq user-full-name "Pavit Kiatkraipob"
      user-mail-address "mek.kiatkrai@shopee.com")

(setq doom-font (font-spec :family "Jetbrains Mono" :size 15 :weight 'semi-light))

(setq doom-theme 'doom-dracula)

(setq display-line-numbers-type 'relative)

(defun my/go-impl--collect-interface (package)
  (with-temp-buffer
    (unless (zerop (process-file "go" nil t nil "doc" "-src" package))
      (error "Failed: 'go doc -src %s'" package))
    (goto-char (point-min))
    (cl-loop with re = "^type\\s-+\\(\\S-+\\)\\s-+interface"
             with real-package = (go-impl--real-package-name package)
             while (re-search-forward re nil t)
             collect (concat real-package "." (match-string-no-properties 1)) into interfaces
             finally return (progn
                              (puthash package (cl-copy-list interfaces) go-impl--interface-cache)
                              interfaces))))
(advice-add 'go-impl--collect-interface
            :override 'my/go-impl--collect-interface)

(setq my/go-build-tags "")
(defun my/go--set-build-tags (build-tags)
    (interactive
     (list (read-from-minibuffer "Go build tags: ")))
    "Accept build tags (separated by commas), set lsp-gopls-build-flags to ['tags=build-tags] and restart language server"
  (setq my/go-build-tags build-tags)
  (setq lsp-go-build-flags (vector (concat "-tags=" my/go-build-tags)))
  (dolist (workspace (lsp-workspaces)) (lsp-workspace-restart workspace)))

(defun my/go--coverage-all ()
  "Show go coverage of current buffer."
  (interactive)
  (shell-command (concat "go test -tags=" my/go-build-tags " -coverprofile=cover.out ./..."))
  (go-coverage "cover.out"))

(defun my/+go--run-tests (args)
  (let ((cmd (concat "go test " (concat "-tags=" my/go-build-tags " " args))))
    (setq +go-test-last (concat "cd " default-directory ";" cmd))
    (+go--spawn cmd)))
(advice-add '+go--run-tests
            :override 'my/+go--run-tests)

(map! :map go-mode-map
      :localleader
      (:prefix ("b" . "build")
       "f" #'my/go--set-build-tags)
      (:prefix ("c" . "coverage")
       "a" #'my/go--coverage-all))

(setq org-directory "~/org/gtd/")
(setq org-agenda-files '("~/org/gtd/next_actions.org" "~/org/gtd/projects.org" "~/org/gtd/calendar.org" "~/org/gtd/waiting_for.org"))
(after! org
  (add-to-list 'org-capture-templates
               '("i" "In" entry
                  (file+headline "~/org/gtd/in.org" "In")
                 "* TODO (Added on %U) %?\n")))
(setq org-refile-targets '(("~/org/gtd/projects.org" :maxlevel . 2)
                           ("~/org/gtd/next_actions.org" :level . 1)
                           ("~/org/gtd/calendar.org" :level . 1)
                           ("~/org/gtd/wanting_for.org" :level . 1)
                           ("~/org/gtd/referrences.org" :level . 1)
                           ("~/org/gtd/someday.org" :level . 1)
                           ("~/org/gtd/trash.org" :level . 1)))
(after! org
        (setq org-todo-keywords
              '((sequence "TODO" "DONE" "WAIT" "HOLD"))))
