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

(defun my/go-impl (receiver-name receiver interface)
  (interactive
   (let* ((packages (go-packages))
          (comp-fn (lambda (input)
                     (go-impl--completing-function packages input nil t)))
          (struct-name (save-excursion (re-search-backward "^type \\(.*\\) struct") (match-string-no-properties 1))))
     (setq go-impl--receiver-cache nil)
     (list
      (read-from-minibuffer "Receiver name: " nil go-impl--local-command-map nil
                            'go-impl--receiver-history nil t)
      (ivy-read "Receiver type: " (list (concat "*" struct-name) struct-name))
      (ivy-read "Interface: " comp-fn :history 'go-impl--interface-history :dynamic-collection t))))
  (when go-impl-aliases-alist
    (setq interface (or (assoc-default interface go-impl-aliases-alist)
                        interface)))
  (let ((stubs (go-impl--execute (concat receiver-name " " receiver) interface)))
    (save-excursion
      (insert stubs))
    (when go-impl-enter-function
      (forward-line)
      (back-to-indentation))))

(advice-add 'go-impl
            :override 'my/go-impl)

(setq my/go-build-tags "")
(defun my/go--set-build-tags (build-tags)
    (interactive
     (list (read-from-minibuffer "Go build tags: ")))
    "Accept build tags (separated by commas), set lsp-go-build-flags to ['tags=build-tags] and restart language server"
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
       "a" #'my/go--coverage-all)
      "i" #'go-impl)

(use-package! company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

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
