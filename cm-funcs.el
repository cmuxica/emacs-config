;;;; WINDOW MANAGEMENT ;;;;
(defun cm/split-window-vertically-and-switch ()
  "Split window vertically and switch to that window."
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun cm/split-window-horizontally-and-switch ()
  "Split window horizontally and switch to that window."
  (interactive)
  (split-window-horizontally)
  (other-window 1))

;;;; BUFFER MANAGEMENT ;;;;
(defun cm/toggle-maximize-buffer ()
  "Toggle maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(defun cm/kill-other-buffers ()
    "Kill all other (non-fallback) buffers."
    (interactive)
    (mapc 'kill-buffer
	  (delq (current-buffer)
		(cl-remove-if-not 'buffer-file-name (buffer-list)))))

(defun cm/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
	 (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
	(error "Buffer '%s' is not visiting a file!" name)
      (let* ((dir (file-name-directory filename))
	     (new-name (read-file-name "New name: " dir)))
	(cond ((get-buffer new-name)
	       (error "A buffer named '%s' already exists!" new-name))
	      (t
	       (let ((dir (file-name-directory new-name)))
		 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
		   (make-directory dir t)))
	       (rename-file filename new-name 1)
	       (rename-buffer new-name)
	       (set-visited-file-name new-name)
	       (set-buffer-modified-p nil)
	       (when (fboundp 'recentf-add-file)
		 (recentf-add-file new-name)
		 (recentf-remove-if-non-kept filename))
	       (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun cm/switch-to-scratch-buffer ()
  "Switch to the scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun cm/new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

(defun cm/split-and-scratch-elisp ()
  "Split window and create new buffer for Emacs Lisp evaluation."
  (interactive)
  (cm/split-window-horizontally-and-switch)
  (cm/switch-to-scratch-buffer))

(defun cm/open-buffer-file-mac ()
  "Open current buffer file using Mac `open' command."
  (interactive)
  (shell-command (concat "open " (buffer-file-name))))

;;;; AESTHETICS ;;;;
(defun cm/toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (cond ((eq (car custom-enabled-themes) dark-theme)
	  (mapc #'disable-theme custom-enabled-themes)
	  (load-theme light-theme t))
	((eq (car custom-enabled-themes) light-theme)
	  (mapc #'disable-theme custom-enabled-themes)
	  (load-theme dark-theme t))))

(defun cm/writing-visual-fill ()
  "Center text on screen for improved writing experience."
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;;;; LATEX ;;;;
;; copy various latex templates to desktop
(defun cm/copy-beamer-template (name)
  (interactive "sName the presentation: ")
  (copy-directory "~/Dropbox/docs/latex-templates/presentation" (concat "~/Desktop/" name))
  (copy-file "~/Dropbox/docs/latex-templates/cite.bib" (concat "~/Desktop/" name "/cite.bib"))
  (switch-to-buffer (find-file (concat "~/Desktop/" name "/master.tex"))))
(defun cm/copy-handout-template (name)
  (interactive "sName the handout: ")
  (copy-directory "~/Dropbox/docs/latex-templates/handout" (concat "~/Desktop/" name))
  (copy-file "~/Dropbox/docs/latex-templates/cite.bib" (concat "~/Desktop/" name "/cite.bib"))
  (switch-to-buffer (find-file (concat "~/Desktop/" name "/master.tex"))))
(defun cm/copy-paper-template (name)
  (interactive "sName the paper: ")
  (copy-directory "~/Dropbox/docs/latex-templates/paper" (concat "~/Desktop/" name))
  (copy-file "~/Dropbox/docs/latex-templates/cite.bib" (concat "~/Desktop/" name "/cite.bib"))
  (switch-to-buffer (find-file (concat "~/Desktop/" name "/master.tex"))))

;;;; NOTES ;;;;
;; make new md note
(defun cm/capture-md (name)
  (interactive "sName the note: ")
  (switch-to-buffer (find-file (concat "~/Dropbox/notes/" name ".md"))))
