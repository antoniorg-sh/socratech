;;; publish.el --- Socratech site publishing config -*- lexical-binding: t; -*-
;;
;; Builds the site by exporting each .org file in posts/ to HTML, and writing
;; a posts.json index used by the Ledger page.
;;
;; Run locally:   emacs --batch --load publish.el --funcall socratech-build
;; Run in CI:     same command; see .github/workflows/publish.yml

(require 'ox-publish)
(require 'ox-html)
(require 'json)
(require 'cl-lib)

(defvar socratech-root
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Absolute path of the site repo root.")

(defun socratech--slurp (rel)
  "Return the contents of REL (path relative to the repo root) as a string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name rel socratech-root))
    (buffer-string)))

(defun socratech--subst (s alist)
  "Return S with every {{KEY}} replaced by its ALIST value."
  (let ((out s))
    (dolist (cell alist out)
      (setq out (replace-regexp-in-string
                 (regexp-quote (format "{{%s}}" (car cell)))
                 (or (cdr cell) "")
                 out t t)))))

;; ── Register the custom post keywords as export options ─────────────
;; CATEGORY / FOLIO / DEK / TAGS / READ_TIME / FEATURED are not standard
;; org export options. Declaring them here makes them accessible to the
;; template function via (plist-get info :category) etc.
(org-export-define-derived-backend 'socratech-html 'html
  :options-alist
  '((:category   "CATEGORY"   nil nil t)
    (:folio      "FOLIO"      nil nil t)
    (:dek        "DEK"        nil nil t)
    (:post-tags  "TAGS"       nil nil t)
    (:read-time  "READ_TIME"  nil nil t)
    (:featured   "FEATURED"   nil nil t))
  :translate-alist
  '((template . socratech--html-template)))

(defun socratech--format-tags (raw)
  "Return HTML <span class='tag'>...</span> for space/comma-separated RAW."
  (mapconcat (lambda (tag) (format "<span class=\"tag\">%s</span>" tag))
             (split-string (or raw "") "[ ,]+" t)
             ""))

(defun socratech--html-template (contents info)
  "Produce the full HTML page for a Socratech post."
  (let* ((title     (org-export-data (plist-get info :title) info))
         (date      (org-export-data (plist-get info :date) info))
         (category  (or (plist-get info :category) ""))
         (folio     (or (plist-get info :folio) ""))
         (dek       (or (plist-get info :dek) ""))
         (tags-raw  (or (plist-get info :post-tags) ""))
         (read-time (or (plist-get info :read-time) ""))
         (head      (socratech--slurp "layouts/head.html"))
         (preamble  (socratech--subst (socratech--slurp "layouts/post-preamble.html")
                                      `(("FOLIO" . ,folio))))
         (postamble (socratech--slurp "layouts/post-postamble.html"))
         (essay-head
          (format "<header class=\"essay-head\">
  <div class=\"eyebrow\"><span class=\"bar\"></span><span class=\"cat\">%s</span><span>&middot; %s</span></div>
  <h1>%s</h1>
  <p class=\"dek\">%s</p>
  <div class=\"essay-meta\">
    <div class=\"group\"><span class=\"k\">Filed</span><span class=\"v\">%s</span></div>
    <div class=\"group\"><span class=\"k\">Read</span><span class=\"v\">%s min</span></div>
    <div class=\"group tags\">%s</div>
  </div>
</header>"
                  category folio title dek date read-time
                  (socratech--format-tags tags-raw))))
    (concat
     "<!DOCTYPE html>\n"
     "<html lang=\"en\">\n"
     (format "<head>\n<meta charset=\"UTF-8\">\n<title>Socratech &middot; %s</title>\n%s</head>\n"
             title head)
     "<body>\n"
     preamble "\n"
     "<main>\n<article>\n"
     essay-head "\n"
     "<div class=\"essay-body\">\n"
     contents "\n"
     "</div>\n"
     "<div class=\"endmark\"></div>\n"
     "</article>\n</main>\n"
     postamble "\n"
     "</body>\n</html>\n")))

;; ── posts.json generator ────────────────────────────────────────────
;; Reads every post's #+KEYWORDS without running a full export, so the
;; Ledger JS can fetch a small data file instead of embedding POSTS inline.
(defun socratech--read-post-meta (file)
  "Return an alist of metadata for post FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((kw (org-collect-keywords
               '("TITLE" "DATE" "CATEGORY" "TAGS" "READ_TIME" "FOLIO" "DEK" "FEATURED"))))
      `((id       . ,(file-name-base file))
        (title    . ,(or (cadr (assoc "TITLE" kw)) ""))
        (date     . ,(or (cadr (assoc "DATE" kw)) ""))
        (cat      . ,(or (cadr (assoc "CATEGORY" kw)) ""))
        (tags     . ,(vconcat (split-string (or (cadr (assoc "TAGS" kw)) "")
                                            "[ ,]+" t)))
        (read     . ,(string-to-number (or (cadr (assoc "READ_TIME" kw)) "0")))
        (fol      . ,(or (cadr (assoc "FOLIO" kw)) ""))
        (dek      . ,(or (cadr (assoc "DEK" kw)) ""))
        (featured . ,(if (string= "t" (downcase (or (cadr (assoc "FEATURED" kw)) "")))
                         t json-false))))))

(defun socratech--write-posts-json ()
  "Collect all post metadata and write docs/posts.json."
  (let* ((posts-dir (expand-file-name "posts" socratech-root))
         (files (cl-remove-if
                 (lambda (f) (string-match-p "/TEMPLATE\\.org\\'" f))
                 (directory-files posts-dir t "\\.org\\'")))
         (records (mapcar #'socratech--read-post-meta files))
         (json-path (expand-file-name "docs/posts.json" socratech-root)))
    (unless (file-directory-p (file-name-directory json-path))
      (make-directory (file-name-directory json-path) t))
    (with-temp-file json-path
      (insert (json-encode (vconcat records))))
    (message "Wrote %s (%d posts)" json-path (length records))))

;; ── Publishing project ─────────────────────────────────────────────
(defun socratech-publish-to-html (plist filename pub-dir)
  "Publish FILENAME using the socratech-html backend."
  (org-publish-org-to 'socratech-html filename ".html" plist pub-dir))

(setq org-publish-project-alist
      `(("posts"
         :base-directory ,(expand-file-name "posts" socratech-root)
         :base-extension "org"
         :exclude "TEMPLATE\\.org"
         :publishing-directory ,(expand-file-name "docs/posts" socratech-root)
         :publishing-function socratech-publish-to-html
         :recursive nil)))

(defun socratech--copy-static ()
  "Copy the hand-authored HTML pages and assets into docs/."
  (let ((docs (expand-file-name "docs" socratech-root)))
    (unless (file-directory-p docs) (make-directory docs t))
    (dolist (f '("Socratech.html" "Socratech-blog.html"
                 "Socratech-post.html" "Socratech-timeline.html"))
      (let ((src (expand-file-name f socratech-root)))
        (when (file-exists-p src)
          (copy-file src (expand-file-name f docs) t))))
    ;; GitHub Pages serves index.html at the root by default.
    (let ((home (expand-file-name "Socratech.html" socratech-root)))
      (when (file-exists-p home)
        (copy-file home (expand-file-name "index.html" docs) t)))
    (let ((src-assets (expand-file-name "assets" socratech-root))
          (dst-assets (expand-file-name "assets" docs)))
      (when (file-directory-p src-assets)
        (copy-directory src-assets dst-assets t t t)))))

(defun socratech--clean-stale-posts ()
  "Remove any .html files in docs/posts/ that no longer have a .org source."
  (let* ((src-dir (expand-file-name "posts" socratech-root))
         (out-dir (expand-file-name "docs/posts" socratech-root)))
    (when (file-directory-p out-dir)
      (dolist (html (directory-files out-dir t "\\.html\\'"))
        (let* ((stem (file-name-base html))
               (src  (expand-file-name (concat stem ".org") src-dir)))
          (unless (file-exists-p src)
            (delete-file html)
            (message "Cleaned stale %s" (file-name-nondirectory html))))))))

(defun socratech-build ()
  "Build the whole site: posts + posts.json + static assets."
  (let ((org-publish-timestamp-directory
         (expand-file-name ".cache/org-timestamps/" socratech-root)))
    (org-publish "posts" t))
  (socratech--clean-stale-posts)
  (socratech--write-posts-json)
  (socratech--copy-static)
  (message "Socratech build complete. Output in docs/"))

(provide 'publish)
;;; publish.el ends here
