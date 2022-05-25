; STUFF FOR #+BEGIN_SRC BLOCKS
;; Set the package installation directory so that packages aren't stored in the
; ;; ~/.emacs.d/elpa path.
; (require 'package)
; (setq package-user-dir (expand-file-name "./.packages"))
; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;                          ("elpa" . "https://elpa.gnu.org/packages/")))
; 
; ;; Initialize the package system
; (package-initialize)
; (unless package-archive-contents
;   (package-refresh-contents))
; 
; ;; Install dependencies
; (package-install 'htmlize)

(require 'ox-publish)

; see https://pank.eu/blog/blog-setup.html

;; Customize the HTML output
(setq org-html-validation-link nil             ;; Don't show validation link
;      org-html-head-include-scripts nil       ;; js scripts to include
;      org-html-head-include-default-style nil ;; don't use default css stylesheet
;      org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />")
       org-html-html5-fancy t                  ;; ...
       org-html-preamble t
       org-html-divs '((preamble "header" "top")
                       (content "main" "content")
                       (postamble "footer" "postamble"))
       org-html-preamble-format '(("en"
                                   ;(concat ; TODO for some reason concat doesn't work here
                                   ; TODO just read in header.html
                                   "<h1 class=\"title\">Website title</h1>
                                    <div id=\"content\" class=\"content\">
                                    <nav class=\"nav\">
                                    <a class=\"nav-link\" href=\"/home.html\">Home</a>
                                    <a class=\"nav-link\" href=\"/projects.html\">Projects</a>
                                    <a class=\"nav-link\" href=\"/articles.html\">Articles</a>
                                    <a class=\"nav-link\" href=\"/feed.rss\">RSS</a>
                                    </nav>
                                    </div>\n"))
       org-html-head "<style>
          body { background-color: #28292b; }
          #content { max-width: 60em; margin: auto; background-color: #28292b; color: #ffffff; }

          nav.nav {
            text-align: center;
            list-style-type: none;
            color: #ffffff;
            background-color: #18191b;
          }
          a:link {
            color: #46D0FF;
            /* background-color: yellow; */
          }
          a:visited {
            color: #0170BF;
          }
          a.nav-link {
            margin: 15px;
            color: #99BB66;
            background-color: #18191b;
          }

          .title  { text-align: center; margin-bottom: .2em; color: #B4916D; }
          .subtitle { text-align: center; font-size: medium; font-weight: bold; margin-top:0; }
          .tag    { background-color: #eee; font-family: monospace; padding: 2px; font-size: 80%; font-weight: normal; }
          .underline { text-decoration: underline; }
          #postamble { text-align: right; }
          #postamble p, #preamble p { font-size: 80%; margin: .2em; }

        </style>"
      )

(defun my-format-rss-feed (title list)
  (let* ((list-entries   (cdr list))
         (orig-buffer (current-buffer))
         (latest-article "")
         (buf-str "")
         (latest-project ""))
    (mapcar (lambda (elem)
              (when (string= (car elem) "articles")
                (setq latest-article (car (car (cdr (car (cdr elem))))))
                (switch-to-buffer (find-file-noselect "articles.org" nil nil nil))
                ; NOTE WORKAROUND for invalid-search-bound bug
                (setq buf-str (buffer-string))
                (with-temp-file "articles.org"
                  (insert
                    (replace-regexp-in-string "@@start:articles@@.*\\(\n.*\\)*@@end:articles@@"
                      (format "@@start:articles@@\n%s\n@@end:articles@@" (org-list-to-org (car (cdr elem))))
                      buf-str nil t)))
                (kill-buffer "articles.org")
                )
              (when (string= (car elem) "projects")
                (setq latest-project (car (car (cdr (car (cdr elem))))))
                (switch-to-buffer (find-file-noselect "projects.org" nil nil nil))
                ; NOTE WORKAROUND for invalid-search-bound bug
                (setq buf-str (buffer-string))
                (with-temp-file "projects.org"
                  (insert
                    (replace-regexp-in-string "@@start:projects@@.*\\(\n.*\\)*@@end:projects@@"
                      (format "@@start:projects@@\n%s\n@@end:projects@@" (org-list-to-org (car (cdr elem))))
                      buf-str nil t)))))
            list-entries)

    ; NOTE WORKAROUND for invalid-search-bound bug
    (switch-to-buffer (find-file-noselect "home.org" nil nil nil))
    (setq buf-str (buffer-string))
    (with-temp-file "home.org"
      (setq buf-str (replace-regexp-in-string "@@start:article@@.*\\(\n.*\\)*@@end:article@@"
          (format "@@start:article@@%s@@end:article@@" latest-article) buf-str nil t))
      (insert
        (replace-regexp-in-string "@@start:project@@.*\\(\n.*\\)*@@end:project@@"
          (format "@@start:project@@%s@@end:project@@" latest-project) buf-str nil t)))
    (switch-to-buffer orig-buffer))

  (concat "#+TITLE: " title "\n\n" (org-list-to-org list))) ; NOTE this writes to sitemap.org

(defun my-format-rss-feed-entry (entry style project)
  ;; RSS entry
  (when (and (string-match-p "articles/" entry) (not (string= entry "articles/")))
    (save-excursion
      (switch-to-buffer (find-file-noselect entry))
      (cd "..") ; go up from "articles/"
      (write-region
        (format "<item>\n<title>%s</title>\n<link>%s</link>\n<guid>%s</guid>\n<description>%s</description>\n<pubDate>%s</pubDate>\n</item>\n"
              (org-publish-find-title entry project)
              (concat "http://andersch.xyz/" (string-replace ".org" ".html" entry))
              (concat "http://andersch.xyz/" (string-replace ".org" ".html" entry))
              (alist-get "DESCRIPTION" (org-collect-keywords '("DESCRIPTION") '("DESCRIPTION")) nil nil 'string=)
              (format-time-string "%a, %d %b %Y %H:%M:%S %z" (seconds-to-time (org-publish-find-date entry project))))
        nil "feed.rss" 'append)))

  ;; sitemap entry
  (cond ((not (directory-name-p entry))
         (format "[[file:%s][%s]]"
             entry
             (org-publish-find-title entry project)))
        ((eq style 'tree)
          ;; Return only last subdir.
          (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(setq org-publish-project-alist
      (list
       (list "andersch.xyz" ;; check https://orgmode.org/manual/Publishing-options.html for more options
             :recursive            t
             :base-directory       "./"
             :publishing-directory "./"
             :publishing-function  'org-html-publish-to-html ;; may be a list of functions
           ; :preparation-function                           ;; called before publishing
           ; :completion-function                            ;; called after
           ; :base-extension                                 ;; extension of source files
             :exclude "sitemap.org"                          ;; regex of files to exclude
           ; :include                                        ;; list of files to include

             :auto-sitemap         t                         ;; https://orgmode.org/manual/Site-map.html
             :sitemap-filename     "sitemap.org"             ;; ...
           ; :sitemap-title
             :sitemap-style        'tree                     ;; list or tree
             :sitemap-sort-files   'anti-chronologically
             :sitemap-function     'my-format-rss-feed
             :sitemap-format-entry 'my-format-rss-feed-entry

           ; :makeindex t                                    ;; https://orgmode.org/manual/Generating-an-index.html

             :with-title           nil                       ;; we include our own header
             :with-author          nil
             :with-creator         nil                       ;; don't include emacs and org versions in footer
             :with-toc             nil                       ;; no table of contents
             :section-numbers      nil                       ;; no section numbers for headings
             :time-stamp-file      nil)))                    ;; don't include "Created: <timestamp>" in footer

(with-temp-file "feed.rss" ; hardcoded rss header, check with  https://validator.w3.org/feed/
  (insert
   (let* ((website-title "My website")
          (homepage      "http://andersch.xyz") ; TODO https instead of http
          (rss-filepath  "/feed.rss"))
   (concat "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
           "<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">\n"
           "<channel>\n"
           (format "<title>%s</title>\n" website-title)
           "<!-- <lastBuildDate>Wed, 15 Dec 2021 00:00:00 +0000</lastBuildDate> -->\n" ; TODO insert todays date
           (format "<atom:link href=\"%s%s\" rel=\"self\" type=\"application/rss+xml\"/>\n" homepage rss-filepath)
           (format "<link>%s/home.html</link>\n" homepage)
           "<description>Stuff on programming</description>\n"
           "<language>en-us</language>\n"))))

(org-publish "andersch.xyz") ;; generate rss feed, expand @@..@@ markers, export html files
(write-region "</channel>\n</rss>" nil "feed.rss" 'append) ;; hardcoded rss ending

(message "Build complete")
