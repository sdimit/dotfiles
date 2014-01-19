
;;  ;; custom variables kludge. Why can't I get these to work via setq?
;;  (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(LaTeX-XeTeX-command "xelatex -synctex=1")
;;  '(TeX-engine (quote xetex))
;;  ;; '(TeX-view-program-list (quote (("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b"))))
;;  ;; '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "Skim") (output-html "xdg-open"))))
;;  '(show-paren-mode t)
;;  '(blink-cursor-mode nil)
;;  '(text-mode-hook (quote (text-mode-hook-identify)))
;;  )

;;  Configure org-mode to export directly to PDF using pdflatex or
;;  xelatex, compiling the bibliography as it goes, with my preferred
;;  setup in each case. There is a good deal of local stuff in this section. The required style files used below are available at https://github.com/kjhealy/latex-custom-kjh. You may need to adjust or remove some of these settings depending on your
;;  preferences and local configuration.

(require 'org-latex)
;; Choose either listings or minted for exporting source code blocks.
;; Using minted (as here) requires pygments be installed. To use the
;; default listings package instead, use
;; (setq org-export-latex-listings t)
;; and change references to "minted" below to "listings"
(setq org-export-latex-listings 'minted)

;; default settings for minted code blocks
(setq org-export-latex-minted-options
      '(;("frame" "single")
        ("bgcolor" "bg") ; bg will need to be defined in the preamble of your document. It's defined in org-preamble-pdflatex.sty and org-preamble-xelatex.sty below.
        ("fontsize" "\\small")
        ))
;; turn off the default toc behavior; deal with it properly in headers to files.
(defun org-export-latex-no-toc (depth)
  (when depth
    (format "%% Org-mode is exporting headings to %s levels.\n"
            depth)))
(setq org-export-latex-format-toc-function 'org-export-latex-no-toc)

(add-to-list 'org-export-latex-classes
             '("memarticle"
               "\\documentclass[11pt,oneside,article]{memoir}\n\\input{vc} % vc package"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-export-latex-classes
             '("membook"
               "\\documentclass[11pt,oneside]{memoir}\n\\input{vc} % vc package"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

;; Originally taken from Bruno Tavernier: http://thread.gmane.org/gmane.emacs.orgmode/31150/focus=31432
;; but adapted to use latexmk 4.22 or higher.
(defun my-auto-tex-cmd ()
  "When exporting from .org with latex, automatically run latex,
                     pdflatex, or xelatex as appropriate, using latexmk."
  (let ((texcmd)))
  ;; default command: pdflatex
  (setq texcmd "latexmk -pdflatex='pdflatex -synctex=1 --shell-escape --' -pdf %f")
  ;; pdflatex -> .pdf
  (if (string-match "LATEX_CMD: pdflatex" (buffer-string))
      (setq texcmd "latexmk -pdflatex='pdflatex -synctex=1 --shell-escape' -pdf %f"))
  ;; xelatex -> .pdf
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq texcmd "latexmk -pdflatex='xelatex -synctex=1 --shell-escape' -pdf %f"))
  ;; LaTeX compilation command
  (setq org-latex-to-pdf-process (list texcmd)))

(add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-cmd)

;; Default packages included in /every/ tex file, latex, pdflatex or xelatex
(setq org-export-latex-packages-alist
      '(("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" )))

;; Custom packages
(defun my-auto-tex-parameters ()
  "Automatically select the tex packages to include. See https://github.com/kjhealy/latex-custom-kjh for the support files included here."
  ;; default packages for ordinary latex or pdflatex export
  (setq org-export-latex-default-packages-alist
        '(("AUTO" "inputenc" t)
          ("minted,minion" "org-preamble-pdflatex" t)))
  ;; Packages to include when xelatex is used
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq org-export-latex-default-packages-alist
            '(("minted" "org-preamble-xelatex" t) ))))

(add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-parameters)


;;  ebib is a bibtex database manager that works inside emacs. It can
;;  talk to org-mode. See [[http://orgmode.org/worg/org-tutorials/org-latex-export.html#sec-17_2][this Worg tutorial]] for details.

(org-add-link-type "ebib" 'ebib)

(org-add-link-type
 "cite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "cite:" desc)))
         (format "\\cite{%s}" path)
       (format "\\cite[%s]{%s}" desc path)
       )))))

(org-add-link-type
 "parencite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "parencite:" desc)))
         (format "\\parencite{%s}" path)
       (format "\\parencite[%s]{%s}" desc path)
       )))))

(org-add-link-type
 "textcite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "textcite:" desc)))
         (format "\\textcite{%s}" path)
       (format "\\textcite[%s]{%s}" desc path)
       )))))

(org-add-link-type
 "autocite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "autocite:" desc)))
         (format "\\autocite{%s}" path)
       (format "\\autocite[%s]{%s}" desc path)
       )))))

(org-add-link-type
 "footcite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "footcite:" desc)))
         (format "\\footcite{%s}" path)
       (format "\\footcite[%s]{%s}" desc path)
       )))))

(org-add-link-type
 "fullcite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "fullcite:" desc)))
         (format "\\fullcite{%s}" path)
       (format "\\fullcite[%s]{%s}" desc path)
       )))))

(org-add-link-type
 "citetitle" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "citetitle:" desc)))
         (format "\\citetitle{%s}" path)
       (format "\\citetitle[%s]{%s}" desc path)
       )))))

(org-add-link-type
 "citetitles" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "citetitles:" desc)))
         (format "\\citetitles{%s}" path)
       (format "\\citetitles[%s]{%s}" desc path)
       )))))

(org-add-link-type
 "headlessfullcite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "headlessfullcite:" desc)))
         (format "\\headlessfullcite{%s}" path)
       (format "\\headlessfullcite[%s]{%s}" desc path)
       )))))

(provide 'init-latex)
