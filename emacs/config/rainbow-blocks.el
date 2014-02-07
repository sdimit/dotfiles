(eval-when-compile (require 'cl))

(defgroup rainbow-delimiters nil
  "Highlight nested parentheses, brackets, and braces according to their depth."
  :prefix "rainbow-delimiters-"
  :link '(url-link :tag "Website for rainbow-delimiters (EmacsWiki)"
                   "http://www.emacswiki.org/emacs/RainbowDelimiters")
  :group 'applications)

(defgroup rainbow-delimiters-faces nil
  "Faces for successively nested pairs of delimiters.

When depth exceeds innermost defined face, colors cycle back through."
  :tag "Color Scheme"
  :group 'rainbow-delimiters
  :link '(custom-group-link "rainbow-delimiters")
  :link '(custom-group-link :tag "Toggle Delimiters" "rainbow-delimiters-toggle-delimiter-highlighting")
  :prefix 'rainbow-delimiters-faces-)

;; Choose which delimiters you want to highlight in your preferred language:

(defgroup rainbow-delimiters-toggle-delimiter-highlighting nil
  "Choose which delimiters to highlight."
  :tag "Toggle Delimiters"
  :group 'rainbow-delimiters
  :link '(custom-group-link "rainbow-delimiters")
  :link '(custom-group-link :tag "Color Scheme" "rainbow-delimiters-faces"))

(defcustom rainbow-delimiters-highlight-parens-p t
  "Enable highlighting of nested parentheses -- ().

Non-nil (default) enables highlighting of parentheses.
Nil disables parentheses highlighting."
  :tag "Highlight Parentheses?"
  :type 'boolean
  :group 'rainbow-delimiters-toggle-delimiter-highlighting)

(defcustom rainbow-delimiters-highlight-brackets-p t
  "Enable highlighting of nested brackets -- [].

Non-nil (default) enables highlighting of brackets.
Nil disables bracket highlighting."
  :tag "Highlight Brackets?"
  :type 'boolean
  :group 'rainbow-delimiters-toggle-delimiter-highlighting)

(defcustom rainbow-delimiters-highlight-braces-p t
  "Enable highlighting of nested braces -- {}.

Non-nil (default) enables highlighting of braces.
Nil disables brace highlighting."
  :tag "Highlight Braces?"
  :type 'boolean
  :group 'rainbow-delimiters-toggle-delimiter-highlighting)


;;; Faces:

;; Unmatched delimiter face:
(defface rainbow-delimiters-unmatched-face
  '((((background light)) (:foreground "#88090B"))
    (((background dark)) (:foreground "#88090B")))
  "Face to highlight unmatched closing delimiters in."
  :group 'rainbow-delimiters-faces)

;; Faces for highlighting delimiters by nested level:
(defface rainbow-delimiters-depth-1-face
  '((((background light)) (:foreground "#707183"))
    (((background dark)) (:foreground "grey55")))
  "Nested delimiters face, depth 1 - outermost set."
  :tag "Rainbow Delimiters Depth 1 Face -- OUTERMOST"
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-2-face
  '((((background light)) (:foreground "#7388d6"))
    (((background dark)) (:foreground "#93a8c6")))
  "Nested delimiters face, depth 2."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-3-face
  '((((background light)) (:foreground "#909183"))
    (((background dark)) (:foreground "#b0b1a3")))
  "Nested delimiters face, depth 3."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-4-face
  '((((background light)) (:foreground "#709870"))
    (((background dark)) (:foreground "#97b098")))
  "Nested delimiters face, depth 4."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-5-face
  '((((background light)) (:foreground "#907373"))
    (((background dark)) (:foreground "#aebed8")))
  "Nested delimiters face, depth 5."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-6-face
  '((((background light)) (:foreground "#6276ba"))
    (((background dark)) (:foreground "#b0b0b3")))
  "Nested delimiters face, depth 6."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-7-face
  '((((background light)) (:foreground "#858580"))
    (((background dark)) (:foreground "#90a890")))
  "Nested delimiters face, depth 7."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-8-face
  '((((background light)) (:foreground "#80a880"))
    (((background dark)) (:foreground "#a2b6da")))
  "Nested delimiters face, depth 8."
  :group 'rainbow-delimiters-faces)

(defface rainbow-delimiters-depth-9-face
  '((((background light)) (:foreground "#887070"))
    (((background dark)) (:foreground "#9cb6ad")))
  "Nested delimiters face, depth 9."
  :group 'rainbow-delimiters-faces)

;;; Faces 10+:
;; NOTE: Currently unused. Additional faces for depths 9+ can be added on request.

(defconst rainbow-delimiters-max-face-count 9
  "Number of faces defined for highlighting delimiter levels.

Determines depth at which to cycle through faces again.")

(defvar rainbow-delimiters-outermost-only-face-count 0
  "Number of faces to be used only for N outermost delimiter levels.

This should be smaller than `rainbow-delimiters-max-face-count'.")

;;; Face utility functions

(defsubst rainbow-delimiters-depth-face (depth)
  "Return face-name for DEPTH as a string 'rainbow-delimiters-depth-DEPTH-face'.

For example: 'rainbow-delimiters-depth-1-face'."
  (intern-soft
   (concat "rainbow-delimiters-depth-"
           (number-to-string
            (or
             ;; Our nesting depth has a face defined for it.
             (and (<= depth rainbow-delimiters-max-face-count)
                  depth)
             ;; Deeper than # of defined faces; cycle back through to
             ;; `rainbow-delimiters-outermost-only-face-count' + 1.
             ;; Return face # that corresponds to current nesting level.
             (+ 1 rainbow-delimiters-outermost-only-face-count
                (mod (- depth rainbow-delimiters-max-face-count 1)
                     (- rainbow-delimiters-max-face-count
                        rainbow-delimiters-outermost-only-face-count)))))
           "-face")))

;;; Nesting level

(defvar rainbow-delimiters-syntax-table nil
  "Syntax table (inherited from buffer major-mode) which uses all delimiters.


When rainbow-delimiters-minor-mode is first activated, it sets this variable and
the other rainbow-delimiters specific syntax tables based on the current
major-mode. The syntax table is constructed by the function
'rainbow-delimiters-make-syntax-table'.")

;; syntax-table: used with syntax-ppss for determining current depth.
(defun rainbow-delimiters-make-syntax-table (syntax-table)
  "Inherit SYNTAX-TABLE and add delimiters intended to be highlighted by mode."
  (let ((table (copy-syntax-table syntax-table)))
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    table))

(defsubst rainbow-delimiters-depth (loc)
  "Return # of nested levels of parens, brackets, braces LOC is inside of."
  (let ((depth
         (with-syntax-table rainbow-delimiters-syntax-table
           (car (syntax-ppss loc)))))
    (if (>= depth 0)
        depth
      0))) ; ignore negative depths created by unmatched closing parens.


;;; Text properties

;; Backwards compatibility: Emacs < v23.2 lack macro 'with-silent-modifications'.
(eval-and-compile
  (unless (fboundp 'with-silent-modifications)
    (defmacro with-silent-modifications (&rest body)
      "Defined by rainbow-delimiters.el for backwards compatibility with Emacs < 23.2.
 Execute BODY, pretending it does not modify the buffer.
If BODY performs real modifications to the buffer's text, other
than cosmetic ones, undo data may become corrupted.

This macro will run BODY normally, but doesn't count its buffer
modifications as being buffer modifications.  This affects things
like buffer-modified-p, checking whether the file is locked by
someone else, running buffer modification hooks, and other things
of that nature.

Typically used around modifications of text-properties which do
not really affect the buffer's content."
      (declare (debug t) (indent 0))
      (let ((modified (make-symbol "modified")))
        `(let* ((,modified (buffer-modified-p))
                (buffer-undo-list t)
                (inhibit-read-only t)
                (inhibit-modification-hooks t)
                deactivate-mark
                ;; Avoid setting and removing file locks and checking
                ;; buffer's uptodate-ness w.r.t the underlying file.
                buffer-file-name
                buffer-file-truename)
           (unwind-protect
               (progn
                 ,@body)
             (unless ,modified
               (restore-buffer-modified-p nil))))))))

(defsubst rainbow-delimiters-propertize-delimiter (loc depth)
  "Highlight a single delimiter at LOC according to DEPTH.

LOC is the location of the character to add text properties to.
DEPTH is the nested depth at LOC, which determines the face to use.

Sets text properties:
`font-lock-face' to the appropriate delimiter face.
`rear-nonsticky' to prevent color from bleeding into subsequent characters typed by the user."
  (with-silent-modifications
    (let ((delim-face (if (<= depth 0)
                          'rainbow-delimiters-unmatched-face
                        (rainbow-delimiters-depth-face depth)))
          (end-pos (save-excursion (forward-sexp) (point))))
      ;; (when (eq depth -1) (message "Unmatched delimiter at char %s." loc))
      (add-text-properties (- 1 loc) (+ 1 end-pos)
                           `(font-lock-face ,delim-face
                             rear-nonsticky t)))))


(defsubst rainbow-delimiters-unpropertize-delimiter (loc)
  "Remove text properties set by rainbow-delimiters mode from char at LOC."
  (with-silent-modifications
    (remove-text-properties loc (1+ loc)
                            '(font-lock-face nil
                              rear-nonsticky nil))))

(defvar rainbow-delimiters-escaped-char-predicate nil)
(make-variable-buffer-local 'rainbow-delimiters-escaped-char-predicate)

(defvar rainbow-delimiters-escaped-char-predicate-list
  '((emacs-lisp-mode . rainbow-delimiters-escaped-char-predicate-emacs-lisp)
    (inferior-emacs-lisp-mode . rainbow-delimiters-escaped-char-predicate-emacs-lisp)
    (lisp-mode . rainbow-delimiters-escaped-char-predicate-lisp)
    (scheme-mode . rainbow-delimiters-escaped-char-predicate-lisp)
    (clojure-mode . rainbow-delimiters-escaped-char-predicate-lisp)
    (inferior-scheme-mode . rainbow-delimiters-escaped-char-predicate-lisp)
    ))

(defun rainbow-delimiters-escaped-char-predicate-emacs-lisp (loc)
  (or (and (eq (char-before loc) ?\?) ; e.g. ?) - deprecated, but people use it
           (not (and (eq (char-before (1- loc)) ?\\) ; special case: ignore ?\?
                     (eq (char-before (- loc 2)) ?\?))))
      (and (eq (char-before loc) ?\\) ; escaped char, e.g. ?\) - not counted
           (eq (char-before (1- loc)) ?\?))))

(defun rainbow-delimiters-escaped-char-predicate-lisp (loc)
  (eq (char-before loc) ?\\))

(defsubst rainbow-delimiters-char-ineligible-p (loc)
  "Return t if char at LOC should be skipped, e.g. if inside a comment.

Returns t if char at loc meets one of the following conditions:
- Inside a string.
- Inside a comment.
- Is an escaped char, e.g. ?\)"
  (let ((parse-state (syntax-ppss loc)))
    (or
     (nth 3 parse-state)                ; inside string?
     (nth 4 parse-state)                ; inside comment?
     (and rainbow-delimiters-escaped-char-predicate
          (funcall rainbow-delimiters-escaped-char-predicate loc)))))


(defsubst rainbow-delimiters-apply-color (delim depth loc)
  "Apply color for DEPTH to DELIM at LOC following user settings.

DELIM is a string specifying delimiter type.
DEPTH is the delimiter depth, or corresponding face # if colors are repeating.
LOC is location of character (delimiter) to be colorized."
  (and
   ;; Ensure user has enabled highlighting of this delimiter type.
   (symbol-value (intern-soft
                  (concat "rainbow-delimiters-highlight-" delim "s-p")))
   (rainbow-delimiters-propertize-delimiter loc
                                            depth)))


;;; JIT-Lock functionality

;; Used to skip delimiter-by-delimiter `rainbow-delimiters-propertize-region'.
(defconst rainbow-delimiters-delim-regex "\\(\(\\|\)\\|\\[\\|\\]\\|\{\\|\}\\)"
  "Regex matching all opening and closing delimiters the mode highlights.")

;; main function called by jit-lock:
(defsubst rainbow-delimiters-propertize-region (start end)
  "Highlight delimiters in region between START and END.

Used by jit-lock for dynamic highlighting."
  (setq rainbow-delimiters-escaped-char-predicate
        (cdr (assoc major-mode rainbow-delimiters-escaped-char-predicate-list)))
  (save-excursion
    (goto-char start)
    ;; START can be anywhere in buffer; determine the nesting depth at START loc
    (let ((depth (rainbow-delimiters-depth start)))
      (while (and (< (point) end)
                  (re-search-forward rainbow-delimiters-delim-regex end t))
        (backward-char) ; re-search-forward places point after delim; go back.
        (unless (rainbow-delimiters-char-ineligible-p (point))
          (let ((delim (char-after (point))))
            (cond ((eq ?\( delim)
                   (setq depth (1+ depth))
                   (rainbow-delimiters-apply-color "paren" depth (point)))
                  ((eq ?\) delim)
                   (rainbow-delimiters-apply-color "paren" depth (point))
                   (setq depth (or (and (<= depth 0) 0) ; unmatched paren
                                   (1- depth))))
                  ((eq ?\[ delim)
                   (setq depth (1+ depth))
                   (rainbow-delimiters-apply-color "bracket" depth (point)))
                  ((eq ?\] delim)
                   (rainbow-delimiters-apply-color "bracket" depth (point))
                   (setq depth (or (and (<= depth 0) 0) ; unmatched bracket
                                   (1- depth))))
                  ((eq ?\{ delim)
                   (setq depth (1+ depth))
                   (rainbow-delimiters-apply-color "brace" depth (point)))
                  ((eq ?\} delim)
                   (rainbow-delimiters-apply-color "brace" depth (point))
                   (setq depth (or (and (<= depth 0) 0) ; unmatched brace
                                   (1- depth)))))))
        ;; move past delimiter so re-search-forward doesn't pick it up again
        (forward-char)))))

(defun rainbow-delimiters-unpropertize-region (start end)
  "Remove highlighting from delimiters between START and END."
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
                (re-search-forward rainbow-delimiters-delim-regex end t))
      ;; re-search-forward places point 1 further than the delim matched:
      (rainbow-delimiters-unpropertize-delimiter (1- (point))))))


;;; Minor mode:

;;;###autoload
(define-minor-mode rainbow-delimiters-mode
  "Highlight nested parentheses, brackets, and braces according to their depth."
  nil "" nil ; No modeline lighter - it's already obvious when the mode is on.
  (if (not rainbow-delimiters-mode)
      (progn
        (jit-lock-unregister 'rainbow-delimiters-propertize-region)
        (rainbow-delimiters-unpropertize-region (point-min) (point-max)))
    (jit-lock-register 'rainbow-delimiters-propertize-region t)
    ;; Create necessary syntax tables inheriting from current major-mode.
    (set (make-local-variable 'rainbow-delimiters-syntax-table)
         (rainbow-delimiters-make-syntax-table (syntax-table)))))

;;;###autoload
(defun rainbow-delimiters-mode-enable ()
  (rainbow-delimiters-mode 1))

;;;###autoload
(defun rainbow-delimiters-mode-disable ()
  (rainbow-delimiters-mode 0))

;;;###autoload
(define-globalized-minor-mode global-rainbow-delimiters-mode
  rainbow-delimiters-mode rainbow-delimiters-mode-enable)

(provide 'rainbow-delimiters)

;;; rainbow-delimiters.el ends here.
