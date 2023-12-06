;;; tbg-jump.el --- ▮ -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020-2027, by 3badguys

;; Author: 3badguys
;; Version: ▮
;; Created: ▮
;; Package-Requires: ▮
;; Keywords: ▮
;; License: GPL v3

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:


(defcustom tbg-jump-tags-file-name "TAGS"
  "Tags file name."
  :group 'tbg-jump
  :type 'string)

(defcustom tbg-jump-ctags-program-name "ctags"
  "Name of ctags(Universal Ctags) program."
  :group 'tbg-jump
  :type 'string)

(defcustom tbg-jump-ctags-program-path nil
  "Path of ctags(Universal Ctags) program.
1. nil, automatically detect.
2. not nil, full path of ctags."
  :group 'tbg-jump
  :type 'string)

(defcustom tbg-jump-project-root-marks '(".git" ".svn")
  "The marks used to locate project root directory."
  :group 'tbg-jump
  :type '(repeat 'string))

(defcustom tbg-jump-project-root nil
  "Project root directory."
  :group 'tbg-jump
  :type 'string)

(defcustom tbg-jump-snippet-before-context-lines 2
  "The count of lines before snippet."
  :group 'tbg-jump
  :type 'integer)

(defcustom tbg-jump-snippet-after-context-lines 3
  "The count of lines after snippet."
  :group 'tbg-jump
  :type 'integer)

(defvar tbg-jump-search-min nil "The minimum point for searching output buffer.")
(defvar tbg-jump-search-max nil "The maximum point for searching output buffer.")

(defvar tbg-jump-ctags-cache (make-hash-table :test 'equal)
  "Cache of ctags files content.")

(defcustom tbg-jump-header-separator
  "hh━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
  "A string as visual separator."
  :group 'tbg-jump)

(defcustom tbg-jump-file-separator
  "ff━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
  "A string as visual separator."
  :group 'tbg-jump)

(defcustom tbg-jump-snippet-separator
  "ss━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n"
  "A string as visual separator."
  :group 'tbg-jump)

(defcustom tbg-jump-tag-in-header-prefix
  "〖"
  "A left-bracket string that marks matched tagname which in header part."
  :group 'tbg-jump)

(defcustom tbg-jump-tag-in-header-postfix
  "〗"
  "A right-bracket string that marks matched tagname which in header part."
  :group 'tbg-jump)

(defcustom tbg-jump-tag-prefix
  "『"
  "A left-bracket string that marks matched tagname and navigate previous/next."
  :group 'tbg-jump)

(defcustom tbg-jump-tag-postfix
  "』"
  "A right-bracket string that marks matched tagname and navigate previous/next."
  :group 'tbg-jump)

(defcustom tbg-jump-filepath-prefix
  "〘"
  "A left-bracket string used to mark file path and navigate previous/next."
  :group 'tbg-jump)

(defcustom tbg-jump-filepath-postfix
  "〙"
  "A right-bracket string used to mark file path and navigate previous/next."
  :group 'tbg-jump)

(defface tbg-jump-tag-in-header-highlight
  '((t :foreground "blue"
       :background "white"
       :weight bold
       :underline t
       ))
  "Face for matched tag which in header part."
  :group 'tbg-jump)

(defface tbg-jump-tag-highlight
  '((t :foreground "black"
       :background "yellow"
       ))
  "Face for matched tag."
  :group 'tbg-jump)

(defface tbg-jump-file-path-highlight
  '((t :foreground "black"
       :background "pink"
       ))
  "Face of file path where a tag match is found."
  :group 'tbg-jump)


(defun tbg-jump--locate-project-root ()
  "Find the root of project."
  (let (($project-root (cl-some (apply-partially 'locate-dominating-file
                                                default-directory)
                               tbg-jump-project-root-marks)))
    (or tbg-jump-project-root (and $project-root (file-name-as-directory $project-root)))))

(defun tbg-jump--locate-tags-file ()
  "Find tags file, search parent directories."
  (let (($tags-dir (locate-dominating-file default-directory tbg-jump-tags-file-name)))
    (and $tags-dir (file-truename (concat (file-name-as-directory $tags-dir)
                                         tbg-jump-tags-file-name)))))

(defun tbg-jump--get-cygwin-program-path ($executable-name $drive-list)
  "Get path of executable file named $EXECUTABLE-NAME in cygwin system.
Search the device in $DRIVE-LIST."
  (catch 'found-path
    (mapc
     #'(lambda ($drive)
         (let (($path
                (format "%s:\\\\cygwin64\\\\bin\\\\%s.exe" $drive $executable-name)))
           (when (file-exists-p $path) (throw 'found-path $path))))
     $drive-list)
    nil))

(defun tbg-jump--get-program-path ($executable-name)
  "Get path of executable file named $EXECUTABLE-NAME."
  (let (($device-list '("c" "d" "e" "f" "g" "h" "a" "b" "i"
                        "j" "k" "l" "m" "n" "o" "p" "q" "r"
                        "s" "t" "u" "v" "w" "x" "y" "z")))
    (or (executable-find $executable-name)
        (and (string-equal system-type "windows-nt")
             (tbg-jump--get-cygwin-program-path $executable-name $device-list)))))

(defun tbg-jump--universal-ctags-p (@ctags-program)
  "Check @CTAGS-PROGRAM whether is Universal Ctags."
  (let (($version-ouput (shell-command-to-string
                         (concat @ctags-program " --version"))))
    (string-match-p "Universal Ctags" $version-ouput)))

(defun tbg-jump--create-tags-file-async (@src-root)
  "Create tags in @SRC-ROOT file async."
  (let (($ctags-program (or tbg-jump-ctags-program-path
                            (tbg-jump--get-program-path tbg-jump-ctags-program-name))))
    (or (tbg-jump--universal-ctags-p $ctags-program)
        (error "Ctags isn't Universal Ctags! Package tbg-jump just support Universal Ctags!"))
    (when $ctags-program
      (start-process-shell-command
       ""
       nil
       (format "%s -f %s -e -R %s"
               $ctags-program
               (expand-file-name tbg-jump-tags-file-name (directory-file-name @src-root))
               (directory-file-name @src-root)))
      (message "created tags async through start-process-shell-command."))))

;;;###autoload
(defun tbg-jump-scan-code (&optional @src-dir)
  "Use ctags to scan code at @SRC-DIR, and generate the TAGS file."
  (interactive)
  (let (($src-root (or @src-dir
                       (read-directory-name "SrcCode root: " (tbg-jump--locate-project-root)))))
    (tbg-jump--create-tags-file-async $src-root)))

(defun tbg-jump--tags-file-precheck ()
  "Do some pretreat operations."
  (let (($tags-file (tbg-jump--locate-tags-file)))
    (when (not $tags-file)
      (error "Can't find %s file. Please run `tbg-jump-scan-code'!" tbg-jump-tags-file-name))))

(defun tbg-jump--tag-at-point ()
  "Get the tag at point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let (($bounds (bounds-of-thing-at-point 'symbol)))
      (and $bounds (buffer-substring-no-properties (car $bounds) (cdr $bounds))))))

(defun tbg-jump--read-file (@file)
  "Read @FILE content."
  (with-temp-buffer
    (insert-file-contents @file)
    (buffer-string)))

(defun tbg-jump--tag-search-regex (@tag)
  "Get the regex to search @TAG in tags file. And @TAG could be nil."
  (concat "\\([^\^?\^A\n]+\\)\^?\\("
          (or @tag "[^\^?\^A\n]+")
          "\\)\^A\\([0-9]+\\),\\([0-9]+\\)"))

(defun tbg-jump--search-tag-candidates (@file-content @tag)
  "Search from @FILE-CONTENT and return the candidates of @TAG."
  (let (($tag-re (tbg-jump--tag-search-regex @tag)) $tag-matched
        ($cands '()))
    (with-temp-buffer
      (insert @file-content)
      (goto-char (point-min))
      (while (re-search-forward $tag-re nil "NOERROR")
        (setq $tag-matched (match-string-no-properties 2))
        ;; Skip the following candidates:
        ;; 1. Anonymous extra tag start with __anon
        ;;    For example, enum/struct/union/class/namespace of C/C++ language.
        ;; 2. Number tag, and it always can't be a symbol in most languages
        ;;    For example, array of json.
        (or (string-match-p "__anon" $tag-matched)
            (string-match-p "^[0-9]+$" $tag-matched)
            (add-to-list '$cands
                         (list :text (match-string-no-properties 1)
                               :tag $tag-matched
                               :line-number (string-to-number (match-string-no-properties 3))
                               :position (string-to-number (match-string-no-properties 4))
                               :src-file (etags-file-of-tag t))
                         "APPEND"))))
    $cands))

(defun tbg-jump--current-date-time-string ()
  "Return current date-time string in this format 「2021-04-28T18:06:51+08:00」"
  (concat
   (format-time-string "%Y-%m-%dT%T")
   (funcall (lambda (x) (format "%s:%s" (substring x 0 3) (substring x 3 5)))
            (format-time-string "%z"))))

(defun tbg-jump--insert-header (@tbg-jump-context @buffer)
  "Insert header into @BUFFER. The jump context info stored in @TBG-JUMP-CONTEXT."
  (let (($tag (plist-get @tbg-jump-context :tag))
        ($orignal-file (plist-get @tbg-jump-context :orignal-file))
        ($orignal-pos (plist-get @tbg-jump-context :orignal-pos)))

    (with-current-buffer @buffer
      (insert
       (concat
        "-*- coding: utf-8; mode: tbg-jump -*-" "\n"
        "Datetime: " (tbg-jump--current-date-time-string) "\n"
        (format "Search tag: %s%s%s\n"
                tbg-jump-tag-in-header-prefix
                (propertize (or $tag "ALL")
                            'tbg-jump-filepath $orignal-file
                            'tbg-jump-pos $orignal-pos
                            'mouse-face 'highlight)
                tbg-jump-tag-in-header-postfix)
        tbg-jump-header-separator)))))

(defun tbg-jump--insert-one-source-snippet (@cand_index @one-cand @buffer)
  "Insert source snippet via $ONE-CAND into @BUFFER.
The index of candidate is @CAND_INDEX."
  (let (($tag (plist-get @one-cand :tag))
        ($src-file (plist-get @one-cand :src-file))
        ($line-number (plist-get @one-cand :line-number))
        $snippet-begin $match-line-begin $snippet-end
        $snippet-before-block $snippet-middle-block $snippet-after-block
        $tag-re $tag-begin $tag-end)
    (with-temp-buffer
      (insert-file-contents $src-file)
      (cond
       ((> (1- $line-number) tbg-jump-snippet-before-context-lines)
        (progn
          (forward-line (- (1- $line-number) tbg-jump-snippet-before-context-lines))
          (setq $snippet-begin (point))
          (forward-line tbg-jump-snippet-before-context-lines)
          (setq $match-line-begin (point))))
       (t
        (progn
          (setq $snippet-begin (point))
          (forward-line (1- $line-number))
          (setq $match-line-begin (point)))))
      (forward-line (1+ tbg-jump-snippet-after-context-lines))
      (setq $snippet-end (point))

      (goto-char $match-line-begin)

      ;; tag will contain redundant spaces. For example, operator overloading of C++ language.
      (setq $tag-re (replace-regexp-in-string "[ \t]+" "[ \t]*" (regexp-quote $tag)))

      (when (search-forward-regexp $tag-re (line-end-position) "NOERROR")
        (setq $tag-begin (match-beginning 0))
        (setq $tag-end (match-end 0))
        (put-text-property $tag-begin $tag-end 'tbg-jump-tag
                           (buffer-substring-no-properties $tag-begin $tag-end))
        (put-text-property $tag-begin $tag-end 'tbg-jump-filepath $src-file)
        (put-text-property $tag-begin $tag-end 'tbg-jump-line-number $line-number)
        (add-text-properties $tag-begin $tag-end '(mouse-face highlight)))

      (setq $snippet-before-block (buffer-substring-no-properties $snippet-begin $tag-begin))
      (setq $snippet-after-block (buffer-substring-no-properties $tag-end $snippet-end))
      (setq $snippet-middle-block (buffer-substring $tag-begin $tag-end)))

    (with-current-buffer @buffer
      (insert
       (concat
        ;; insert source file path info.
        (format "%d %s%s%s\n"
                @cand_index
                tbg-jump-filepath-prefix
                (propertize $src-file
                            'tbg-jump-filepath $src-file
                            'mouse-face 'highlight)
                tbg-jump-filepath-postfix)
        tbg-jump-file-separator
        ;; insert snippet info.
        $snippet-before-block
        tbg-jump-tag-prefix
        $snippet-middle-block
        tbg-jump-tag-postfix
        $snippet-after-block
        tbg-jump-snippet-separator)))))

(defun tbg-jump--switch-to-output (@buffer)
  "Switch to @BUFFER and highlight stuff."
  (switch-to-buffer @buffer)
  (setq buffer-read-only t)
  (goto-char (point-min))
  (search-forward (format "Search tag: %s" tbg-jump-tag-in-header-prefix) nil "NOERROR")
  (setq tbg-jump-search-min (point))
  (setq tbg-jump-search-max (1- (point-max)))
  (tbg-jump-mode))

(defun tbg-jump--output-candidates (@tag @cands)
  "Output candidates of @TAG stored in @CANDS."
  (let ($tbg-jump-context
        ($buffer-name "*tbg-jump output*")
        $output-buffer ($cand_index 0))
    (setq $tbg-jump-context
          (list :tag @tag
                :orignal-file (buffer-file-name)
                :orignal-pos (point)))

    (when (get-buffer $buffer-name) (kill-buffer $buffer-name))
    (setq $output-buffer (generate-new-buffer $buffer-name))

    (switch-to-buffer-other-window $output-buffer)
    (tbg-jump--insert-header $tbg-jump-context $output-buffer)
    (mapc (lambda ($one-cand)
            (setq $cand_index (1+ $cand_index))
            (tbg-jump--insert-one-source-snippet $cand_index $one-cand $output-buffer))
          @cands)
    (princ "Done." $output-buffer)
    (tbg-jump--switch-to-output $output-buffer)))

(defun tbg-jump--jump-to-location-internal (@tag @filepath @pos @line-number)
  "Open @FILEPATH. Then jump to specific position:
1. Jump to position @POS.
2. Jump to @LINE-NUMBER. Then search @TAG."
  (when (and @filepath (file-exists-p @filepath))
    (or (equal (buffer-file-name) @filepath) (find-file-other-window @filepath))
    (if @pos
        (goto-char @pos)
      (when @line-number
        (goto-char (point-min))
        (forward-line (1- @line-number))
        (and @tag (search-forward @tag (line-end-position) "NOERROR"))))))

(defun tbg-jump--jump-one-candidate-location (@one-cand)
  "Jump to the location of one candidate @ONE-CAND."
  (let (($tag (plist-get @one-cand :tag))
        ($src-file (plist-get @one-cand :src-file))
        ($line-number (plist-get @one-cand :line-number)))
    (tbg-jump--jump-to-location-internal $tag $src-file nil $line-number)))

(defun tbg-jump--get-cache-tags-content (@tags-file)
  "Read @TAGS-FILE content through ctags cache."
  (let (($tags-info (gethash @tags-file tbg-jump-ctags-cache)))
    (plist-get $tags-info :content)))

(defun tbg-jump--get-cache-tags-filesize (@tags-file)
  "Read filesize of @TAGS-FILE through ctags cache."
  (let (($tags-info (gethash @tags-file tbg-jump-ctags-cache)))
    (or (plist-get $tags-info :filesize) 0)))

(defun tbg-jump--search-tags-file (@tag)
  "Search @TAG in tags file."
  (let (($tags-file (tbg-jump--locate-tags-file))
        $tags-file-size $tags-file-content
        $cands)
    (when (and $tags-file (file-exists-p $tags-file))
      (when (< (tbg-jump--get-cache-tags-filesize $tags-file)
               (setq $tags-file-size (nth 7 (file-attributes $tags-file))))
        (puthash $tags-file (list :content (tbg-jump--read-file $tags-file)
                                  :filesize $tags-file-size) tbg-jump-ctags-cache))

      (when (setq $tags-file-content (tbg-jump--get-cache-tags-content $tags-file))
        (setq $cands (tbg-jump--search-tag-candidates $tags-file-content @tag))))
    (cond
     ((not $cands) (message "No candidate found for tag(%s)." @tag))
     ((= 1 (length $cands)) (tbg-jump--jump-one-candidate-location (car $cands)))
     (t (tbg-jump--output-candidates @tag $cands)))))

;;;###autoload
(defun tbg-jump-find-tag-at-point ()
  "Find tag using tagname at point. Use `pop-tag-mark' to jump back."
  (interactive)
  (tbg-jump--tags-file-precheck)
  (let (($tag (tbg-jump--tag-at-point)))
    (cond
     ($tag (tbg-jump--search-tags-file $tag))
     (t (message "No tag found at point.")))))

;;;###autoload
(defun tbg-jump-list-tag ()
  "List all tags."
  (interactive)
  (tbg-jump--tags-file-precheck)
  (tbg-jump--search-tags-file nil))


(setq tbg-jump-fock-lock-keyworks
      (let (
            (xTagInHeader (format "%s\\([^%s]+\\)%s"
                                  tbg-jump-tag-in-header-prefix
                                  tbg-jump-tag-in-header-postfix
                                  tbg-jump-tag-in-header-postfix))
            (xTag (format "%s\\([^%s]+\\)%s"
                          tbg-jump-tag-prefix
                          tbg-jump-tag-postfix
                          tbg-jump-tag-postfix))
            (xfPath (format "%s\\([^%s]+\\)%s"
                            tbg-jump-filepath-prefix
                            tbg-jump-filepath-postfix
                            tbg-jump-filepath-postfix)))
        `((,xTagInHeader . (1 'tbg-jump-tag-in-header-highlight))
          (,xTag . (1 'tbg-jump-tag-highlight))
          (,xfPath . (1 'tbg-jump-file-path-highlight)))))

(defun tbg-jump-previous-filepath ()
  "Put cursor to previous filepath."
  (interactive)
  (when (search-backward tbg-jump-filepath-postfix tbg-jump-search-min "NOERROR")
    (left-char)))

(defun tbg-jump-next-filepath ()
  "Put cursor to next filepath."
  (interactive)
  (search-forward tbg-jump-filepath-prefix tbg-jump-search-max "NOERROR"))

(defun tbg-jump-previous-tag ()
  "Put cursor to previous tag."
  (interactive)
  (when (search-backward tbg-jump-tag-postfix tbg-jump-search-min "NOERROR")
    (left-char)))

(defun tbg-jump-next-tag ()
  "Put cursor to next tag."
  (interactive)
  (search-forward tbg-jump-tag-prefix tbg-jump-search-max "NOERROR"))

(defun tbg-jump-loop-jump-tag ()
  "Put cursor to next tag and loop from the tag which in header part."
  (interactive)
  (or (search-forward tbg-jump-tag-prefix tbg-jump-search-max "NOERROR")
      (goto-char tbg-jump-search-min)))

(defun tbg-jump-jump-to-location ()
  "Open the specific file and put cursor to the specific location."
  (interactive)
  (let (($tag (get-text-property (point) 'tbg-jump-tag))
        ($filepath (get-text-property (point) 'tbg-jump-filepath))
        ($pos (get-text-property (point) 'tbg-jump-pos))
        ($line-number (get-text-property (point) 'tbg-jump-line-number)))
    (tbg-jump--jump-to-location-internal $tag $filepath $pos $line-number)))

(defun tbg-jump-mouse-jump-to-location (@event)
  "Open the specific file and put cursor to the specific location."
  (interactive "e")
  (let* (($clickPos (posn-point (event-end @event)))
         ($tag (get-text-property $clickPos 'tbg-jump-tag))
         ($filepath (get-text-property $clickPos 'tbg-jump-filepath))
         ($pos (get-text-property $clickPos 'tbg-jump-pos))
         ($line-number (get-text-property $clickPos 'tbg-jump-line-number)))
    (tbg-jump--jump-to-location-internal $tag $filepath $pos $line-number)))

(defvar tbg-jump-mode-map nil "Keybinding for `tbg-jump.el output'")
(progn
  (setq tbg-jump-mode-map (make-sparse-keymap))

  (define-key tbg-jump-mode-map (kbd "<up>") 'tbg-jump-previous-filepath)
  (define-key tbg-jump-mode-map (kbd "<down>") 'tbg-jump-next-filepath)

  (define-key tbg-jump-mode-map (kbd "<left>") 'tbg-jump-previous-tag)
  (define-key tbg-jump-mode-map (kbd "<right>") 'tbg-jump-next-tag)

  (define-key tbg-jump-mode-map (kbd "TAB") 'tbg-jump-loop-jump-tag)

  (define-key tbg-jump-mode-map (kbd "RET") 'tbg-jump-jump-to-location)
  (define-key tbg-jump-mode-map (kbd "<mouse-1>") 'tbg-jump-mouse-jump-to-location))

(define-derived-mode tbg-jump-mode fundamental-mode "tbg-jump"
  "Major mode for reading output for tbg-jump commands."

  (setq font-lock-defaults '((tbg-jump-fock-lock-keyworks)))

  ;; actually no need
  (use-local-map tbg-jump-mode-map)

  :group 'tbg-jump)

(provide 'tbg-jump)

;;; tbg-jump.el ends here
