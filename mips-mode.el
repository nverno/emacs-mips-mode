;;; mips-mode.el --- Major-mode for MIPS assembly
;;
;; Copyright (C) 2016 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: September 8, 2016
;; Modified: September 22, 2016
;; Version: 1.1.0
;; Keywords: mips assembly
;; Homepage: https://github.com/hlissner/emacs-mips-mode
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; A major mode for MIPS Assembly based off [haxor-mode]. Written for the
;; MIPS Assembly track on exercism.io. A MIPS interpreter such as spim
;; must be installed for the code evaluation features.
;;
;;; Code:

(defgroup mips nil
  "Major mode for editing MIPS assembly"
  :prefix "mips-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/hlissner/emacs-mips-mode")
  :link '(emacs-commentary-link :tag "Commentary" "ng2-mode"))

(defcustom mips-tab-width tab-width
  "Width of a tab for MIPS mode"
  :tag "Tab width"
  :group 'mips
  :type 'integer)

(defcustom mips-interpreter "spim"
  "Interpreter to run mips code in"
  :tag "MIPS Interpreter"
  :group 'mips
  :type 'string)

;; -------------------------------------------------------------------
;;; Util

(defsubst mips--interpreter-buffer-name ()
  "Return a buffer name for the preferred mips interpreter"
  (format "*%s*" mips-interpreter))

(defun mips--interpreter-file-arg ()
  "Return the appropriate argument to accept a file for the current mips interpreter"
  (cond ((equal (file-name-nondirectory mips-interpreter) "spim") "-file")))

;; -------------------------------------------------------------------
;;; Font-Lock

(defconst mips-keywords
  '(;; Arithmetic insturctions
    "add"
    "sub"
    "addi"
    "addu"
    "subu"
    "addiu"
    ;; Multiplication/division
    "mult"
    "div"
    "rem"
    "multu"
    "divu"
    "mfhi"
    "mflo"
    "mul"
    "mulu"
    "mulo"
    "mulou"
    ;; Bitwise operations
    "not"
    "and"
    "or"
    "nor"
    "xor"
    "andi"
    "ori"
    "xori"
    ;; Shifts
    "sll"
    "srl"
    "sra"
    "sllv"
    "srlv"
    "srav"
    ;; Comparisons
    "seq"
    "sne"
    "sgt"
    "sgtu"
    "sge"
    "sgeu"
    "slt"
    "sltu"
    "slti"
    "sltiu"
    ;; Jump/branch
    "j"
    "jal"
    "jr"
    "jalr"
    "beq"
    "bne"
    "syscall"
    ;; Load/store
    "lui"
    "lb"
    "lbu"
    "lh"
    "lhu"
    "lw"
    "lwl"
    "lwr"
    "sb"
    "sh"
    "sw"
    "swl"
    "swr"
    ;; Concurrent load/store
    "ll"
    "sc"
    ;; Trap handling
    "break"
    "teq"
    "teqi"
    "tge"
    "tgei"
    "tgeu"
    "tgeiu"
    "tlt"
    "tlti"
    "tltu"
    "tltiu"
    "tne"
    "tnei"
    "rfe"
    ;; Pseudoinstructions
    "b"
    "bal"
    "bge"
    "bgt"
    "ble"
    "blt"
    "bgeu"
    "bleu"
    "bltu"
    "bgtu"
    "bgez"
    "blez"
    "bgtz"
    "bltz"
    "bnez"
    "beqz"
    "bltzal"
    "bgezal"
    "bgtu"
    "la"
    "li"
    "move"
    "movz"
    "movn"
    "nop"
    "clear"
    ;; Deprecated branch-hint pseudoinstructions
    "beql"
    "bnel"
    "bgtzl"
    "bgezl"
    "bltzl"
    "blezl"
    "bltzall"
    "bgezall"
    ;; Floating point instuctions
    ;; Arithmetic
    "add.s"
    "add.d"
    "sub.s"
    "sub.d"
    "mul.s"
    "mul.d"
    "div.s"
    "div.d"
    ;; Comparison
    "c.lt.s"
    "c.lt.d"
    "c.gt.s"
    "c.gt.d"
    "madd.s"
    "madd.d"
    "msub.s"
    "msub.d"
    "movt.s"
    "movt.d"
    "movn.s"
    "movn.d"
    "movz.s"
    "movz.d"
    "trunc.w.d"
    "trunc.w.s"
    ;; Conversion
    "cvt.s.d"
    "cvt.d.s"
    ;; Math
    "abs.s"
    "abs.d"
    "sqrt.s"
    "sqrt.d"
    ;; Load-store
    "l.s"
    "l.d"
    "s.s"
    "s.d"
    ))

(defconst mips-defs
  '(".align"
    ".ascii"
    ".asciiz"
    ".byte"
    ".data"
    ".double"
    ".extern"
    ".float"
    ".globl"
    ".half"
    ".kdata"
    ".ktext"
    ".space"
    ".text"
    ".word"))

(defconst mips-font-lock-defaults
  `((;; numbers
     ("\\_<-?[0-9]+\\>" . font-lock-constant-face)
     ;; stuff enclosed in "
     ("\"\\.\\*\\?" . font-lock-string-face)
     ;; labels
     ("[a-zA-Z][a-zA-Z_0-9]*:" . font-lock-function-name-face)
     (,(regexp-opt mips-keywords 'words) . font-lock-keyword-face)
     ;; coprocessor load-store instructions
     ("[sl]wc[1-9]" . font-lock-keyword-face)
     (,(regexp-opt mips-defs) . font-lock-preprocessor-face)
     ;; registers
     ("$\\(f?[0-2][0-9]\\|f?3[01]\\|[ft]?[0-9]\\|[vk][01]\\|a[0-3]\\|s[0-7]\\|[gsf]p\\|ra\\|at\\|zero\\)" . font-lock-type-face)
     ;; ("$\\([a-z0-9]\\{2\\}\\|zero\\)" . font-lock-constant-face)
     ;; special characters
     (":\\|,\\|;\\|{\\|}\\|=>\\|@\\|\\$\\|=" . font-lock-builtin-face))))

;; -------------------------------------------------------------------
;;; Indentation

(defvar mips--label-re "^[ \t]*[a-zA-Z][a-zA-Z_0-9]*:")

;; are we on a label line
(defsubst mips--label-line-p ()
  (beginning-of-line)
  (looking-at-p mips--label-re))

(defun mips--calculate-indent-level ()
  "Returns the number of spaces indenting the last label."
  (save-excursion
    (if (mips--label-line-p)
        (current-indentation)
      (if (mips--goto-last-label-line)
          (+ mips-tab-width (current-indentation))
        (current-indentation)))))

;; goto last label line if there is one, otherwise nil -- moves point
(defun mips--goto-last-label-line ()
  (ignore-errors
    (and (re-search-backward "^[ \t]*[a-zA-Z][a-zA-Z_0-9]*:")
        (line-number-at-pos))))

;; works like python indent, multiple tabs in a row adjusts indent
(defun mips-indent (&optional region)
  (interactive)
  (let ((ci (current-indentation))
        (need (mips--calculate-indent-level)))
    (save-excursion
      (if (and (not region) (equal last-command 'indent-for-tab-command))
          (if (/= ci 0)
              (indent-line-to (* (/ (- ci 1) mips-tab-width) mips-tab-width))
            (indent-line-to (+ mips-tab-width need)))
        (indent-line-to need)))
    (if (< (current-column) (current-indentation))
        (forward-to-indentation 0))))

(defun mips-dedent ()
  (interactive)
  (indent-line-to (- (mips--calculate-indent-level)
                     mips-tab-width)))

;; -------------------------------------------------------------------
;;; Commands

(defun mips-run-buffer ()
  "Run the current buffer in a mips interpreter, and display the output in another window"
  (interactive)
  (let ((tmp-file (format "/tmp/mips-%s" (file-name-base))))
    (write-region (point-min) (point-max) tmp-file nil nil nil nil)
    (mips-run-file tmp-file)
    (delete-file tmp-file)))

(defun mips-run-region ()
  "Run the current region in a mips interpreter, and display the output in another window"
  (interactive)
  (let ((tmp-file (format "/tmp/mips-%s" (file-name-base))))
    (write-region (region-beginning) (region-end) tmp-file nil nil nil nil)
    (mips-run-file tmp-file)
    (delete-file tmp-file)))

(defun mips-run-file (&optional filename)
  "Run the file in a mips interpreter, and display the output in another window.
The interpreter will open filename. If filename is nil, it will open the current
buffer's file"
  (interactive (list (or filename buffer-file-name)))
  (when (buffer-live-p (get-buffer (mips--interpreter-buffer-name)))
    (kill-buffer (mips--interpreter-buffer-name)))
  (set-process-sentinel
   (start-process mips-interpreter
                  (mips--interpreter-buffer-name)
                  mips-interpreter (mips--interpreter-file-arg) filename)
   #'(lambda (p _m)
       (with-current-buffer (mips--interpreter-buffer-name)
         (if (not (zerop (process-exit-status p)))
             (comint-mode)
           (view-mode))
         (pop-to-buffer (current-buffer))))))

(defun mips-goto-label (&optional label)
  (interactive)
  (let ((label (or label (read-minibuffer "Go to Label: "))))
    (goto-char (point-min))
    (re-search-forward (format "[ \t]*%s:" label))))

(defun mips-goto-label-at-cursor ()
  (interactive)
  (mips-goto-label (symbol-at-point)))

;; move point to next column, add spaces if necessary
;; with ARG move backwards column
(defun mips-goto-next-column (&optional arg)
  (interactive "P")
  (let* ((cc (current-column))
         (colnum (/ cc mips-tab-width))
         (spaces (- (* mips-tab-width (1+ colnum))
                    cc)))
    (move-to-column (+ (if arg
                           (- spaces (* 2 mips-tab-width))
                         spaces)
                       cc)
                    t)))

(defun mips-goto-prev-column ()
  (interactive)
  (mips-goto-next-column 'back))

(defvar mips-map
  (let ((map (make-keymap)))
    (define-key map (kbd "<backtab>")    'mips-dedent)
    (define-key map (kbd "C-<return>")   'mips-goto-next-column)
    (define-key map (kbd "C-M-<return>") 'mips-goto-prev-column)
    (define-key map (kbd "C-c C-c")      'mips-run-buffer)
    (define-key map (kbd "C-c C-r")      'mips-run-region)
    (define-key map (kbd "C-c C-l")      'mips-goto-label-at-cursor)
    map)
  "Keymap for mips-mode")

;; -------------------------------------------------------------------
;;; Major Mode

;;;###autoload
(define-derived-mode mips-mode prog-mode "MIPS Assembly"
  "Major mode for editing MIPS assembler code."

  (setq font-lock-defaults mips-font-lock-defaults)
  (when mips-tab-width
    (setq tab-width mips-tab-width))

  (setq comment-start "#")
  (setq comment-end "")

  (use-local-map mips-map)
  (setq indent-line-function 'mips-indent)

  (modify-syntax-entry ?. "w" mips-mode-syntax-table)
  (modify-syntax-entry ?# "< b" mips-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" mips-mode-syntax-table))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mips\\'" . mips-mode))

(provide 'mips-mode)
;;; mips-mode.el ends here
