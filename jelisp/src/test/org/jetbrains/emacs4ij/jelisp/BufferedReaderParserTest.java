package org.jetbrains.emacs4ij.jelisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Test;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;

/**
 * Created by IntelliJ IDEA.
 * User: Kate
 * Date: 25.07.11
 * Time: 15:14
 * To change this template use File | Settings | File Templates.
 */
public class BufferedReaderParserTest {


    @Test
    public void testParseSplitString() throws IOException {
        StringReader r = new StringReader ("\"one\ntwo\"");
        BufferedReader br = new BufferedReader(r);
        BufferedReaderParser bufferedReaderParser = new BufferedReaderParser(br);
        String s;
        try {
            s = br.readLine();
        } catch (IOException e) {
            throw e;
        }
        LObject lispObject = bufferedReaderParser.parse(s);
        Assert.assertEquals(new LispString("one\ntwo"), lispObject);
    }

    @Test
    public void testParseList() throws IOException {
        BufferedReader br = new BufferedReader(new StringReader("(defun test (\n) \"doc\ndoc\ndoc()\" (message\n\"test\")\n)"));
        BufferedReaderParser bufferedReaderParser = new BufferedReaderParser(br);
        String s;
        try {
            s = br.readLine();
        } catch (IOException e) {
            throw e;
        }
        LObject lispObject = bufferedReaderParser.parse(s);
        Assert.assertEquals(LispList.list(new LispSymbol("defun"), new LispSymbol("test"), LispList.list(), new LispString("doc\ndoc\ndoc()"), LispList.list(new LispSymbol("message"), new LispString("test"))), lispObject);
    }
    
    @Test
    public void testParseFromFile() {
        GlobalEnvironment.setEmacsSource("/home/kate/Downloads/emacs-23.4");
        LispList when = GlobalEnvironment.getDefFromFile(GlobalEnvironment.getEmacsSource() + "/lisp/subr.el", "when");
        ForwardParser p = new ForwardParser();
        String def = "(defmacro when (cond &rest body)\n" +
                "  \"If COND yields non-nil, do BODY, else return nil.\n" +
                "When COND yields non-nil, eval BODY forms sequentially and return\n" +
                "value of last one, or nil if there are none.\n" +
                "\n" +
                "\\(fn COND BODY...)\"\n" +
                "  (declare (indent 1) (debug t))\n" +
                "  (list 'if cond (cons 'progn body)))";
        LObject parsed = p.parseLine(def);
        Assert.assertEquals(parsed, when);
    }
    
    @Test
    public void testParseDefineMinorMode() {
        try {
            GlobalEnvironment.setEmacsSource("/home/kate/Downloads/emacs-23.4");
            LObject dmm = GlobalEnvironment.getDefFromFile(GlobalEnvironment.getEmacsSource() + "/lisp/emacs-lisp/easy-mmode.el", "define-minor-mode");
            String def = "(defmacro define-minor-mode (mode doc &optional init-value lighter keymap &rest body)\n" +
                    "  \"Define a new minor mode MODE.\n" +
                    "This defines the control variable MODE and the toggle command MODE.\n" +
                    "DOC is the documentation for the mode toggle command.\n" +
                    "\n" +
                    "Optional INIT-VALUE is the initial value of the mode's variable.\n" +
                    "Optional LIGHTER is displayed in the modeline when the mode is on.\n" +
                    "Optional KEYMAP is the default keymap bound to the mode keymap.\n" +
                    "  If non-nil, it should be a variable name (whose value is a keymap),\n" +
                    "  or an expression that returns either a keymap or a list of\n" +
                    "  arguments for `easy-mmode-define-keymap'.  If KEYMAP is not a symbol,\n" +
                    "  this also defines the variable MODE-map.\n" +
                    "\n" +
                    "BODY contains code to execute each time the mode is enabled or disabled.\n" +
                    "  It is executed after toggling the mode, and before running MODE-hook.\n" +
                    "  Before the actual body code, you can write keyword arguments, i.e.\n" +
                    "  alternating keywords and values.  These following special keywords\n" +
                    "  are supported (other keywords are passed to `defcustom' if the minor\n" +
                    "  mode is global):\n" +
                    "\n" +
                    ":group GROUP\tCustom group name to use in all generated `defcustom' forms.\n" +
                    "\t\tDefaults to MODE without the possible trailing \\\"-mode\\\".\n" +
                    "\t\tDon't use this default group name unless you have written a\n" +
                    "\t\t`defgroup' to define that group properly.\n" +
                    ":global GLOBAL\tIf non-nil specifies that the minor mode is not meant to be\n" +
                    "\t\tbuffer-local, so don't make the variable MODE buffer-local.\n" +
                    "\t\tBy default, the mode is buffer-local.\n" +
                    ":init-value VAL\tSame as the INIT-VALUE argument.\n" +
                    ":lighter SPEC\tSame as the LIGHTER argument.\n" +
                    ":keymap MAP\tSame as the KEYMAP argument.\n" +
                    ":require SYM\tSame as in `defcustom'.\n" +
                    "\n" +
                    "For example, you could write\n" +
                    "  (define-minor-mode foo-mode \\\"If enabled, foo on you!\\\"\n" +
                    "    :lighter \\\" Foo\\\" :require 'foo :global t :group 'hassle :version \\\"27.5\\\"\n" +
                    "    ...BODY CODE...)\"\n" +
                    "  (declare (debug (&define name stringp\n" +
                    "\t\t\t   [&optional [&not keywordp] sexp\n" +
                    "\t\t\t    &optional [&not keywordp] sexp\n" +
                    "\t\t\t    &optional [&not keywordp] sexp]\n" +
                    "\t\t\t   [&rest [keywordp sexp]]\n" +
                    "\t\t\t   def-body)))\n" +
                    "\n" +
                    "  ;; Allow skipping the first three args.\n" +
                    "  (cond\n" +
                    "   ((keywordp init-value)\n" +
                    "    (setq body (list* init-value lighter keymap body)\n" +
                    "\t  init-value nil lighter nil keymap nil))\n" +
                    "   ((keywordp lighter)\n" +
                    "    (setq body (list* lighter keymap body) lighter nil keymap nil))\n" +
                    "   ((keywordp keymap) (push keymap body) (setq keymap nil)))\n" +
                    "\n" +
                    "  (let* ((last-message (make-symbol \"last-message\"))\n" +
                    "         (mode-name (symbol-name mode))\n" +
                    "\t (pretty-name (easy-mmode-pretty-mode-name mode lighter))\n" +
                    "\t (globalp nil)\n" +
                    "\t (set nil)\n" +
                    "\t (initialize nil)\n" +
                    "\t (group nil)\n" +
                    "\t (type nil)\n" +
                    "\t (extra-args nil)\n" +
                    "\t (extra-keywords nil)\n" +
                    "\t (require t)\n" +
                    "\t (hook (intern (concat mode-name \"-hook\")))\n" +
                    "\t (hook-on (intern (concat mode-name \"-on-hook\")))\n" +
                    "\t (hook-off (intern (concat mode-name \"-off-hook\")))\n" +
                    "\t keyw keymap-sym)\n" +
                    "\n" +
                    "    ;; Check keys.\n" +
                    "    (while (keywordp (setq keyw (car body)))\n" +
                    "      (setq body (cdr body))\n" +
                    "      (case keyw\n" +
                    "\t(:init-value (setq init-value (pop body)))\n" +
                    "\t(:lighter (setq lighter (purecopy (pop body))))\n" +
                    "\t(:global (setq globalp (pop body)))\n" +
                    "\t(:extra-args (setq extra-args (pop body)))\n" +
                    "\t(:set (setq set (list :set (pop body))))\n" +
                    "\t(:initialize (setq initialize (list :initialize (pop body))))\n" +
                    "\t(:group (setq group (nconc group (list :group (pop body)))))\n" +
                    "\t(:type (setq type (list :type (pop body))))\n" +
                    "\t(:require (setq require (pop body)))\n" +
                    "\t(:keymap (setq keymap (pop body)))\n" +
                    "\t(t (push keyw extra-keywords) (push (pop body) extra-keywords))))\n" +
                    "\n" +
                    "    (setq keymap-sym (if (and keymap (symbolp keymap)) keymap\n" +
                    "\t\t       (intern (concat mode-name \"-map\"))))\n" +
                    "\n" +
                    "    (unless set (setq set '(:set 'custom-set-minor-mode)))\n" +
                    "\n" +
                    "    (unless initialize\n" +
                    "      (setq initialize '(:initialize 'custom-initialize-default)))\n" +
                    "\n" +
                    "    (unless group\n" +
                    "      ;; We might as well provide a best-guess default group.\n" +
                    "      (setq group\n" +
                    "\t    `(:group ',(intern (replace-regexp-in-string\n" +
                    "\t\t\t\t\"-mode\\\\'\" \"\" mode-name)))))\n" +
                    "\n" +
                    "    (unless type (setq type '(:type 'boolean)))\n" +
                    "\n" +
                    "    `(progn\n" +
                    "       ;; Define the variable to enable or disable the mode.\n" +
                    "       ,(if (not globalp)\n" +
                    "\t    `(progn\n" +
                    "\t       (defvar ,mode ,init-value ,(format \"Non-nil if %s is enabled.\n" +
                    "Use the command `%s' to change this variable.\" pretty-name mode))\n" +
                    "\t       (make-variable-buffer-local ',mode))\n" +
                    "\n" +
                    "\t  (let ((base-doc-string\n" +
                    "                 (concat \"Non-nil if %s is enabled.\n" +
                    "See the command `%s' for a description of this minor mode.\"\n" +
                    "                         (if body \"\n" +
                    "Setting this variable directly does not take effect;\n" +
                    "either customize it (see the info node `Easy Customization')\n" +
                    "or call the function `%s'.\"))))\n" +
                    "\t    `(defcustom ,mode ,init-value\n" +
                    "\t       ,(format base-doc-string pretty-name mode mode)\n" +
                    "\t       ,@set\n" +
                    "\t       ,@initialize\n" +
                    "\t       ,@group\n" +
                    "\t       ,@type\n" +
                    "\t       ,@(unless (eq require t) `(:require ,require))\n" +
                    "               ,@(nreverse extra-keywords))))\n" +
                    "\n" +
                    "       ;; The actual function.\n" +
                    "       (defun ,mode (&optional arg ,@extra-args)\n" +
                    "\t ,(or doc\n" +
                    "\t      (format (concat \"Toggle %s on or off.\n" +
                    "Interactively, with no prefix argument, toggle the mode.\n" +
                    "With universal prefix ARG turn mode on.\n" +
                    "With zero or negative ARG turn mode off.\n" +
                    "\\\\{%s}\") pretty-name keymap-sym))\n" +
                    "\t ;; Use `toggle' rather than (if ,mode 0 1) so that using\n" +
                    "\t ;; repeat-command still does the toggling correctly.\n" +
                    "\t (interactive (list (or current-prefix-arg 'toggle)))\n" +
                    "\t (let ((,last-message (current-message)))\n" +
                    "           (setq ,mode\n" +
                    "                 (cond\n" +
                    "                  ((eq arg 'toggle) (not ,mode))\n" +
                    "                  (arg (> (prefix-numeric-value arg) 0))\n" +
                    "                  (t\n" +
                    "                   (if (null ,mode) t\n" +
                    "                     (message\n" +
                    "                      \"Toggling %s off; better pass an explicit argument.\"\n" +
                    "                      ',mode)\n" +
                    "                     nil))))\n" +
                    "           ,@body\n" +
                    "           ;; The on/off hooks are here for backward compatibility only.\n" +
                    "           (run-hooks ',hook (if ,mode ',hook-on ',hook-off))\n" +
                    "           (if (called-interactively-p 'any)\n" +
                    "               (progn\n" +
                    "                 ,(if globalp `(customize-mark-as-set ',mode))\n" +
                    "                 ;; Avoid overwriting a message shown by the body,\n" +
                    "                 ;; but do overwrite previous messages.\n" +
                    "                 (unless (and (current-message)\n" +
                    "                              (not (equal ,last-message\n" +
                    "                                          (current-message))))\n" +
                    "                   (message ,(format \"%s %%sabled\" pretty-name)\n" +
                    "                            (if ,mode \"en\" \"dis\"))))))\n" +
                    "\t (force-mode-line-update)\n" +
                    "\t ;; Return the new setting.\n" +
                    "\t ,mode)\n" +
                    "\n" +
                    "       ;; Autoloading a define-minor-mode autoloads everything\n" +
                    "       ;; up-to-here.\n" +
                    "       :autoload-end\n" +
                    "\n" +
                    "       ;; Define the minor-mode keymap.\n" +
                    "       ,(unless (symbolp keymap)\t;nil is also a symbol.\n" +
                    "\t  `(defvar ,keymap-sym\n" +
                    "\t     (let ((m ,keymap))\n" +
                    "\t       (cond ((keymapp m) m)\n" +
                    "\t\t     ((listp m) (easy-mmode-define-keymap m))\n" +
                    "\t\t     (t (error \"Invalid keymap %S\" m))))\n" +
                    "\t     ,(format \"Keymap for `%s'.\" mode-name)))\n" +
                    "\n" +
                    "       (add-minor-mode ',mode ',lighter\n" +
                    "\t\t       ,(if keymap keymap-sym\n" +
                    "\t\t\t  `(if (boundp ',keymap-sym) ,keymap-sym))))))";
            ForwardParser p = new ForwardParser();
            LObject parsed = p.parseLine(def);
            Assert.assertEquals(parsed, dmm);
        } catch (Exception e) {
            System.err.println(e.getMessage());
        }
    }
}
