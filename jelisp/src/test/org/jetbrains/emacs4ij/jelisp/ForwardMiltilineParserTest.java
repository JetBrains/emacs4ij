package org.jetbrains.emacs4ij.jelisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.parser.ForwardParser;
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
public class ForwardMiltilineParserTest {
    @Test
    public void testParseSplitString() throws IOException {
        StringReader r = new StringReader ("\"one\ntwo\"");
        BufferedReader br = new BufferedReader(r);
        TestForwardMultilineParser bufferedReaderParser = new TestForwardMultilineParser(br, "test");
        String s;
        s = br.readLine();
        LispObject lispObject = bufferedReaderParser.parse(s, 0);
        Assert.assertEquals(new LispString("one\ntwo"), lispObject);
    }

    @Test
    public void testParseList() throws IOException {
        BufferedReader br = new BufferedReader(new StringReader("(defun test (\n) \"doc\ndoc\ndoc()\" (message\n\"test\")\n)"));
        TestForwardMultilineParser bufferedReaderParser = new TestForwardMultilineParser(br, "test");
        String s;
        s = br.readLine();
        LispObject lispObject = bufferedReaderParser.parse(s, 0);
        Assert.assertEquals(LispList.list(new LispSymbol("defun"), new LispSymbol("test"), LispList.list(),
                new LispString("doc\ndoc\ndoc()"), LispList.list(new LispSymbol("message"), new LispString("test"))),
                lispObject);
    }

    @Test
    public void testParseFromFile() {
        GlobalEnvironment.setEmacsSource("/home/kate/Downloads/emacs-23.4");
        LispList when = DefinitionLoader.getDefFromFile(GlobalEnvironment.getEmacsSource() + "/lisp/subr.el", "when",
                DefinitionLoader.DefType.FUN);
        ForwardParser p = new ForwardParser();
        String def = "(defmacro when (cond &rest body)\n" +
                "  \"If COND yields non-nil, do BODY, else return nil.\n" +
                "When COND yields non-nil, eval BODY forms sequentially and return\n" +
                "value of last one, or nil if there are none.\n" +
                "\n" +
                "\\(fn COND BODY...)\"\n" +
                "  (declare (indent 1) (debug t))\n" +
                "  (list 'if cond (cons 'progn body)))";
        LispObject parsed = p.parseLine(def);
        Assert.assertEquals(parsed, when);
    }

    @Test
    public void testParseString() throws IOException {
        StringReader r = new StringReader ("(while (not (memq (setq command (read-event highlight)) '(? return)))\n" +
                "\t  (cond\n" +
                "\t   ((eq command ?\\M-n)\n" +
                "\t    (setq line (min (1+ line) max-lines)))\n" +
                "\t   ((eq command ?\\M-p)\n" +
                "\t    (setq line (max (1- line) 0))))\n" +
                "\t  (setq highlight (ecomplete-highlight-match-line matches line)))");
        BufferedReader br = new BufferedReader(r);
        TestForwardMultilineParser bufferedReaderParser = new TestForwardMultilineParser(br, "test");
        String s;
        s = br.readLine();
        LispObject lispObject = bufferedReaderParser.parse(s, 1);
        Assert.assertTrue(lispObject instanceof LispList);
    }

    @Test
    public void testParseStringWithManyDefs() throws IOException {
        StringReader r = new StringReader("(defvar problems)       (defvar \nqlist) a");
        BufferedReader br = new BufferedReader(r);
        TestForwardMultilineParser p = new TestForwardMultilineParser(br, "test");
        String s;
        s = br.readLine();
        LispObject lispObject = p.parse(s, 0);
        Assert.assertEquals(LispList.list(new LispSymbol("defvar"), new LispSymbol("problems")), lispObject);
        lispObject = p.parseNext();
        Assert.assertEquals(LispList.list(new LispSymbol("defvar"), new LispSymbol("qlist")), lispObject);
        lispObject = p.parseNext();
        Assert.assertEquals(new LispSymbol("a"), lispObject);
        Assert.assertTrue(p.isFinished());
    }

    @Test
    public void parseHugeVarDef() throws IOException {
        String def = "(defvar desktop-minor-mode-handlers\n" +
                "  nil\n" +
                "  \"Alist of functions to restore non-standard minor modes.\n" +
                "Functions are called by `desktop-create-buffer' to restore minor modes.\n" +
                "List elements must have the form\n" +
                "\n" +
                "   (MINOR-MODE . RESTORE-FUNCTION).\n" +
                "\n" +
                "Minor modes not specified here, are restored by the standard minor mode\n" +
                "function.\n" +
                "\n" +
                "Handlers are called with argument list\n" +
                "\n" +
                "   (DESKTOP-BUFFER-LOCALS)\n" +
                "\n" +
                "Furthermore, they may use the following variables:\n" +
                "\n" +
                "   desktop-file-version\n" +
                "   desktop-buffer-file-name\n" +
                "   desktop-buffer-name\n" +
                "   desktop-buffer-major-mode\n" +
                "   desktop-buffer-minor-modes\n" +
                "   desktop-buffer-point\n" +
                "   desktop-buffer-mark\n" +
                "   desktop-buffer-read-only\n" +
                "   desktop-buffer-misc\n" +
                "\n" +
                "When a handler is called, the buffer has been created and the major mode has\n" +
                "been set, but local variables listed in desktop-buffer-locals has not yet been\n" +
                "created and set.\n" +
                "\n" +
                "Modules that define a minor mode that needs a special handler should contain\n" +
                "code like\n" +
                "\n" +
                "   (defun foo-desktop-restore\n" +
                "   ...\n" +
                "   (add-to-list 'desktop-minor-mode-handlers\n" +
                "                '(foo-mode . foo-desktop-restore))\n" +
                "\n" +
                "Furthermore the minor mode function must be autoloaded.\n" +
                "\n" +
                "See also `desktop-minor-mode-table'.\")";
        StringReader r = new StringReader(def);
        BufferedReader br = new BufferedReader(r);
        TestForwardMultilineParser p = new TestForwardMultilineParser(br, "test");
        String s;
        s = br.readLine();
        LispObject lispObject = p.parse(s, 0);
        Assert.assertTrue(lispObject instanceof LispList);
    }
}
