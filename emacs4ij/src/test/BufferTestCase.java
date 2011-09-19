import com.intellij.testFramework.fixtures.CodeInsightFixtureTestCase;
import junit.framework.Assert;
import org.jetbrains.emacs4ij.IdeaEditor;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 9/19/11
 * Time: 2:54 PM
 * To change this template use File | Settings | File Templates.
 */
public class BufferTestCase extends CodeInsightFixtureTestCase {
    Environment myEnvironment;
    Parser myParser;
    String myTestsPath = "/home/kate/emacs4ij/emacs4ij/src/testSrc/";
    HashMap<String, IdeaEditor> myTests;
    ArrayList<String> myTestFiles;

    @Override
    public void setUp() throws Exception {
        super.setUp();
        myTests = new HashMap<String, IdeaEditor>();
        myTestFiles = new ArrayList<String>();
        myTestFiles.add("1.txt");
        myTestFiles.add("2.txt");

        myEnvironment = new Environment(new Environment());
        myParser = new Parser();

        for (String fileName: myTestFiles) {
            myFixture.configureByFile(myTestsPath + fileName);
            myTests.put(fileName, new IdeaEditor(fileName, getEditor()));
            myEnvironment.defineBuffer(new IdeaEditor(fileName, getEditor()));
        }

        /*FileEditorManager editorManager = FileEditorManager.getInstance(getProject());
        VirtualFile[] openFiles = editorManager.getOpenFiles();
        Editor q = editorManager.getSelectedTextEditor(); */
    }

    private LispObject eval (String lispCode) {
        return myParser.parseLine(lispCode).evaluate(myEnvironment);
    }

    @Test
    public void testCurrentBuffer() {
        LispObject lispObject = eval("(current-buffer)");
        Assert.assertEquals(myTests.get(myTestFiles.get(myTestFiles.size()-1)), lispObject);
    }

    @Test
    public void testBufferp() {
        LispObject lispObject = eval("(bufferp (current-buffer))");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
    }

    @Test (expected = WrongNumberOfArgumentsException.class)
    public void testBufferpWrongNargs() {
        //eval("(bufferp)");
        try {
            eval("(bufferp)");
        } catch (WrongNumberOfArgumentsException e) {
        }
    }

    @Test
    public void testGetBufferByName() {
        LispObject lispObject = eval("(get-buffer \"1.txt\")");
        Assert.assertEquals(myTests.get("1.txt"), lispObject);
    }

    @Test
    public void testGetBufferByBuffer() {
        LispObject lispObject = eval("(get-buffer (current-buffer))");
        Assert.assertEquals(myTests.get(myTestFiles.get(myTestFiles.size()-1)), lispObject);
    }

    @Test
    public void testBufferSize() {
        LispObject lispObject = eval("(buffer-size)");
        Assert.assertEquals(new LispInteger(28), lispObject);
    }

    @Test
    public void testBufferName () {
        LispObject lispObject = eval("(buffer-name (get-buffer \"1.txt\"))");
        Assert.assertEquals(new LispString("1.txt"), lispObject);
    }

    /*@Ignore
    public void testSwitchToBufferByName () {
        LispObject lispObject = eval("(switch-to-buffer \"1.txt\")");
        Assert.assertEquals(new LispString("1.txt"), lispObject);
    }

    @Ignore
    public void testSwitchToBufferByBuffer () {
        LispObject lispObject = eval("(switch-to-buffer (get-buffer \"2.txt\"))");
        Assert.assertEquals(new LispString("2.txt"), lispObject);
    }  /*




/*
        mySymbols.put("set-buffer", new LispSymbol("set-buffer", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("switch-to-buffer", new LispSymbol("switch-to-buffer", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("other-buffer", new LispSymbol("other-buffer", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("point", new LispSymbol("point", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("point-min", new LispSymbol("point-min", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("point-max", new LispSymbol("point-max", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("buffer-end", new LispSymbol("buffer-end", LispSymbol.FunctionType.BuiltIn)); //note: it is compiled lisp function in emacs
        mySymbols.put("goto-char", new LispSymbol("goto-char", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("forward-char", new LispSymbol("forward-char", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("backward-char", new LispSymbol("backward-char", LispSymbol.FunctionType.BuiltIn));
     */
}
