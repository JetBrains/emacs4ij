package org.jetbrains.emacs4ij;

import com.intellij.ui.EditorTextField;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.junit.Before;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/4/11
 * Time: 4:24 PM
 * To change this template use File | Settings | File Templates.
 */
public class InteractiveTest {
    private IdeaMiniBuffer myMiniBuffer;
    private Environment myEnvironment;

    @Before
    public void setUp() throws Exception {
        myEnvironment = new Environment(new Environment());
        EditorTextField t = new EditorTextField();
        myMiniBuffer = new IdeaMiniBuffer(0, t.getEditor(), myEnvironment);
    }

    private LObject evaluateString (String lispCode) throws LispException {
        Parser parser = new Parser();
        return parser.parseLine(lispCode).evaluate(myEnvironment);
    }


}
