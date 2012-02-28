package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.ArgumentOutOfRange;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates;

import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/28/12
 * Time: 3:40 PM
 * To change this template use File | Settings | File Templates.
 */
public class LispCharTable extends LispObject {
    private LispObject myDefault;
    private LispObject myParent;
    private LispSymbol myPurpose;
    private LispSubCharTable myAscii;
    private LispVector myContent; //size == 64
    private ArrayList<LObject> myExtras;  //size == 1

    @Override
    public LObject evaluate(Environment environment) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }
    
    public LispCharTable(LispSymbol purpose, @Nullable LispObject init) {
        LObject n = purpose.getProperty("char-table-extra-slots");
        int nExtras = 0;
        if (!n.equals(LispSymbol.ourNil)) {
            if (!BuiltinPredicates.isWholeNumber(n))
                throw new WrongTypeArgumentException("wholenump", n.toString());
            nExtras = ((LispInteger)n).getData();
            if (nExtras > 10)
                throw new ArgumentOutOfRange(n, LispSymbol.ourNil);
        }
        //todo: use nExtras =)
        myParent = LispSymbol.ourNil;
        myDefault = init == null ? LispSymbol.ourNil : init;
        myPurpose = purpose;
        //todo: content must be indexed with all chars
        myContent = LispVector.make(1, myDefault);
    }


}
