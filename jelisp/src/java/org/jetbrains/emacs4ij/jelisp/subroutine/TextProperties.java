package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.OddTextPropListLengthException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;

import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/10/12
 * Time: 6:17 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class TextProperties {
    private TextProperties() {}

    @Subroutine("propertize")
    public static LispObject propertize (Environment environment, LispString string, @Optional LispObject... properties) {
        int propLength = properties.length;
        if (propLength == 0 || propLength % 2 != 0)
            throw new WrongNumberOfArgumentsException("propertize");
        LispList propertyList = revert(Arrays.asList(properties));
        LispString copy = (LispString) string.copy();
        addTextProperties(environment, new LispInteger(0), new LispInteger(copy.size()), propertyList, copy);
        return copy;
    }

    @Subroutine("put-text-property")
    public static LispSymbol putTextProperty(Environment environment, MarkerOrInteger start, MarkerOrInteger end,
                                             LispObject propertyName, LispObject propertyValue,
                                             @Optional LispObject bufferOrString) {
        addTextProperties(environment, start, end, LispList.list(propertyName, propertyValue), bufferOrString);
        return LispSymbol.ourNil;
    }

    private static LispList normalizeProperties(LispObject properties, boolean revert) {
        if (properties instanceof LispList && ((LispList) properties).size() % 2 != 0)
            throw new OddTextPropListLengthException();
        if (!(properties instanceof LispList))
            return LispList.list(properties, LispSymbol.ourNil);
        if (!revert)
            return (LispList) properties;
        return revert(((LispList) properties).toLispObjectList());
    }

    private static LispList revert (List<LispObject> list) {
        ArrayDeque<LispObject> deque = new ArrayDeque<>(list.size());
        for (int i = 0; i + 1 < list.size(); i += 2) {
            deque.addFirst(list.get(i + 1));
            deque.addFirst(list.get(i));
        }
        return LispList.list(deque.toArray(new LispObject[list.size()]));
    }

    private static TextPropertiesHolder normalizeHolder (Environment environment, @Nullable LispObject holder) {
        if (Predicate.isNil(holder))
            holder = environment.getBufferCurrentForEditing();
        if (!(holder instanceof LispString) && !(holder instanceof LispBuffer))
            throw new WrongTypeArgumentException("buffer-or-string-p", holder);
        return (TextPropertiesHolder) holder;
    }

    public static void addTextProperties (LispObject start, LispObject end, LispObject properties, LispString holder) {
        if (!(start instanceof MarkerOrInteger))
            throw new WrongTypeArgumentException("integer-or-marker-p", start);
        if (!(end instanceof MarkerOrInteger))
            throw new WrongTypeArgumentException("integer-or-marker-p", end);
        LispList propertyList = normalizeProperties(properties, false);
        holder.actOnTextProperties(((MarkerOrInteger)start).getPosition(), ((MarkerOrInteger)end).getPosition(),
                propertyList, TextPropertiesHolder.Action.ADD);
    }

    @Subroutine("add-text-properties")
    public static LispSymbol addTextProperties (Environment environment, MarkerOrInteger start, MarkerOrInteger end,
                                               LispObject properties, @Optional LispObject bufferOrString) {
        TextPropertiesHolder holder = normalizeHolder(environment, bufferOrString);
        LispList propertyList = normalizeProperties(properties, false);
        return LispSymbol.bool(holder.actOnTextProperties(start.getPosition(), end.getPosition(),
                propertyList, TextPropertiesHolder.Action.ADD));
    }

    @Subroutine("set-text-properties")
    public static LispSymbol setTextProperties (Environment environment, MarkerOrInteger start, MarkerOrInteger end,
                                               LispObject properties, @Optional LispObject bufferOrString) {
        TextPropertiesHolder holder = normalizeHolder(environment, bufferOrString);
        LispList propertyList = normalizeProperties(properties, true);
        return LispSymbol.bool(holder.actOnTextProperties(start.getPosition(), end.getPosition(),
                propertyList, TextPropertiesHolder.Action.SET));
    }

    @Subroutine("remove-text-properties")
    public static LispSymbol removeTextProperties (Environment environment, MarkerOrInteger start, MarkerOrInteger end,
                                    LispObject properties, @Optional LispObject bufferOrString) {
        TextPropertiesHolder holder = normalizeHolder(environment, bufferOrString);
        if (!(properties instanceof LispList))
            return LispSymbol.ourNil;
        return LispSymbol.bool(holder.actOnTextProperties(start.getPosition(), end.getPosition(),
                (LispList) properties, TextPropertiesHolder.Action.REMOVE));
    }

    @Subroutine("remove-list-of-text-properties")
    public static LispSymbol removeListOfTextProperties (Environment environment, MarkerOrInteger start, MarkerOrInteger end,
                                                   LispObject propertyNames, @Optional LispObject bufferOrString) {
        TextPropertiesHolder holder = normalizeHolder(environment, bufferOrString);
        if (!(propertyNames instanceof LispList))
            return LispSymbol.ourNil;
        return LispSymbol.bool(holder.actOnTextProperties(start.getPosition(), end.getPosition(),
                (LispList) propertyNames, TextPropertiesHolder.Action.REMOVE_LIST));
    }

    @Subroutine("text-properties-at")
    public static LispObject textPropertiesAt(Environment environment, MarkerOrInteger position,
                                              @Optional LispObject bufferOrString) {
        return normalizeHolder(environment, bufferOrString).getTextPropertiesAt(position.getPosition());
    }

    @Subroutine("get-text-property")
    public static LispObject getTextProperty(Environment environment, MarkerOrInteger position, LispObject property,
                                             @Optional LispObject bufferOrString) {
        return normalizeHolder(environment, bufferOrString).getTextPropertyAt(position.getPosition(), property);
    }

    @Subroutine("get-char-property")
    public static LispObject getCharProperty (Environment environment, MarkerOrInteger position, LispObject property,
                                              @Optional LispObject object) {
        //todo: object may be a window
        return getTextProperty(environment, position, property, object);
    }

}
