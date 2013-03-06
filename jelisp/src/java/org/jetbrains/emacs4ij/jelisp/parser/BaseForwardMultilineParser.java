package org.jetbrains.emacs4ij.jelisp.parser;

import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.LogUtil;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.exception.ReadException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.EndOfLineException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.InvalidReadSyntax;
import org.jetbrains.emacs4ij.jelisp.parser.exception.ParserException;

import java.io.IOException;
import java.util.Observable;
import java.util.Observer;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/23/12
 * Time: 12:40 PM
 * To change this template use File | Settings | File Templates.
 */
abstract class BaseForwardMultilineParser implements Observer {
  protected ForwardParser myForwardParser = new ForwardParser();
  protected String myFilename;
  protected long myLine = 0;

  protected abstract void gotoOffset(long offset) throws IOException;
  protected abstract String readLine() throws IOException;
  protected abstract String readerInfo();

  public boolean isFinished() {
    return myForwardParser.isFinished();
  }

  public LispObject parse (String firstLine, long offset, int index) throws ParserException {
    try {
      myLine = offset;
      gotoOffset(offset);
      return myForwardParser.parseLine(firstLine, index);
    } catch (ParserException | IOException e) {
      return processException(e);
    }
  }

  public LispObject parseNext() throws ParserException {
    try {
      return myForwardParser.parseNext();
    } catch (ParserException e) {
      return processException(e);
    }
  }

  protected LispObject processException (Throwable e) {
    if (e instanceof InvalidReadSyntax) {
      LogUtil.log(myFilename + ", ln " + myLine + ": " + e.getMessage() + "\n   " + myForwardParser.getCode(), GlobalEnvironment.MessageType.ERROR);
      return null;
    }
    throw new ParserException(myFilename, myLine, e.getMessage(), myForwardParser.getCode());
  }

  public LispObject parse (String firstLine, long line) throws ParserException {
    return parse(firstLine, line, 0);
  }

  public void update(Observable o, Object arg) {
    try {
      String nextLine = readLine();
      if (nextLine == null)
        if (arg instanceof ParserException)
          throw (ParserException) arg;
        else throw new EndOfLineException();
      myLine++;
      myForwardParser.append(nextLine);
    } catch (IOException e) {
      if (arg instanceof ParserException) {
        throw (ParserException) arg;
      }
      throw new ReadException(readerInfo());
    }
  }

  //todo: fix to return line
  public long getLine() {
    return myLine;
  }
}
