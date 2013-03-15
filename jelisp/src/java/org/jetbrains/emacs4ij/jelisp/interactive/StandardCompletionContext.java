package org.jetbrains.emacs4ij.jelisp.interactive;

import com.intellij.openapi.util.text.StringUtil;
import org.apache.commons.lang.NotImplementedException;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.subroutine.Predicate;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

class StandardCompletionContext extends CompletionContext {
  private char myInteractiveChar;

  public StandardCompletionContext () {
    requiresMatch = true;
    isMatch = true;
  }

  public LispObject verify (Environment environment, String parameter, String defaultValue) {
    switch (myInteractiveChar) {
      case 'a':
        LispSymbol f = environment.find(parameter);
        if (f != null && f.isFunction())
          return new LispSymbol(parameter, f.getFunction());
        break;
      case 'b':
        if (parameter.equals("")) {
          parameter = defaultValue;
          return new LispString(parameter);
        } else {
          LispBuffer b = environment.findBuffer(parameter);
          if (b != null)
            return new LispString(parameter);
        }
        break;
      case 'c': // -- Character
        return new LispInteger(Integer.parseInt(parameter));
      case 'C': //command
        LispSymbol cmd = environment.find(parameter);
        if (cmd != null && Predicate.commandp(cmd, null).equals(LispSymbol.T))
          return cmd;
        break;
      case 'D': // -- Directory name.
        if (parameter.length() > 1) {
          if (parameter.charAt(0) == '~') {
            parameter = System.getProperty("user.home") + parameter.substring(1);
          }
        }
        File dir = new File(parameter);
        if (dir.exists() && dir.isDirectory()) {
          return new LispString(parameter);
        }
        break;
      case 'f': // -- Existing file name. Directory also fits.
        if (parameter.length() > 1) {
          if (parameter.charAt(0) == '~') {
            parameter = System.getProperty("user.home") + parameter.substring(1);
          }
        }
        File file = new File(parameter);
        if (file.exists())
          return new LispString(parameter);
        break;
      case 'F': // -- Possibly nonexistent file name. -- no check
        if (parameter.length() > 1) {
          if (parameter.charAt(0) == '~') {
            parameter = System.getProperty("user.home") + parameter.substring(1);
          }
        }
        File ffile = new File(parameter);
        if (ffile.exists() && ffile.isDirectory()) {
          String[] filling = ffile.list();
          String firstFileName = null;
          if (!parameter.endsWith("/"))
            parameter += '/';
          for (String fileName: filling) {
            if (new File(parameter + fileName).isFile()) {
              firstFileName = fileName;
              break;
            }
          }
          if (firstFileName != null)
            parameter += firstFileName;
        }
        return new LispString(parameter);
      case 'G': // -- Possibly nonexistent file name, defaulting to just directory name.
        if (parameter.length() > 1) {
          if (parameter.charAt(0) == '~') {
            parameter = System.getProperty("user.home") + parameter.substring(1);
          }
        }
        return new LispString(parameter);
      case 'k': // -- Key sequence (downcase the last event if needed to get a definition).
        throw new NotImplementedException("read defined key sequence");
      case 'K': // -- Key sequence to be redefined (do not downcase the last event).
        throw new NotImplementedException("read possibly undefined key sequence");
      case 'n': // -- Number read using minibuffer.
        try {
          int n = Integer.parseInt(parameter);
          return new LispInteger(n);
        } catch (NumberFormatException e1) {
          try {
            int n = (int) Double.parseDouble(parameter);
            return new LispInteger(n);
          } catch (NumberFormatException e2) {
            //todo: don't show prompt
            break;
          }
        }
      case 'v': // -- Variable name: symbol that is user-variable-p.
        LispSymbol var = environment.find(parameter);
        if (var != null && Predicate.isUserOption(var))
          return var;
        break;
      case 'z': // -- Coding system.
        if (StringUtil.isEmpty(parameter))
          return LispSymbol.NIL;
        throw new NotImplementedException("verification for 'z'");
      case 'Z':
        //todo: if prefix-arg provided -- try to verify.
        //otherwise nil:
        return LispSymbol.NIL;
      default:
        setMatch(true);
        return null;
    }
    setMatch(false);
    return null;
  }

  public List<String> getCompletions (String parameter) {
    List<String> completions = new ArrayList<>();
    switch (myInteractiveChar) {
      case 'a':
        completions = GlobalEnvironment.INSTANCE.getFunctionList(parameter);
        break;
      case 'b': case 'B':// -- Name of existing buffer.
        completions = GlobalEnvironment.INSTANCE.getBufferNamesList(parameter);
        break;
      case 'C': // -- Command name: symbol with interactive function definition.
        completions = GlobalEnvironment.INSTANCE.getCommandList(parameter);
        break;
      case 'D': // -- Directory name.
        completions = fileCompletions(parameter, true);
        break;
      case 'f': case 'F': case 'G':
        completions = fileCompletions(parameter, false);
        break;
      case 'v': // -- Variable name: symbol that is user-variable-p.
        completions = GlobalEnvironment.INSTANCE.getUserOptions(parameter);
        break;
      case 'z': case 'Z':
        //todo
        break;
    }
    if (!completions.isEmpty())
      Collections.sort(completions);
    return completions;
  }

  public char getInteractiveChar() {
    return myInteractiveChar;
  }

  public void setInteractiveChar(char interactiveChar) {
    myInteractiveChar = interactiveChar;
  }
}
