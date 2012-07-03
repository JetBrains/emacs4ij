package org.jetbrains.emacs4ij.jelisp.interactive;

import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/20/12
 * Time: 11:01 AM
 * To change this template use File | Settings | File Templates.
 */
abstract class CompletionContext {
    private final static String ourStandardNoMatchMessage = JelispBundle.message("standard.no.match.msg");
//    private LispObject myCollection = null; //GlobalEnvironment.INSTANCE.getObjectArray();
//    private LispObject myPredicate = null;
    protected boolean requiresMatch = false;
    protected boolean isMatch = true;
    protected String myNoMatchMessage;

    public void setMatch(boolean match) {
        isMatch = match;
        myNoMatchMessage = null;
    }

    public boolean isMatch() {
        return isMatch;
    }

    public boolean requiresMatch() {
        return requiresMatch;
    }

    public String getNoMatchMessage () {
        return isMatch ? ""
                : StringUtil.isEmpty(myNoMatchMessage) ? ourStandardNoMatchMessage : myNoMatchMessage;
    }

    public boolean toShowSpecialNoMatchMessage() {
        return !isMatch && !StringUtil.isEmpty(myNoMatchMessage) && !myNoMatchMessage.equals(ourStandardNoMatchMessage);
    }

    protected static ArrayList<String> fileCompletions (String parameter, final boolean isDirectory) {
        //todo: if result is not unique, show "[Complete, but not unique]"
        if (parameter.length() > 1) {
            if (parameter.charAt(0) == '~') {
                parameter = System.getProperty("user.home") + parameter.substring(1);
            }
        }
        File d = new File(parameter);
        File parent;
        final String begin;
        if (!d.exists()) {
            int lastDelimiter = parameter.lastIndexOf('/');
            if (lastDelimiter == -1) {
                return new ArrayList<>();
            } else {
                parent = new File(parameter.substring(0, lastDelimiter+1));
                begin = parameter.substring(lastDelimiter+1);
            }
        } else {
            parent = d;
            begin = "";
        }
        File[] files =  parent.listFiles(new FilenameFilter() {
            @Override
            public boolean accept(File dir, String s) {
                if (s.length() < begin.length())
                    return false;
                if (!s.substring(0, begin.length()).equals(begin))
                    return false;
                if (!isDirectory)
                    return true;
                File f = new File(dir.getAbsolutePath() + '/' + s);
                return f.isDirectory();
            }
        });
        ArrayList<String> completions = new ArrayList<>();
        for (File file: files) {
            completions.add(file.getAbsolutePath());
        }
        return completions;
    }

}
