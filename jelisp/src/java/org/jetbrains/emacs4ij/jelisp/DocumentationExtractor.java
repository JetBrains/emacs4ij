package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispSubroutine;
import org.jetbrains.emacs4ij.jelisp.exception.DocumentationExtractorException;
import org.jetbrains.emacs4ij.jelisp.exception.ReadException;
import org.jetbrains.emacs4ij.jelisp.subroutine.Subroutine;

import java.io.*;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/5/11
 * Time: 9:35 AM
 *
 * this class is a helper for doc extraction. 
 * for each subroutine and variable, initially defined in C source, the doc string is extracted
 *
 */
public class DocumentationExtractor {
    private class ObjectToDocument {
        String myName;
        String myDoc = null;
        public void setDoc(String doc) {
            myDoc = doc;
        }
        public String getDoc() {
            return myDoc;
        }
        public String getName() {
            return myName;
        }
    }

    private class Variable extends ObjectToDocument {
        VarType myType;

        public Variable (String name, VarType type) {
            myName = name;
            myType = type;
        }
    }

    private class Subr extends ObjectToDocument {
        public Subr(String name) {
            myName = name;
        }
    }

    private String mySourcePath;
    private HashMap<String, Subr> mySubr = new HashMap<>();
    private HashMap<String, Variable> myVar = new HashMap<>();
    public enum VarType {LISP, LISP_NOPRO, BOOL, INT, KBOARD, PER_BUFFER, BUFFER_DEFAULTS}

    public DocumentationExtractor (String srcPath) {
        mySourcePath = srcPath;
    }

    public String getSubroutineDoc (String name) {
        if (mySubr.containsKey(name))
            return mySubr.get(name).getDoc();
        return null;
    }

    public String getVariableDoc (String name) {
        if (myVar.containsKey(name))
            return myVar.get(name).getDoc();
        return null;
    }

    private List<String> getUndocumentedSubroutines()  {
        ArrayList<String> names = new ArrayList<>();
        for (Class c: LispSubroutine.getSubroutineContainers()) {
            Method[] methods = c.getMethods();
            for (Method m: methods) {
                Subroutine annotation = m.getAnnotation(Subroutine.class);
                if (annotation == null)
                    continue;
                String name = annotation.value();

                try {
                    if (getSubroutineDoc(name) == null)
                        names.add(name);
                } catch (NullPointerException e) {
                    names.add(name);
                }
            }
        }
        return names;
    }

    private Subr lineContainsAnySubroutineDefinition (String line) {
        String funDef = "DEFUN (\"";
        if (line.contains(funDef)) {
            int start = line.indexOf('"', line.indexOf(funDef)) + 1;
            if (start == 0)
                return null;
            int end = line.indexOf('"', start);
            String name = line.substring(start, end);
            Subr s = new Subr(name);
            mySubr.put(name,s);
            return s;
        }
        return null;
    }

    private Variable lineContainsAnyVariableDefinition (String line) {
        String myVarDef = "DEFVAR_";
        if (line.contains(myVarDef)) {
            int start = line.indexOf('"', line.indexOf(myVarDef)) + 1;
            if (start == 0)
                return null;
            int end = line.indexOf('"', start);
            String name = line.substring(start, end);
            start = line.indexOf('_', line.indexOf(myVarDef)) + 1;
            end = line.indexOf(' ', start);
            try {
                VarType type = VarType.valueOf(line.substring(start, end).toUpperCase());
                Variable var = new Variable(name, type);
                myVar.put(name, var);
                return var;
            } catch (IllegalArgumentException e) {
                throw new DocumentationExtractorException(JelispBundle.message("unknown.var.def", myVarDef, line.substring(start, end).toUpperCase()));
            }
        }
        return null;
    }

    private ObjectToDocument lineContainsAnyDefinition (String line) {
        if (line.contains("DEF")) {
            ObjectToDocument object = lineContainsAnySubroutineDefinition(line);
            if (object != null)
                return object;
            return lineContainsAnyVariableDefinition(line);
        }
        return null;
    }

    private String readLine (BufferedReader reader, String fileName) {
        try {
            return reader.readLine();
        } catch (IOException e) {
            throw new ReadException(fileName);
        }
    }

    public void scanFile (File file) throws DocumentationExtractorException {
        BufferedReader reader;
        try {
            reader = new BufferedReader(new FileReader(file));
        } catch (FileNotFoundException e) {
            throw new DocumentationExtractorException(e.getMessage());
        }
        String line = readLine(reader, file.getName());
        ObjectToDocument object;
        final String myDocStartFlag = "doc: /*";
        final String myDocEndFlag = "*/";
        while (line != null) {
            if ((object = lineContainsAnyDefinition(line)) != null) {
                while (!line.contains(myDocStartFlag)) {
                    line = readLine(reader, file.getName());
                    if (line == null)
                        throw new DocumentationExtractorException(JelispBundle.message("object.def.no.doc", file.getName(), object.getName()));
                }
                if (line.contains(myDocEndFlag)) {
                    int start = line.indexOf(myDocStartFlag) + myDocStartFlag.length();
                    int end = line.indexOf(myDocEndFlag);
                    String doc = (start > end) ? "" : line.substring(start, end);
                    object.setDoc(doc.trim());
                } else {
                    String doc = line.substring(line.indexOf(myDocStartFlag) + myDocStartFlag.length());
                    while (true) {
                        line = readLine(reader, file.getName());
                        if (line == null)
                            throw new DocumentationExtractorException(JelispBundle.message("unexpected.doc.end", file.getName(), object.getName()));
                        if (line.contains(myDocEndFlag)) {
                            int endStart = line.indexOf(myDocEndFlag);
                            if (endStart > 0) {
                                doc += '\n' + line.substring(0, line.indexOf(myDocEndFlag)-1);
                                object.setDoc(doc.trim());
                            }
                            break;
                        }
                        doc += '\n' + line;
                    }
                }                
            }
            line = readLine(reader, file.getName());
        }
        try {
            reader.close();
        } catch (IOException e) {
            throw new DocumentationExtractorException(e.getMessage());
        }
    }

    public int scanAll () throws DocumentationExtractorException {
        File sourceDir = new File(mySourcePath);
        File[] cSrc = sourceDir.listFiles(new FilenameFilter() {
            @Override
            public boolean accept(File file, String s) {
                return (s.endsWith(".c") && !s.startsWith("."));
            }
        });
        if (cSrc!=null && cSrc.length != 0)
            for (File file: cSrc) {                
                scanFile(file);
            }
        List<String> str = getUndocumentedSubroutines();
//        System.out.println("Undocumented: " + str.size());
//        System.out.println(str);
        return str.size();
    }
}

   