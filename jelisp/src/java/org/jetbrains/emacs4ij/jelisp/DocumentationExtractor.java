package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispSubroutine;
import org.jetbrains.emacs4ij.jelisp.exception.DocumentationExtractorException;
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
 * this class is a helper for doc extraction. It affects classes in this package.
 * for each subroutine, initially defined in C source, we extract doc string and copy it to our @Subroutine annotation field 'doc'.
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
    //    private ArrayList<Subr> mySubroutines = new ArrayList<>();
//    private ArrayList<Variable> myVariables = new ArrayList<>();
    public enum VarType {LISP, LISP_NOPRO, BOOL, INT, KBOARD, PER_BUFFER}

    public DocumentationExtractor (String srcPath) {
        mySourcePath = srcPath;
        /*for (Class c: LispSubroutine.getSubroutineContainers()) {
            Method[] methods = c.getMethods();
            for (Method m: methods) {
                Subroutine annotation = m.getAnnotation(Subroutine.class);
                if (annotation == null)
                    continue;
                String name = annotation.value();
                mySubr.put(name, new Subr(name));
            }
        }*/
    }

    public String getSubroutineDoc (String name) {
        if (mySubr.containsKey(name))
            return mySubr.get(name).getDoc();
        return null;
    }

    public String getVariableDoc (String name) {
        try {
            return myVar.get(name).getDoc();
        } catch (NullPointerException e) {
            return null;
        }
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
            VarType type = VarType.valueOf(line.substring(start, end).toUpperCase());
            if (type == null)
                throw new DocumentationExtractorException("Unknown variable definition: " + myVarDef + line.substring(start, end).toUpperCase());
            Variable var = new Variable(name, type);
            myVar.put(name, var);
            return var;
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

//    private boolean isComment = false;
//    private int objectStart = 0;
//    private ObjectToDocument lineContainsAnyDefinition (String line) {
//        if (isComment) {
//            if (line.contains("*/")) {
//                isComment = false;
//                objectStart = line.indexOf("*/") + 2;
//                line = line.substring(objectStart);
//            } else
//                return null;
//        }
//        if (line.contains("/*")) {
//            int k = line.indexOf("DEF");
//            if (k > line.indexOf("/*") || k == -1) {
//                isComment = true;
//                return null;
//            }
//            ObjectToDocument object = lineContainsAnySubroutineDefinition(line);
//            if (object != null)
//                return object;
//            return lineContainsAnyVariableDefinition(line);
//            //}
//            //isComment = true;
//            //return null;
//        }
//        if (line.contains("DEF")) {
//            ObjectToDocument object = lineContainsAnySubroutineDefinition(line);
//            if (object != null)
//                return object;
//            return lineContainsAnyVariableDefinition(line);
//        }
//        return null;
//    }

    private String readLine (BufferedReader reader, String fileName) {
        try {
            return reader.readLine();
        } catch (IOException e) {
            throw new RuntimeException("Error while reading " + fileName);
        }
    }

    public void scanFile (File file) throws DocumentationExtractorException {
        BufferedReader reader;
        try {
            reader = new BufferedReader(new FileReader(file));
        } catch (FileNotFoundException e) {
            throw new DocumentationExtractorException(e.getMessage());
        }
//        isComment = false;
        String line = readLine(reader, file.getName());
        ObjectToDocument object;
        final String myDocStartFlag = "doc: /*";
        final String myDocEndFlag = "*/";
        while (line != null) {
//            if (line.contains("DEFUN (\"eval\","))
//                System.out.print(1);
//            objectStart = 0;
            if ((object = lineContainsAnyDefinition(line)) != null) {
//                isComment = false;
//                line = line.substring(objectStart);
                while (!line.contains(myDocStartFlag)) {
                    line = readLine(reader, file.getName());
                    if (line == null)
                        throw new RuntimeException("Object definition found, but no documentation! File = " + file.getName() + ", object = " + object.getName());
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
                            throw new DocumentationExtractorException("function definition&documentation found, but documentation is not finished! File = " + file.getName() + ", function = " + object);
                        if (line.contains(myDocEndFlag)) {
                            doc += '\n' + line.substring(0, line.indexOf(myDocEndFlag)-1);
                            object.setDoc(doc.trim());
                            break;
                        }
                        doc += '\n' + line;
                    }
                }
                //if (getUndocumentedSubroutines().isEmpty()) break;
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
                //if (getUndocumentedSubroutines().isEmpty()) break;
                scanFile(file);
            }

        List<String> str = getUndocumentedSubroutines();
//        Collections.sort(str);
//        for (String s: str) {
//            System.out.println(s);
//        }
        //System.out.println("Undocumented: " + str.size());
        return str.size();
    }
}

   