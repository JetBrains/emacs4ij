package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.*;
import org.jetbrains.emacs4ij.jelisp.parser.ForwardMultilineParser;
import org.jetbrains.emacs4ij.jelisp.subroutine.Predicate;

import java.io.*;
import java.util.*;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/5/12
 * Time: 1:28 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class DefinitionLoader {
    static enum DefType {VAR, FUN} //todo: not private for test only
    private static enum SymbolType {VAR, FUN, CMD}
    private static List<String> myDefVars = Arrays.asList("defcustom", "defvar", "defconst", "defgroup", "defface");
    private static List<String> myDefFuns = Arrays.asList("defun", "defmacro", "defsubst", "defalias");
    private static Map<String, File> myUploadHistory = new HashMap<>();
    private static Map<Identifier, List<String>> myIndex = new HashMap<>();
    //for test
    private static List<LispSymbol> mySkipFunctions = new ArrayList<>();

    static { //index Emacs lisp sources
        scan(new File(GlobalEnvironment.getEmacsSource() + "/lisp/"));
    }

    private static void scan (File file) {
        if (file.isDirectory()) {
            File[] files = file.listFiles();
            if (files == null)
                return;
            for (File f: files) {
                scan(f);
            }
            return;
        }
        if (file.getAbsolutePath().endsWith(".el")) {
            FileScanner.scan(file);
        }
    }

    //for test
    public static void addSkipFunctions (String... names) {
        for (String name: names)
            mySkipFunctions.add(new LispSymbol(name));
    }

    //for test
    public static void test () {}

    //for test
    public static List<String> getFileName(String name, DefType type) {
        return myIndex.get(new Identifier(name, type));
    }

    public static void loadFile (String fileName) {
        String fullName = GlobalEnvironment.getEmacsSource() + "/lisp/" + fileName;
        BufferedReader reader;
        try {
            reader = new BufferedReader(new FileReader(fullName));
        } catch (FileNotFoundException e) {
            throw new EnvironmentException(e.getMessage());
        }
        String line;
        int index = 0;
        ForwardMultilineParser p = new ForwardMultilineParser(reader, fullName);
        while (true){
            try {
                line = reader.readLine();
                index++;
            } catch (IOException e) {
                throw new ReadException(fullName +  ", line " + index);
            }
            if (line == null)
                break;

            LispObject parsed = p.parse(line, index);
            index = p.getLine();
            if (Predicate.isNil(parsed))
                continue;
            if (parsed instanceof LispList &&
                    ((LispList) parsed).car() instanceof LispSymbol
                    && mySkipFunctions.contains(((LispList) parsed).car()))
                continue;
            try {
                parsed.evaluate(GlobalEnvironment.INSTANCE);
            } catch (LispException e) {
                throw new EnvironmentException(JelispBundle.message("loader.error", fullName, p.getLine(), e.getMessage()));
            }
        }
    }

    //TODO: its only for test
    public static LispList getDefFromFile(String fileName, String functionName, DefType type) {
        return FileScanner.getDefFromFile(new File(fileName), new Identifier(functionName, type));
    }

    private static boolean containsDef (String line, String name, List<String> defForms) {
        for (String defForm: defForms) {
            if (line.contains('(' + defForm + ' ' + name + ' ')
                    || line.contains('(' + defForm + " '" + name + ' ')) {
                return true;
            }
        }
        return false;
    }

    private static LispSymbol processDef (LispList definition, String name, DefType type) {
        if (definition == null)
            return null;
        LispObject evaluated = definition.evaluate(GlobalEnvironment.INSTANCE);
        if (!(evaluated instanceof LispSymbol))
            throw new InternalError(JelispBundle.message("function.failed", "find and register emacs form", name));
        LispSymbol value = GlobalEnvironment.INSTANCE.find(((LispSymbol) evaluated).getName());
        if (value == null) {
            value = findAndRegisterEmacsForm(((LispSymbol) evaluated).getName(), type);
        }
        return value;
    }

    private static LispList lookInSubrFirst (Identifier id) {
        for (String filename: myIndex.get(id)) {
            if (filename.endsWith("/lisp/subr.el")) {
                return FileScanner.getDefFromFile(new File(filename), id);
            }
        }
        return null;
    }

    private static LispList lookExceptSubr (Identifier id) {
        for (String filename: myIndex.get(id)) {
            if (filename.endsWith("/lisp/subr.el")) {
                continue;
            }
            LispList def = FileScanner.getDefFromFile(new File(filename), id);
            if (def != null)
                return def;
        }
        return null;
    }

    private static LispSymbol findAndRegisterEmacsForm (String name, DefType type) {
        Identifier id = new Identifier(name, type);
        if (!myIndex.containsKey(id)) {
            //throw new InternalError(JelispBundle.message("unknown.lisp.object", id.toString()));
            if (type == DefType.FUN)
                throw new VoidFunctionException(name);
            throw new VoidVariableException(name);
        }


        LispList definition = getDefFromInvokersSrc(id);
        if (definition == null) {
            definition = lookInSubrFirst(id);
            if (definition == null)
                definition = lookExceptSubr(id);
        }
        return processDef(definition, name, type);
    }

    public static LispSymbol findAndRegisterEmacsFunction(String name) {
        return findAndRegisterEmacsForm(name, DefType.FUN);
    }

    public static LispSymbol findAndRegisterEmacsVariable(String name) {
        return findAndRegisterEmacsForm(name, DefType.VAR);
    }

    private static LispList getDefFromInvokersSrc(Identifier id) {
        for (String invoker: GlobalEnvironment.ourCallStack) {
            if (myUploadHistory.containsKey(invoker)) {
                LispList def = FileScanner.getDefFromFile(myUploadHistory.get(invoker), id);
                if (def != null)
                    return def;
            }
        }
        return null;
    }

    private static class Identifier {
        private final String myName;
        private final DefType myType;
        private final SymbolType mySymbolType;

        Identifier (String name, DefType type) {
            myName = name;
            myType = type;
            mySymbolType = type == DefType.VAR ? SymbolType.VAR : SymbolType.FUN;
        }

        Identifier (String name, SymbolType type) {
            myName = name;
            myType = type == SymbolType.VAR ? DefType.VAR : DefType.FUN;
            mySymbolType = type;
        }

        String getName() {
            return myName;
        }

        DefType getType() {
            return myType;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof Identifier)) return false;

            Identifier that = (Identifier) o;

            if (myName != null ? !myName.equals(that.myName) : that.myName != null) return false;
            if (myType != that.myType) return false;

            return true;
        }

        @Override
        public int hashCode() {
            int result = myName != null ? myName.hashCode() : 0;
            result = 31 * result + (myType != null ? myType.hashCode() : 0);
            return result;
        }

        @Override
        public String toString() {
            return myType + " " + myName;
        }
    }

    private static class FileScanner {
        private static int myFileIndex;
        private static File myFile;

        static void scan (File file) {
            myFile = file;
            BufferedReader reader;
            try {
                reader = new BufferedReader(new FileReader(myFile));
            } catch (FileNotFoundException e) { //don't scan =)
                return;
            }
            String line = "";
            myFileIndex = 0;
            while (true) {
                try {
                    line = reader.readLine();
                    myFileIndex++;
                } catch (IOException e) {
                    return;
                }
                if (line == null)
                    break;

                scanLine(reader, line);
            }
        }

        static LispList getDefFromFile (File file, Identifier id) {
            myFile = file;
            BufferedReader reader;
            try {
                reader = new BufferedReader(new FileReader(myFile));
            } catch (FileNotFoundException e) {
                throw new ReadException(JelispBundle.message("no.file", myFile.getName()));
            }
            String line;
            myFileIndex = 0;
            while (true) {
                try {
                    line = reader.readLine();
                    myFileIndex++;
                } catch (IOException e) {
                    throw new ReadException(myFile.getName());
                }
                if (line == null)
                    return null;
                if (containsDef(line, id.getName(), (id.getType() == DefType.FUN ? myDefFuns : myDefVars)))
                    break;
            }
            return getDef(reader, line, id.getName());
        }

        private static LispList getDef(BufferedReader reader, String line, String name) {
            ForwardMultilineParser p = new ForwardMultilineParser(reader, myFile.getAbsolutePath());
            LispObject parsed = p.parse(line, myFileIndex);
            if (parsed instanceof LispList) {
                myUploadHistory.put(name, myFile);
                myFileIndex = p.getLine();
                return (LispList) parsed;
            }
            throw new InternalException(JelispBundle.message("unexpected.object.type"));
        }

        private static void scanLine(BufferedReader reader, String line) {
            if (!scanLine(DefType.FUN, reader, line))
                scanLine(DefType.VAR, reader, line);
        }

        private static boolean scanLine (DefType type, BufferedReader reader, String line) {
            List<String> defs = type == DefType.FUN ? myDefFuns : myDefVars;
            for (String def: defs) {
                if (line.startsWith("(" + def + " ")) {
                    int start = 2 + def.length();
                    if (line.charAt(start) == '\'')
                        start++;
                    int end = line.indexOf(' ', start);
                    if (end == -1) {
                        end = line.charAt(line.length()-1) == ')' ? line.length() - 2 : line.length() - 1;
                    }
                    String name = line.substring(start, end);
                    //todo: back
                    Identifier id = new Identifier(name, type);
                    /*Identifier id = null;
                    if (type == DefType.FUN) {//check if it is command
                        try {
                            LispList definition = getDef(reader, line, name);
                            if (BuiltinPredicates.commandp(definition, null).equals(LispSymbol.ourT))
                                id = new Identifier(name, SymbolType.CMD);
                        } catch (ParserException e) {
                            if (!e.getMessage().contains("Unknown code block: )")) //this means we've found def inside other def
                                if (!e.getMessage().contains("\u001B") && !e.getMessage().contains("�")) {
//                                    System.err.println("Function " + name + ": " + e.getMessage());
                                } else {
//                                    System.err.println(myFile.getAbsolutePath());
                                }
                            //skip here
                        }
                    }
                    if (id == null)
                        id = new Identifier(name, type);*/
                    if (myIndex.containsKey(id)) {
                        List<String> files = myIndex.get(id);
                        if (files.contains(myFile.getAbsolutePath()))
                            return true;
                        files.add(myFile.getAbsolutePath());
                        return true;
                    }
                    myIndex.put(id, new ArrayList<>(Arrays.asList(myFile.getAbsolutePath())));
                    return true;
                }
            }
            return false;
        }
    }

}
