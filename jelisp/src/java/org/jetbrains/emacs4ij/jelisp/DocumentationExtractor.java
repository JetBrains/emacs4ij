package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispSubroutine;
import org.jetbrains.emacs4ij.jelisp.subroutine.Subroutine;

import java.io.*;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
    private String mySourcePath;
    private HashMap<String, String> mySubroutineDoc;

    public DocumentationExtractor (String srcPath) {
        mySourcePath = srcPath;
        mySubroutineDoc = new HashMap<String, String>();

        for (Class c: LispSubroutine.getSubroutineContainers()) {
            Method[] methods = c.getMethods();
            for (Method m: methods) {
                Subroutine annotation = m.getAnnotation(Subroutine.class);
                if (annotation == null)
                    continue;
                String name = annotation.value();
                /*if (!annotation.doc().equals("")) {
                    System.out.println(c.getSimpleName() + '.' + name + " is already documented.");
                    continue;
                } */
                mySubroutineDoc.put(name, null);
            }
        }
    }

    public HashMap<String, String> getSubroutineDoc() {
        return mySubroutineDoc;
    }

    private List<String> getUndocumentedSubroutines()  {
        ArrayList<String> undocumentedSubroutines = new ArrayList<String>();
        for (Map.Entry<String, String> subroutineDoc: mySubroutineDoc.entrySet()) {
            if (subroutineDoc.getValue() == null)
                undocumentedSubroutines.add(subroutineDoc.getKey());
        }
        return undocumentedSubroutines;
    }

    private String lineContainsAnySubroutineDefinition (String line) {
        for (String subroutine: getUndocumentedSubroutines()) {
            if (line.contains("DEFUN (\"" + subroutine + "\","))
                return subroutine;
        }
        return null;
    }

    private String readLine (BufferedReader reader, String fileName) {
        try {
            return reader.readLine();
        } catch (IOException e) {
            throw new RuntimeException("Error while reading " + fileName);
        }
    }

    private void scanFile (File file) {
        BufferedReader reader;
        try {
            reader = new BufferedReader(new FileReader(file));
        } catch (FileNotFoundException e) {
            throw new RuntimeException("File not found: " + file);
        }

        String line = readLine(reader, file.getName());
        String subroutine;
        final String myDocStartFlag = "doc: /* ";
        final String myDocEndFlag = " */";
        while (line != null) {
            if ((subroutine = lineContainsAnySubroutineDefinition(line)) != null) {
                while (!line.contains(myDocStartFlag)) {
                    line = readLine(reader, file.getName());
                    if (line == null)
                        throw new RuntimeException("function definition found, but no documentation! File = " + file.getName() + ", function = " + subroutine);
                }

                if (line.contains(myDocEndFlag)) {
                    mySubroutineDoc.put(subroutine,
                            line.substring(line.indexOf(myDocStartFlag) + myDocStartFlag.length(),
                                           line.indexOf(myDocEndFlag)));
                } else {
                    String doc = line.substring(line.indexOf(myDocStartFlag) + myDocStartFlag.length());
                    while (true) {
                        line = readLine(reader, file.getName());
                        if (line == null)
                            throw new RuntimeException("function definition&documentation found, but documentation is not finished! File = " + file.getName() + ", function = " + subroutine);
                        if (line.contains(myDocEndFlag)) {
                            doc += '\n' + line.substring(0, line.indexOf(myDocEndFlag)-1);
                            mySubroutineDoc.put(subroutine, doc);
                            break;
                        }
                        doc += '\n' + line;
                    }
                }
                if (getUndocumentedSubroutines().isEmpty())
                    break;
            }
            line = readLine(reader, file.getName());
        }
        try {
            reader.close();
        } catch (IOException e) {
            throw new RuntimeException(e.getMessage());
        }
    }

    public int scanAll () {
        File sourceDir = new File(mySourcePath);
        File[] cSrc = sourceDir.listFiles(new FilenameFilter() {
            @Override
            public boolean accept(File file, String s) {
                return (s.endsWith(".c"));
            }
        });
        for (File file: cSrc) {
            if (getUndocumentedSubroutines().isEmpty())
                    break;
            scanFile(file);
        }

        List<String> str = getUndocumentedSubroutines();

        for (String s: str) {
            System.out.println("Undocumented: " + s);
        }

        return str.size();
    }

}
