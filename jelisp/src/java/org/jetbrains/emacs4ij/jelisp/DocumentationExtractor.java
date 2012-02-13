package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispSubroutine;
import org.jetbrains.emacs4ij.jelisp.exception.DocumentationExtractorException;
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

    private void scanFile (File file) throws DocumentationExtractorException {
        BufferedReader reader;
        try {
            reader = new BufferedReader(new FileReader(file));
        } catch (FileNotFoundException e) {
            throw new DocumentationExtractorException(e.getMessage());
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
                            throw new DocumentationExtractorException("function definition&documentation found, but documentation is not finished! File = " + file.getName() + ", function = " + subroutine);
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
                if (getUndocumentedSubroutines().isEmpty())
                    break;
                scanFile(file);
            }

        List<String> str = getUndocumentedSubroutines();

        /*for (String s: str) {
            System.out.println("Undocumented: " + s);
        }        */
        System.out.println("Undocumented: " + str.size());
        return str.size();
    }

    //------------

    private int n;

    private String lineContainsAnySubroutineDefinition2 (String line) {
        String defun = "DEFUN (\"";
        if (line.contains(defun)) {
            int k = line.indexOf(defun) + defun.length();
            String subroutine = line.substring(k, line.indexOf("\"", k));
            return subroutine;
        }
        return null;
    }

    private void scanFile2 (BufferedWriter writer, File file) {

        BufferedReader reader;
        try {
            reader = new BufferedReader(new FileReader(file));
        } catch (FileNotFoundException e) {
            throw new RuntimeException("File not found: " + file.getName());
        }

        String line = readLine(reader, file.getName());
        String subroutine;
        final String myDocStartFlag = "doc: /* ";
        final String myDocEndFlag = " */";
        while (line != null) {
            if ((subroutine = lineContainsAnySubroutineDefinition2(line)) != null) {
                while (!line.contains(myDocStartFlag)) {
                    line = readLine(reader, file.getName());
                    if (line == null) {
                        try {
                            String s = "function definition found, but no documentation! File = " + file.getName() + ", function = " + subroutine;
                            System.out.println(s);
                            writer.write(s);
                            n++;
                        }  catch (IOException e) {
                            throw new RuntimeException("Write error.");
                        }

                        try {
                            reader.close();
                            writer.flush();
                            return;
                        } catch (IOException e) {
                            throw new RuntimeException(e.getMessage());
                        }
                    }
                }

                if (line.contains(myDocEndFlag)) {
                    try {
                        writer.write(" == " + subroutine + " == \n" + line.substring(line.indexOf(myDocStartFlag) + myDocStartFlag.length()-1,
                                line.indexOf(myDocEndFlag)) + "\n\n");
                        n++;
                    } catch (StringIndexOutOfBoundsException e1) {
                        throw new RuntimeException("Write error.");
                    } catch (IOException e) {
                        throw new RuntimeException("Write error.");
                    }

                } else {
                    String doc = line.substring(line.indexOf(myDocStartFlag) + myDocStartFlag.length());
                    while (true) {
                        line = readLine(reader, file.getName());
                        if (line == null) {
                            try {
                                String s = "function definition&documentation found, but documentation is not finished! File = " + file.getName() + ", function = " + subroutine;
                                System.out.println(s);
                                writer.write(s);
                                n++;
                            }  catch (IOException e) {
                                throw new RuntimeException("Write error.");
                            }

                            try {
                                reader.close();
                                writer.flush();
                                return;
                            } catch (IOException e) {
                                throw new RuntimeException(e.getMessage());
                            }
                        }
                        if (line.contains(myDocEndFlag)) {
                            doc += '\n' + line.substring(0, line.indexOf(myDocEndFlag)-1);

                            try {
                                writer.write(" == " + subroutine + " == \n" + doc + "\n\n");
                                n++;
                            } catch (IOException e) {
                                throw new RuntimeException("Write error.");
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
            writer.flush();
        } catch (IOException e) {
            throw new RuntimeException(e.getMessage());
        }
    }

    public void scanAll2 () {
        File sourceDir = new File(mySourcePath);
        File[] cSrc = sourceDir.listFiles(new FilenameFilter() {
            @Override
            public boolean accept(File file, String s) {
                return (s.endsWith(".c") && !s.startsWith("."));
            }
        });
        n = 0;
        if (cSrc!=null && cSrc.length != 0) {
            File out = new File(mySourcePath + "/all.txt");
            BufferedWriter writer;
            try {
                out.createNewFile();
                writer = new BufferedWriter(new FileWriter(out));
            } catch (IOException e) {
                throw new RuntimeException("Write error: " + out.getName());
            }

            for (File file: cSrc) {
                scanFile2(writer, file);
            }

            try {
                writer.write('\n' + Integer.toString(n));
                writer.close();
            } catch (IOException e) {
                throw new RuntimeException("Close error: " + out.getName());
            }
        }

    }

}
