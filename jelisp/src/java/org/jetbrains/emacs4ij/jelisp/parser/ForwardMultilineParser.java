package org.jetbrains.emacs4ij.jelisp.parser;

import java.io.IOException;
import java.io.RandomAccessFile;

/**
 * Created by IntelliJ IDEA.
 * User: Kate
 * Date: 22.07.11
 * Time: 20:30
 * To change this template use File | Settings | File Templates.
 */
public class ForwardMultilineParser extends BaseForwardMultilineParser {
    private RandomAccessFile myReader;

    public ForwardMultilineParser(RandomAccessFile reader, String filename) {
        myReader = reader;
        myFilename = filename;
        myForwardParser.addObserver(this);
    }

    @Override
    protected void gotoOffset(long offset) throws IOException {
        myReader.seek(offset);
    }

    @Override
    protected String readLine() throws IOException {
        return myReader.readLine();
    }

    @Override
    protected String readerInfo() {
        return myReader.toString();
    }
}
