package org.jetbrains.emacs4ij.jelisp;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import java.io.File;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/5/11
 * Time: 10:51 AM
 * To change this template use File | Settings | File Templates.
 */
public class DocumentationExtractorTest {
    @Test
    public void testScanAll() throws Exception {
        String ourEmacsSource = "/home/kate/Downloads/emacs-23.4";
        DocumentationExtractor d = new DocumentationExtractor(ourEmacsSource + "/src");
        Assert.assertEquals(5, d.scanAll());
    }

    @Ignore
    @Test
    public void testKillBufferDoc() {
        String ourEmacsSource = "/home/kate/Downloads/emacs-23.4/src";
        DocumentationExtractor d = new DocumentationExtractor(ourEmacsSource);
        d.scanFile(new File(ourEmacsSource + "/mytest.c"));
        Assert.assertNotNull(d.getSubroutineDoc("kill-buffer"));
        System.out.println(d.getSubroutineDoc("kill-buffer"));
        System.out.println(d.getSubroutineDoc("listp"));
        System.out.println(d.getSubroutineDoc("eval"));
    }
}
