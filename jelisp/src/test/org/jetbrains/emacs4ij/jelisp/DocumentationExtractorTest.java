package org.jetbrains.emacs4ij.jelisp;

import org.junit.Assert;
import org.junit.Test;

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
        String ourEmacsSource = "/home/kate/Downloads/emacs 23.2a/emacs-23.2";
        DocumentationExtractor d = new DocumentationExtractor(ourEmacsSource + "/src");
        // it is not = alias null
        Assert.assertEquals(1, d.scanAll());
    }
}