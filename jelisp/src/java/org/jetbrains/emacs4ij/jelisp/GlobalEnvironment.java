package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBufferFactory;

import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/14/11
 * Time: 11:20 AM
 * To change this template use File | Settings | File Templates.
 */
public class GlobalEnvironment extends Environment {
    private ArrayList<LispBuffer> myBuffers = new ArrayList<LispBuffer>();
    private ArrayList<LispBuffer> myDeadBuffers = new ArrayList<LispBuffer>();
    private ArrayList<LispBuffer> myServiceBuffers = new ArrayList<LispBuffer>();
    private LispBuffer myBufferCurrentForEditing = null;

    private boolean selectionManagedBySubroutine = false;

    public static String ourEmacsPath = "";
    private final LispBufferFactory myBufferFactory;
    private final Object myProject;



    public static final String ourMiniBufferName = " *Minibuf-0*";
    public static final String ourScratchBufferName = "*scratch*";
    public static final String ourUnsetInteractiveString = "0";


    private static GlobalEnvironment myInstance = null;




    public static void initialize (@NotNull LispBufferFactory bufferFactory, Object project) {
        if (myInstance != null)
            return;
        myInstance = new GlobalEnvironment(bufferFactory, project);
    }

    private GlobalEnvironment (@NotNull LispBufferFactory bufferFactory, Object project) {
        myProject = project;
        myBufferFactory = bufferFactory;
        setGlobal();
    }

}
