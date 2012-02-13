package org.jetbrains.emacs4ij;

import com.intellij.openapi.components.ProjectComponent;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.FileEditorManagerEvent;
import com.intellij.openapi.fileEditor.FileEditorManagerListener;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.EnvironmentException;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.exception.DoubleBufferException;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/5/11
 * Time: 12:26 PM
 * To change this template use File | Settings | File Templates.
 */
public class MyProjectComponent implements ProjectComponent {
    private CustomEnvironment myEnvironment = null;
  //  private MyApplicationComponent myApplication;
    private Project myProject;

    public MyProjectComponent(Project project) {
        myProject = project;
       // myApplication = ApplicationManager.getApplication().getComponent(MyApplicationComponent.class);
        IdeaBuffer.setProject(project);
    }

    @NotNull
    public String getComponentName() {
        return "org.jetbrains.emacs4ij.MyProjectComponent";
    }

    public void projectOpened() {
        myProject.getMessageBus().connect().subscribe(FileEditorManagerListener.FILE_EDITOR_MANAGER, new FileEditorManagerListener() {
            @Override
            public void fileOpened(FileEditorManager fileEditorManager, VirtualFile virtualFile) {
                if (myEnvironment == null)
                    return;

                IdeaBuffer newBuffer = new IdeaBuffer(myEnvironment, virtualFile.getName(), virtualFile.getParent().getPath()+'/', fileEditorManager.getSelectedTextEditor());
                try {
                    myEnvironment.defineBuffer(newBuffer);
                } catch (DoubleBufferException e) {
                    System.err.println(e.getMessage());
                }
                System.out.print("open: ");
                //setHeaders(newBuffer);
                myEnvironment.printBuffers();
            }

            @Override
            public void fileClosed(FileEditorManager fileEditorManager, VirtualFile virtualFile) {
                if (myEnvironment == null)
                    return;

                if (!(myEnvironment.isSelectionManagedBySubroutine()))
                    myEnvironment.killBuffer(virtualFile.getName());
                else myEnvironment.setSelectionManagedBySubroutine(false);

                System.out.print("close: ");
                myEnvironment.printBuffers();
            }

            @Override
            public void selectionChanged(FileEditorManagerEvent fileEditorManagerEvent) {
                if (myEnvironment == null)
                    return;

                if (fileEditorManagerEvent.getNewFile() == null) {
                    if (myEnvironment.getBuffersSize() != 1)
                        throw new RuntimeException("the number of opened buffers doesn't correspond to number of opened files!");
                    return;
                }
                try {
                    if (!(myEnvironment.isSelectionManagedBySubroutine()))
                        myEnvironment.switchToBuffer(fileEditorManagerEvent.getNewFile().getName());
                    else myEnvironment.setSelectionManagedBySubroutine(false);
                    //setHeaders((IdeaBuffer)myEnvironment.getBufferCurrentForEditing());
                    System.out.print("select: ");
                    myEnvironment.printBuffers();
                } catch (EnvironmentException e) {
                    //ignore
                }
            }
        });

        new Task.Backgroundable(myProject, "Initializing Emacs environment", false) {
            public void run(@NotNull ProgressIndicator indicator) {
                indicator.setText("Loading Emacs functions");
                indicator.setFraction(0.0);
                if (EnvironmentInitializer.silentInitGlobal()) {
                    myEnvironment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
                    EnvironmentInitializer.initProjectEnv(myProject, myEnvironment);
                }
                indicator.setFraction(1.0);
            }
        }.queue();
    }

    public boolean initEnv () {
        //todo: invoke on form action
        if (myEnvironment != null)
            return true;
        if (!EnvironmentInitializer.initGlobal())
            return false;
        myEnvironment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
        EnvironmentInitializer.initProjectEnv(myProject, myEnvironment);
        return true;
    }

    public CustomEnvironment getEnvironment() {
        return myEnvironment;
    }

    public void initComponent() {
        // TODO: insert component initialization logic here
    }

    public void disposeComponent() {
        // TODO: insert component disposal logic here
    }

    public void projectClosed() {
        // called when project is being closed
    }
}
