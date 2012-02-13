package org.jetbrains.emacs4ij;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/5/11
 * Time: 1:56 PM
 * To change this template use File | Settings | File Templates.
 */

public abstract class EmacsService  {
    protected String myEmacsParameter;

    public String getEmacsParameter() {
        return myEmacsParameter;
    }

    public void setEmacsParameter(String myEmacsParameter) {
        this.myEmacsParameter = myEmacsParameter;
    }

   /* private void showInfoMessage (final String name) {
        UIUtil.invokeLaterIfNeeded(new Runnable() {
            @Override
            public void run() {
                ApplicationManager.getApplication().runWriteAction(new Runnable() {
                    @Override
                    public void run() {
                        //Messages.showInfoMessage("You should choose Emacs " + name + " directory!", "Emacs4ij");
                        JFileChooser fileChooser = new JFileChooser(System.getProperty("user.home"));
                        fileChooser.setDialogTitle("Select Emacs " + name + "  directory");
                        fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                        if (fileChooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
                            myEmacsParameter = fileChooser.getSelectedFile().getAbsolutePath();
                        } else {
                            Messages.showInfoMessage("You didn't choose Emacs " + name + " directory!\nUntil you set Emacs environment, no Emacs emulation will work.\nYou can set it by clicking on any of Emacs4ij icons.", "Emacs4ij");
                        }
                    }
                });
            }
        });
    }*/

    
    protected abstract boolean isParameterSet();

}

