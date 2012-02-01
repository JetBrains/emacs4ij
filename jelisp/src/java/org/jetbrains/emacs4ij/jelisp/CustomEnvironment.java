package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.NotNull;


/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/11/11
 * Time: 5:37 PM
 * To change this template use File | Settings | File Templates.
 */
public class CustomEnvironment extends Environment {
    private boolean mySelectionManagedBySubroutine = false;

    //protected LispBuffer myBufferCurrentForEditing = null;

    public CustomEnvironment(@NotNull final Environment outerEnv) {
        myOuterEnv = outerEnv;
    }






    /*public boolean isMainEnvironment() {
        return myOuterEnv instanceof GlobalEnvironment;
    } */

    /*public void updateFunction (LispSymbol symbol) {
        GlobalEnvironment.getInstance().updateFunction(symbol);
    }

    public void defineBuffer (LispBuffer buffer) {
        GlobalEnvironment.getInstance().defineBuffer(buffer);
    }

    public LispBuffer createBuffer (String bufferName) {
        return GlobalEnvironment.getInstance().createBuffer(bufferName);
    }

    public void defineServiceBuffer (LispBuffer buffer) {
        GlobalEnvironment.getInstance().defineServiceBuffer(buffer);
    }

    public void updateBuffer(LispBuffer buffer) {
        GlobalEnvironment.getInstance().updateBuffer(buffer);
    }

    public void updateServiceBuffer (LispBuffer buffer) {
        GlobalEnvironment.getInstance().updateServiceBuffer(buffer);
    }

    public void switchToBuffer(String bufferName) {
        GlobalEnvironment.getInstance().switchToBuffer(bufferName);
    }        */


   /* public LispBuffer findBuffer (String bufferName) {
        return GlobalEnvironment.getInstance().findBuffer(bufferName);
    }

    public LispBuffer getServiceBuffer (String bufferName) {
        return GlobalEnvironment.getInstance().getServiceBuffer(bufferName);
    }


    public ArrayList<LispBuffer> getBuffers () {
        return GlobalEnvironment.getInstance().getBuffers();
    }*/



   /* public LispBuffer getOtherBuffer (String bufferName) {
        return GlobalEnvironment.getInstance().getOtherBuffer(bufferName);
    }

    public int getBuffersSize() {
        return GlobalEnvironment.getInstance().getBuffersSize();
    }

    public LispList getBufferList() {
        return GlobalEnvironment.getInstance().getBufferList();
    }

    public void closeCurrentBuffer () {
        GlobalEnvironment.getInstance().closeCurrentBuffer();
    }

    public void killBuffer (String bufferName) {
        GlobalEnvironment.getInstance().killBuffer(bufferName);
    }

    public void killBuffer (LispBuffer buffer) {
        GlobalEnvironment.getInstance().killBuffer(buffer);
    }

    public void closeAllBuffers () {
        GlobalEnvironment.getInstance().closeAllBuffers();
    }

    public LispBuffer getBufferByIndex (int index) {
        return GlobalEnvironment.getInstance().getBufferByIndex(index);
    }

    public void printBuffers() {
        GlobalEnvironment.getInstance().printBuffers();
    }

    public String[] getBuffersNames () {
        return GlobalEnvironment.getInstance().getBuffersNames();
    }            */



   /* public void buryBuffer (LispBuffer buffer) {
        GlobalEnvironment.getInstance().buryBuffer(buffer);
    } */

   /* public LispBuffer lastBuffer () {
        return lastBuffer("");
    }*/

   /* public LispBuffer lastBuffer (String bufferName) {
        return GlobalEnvironment.getInstance().lastBuffer(bufferName);
    }

    public boolean containsBuffer (String bufferName) {
        return findBuffer(bufferName) != null;
    }

    public boolean isDead (String bufferName) {
        return GlobalEnvironment.getInstance().isDead(bufferName);
    }

    public ArrayList<String> getCommandList (String begin) {
        return GlobalEnvironment.getInstance().getCommandList(begin);
    }                                                                   */


}
