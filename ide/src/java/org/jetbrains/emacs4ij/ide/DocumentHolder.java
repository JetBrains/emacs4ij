package org.jetbrains.emacs4ij.ide;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.util.ui.UIUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.exception.AssignNullDocument;
import org.jetbrains.emacs4ij.jelisp.exception.BufferOpenException;
import org.jetbrains.emacs4ij.jelisp.exception.NullBufferDocument;

abstract class DocumentHolder {
  protected Document myDocument;

  private final DocumentListener myDocumentListener = new DocumentListener() {
    private int myOldPosition;
    @Override
    public void beforeDocumentChange(DocumentEvent documentEvent) {
      myOldPosition = point();
    }

    @Override
    public void documentChanged(DocumentEvent documentEvent) {
      int shift = documentEvent.getNewLength() - documentEvent.getOldLength();
      if (shift < 0) {   //delete
        updateMarkersPositions(point(), shift, false);
        return;
      }
      if (shift > 0) { //insert
        updateMarkersPositions(myOldPosition, shift, false);
      }
    }
  };

  public void onOpen(Document document) {
    if (myDocument != null && myDocument != document)
      throw new BufferOpenException(getOwnerName());
    if (document == null)
      throw new AssignNullDocument(getOwnerName());
    if (myDocument != null)
      return;
    myDocument = document;
    myDocument.addDocumentListener(myDocumentListener);
  }

  @NotNull
  private Document getDocument() {
    if (myDocument == null) throw new NullBufferDocument(getOwnerName());
    return myDocument;
  }

  public void onKill() {
    getDocument().removeDocumentListener(myDocumentListener);
    myDocument = null;
  }

  public void addDocumentListener(DocumentListener listener) {
    getDocument().addDocumentListener(listener);
  }

  public void removeDocumentListener(DocumentListener listener) {
    getDocument().removeDocumentListener(listener);
  }

  public int size() {
    return getDocument().getTextLength();
  }

  public String getText() {
    return getDocument().getText();
  }

  public void setText(String text) {
    getDocument().setText(text);
  }

  public void insertAt (final int position, final String insertion) {
    UIUtil.invokeLaterIfNeeded(new Runnable() {
      @Override
      public void run() {
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
          @Override
          public void run() {
            getDocument().insertString(position, insertion);
          }
        });
      }
    });
  }

  public void replace(final int from, final int to, final String text) {
    UIUtil.invokeLaterIfNeeded(new Runnable() {
      @Override
      public void run() {
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
          @Override
          public void run() {
            getDocument().replaceString(from - 1, to - 1, text);
            afterReplace(from + text.length());
          }
        });
      }
    });
  }

  protected abstract void afterReplace(int newFinishPosition);
  protected abstract String getOwnerName();
  protected abstract void updateMarkersPositions(int point, int shift, boolean b);
  protected abstract int point();
}
