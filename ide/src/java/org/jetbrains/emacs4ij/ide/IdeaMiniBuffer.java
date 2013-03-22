package org.jetbrains.emacs4ij.ide;

import com.intellij.ide.IdeEventQueue;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.editor.ex.FocusChangeListener;
import com.intellij.openapi.fileTypes.FileTypes;
import com.intellij.ui.EditorTextField;
import com.intellij.util.Alarm;
import com.intellij.util.ui.UIUtil;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.TestMode;
import org.jetbrains.emacs4ij.jelisp.exception.Emacs4ijFatalException;
import org.jetbrains.emacs4ij.jelisp.exception.UnregisteredBufferException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispMinibuffer;

import java.awt.*;
import java.awt.event.KeyEvent;

public final class IdeaMiniBuffer extends LispMinibuffer {
  private static IdeaMiniBuffer myInstance;

  private Integer myCharCode = null;
  private Alarm myAlarm;

  private DocumentHolder myDocumentHolder = new DocumentHolder() {
    @Override
    protected void afterReplace(int newFinishPosition) {
      gotoChar(newFinishPosition);
    }
    @Override
    protected String getOwnerName() {
      return getName();
    }
    @Override
    protected void updateMarkersPositions(int point, int shift, boolean b) {
      IdeaMiniBuffer.this.updateMarkersPositions(point, shift, b);
    }
    @Override
    protected int point() {
      return IdeaMiniBuffer.this.point();
    }
  };

  private IdeaMiniBuffer (int number, Editor editor, Environment environment, LispBuffer parent) {
    super(" *Minibuf-" + number + '*', environment, parent);
    myAlarm = new Alarm();
    if (editor != null) {
      openStandardBuffer(editor);
    }
  }

  public static void init (@Nullable Editor editor, @Nullable Environment environment) {
    if (myInstance == null) {
      myInstance = new IdeaMiniBuffer(0, editor, environment == null ? GlobalEnvironment.INSTANCE : environment, null);
    }
  }

  public static IdeaMiniBuffer getInstance() {
    return myInstance;
  }

  private final DocumentListener myMiniBufferChangedListener = new DocumentListener() {
    @Override
    public void beforeDocumentChange(DocumentEvent documentEvent) {
    }

    @Override
    public void documentChanged(DocumentEvent documentEvent) {
      documentEvent.getDocument().removeDocumentListener(this);
      myAlarm.cancelAllRequests();
      if (getInteractive().isNoMatch()) {
        String newText = documentEvent.getDocument().getText();
        int k = newText.indexOf(getInteractive().getNoMatchMessage());
        if (k < 0)
          return;
        newText = newText.substring(0, k);
        write(newText);
      }
    }
  };

  private final FocusChangeListener myFocusListener = new FocusChangeListener() {
    private LispKeymap myOldKeymap;

    @Override
    public void focusGained(Editor editor) {
      myOldKeymap = getEnvironment().getActiveKeymap();
      getEnvironment().setActiveKeymap("minibuffer-local-completion-map");
    }

    @Override
    public void focusLost(Editor editor) {
      getEnvironment().setActiveKeymap(myOldKeymap);
    }
  };

  private void openStandardBuffer(Editor editor) {
    getEnvironment().onBufferOpened(this, new IdeaEditorWrapper(editor));
    myDocumentHolder.onOpen(editor.getDocument());
  }

  @Override
  protected Integer getCharCode() {
    return myCharCode;
  }

  @Override
  protected void resetCharCode() {
    myCharCode = null;
  }

  @Override
  public int size() {
    return myDocumentHolder.size();
  }

  @Override
  public void kill() {
    super.kill();
    myDocumentHolder.onKill();
  }

  @Override
  public void closeHeader() {
    throw new UnsupportedOperationException();
  }

  @Override
  protected void cancelNoMatchMessageUpdate() {
    myAlarm.cancelAllRequests();
  }

  @Override
  public void setCharListener() {
    IdeEventQueue.getInstance().addActivityListener(new Runnable() {
      @Override
      public void run() {
        IdeEventQueue ideEventQueue = IdeEventQueue.getInstance();
        AWTEvent currentEvent = ideEventQueue.getTrueCurrentEvent();
        if (currentEvent instanceof KeyEvent) {
          if (currentEvent.getID() == KeyEvent.KEY_PRESSED) {
            if (((KeyEvent) currentEvent).getKeyChar() != KeyEvent.CHAR_UNDEFINED) {
              myCharCode = ((KeyEvent) currentEvent).getKeyCode();
              ((KeyEvent) currentEvent).consume();
              ideEventQueue.removeActivityListener(this);
              onReadInput();
            }
          }
        }
      }
    });
  }

  @Override
  protected void clearNoMatch () {
    myDocumentHolder.addDocumentListener(myMiniBufferChangedListener);
    myAlarm.addRequest(new Runnable() {
      @Override
      public void run() {
        myDocumentHolder.removeDocumentListener(myMiniBufferChangedListener);
        String text = getText();
        String prompt = getInteractive().getPrompt();
        if (getInteractive().isNoMatch()
            && text.endsWith(getInteractive().getNoMatchMessage())
            && text.startsWith(prompt)) {
          String input = text.substring(prompt.length(), text.length() - getInteractive().getNoMatchMessage().length());
          write(prompt + input);
        }
      }
    }, 3000);
  }

  @Override
  protected void open(LispBuffer parent) {
    super.open(parent);

    final EditorTextField input = new EditorTextField("", IdeaBuffer.getProject(), FileTypes.PLAIN_TEXT);

    if (!(TestMode.TEST && parent == null)) {
      ((IdeaBuffer)parent).getEditor().setHeaderComponent(input);
    }

    input.setEnabled(true);
    Editor editor = input.getEditor();

    if (TestMode.TEST && editor == null) {
      return;
    }

    if (editor == null) {
      throw new IllegalStateException();
    }

    ((EditorEx) editor).addFocusListener(myFocusListener);
    openStandardBuffer(editor);
    ((EditorEx) editor).setBackgroundColor(Color.GREEN);
    setActive();
  }

  @Override
  protected void write (final String text) {
    UIUtil.invokeLaterIfNeeded(new Runnable() {
      @Override
      public void run() {
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
          @Override
          public void run() {
            setText(text);
            gotoChar(pointMax());
          }
        });
      }
    });
  }

  //for test
  public void appendText (String text) {
    write(getText() + text);
  }

  @Override
  public void setActive() {
    if (getEnvironment().findBuffer(getName()) == null)
      throw new UnregisteredBufferException(getName());
    try {
      getEditor().getContentComponent().grabFocus();
    } catch (NullPointerException e) {
      throw new Emacs4ijFatalException("Null editor!");
    }
  }

  @Override
  public String getText() {
    return myDocumentHolder.getText();
  }

  @Override
  public void setText(String text) {
    myDocumentHolder.setText(text);
  }

  @Override
  protected void insertAt(int position, String insertion) {
    myDocumentHolder.insertAt(position, insertion);
  }

  @Override
  public void replace(int from, int to, String text) {
    myDocumentHolder.replace(from, to, text);
  }

  @Override
  public int point() {
    Editor editor = getEditor();
    return editor.logicalPositionToOffset(editor.getCaretModel().getLogicalPosition()) + 1;
  }

  @Override
  public void setPoint(int position) {
    getEditor().getCaretModel().moveToOffset(position - 1);
  }

  @Override
  public void goTo(int line, int column) {
    throw new UnsupportedOperationException();
  }

  @Override
  public int getLine() {
    return getEditor().getCaretModel().getVisualPosition().getLine();
  }

  @Override
  public int getColumn() {
    return getEditor().getCaretModel().getVisualPosition().getColumn() - 1;
  }

  @Override
  public int getLineStartIndex() {
    return 1;
  }

  private Editor getEditor() {
    return ((IdeaWindow)getEnvironment().getBufferLastSelectedWindow(this)).getEditor();
  }

  public void message(final String text) {
    myDocumentHolder.addDocumentListener(myMiniBufferChangedListener);
    myAlarm.addRequest(new Runnable() {
      @Override
      public void run() {
        myDocumentHolder.removeDocumentListener(myMiniBufferChangedListener);
        appendText(text);
      }
    }, 3000);
  }
}
