package org.jetbrains.emacs4ij.ide;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.RoamingType;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.emacs4ij.jelisp.DefinitionIndex;

@State(
    name="EmacsIndex",
    storages = @Storage(id="emacsIndex", file = "$APP_CONFIG$/emacs4ij_index.xml"),
    reloadable = true,
    roamingType = RoamingType.DISABLED
)

public class EmacsIndexService implements PersistentStateComponent<EmacsIndexService> {
  protected DefinitionIndex myEmacsIndex = new DefinitionIndex();

  public DefinitionIndex getEmacsIndex() {
    return myEmacsIndex;
  }

  public void setEmacsIndex(DefinitionIndex index) {
    myEmacsIndex = index;
  }

  @Override
  public EmacsIndexService getState() {
    return myEmacsIndex.isEmpty() ? null : this;
  }

  @Override
  public void loadState(EmacsIndexService state) {
    XmlSerializerUtil.copyBean(state, this);
  }
}
