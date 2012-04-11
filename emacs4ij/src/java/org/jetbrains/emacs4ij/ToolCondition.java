package org.jetbrains.emacs4ij;

import com.intellij.openapi.util.Condition;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/11/12
 * Time: 6:44 PM
 * To change this template use File | Settings | File Templates.
 */
public class ToolCondition implements Condition {
    @Override
    public boolean value(Object o) {
        return true;
//        return EnvironmentInitializer.isGlobalInitialized();
    }
}
