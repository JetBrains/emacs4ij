package org.jetbrains.emacs4ij;

import com.intellij.execution.ui.ConsoleViewContentType;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.project.ProjectManager;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.platform_dependent.Ide;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/24/11
 * Time: 3:46 PM
 * To change this template use File | Settings | File Templates.
 */
public class IdeProvider implements Ide {
    public IdeProvider () {}

    private EchoArea getEchoArea() {
        return ProjectManager.getInstance().getOpenProjects()[0].getComponent(MyProjectComponent.class).getEchoArea();
    }

    @Override
    public void echo(final String message, @NotNull GlobalEnvironment.MessageType type) {
        TextAttributesKey outputKey;
        switch (type) {
            case OUTPUT:
                outputKey = ConsoleViewContentType.NORMAL_OUTPUT_KEY;
                break;
            case WARNING:
                outputKey = ConsoleViewContentType.LOG_WARNING_OUTPUT_KEY;
                break;
            case ERROR:
                outputKey = ConsoleViewContentType.ERROR_OUTPUT_KEY;
                break;
            default:
                throw new InternalException(Emacs4ijBundle.message("unsupported.msg.type", type.toString()));
        }
        getEchoArea().print(message + "\n", outputKey);
    }
}
