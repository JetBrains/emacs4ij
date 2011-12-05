package org.jetbrains.emacs4ij;

import com.intellij.openapi.components.ServiceManager;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/1/11
 * Time: 2:39 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class Checker {
    public static boolean isEnvironmentInitialized;

    public static boolean isReady () {
        //test mode
        /*if (GlobalEnvironment.ourEmacsPath.equals("") || GlobalEnvironment.ourEmacsSource.equals(""))
            return false;
                 */
        if (isEnvironmentInitialized)
            return true;
        boolean answer = true;
        EmacsHomeService emacsHomeService = ServiceManager.getService(EmacsHomeService.class);
        if (!emacsHomeService.checkSetEmacsHome())
            answer = false;
        EmacsSourceService emacsSourceService = ServiceManager.getService(EmacsSourceService.class);
        if (!emacsSourceService.checkSetEmacsSource())
            return false;

        return answer;
    }
}
