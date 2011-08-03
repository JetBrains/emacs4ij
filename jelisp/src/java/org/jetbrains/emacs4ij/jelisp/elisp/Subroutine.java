package org.jetbrains.emacs4ij.jelisp.elisp;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/2/11
 * Time: 3:43 PM
 * To change this template use File | Settings | File Templates.
 */

@Target({ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface Subroutine {
    String value();
    int min() default -1;
    int max() default -1;
    int exact() default -1;
}
