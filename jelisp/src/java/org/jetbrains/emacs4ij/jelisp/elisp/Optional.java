package org.jetbrains.emacs4ij.jelisp.elisp;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 9/26/11
 * Time: 8:52 PM
 * To change this template use File | Settings | File Templates.
 */

/*
 * all parameters are optional after first usage of this annotation
 */
@Target({ElementType.PARAMETER})
@Retention(RetentionPolicy.RUNTIME)
public @interface Optional {
    String value() default "";
}
