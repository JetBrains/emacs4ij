package org.jetbrains.emacs4ij.jelisp.subroutine;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Subroutine {
  String value();
  boolean isCmd() default false;
  String interactive() default "";
  String key() default "";
}
