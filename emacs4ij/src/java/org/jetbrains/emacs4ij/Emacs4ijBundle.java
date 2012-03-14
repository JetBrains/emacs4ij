package org.jetbrains.emacs4ij;

import com.intellij.CommonBundle;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.PropertyKey;

import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.ResourceBundle;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/14/12
 * Time: 11:21 AM
 * To change this template use File | Settings | File Templates.
 */
public class Emacs4ijBundle {
    private static Reference<ResourceBundle> ourBundle;

    @NonNls
    private static final String BUNDLE = "org.jetbrains.emacs4ij.Emacs4ijBundle";

    private Emacs4ijBundle() {
    }

    public static String message(@PropertyKey(resourceBundle = BUNDLE)String key, Object... params) {
        return CommonBundle.message(getBundle(), key, params);
    }

    public static String message(@PropertyKey(resourceBundle = BUNDLE)String key) {
        return CommonBundle.message(getBundle(), key);
    }

    private static ResourceBundle getBundle() {
        ResourceBundle bundle = null;
        if (ourBundle != null) bundle = ourBundle.get();
        if (bundle == null) {
            bundle = ResourceBundle.getBundle(BUNDLE);
            ourBundle = new SoftReference<>(bundle);
        }
        return bundle;
    }
}