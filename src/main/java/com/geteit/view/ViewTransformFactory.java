package com.geteit.view;

import android.os.Build;
import android.view.View;

public abstract class ViewTransformFactory {

    private static class FactoryHolder {
        private static ViewTransformFactory FACTORY = new ViewTransformFactory() {

            @Override
            ViewTransform createViewTransform(View view) {
                return new ViewTransformImpl(view);
            }
        };
    }
    
    private static class HCFactoryHolder {
        private static ViewTransformFactory FACTORY = new ViewTransformFactory() {

            @Override
            ViewTransform createViewTransform(View view) {
                return new ViewTransformHC(view);
            }
        };
    }
    
    public static ViewTransform createTransform(View view) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB) {
            return HCFactoryHolder.FACTORY.createViewTransform(view);
        } else {
            return FactoryHolder.FACTORY.createViewTransform(view);
        }
    }
    
    abstract ViewTransform createViewTransform(View view);
}
