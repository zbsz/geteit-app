package com.geteit.view;

import android.annotation.TargetApi;
import android.view.View;

@TargetApi(11)
class ViewTransformHC implements ViewTransform {

    private View view;

    public ViewTransformHC(View view) {
        this.view = view;
    }
    
    @Override
    public float getAlpha() {
        return view.getAlpha();
    }

    @Override
    public void setAlpha(float alpha) {
        view.setAlpha(alpha);
    }

    @Override
    public float getTranslationX() {
        return view.getTranslationX();
    }

    @Override
    public void setTranslationX(float translateX) {
        view.setTranslationX(translateX);
    }
    
    @Override
    public float getTranslationY() {
        return view.getTranslationY();
    }
    
    @Override
    public void setTranslationY(float translateY) {
        view.setTranslationY(translateY);
    }

    @Override
    public float getScaleX() {
        return view.getScaleX();
    }

    @Override
    public void setScaleX(float scaleX) {
        view.setScaleX(scaleX);
    }

    @Override
    public float getScaleY() {
        return view.getScaleY();
    }

    @Override
    public void setScaleY(float scaleY) {
        view.setScaleY(scaleY);
    }

    @Override
    public float getRotation() {
        return view.getRotation();
    }

    @Override
    public void setRotation(float rotation) {
        view.setRotation(rotation);
    }

    @Override
    public void setVisibility(int visibility) {
        view.setVisibility(visibility);
    }
}
