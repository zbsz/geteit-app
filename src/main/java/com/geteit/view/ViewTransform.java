package com.geteit.view;


public interface ViewTransform {

    public float getAlpha();

    public void setAlpha(float alpha);

    public float getTranslationX();

    public void setTranslationX(float translateX);

    public float getTranslationY();

    public void setTranslationY(float translateY);
    
    public float getScaleX();

    public void setScaleX(float scaleX);

    public float getScaleY();

    public void setScaleY(float scaleY);
    
    public float getRotation();
    
    public void setRotation(float rotation);

    public void setVisibility(int visibility);
}
