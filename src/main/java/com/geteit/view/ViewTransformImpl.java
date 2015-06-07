package com.geteit.view;

import android.graphics.Matrix;
import android.graphics.Rect;
import android.view.View;
import android.view.ViewParent;
import android.view.animation.AlphaAnimation;
import android.view.animation.Transformation;
import com.geteit.util.MathUtils;

class ViewTransformImpl extends AlphaAnimation implements ViewTransform {

    private static final Rect rect = new Rect();
    
    public float alpha = 1;
    public float translationX;
    public float translationY;
    public float scaleX = 1;
    public float scaleY = 1;
    public float rotation = 0;
    public View view;

    private boolean justSetAlpha;
        
    public ViewTransformImpl() {
        super(0, 0);
        
        setFillEnabled(true);
        setFillAfter(true);
        setFillBefore(true);
        setRepeatCount(INFINITE);
        setDuration(1000);
    }
    
    public ViewTransformImpl(View view) {
        this();
        
        this.view = view;
    }
    
    void invalidate(float dx, float dy) {
        if (view != null) {
            if (view.getVisibility() != View.VISIBLE) view.clearAnimation();
            else {
                if (view.getAnimation() != this) view.setAnimation(this);

                view.invalidate();
                ViewParent parent = view.getParent();
                if (parent != null && (parent instanceof View)) {
                    rect.left = view.getLeft() + (int)Math.min(translationX, translationX - dx);
                    rect.right = view.getRight() + MathUtils.ceil(Math.max(translationX, translationX - dx));
                    rect.top = view.getTop() + (int)Math.min(translationY, translationY - dy);
                    rect.bottom = view.getTop() + MathUtils.ceil(Math.max(translationY, translationY - dy));

                    ((View) parent).invalidate(rect);
                    ((View) parent).invalidate();
                }
            }
        }
    }
    
    @Override
    public boolean getTransformation(long currentTime, Transformation t) {
        if (view.getVisibility() != View.VISIBLE) view.clearAnimation();

        if (justSetAlpha && (alpha == 1 || alpha == 0)) { // HACK: seems that alpha is ignored if it's set to 1, it works better to return 'almost 1' first
            t.setAlpha(alpha == 1 ? .9999f : .0001f);
        } else {
            t.setAlpha(alpha == 1 ? .9999f : alpha);
        }
        justSetAlpha = false;
        t.setTransformationType(Transformation.TYPE_BOTH);
        Matrix m = t.getMatrix();
        m.setTranslate(translationX, translationY);
        if (rotation != 0) {
            float w2 = view.getWidth() / 2f;
            float h2 = view.getHeight() / 2f;
            m.postRotate(rotation, w2, h2);
        }
        if (scaleX != 1 || scaleY != 1) {
            float w2 = view.getWidth() / 2f;
            float h2 = view.getHeight() / 2f;
            m.postTranslate(-w2, -h2);
            m.postScale(scaleX, scaleY);
            m.postTranslate(w2, h2);
        }
        return false;
    }

    @Override
    public boolean willChangeBounds() {
        return translationY != 0 || translationX != 0 || rotation != 0 || scaleX != 1 || scaleY != 1;
    }
    
    @Override
    public boolean willChangeTransformationMatrix() {
        return translationY != 0 || translationX != 0 || rotation != 0 || scaleX != 1 || scaleY != 1;
    }

    @Override
    public float getAlpha() {
        return alpha;
    }

    @Override
    public void setAlpha(float alpha) {
        if (this.alpha != alpha) {
            justSetAlpha = true;
            
            this.alpha = alpha;
            invalidate(0, 0);
        }
    }

    @Override
    public float getTranslationX() {
        return translationX;
    }

    @Override
    public void setTranslationX(float translateX) {
        if (this.translationX != translateX) {
            float dx = translateX - this.translationX;
            this.translationX = translateX;
            invalidate(dx, 0);
        }
    }

    @Override
    public float getTranslationY() {
        return translationY;
    }

    @Override
    public void setTranslationY(float translateY) {
        if (this.translationY != translateY) {
            float dy = translateY - this.translationY;
            this.translationY = translateY;
            invalidate(0, dy);
        }
    }

    @Override
    public float getScaleX() {
        return scaleX;
    }
    
    @Override
    public float getScaleY() {
        return scaleY;
    }

    @Override
    public void setScaleX(float scaleX) {
        if (this.scaleX != scaleX) {
            this.scaleX = scaleX;
            invalidate(0, 0); // FIXME
        }
    }

    @Override
    public void setScaleY(float scaleY) {
        if (this.scaleY != scaleY) {
            this.scaleY = scaleY;
            invalidate(0, 0); // FIXME
        }
    }

    @Override
    public float getRotation() {
        return rotation;
    }

    @Override
    public void setRotation(float rotation) {
        if (this.rotation != rotation) {
            this.rotation = rotation;
            invalidate(0, 0); //FIXME
        }
    }

    @Override
    public void setVisibility(int visibility) {
        view.setVisibility(visibility);
        invalidate(0, 0);
    }
}
