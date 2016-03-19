package com.geteit.view

import android.content.Context
import android.graphics.Typeface
import android.support.v7.widget.{AppCompatCheckBox, AppCompatTextView}
import android.util.AttributeSet
import android.widget.TextView
import com.geteit.app.R

import scala.collection.mutable

trait RoboView { self: TextView =>
  import RoboView._

  protected def initFont(attrs: AttributeSet)(implicit context: Context) = {
    val a = context.obtainStyledAttributes(attrs, R.styleable.RoboView)
    val typeId = a.getInt(R.styleable.RoboView_fontFamily, 0)
    setTypeface(roboTypeface(typeId))
    a.recycle()
  }
}

object RoboView {
  val typefaceNames = Array("Roboto-Regular.ttf", "Roboto-Light.ttf", "Roboto-Thin.ttf")
  val typefaces = new mutable.HashMap[String, Typeface]

  def roboTypeface(id: Int)(implicit context: Context) = {
    val name = typefaceNames(id)
    typefaces.getOrElseUpdate(name, Typeface.createFromAsset(context.getAssets, name))
  }
}

class RoboTextView(context: Context, attrs: AttributeSet, style: Int) extends AppCompatTextView(context, attrs, style) with RoboView {

  def this(context: Context, attrs: AttributeSet) = this(context, attrs, 0)
  def this(context: Context) = this(context, null, 0)

  initFont(attrs)(context)
}

class RoboCheckBox(context: Context, attrs: AttributeSet, style: Int) extends AppCompatCheckBox(context, attrs, style) with RoboView {

  def this(context: Context, attrs: AttributeSet) = this(context, attrs, 0)
  def this(context: Context) = this(context, null, 0)

  initFont(attrs)(context)
}
