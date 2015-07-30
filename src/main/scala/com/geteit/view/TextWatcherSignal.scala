package com.geteit.view

import android.text.{Editable, TextWatcher}
import android.widget.EditText
import com.geteit.app.ViewFinder
import com.geteit.events.Signal
import com.geteit.util.Log._
import com.geteit.util._

class TextWatcherSignal(current: String = "") extends Signal[String] with TextWatcher {
  import TextWatcherSignal._

  value = Some(current)

  override def beforeTextChanged(s: CharSequence, start: Int, count: Int, after: Int): Unit = ()

  override def onTextChanged(s: CharSequence, start: Int, before: Int, count: Int): Unit = publish(s.toString)

  override def afterTextChanged(s: Editable): Unit = ()
}

object TextWatcherSignal {
  private implicit val tag: LogTag = "TextWatcherSignal"

  def apply(et: EditText): TextWatcherSignal = returning(new TextWatcherSignal(et.getText.toString)) { et.addTextChangedListener}

  def apply(id: Int)(implicit vf: ViewFinder): TextWatcherSignal = apply(vf.findById[EditText](id))
}
