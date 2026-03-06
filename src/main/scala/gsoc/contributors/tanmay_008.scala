package gsoc
package contributors

import cats.effect.*
import fs2.concurrent.*
import calico.html.io.{*, given}
import calico.syntax.*

val tanmay_008: Contributor = Contributor("tanmay_008"):
  SignallingRef[IO].of("").toResource.flatMap { userInput =>

    val metricsSignal = userInput.map(CodeAnalyzer.calculate)

    div(
      p(
        styleAttr := "color: #4b4e53; font-size: 14px; margin-right:10px;",
        "I am ",
        span(styleAttr := "color: #38bdf8; font-weight: bold;", "@Tanmay-008"),
        " on GitHub. I agree to follow the Typelevel CoC and GSoC AI policy."
      ),
      div(
        styleAttr := "font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; background: #0f172a; color: #e2e8f0; padding: 20px; border-radius: 12px; max-width: 800px; margin-top: 15px;",
        p(
          styleAttr := "font-size: 13px; color: #888; margin: 4px 0 10px 0;",
          "Go below and write your scala code to analyze metrics! (Or copy the test snippet below)"
        ),
        div(
          styleAttr := "background: #010409; padding: 12px; border-radius: 6px; border: 1px solid #30363d; margin-bottom: 15px; overflow-x: auto;",
          pre(
            styleAttr := "margin: 0; font-family: 'Courier New', Courier, monospace; font-size: 13px; color: #e6edf3;",
            """def inspireCoder() = {
  val message = "Hello developers."
  println(message)
}"""
          )
        ),
        textArea.withSelf { self =>
          (
            placeholder := "Write your Scala code here...",
            styleAttr := "width: 100%; height: 200px; font-family: 'Courier New', Courier, monospace; padding: 12px; background: #1e293b; color: #f8fafc; border: 1px solid #334155; border-radius: 6px; box-sizing: border-box; outline: none !important; box-shadow: none !important; border-color: #334155 !important; resize: vertical;",
            onInput --> (_.foreach { _ => self.value.get.flatMap(userInput.set) })
          )
        },
        div(
          styleAttr := "margin-top: 20px; display: flex; gap: 20px;",
          div(
            styleAttr := "flex: 1; background: #1e293b; padding: 15px; border-radius: 8px; text-align: center; border: 1px solid #334155;",
            div(
              styleAttr := "font-size: 12px; color: #94a3b8; text-transform: ;",
              "TOTAL LINES"),
            div(
              styleAttr := "font-size: 24px; font-weight: bold; color: #f1f5f9;",
              metricsSignal.map(m => span(m.lines.toString)))
          ),
          div(
            styleAttr := "flex: 1; background: #1e293b; padding: 15px; border-radius: 8px; text-align: center; border: 1px solid #334155;",
            div(
              styleAttr := "font-size: 12px; color: #94a3b8; text-transform: ",
              "FUNCTIONS (def)"),
            div(
              styleAttr := "font-size: 24px; font-weight: bold; color: #f1f5f9;",
              metricsSignal.map(m => span(m.functions.toString)))
          ),
          div(
            styleAttr := "flex: 1; background: #1e293b; padding: 15px; border-radius: 8px; text-align: center; border: 1px solid #334155;",
            div(styleAttr := "font-size: 12px; color: #94a3b8;  ", "VARIABLES (val/var)"),
            div(
              styleAttr := "font-size: 24px; font-weight: bold; color: #f1f5f9;",
              metricsSignal.map(m => span(m.variables.toString)))
          )
        )
      )
    )
  }

case class CodeMetrics(lines: Int, functions: Int, variables: Int)

private object CodeAnalyzer:

  def calculate(rawCode: String): CodeMetrics =
    if rawCode.trim.isEmpty then CodeMetrics(0, 0, 0)
    else
      val lines = rawCode.split("\n")
      val totalLines = lines.length

      val funcCount = lines.count(_.trim.contains("def"))
      val varCount = lines.count { l =>
        val trimmed = l.trim
        trimmed.contains("val") || trimmed.contains("var")
      }

      CodeMetrics(totalLines, funcCount, varCount)
