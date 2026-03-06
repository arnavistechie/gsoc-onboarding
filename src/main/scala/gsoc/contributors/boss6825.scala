package gsoc
package contributors

import cats.effect.*
import cats.syntax.all.*
import fs2.concurrent.*
import calico.html.io.{*, given}
import calico.syntax.*
import io.circe.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dom.FetchClientBuilder
import scala.util.Random

val boss6825: Contributor = Contributor("boss6825"):
  case class Quote(content: String, author: String)
  object Quote:
    given Decoder[Quote] = Decoder.instance { c =>
      c.downField("content").as[String].flatMap { content =>
        c.downField("author").as[String].map { author =>
          Quote(content, author)
        }
      }
    }

  case class State(quote: Quote, loading: Boolean, darkMode: Boolean)

  val fallbackQuotes = Vector(
    Quote("Programs must be written for people to read.", "Harold Abelson"),
    Quote("Any fool can write code that a computer can understand.", "Martin Fowler"),
    Quote("First, solve the problem. Then, write the code.", "John Johnson"),
    Quote("Simplicity is the soul of efficiency.", "Austin Freeman"),
    Quote("Code is like humor. When you have to explain it, it's bad.", "Cory House"),
    Quote("Make it work, make it right, make it fast.", "Kent Beck"),
    Quote("The best error message is the one that never shows up.", "Thomas Fuchs"),
    Quote("Talk is cheap. Show me the code.", "Linus Torvalds")
  )

  def randomFallback: Quote = fallbackQuotes(Random.nextInt(fallbackQuotes.length))

  val apiUrl = Uri.unsafeFromString("https://corsproxy.io/?https://api.quotable.io/random")
  val client = FetchClientBuilder[IO].create

  def fetchQuote: IO[Quote] =
    client.expect[Quote](apiUrl).handleError(_ => randomFallback)

  SignallingRef[IO].of(State(randomFallback, true, true)).toResource.flatMap { state =>
    Resource.eval(fetchQuote.flatMap(q => state.set(State(q, false, true)))) *>
    div(
      styleAttr <-- state.map { s =>
        val bg = if s.darkMode then "#2d2a24" else "#f5f0e6"
        val textColor = if s.darkMode then "#d4cfc4" else "#3d3a34"
        s"padding: 24px; background: $bg; color: $textColor; border-radius: 4px; max-width: 480px; border: 1px solid ${if s.darkMode then "#4a463d" else "#c9c2b4"}; font-family: 'Comic Sans MS', 'Chalkboard SE', cursive;"
      },
      div(
        styleAttr := "display: flex; justify-content: space-between; align-items: center; margin-bottom: 16px;",
        p(
          styleAttr := "margin: 0;",
          "Hello, I'm ",
          span(styleAttr := "color: #b58863; font-weight: bold;", "@boss6825"),
          " on GitHub."
        ),
        button(
          styleAttr := "padding: 6px 12px; background: transparent; border: 1px solid #8b7355; color: #8b7355; border-radius: 2px; cursor: pointer; font-size: 13px;",
          onClick --> (_.foreach(_ => state.update(st => st.copy(darkMode = !st.darkMode)))),
          state.map(s => if s.darkMode then "☀" else "☽")
        )
      ),
      p(
        styleAttr := "margin: 0 0 20px 0; font-size: 14px; opacity: 0.85;",
        "I agree to follow the Typelevel CoC and GSoC AI policy."
      ),
      div(
        styleAttr <-- state.map { s =>
          val quoteBg = if s.darkMode then "#252219" else "#ebe5d9"
          s"padding: 20px; background: $quoteBg; border-left: 3px solid #8b7355; margin-bottom: 20px;"
        },
        p(
          styleAttr := "font-style: italic; margin: 0 0 12px 0; line-height: 1.6;",
          state.map(s => if s.loading then "Loading..." else s"\"${s.quote.content}\"")
        ),
        p(
          styleAttr <-- state.map(s => s"text-align: right; color: ${if s.darkMode then "#9a9286" else "#6b6560"}; margin: 0; font-size: 14px;"),
          state.map(s => s"— ${s.quote.author}")
        )
      ),
      button(
        styleAttr := "padding: 10px 20px; background: #8b7355; color: #f5f0e6; border: none; border-radius: 2px; cursor: pointer; font-size: 14px;",
        disabled <-- state.map(_.loading),
        onClick --> (_.foreach { _ =>
          state.update(_.copy(loading = true)) *>
          fetchQuote.flatMap(q => state.update(s => s.copy(quote = q, loading = false)))
        }),
        state.map(s => if s.loading then "Loading..." else "New Quote")
      )
    )
  }
