package gsoc
package contributors

import calico.*
import calico.html.io.{*, given}
import cats.effect.*
import fs2.concurrent.*
import fs2.dom.*
import scala.util.Random

val yummy_yums = Contributor("yummy_yums"):
  import Guesser.*
  SignallingRef[IO].of(freshState).toResource.flatMap { state =>
    val submit: IO[Unit] = state.get.flatMap { s =>
      if s.won then IO.unit
      else
        parseGuess(s.input) match
          case None =>
            state.update(
              _.copy(
                input = "",
                message = "Please enter a valid integer."
              ))
          case Some(guess) =>
            if guess == s.secret then
              state.update(
                _.copy(
                  hint = Hint.Correct,
                  attempts = s.attempts + 1,
                  won = true,
                  input = "",
                  message = s"Correct! You got it in ${s.attempts + 1} attempts."
                ))
            else if guess < s.secret then
              state.update(
                _.copy(
                  hint = Hint.TooLow,
                  attempts = s.attempts + 1,
                  input = "",
                  message = "Too low, try again."
                ))
            else if guess > max then
              state.update(
                _.copy(
                  hint = Hint.OverBound,
                  attempts = s.attempts + 1,
                  input = "",
                  message = s"This is more than $max, please enter a number between 1 and $max"
                ))
            else
              state.update(
                _.copy(
                  hint = Hint.TooHigh,
                  attempts = s.attempts + 1,
                  input = "",
                  message = "Too high, try again."
                ))
    }

    div(
      state.map(_.message).changes.map { msg => p(styleAttr := "margin: 8px 0", msg) },
      state.map(_.hint).map {
        case Hint.NoGuess => p("Make your guess!")
        case Hint.TooLow => p(styleAttr := "color: red", "↑ Too low, try again.")
        case Hint.TooHigh => p(styleAttr := "color: blue", "↓ Too high, try again.")
        case Hint.OverBound =>
          p(styleAttr := "color: indigo", " OverBound, please try within the limits.")
        case Hint.Correct => p(styleAttr := "color: green; font-weight: bold", "✓ Correct!")
      },
      state.map(s => s"Attempts: ${s.attempts}").changes.map { t =>
        p(styleAttr := "font-size: 0.85em; color: gray", t)
      },
      input.withSelf { self =>
        (
          typ := "number",
          disabled <-- state.map(_.won),
          value <-- state.map(_.input),
          onInput --> (_.foreach { ev =>
            self.value.get.flatMap(e => state.update(_.copy(input = e)))
          }),
          onKeyDown --> (_.foreach { ev => if ev.key == "Enter" then submit else IO.unit })
        )
      },
      button(
        disabled <-- state.map(_.won),
        onClick --> (_.foreach(_ => submit)),
        state.map(s => if s.won then "✓ Done" else "Guess").changes
      ),
      state.map(_.won).changes.map { won =>
        if won then
          button(
            onClick --> (_.foreach(_ => state.set(freshState))),
            "Play Again"
          )
        else div("")
      }
    )
  }

private object Guesser:
  val max = 5

  enum Hint:
    case NoGuess
    case TooLow
    case TooHigh
    case OverBound
    case Correct

  case class GameState(
      secret: Int,
      input: String,
      hint: Hint,
      attempts: Int,
      won: Boolean,
      message: String
  )

  def freshState: GameState =
    GameState(
      secret = Random.nextInt(max) + 1,
      input = "",
      hint = Hint.NoGuess,
      attempts = 0,
      won = false,
      message = s"I'm thinking of a number between 1 and $max. Can you guess it?"
    )

  def parseGuess(str: String): Option[Int] = str.trim.toIntOption
