package gsoc
package contributors

import cats.effect.*
import calico.html.io.{*, given}
import calico.syntax.*
import fs2.concurrent.*
import fs2.dom.HtmlElement
import scala.concurrent.duration.*

object ReactionState:
  enum Phase:
    case Idle, Wait, Ready, Done

  case class State(
      phase: Phase,
      msg: String,
      clicked: Option[Long] = None,
      took: Option[Long] = None
  )

  val fresh = State(Phase.Idle, "press start and then react")

val saifk16: Contributor = Contributor("saifk16"):
  import ReactionState.*
  import ReactionState.Phase.*

  def getTime = IO.realTime.map(_.toMillis)

  SignallingRef[IO].of(fresh).toResource.flatMap { gs =>

    val start = for
      _ <- gs.set(fresh.copy(phase = Wait, msg = "wait wait wait wait wait"))
      _ <- IO.sleep(2.seconds)
      t <- getTime
      _ <- gs.modify { s =>
        if s.phase == Wait then (s.copy(phase = Ready, msg = "click", clicked = Some(t)), ())
        else (s, ())
      }
    yield ()

    val react = gs.get.flatMap { s =>
      if s.phase == Ready then
        for
          t <- getTime
          diff = s.clicked.map(t - _)
          _ <- gs.update(
            _.copy(
              phase = Done,
              msg = s"${diff.getOrElse(0L)} ms",
              took = diff
            ))
        yield ()
      else if s.phase == Wait then gs.update(_.copy(phase = Idle, msg = "too early"))
      else IO.unit
    }

    val reset = gs.set(fresh)

    div(
      p(
        "Hi, I am ",
        calico
          .html
          .io
          .span(
            styleAttr := "color: #0f899c; font-weight: bold",
            "@saifk16"
          ),
        " and I agree to follow Typelevel CoC and GSoC AI policy."
      ),
      p("Fastest clicker in the world?"),
      gs.map(s => p(s.msg)),
      button(onClick --> (_.foreach(_ => start)), "start"),
      button(onClick --> (_.foreach(_ => react)), "react"),
      button(onClick --> (_.foreach(_ => reset)), "reset")
    )
  }
