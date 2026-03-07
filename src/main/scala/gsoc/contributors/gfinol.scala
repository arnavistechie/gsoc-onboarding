package gsoc
package contributors

import cats.effect.*

import fs2.concurrent.*
import fs2.dom.HtmlElement

import calico.html.io.{*, given}
import calico.syntax.*


val gfinol = Contributor("gfinol"):
  SignallingRef[IO].of(false).toResource.flatMap { revealed =>
    div(
      p(
        "I am ",
        revealed.map(r => if r then "red" else "inherit").changes.map { color =>
          span(
            styleAttr := s"color: $color; font-weight: bold",
            "@gfinol"
          )
        },
        " on GitHub. I agree to follow the Typelevel CoC and GSoC AI policy."
      ),
      revealed
        .map(r => if r then p(s"My favorite programming language is Scala!") else div(s"")),
      button(
        onClick --> (_.foreach(_ => revealed.update(!_))),
        revealed.map(r =>
          if r then "Hide my favorite programming language"
          else "Click to learn my favorite programming language!")
      )
    )
  }

