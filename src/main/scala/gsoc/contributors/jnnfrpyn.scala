package gsoc
package contributors

import cats.effect.*
import fs2.concurrent.*
import fs2.dom.HtmlElement
import calico.html.io.{*, given}
import calico.syntax.*

val jnnfrpyn: Contributor = Contributor("jnnfrpyn"):
  SignallingRef[IO].of(0).toResource.flatMap { count =>
    div(
      styleAttr := "padding: 24px;",
      p(
        "Hi folks! I'm ",
        span(styleAttr := "color: #fd6161; font-weight: bold", "@jnnfrpyn "),
        " on GitHub. I heartily agree to follow the Typelevel CoC and GSoC AI policy! ❤️"
      ),
      button(
        onClick --> (_.foreach(_ => count.update(_ + 1))),
        "Add a circle!"
      ),
      count.map(n =>
        div(
          styleAttr := "display: flex; gap: 8px; flex-wrap: wrap;",
          List.fill(n)(
            div(
              styleAttr := "width: 40px; height: 40px; border-radius: 50%; background-color: #fd6161"
            )
          )
        )
      )
    )
  }
