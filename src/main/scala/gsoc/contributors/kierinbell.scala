package gsoc
package contributors

import cats.effect.*
import cats.syntax.all.*
import fs2.concurrent.*
import calico.html.io.{*, given}
import calico.syntax.*
import fs2.dom.HtmlElement

import java.time.{LocalDate, ZoneId}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

val kierinbell: Contributor = Contributor("kierinbell"):

  case class Period(name: String, startDate: LocalDate, endDate: LocalDate) {
    def contains(date: LocalDate): Boolean = {
      !date.isBefore(startDate) && !date.isAfter(endDate)
    }
  }

  def makeDateOutput(
      name: String,
      outputForId: String,
      date: Option[LocalDate]): Resource[IO, HtmlElement[IO]] =
    outputTag(
      nameAttr := name,
      forId := outputForId,
      styleTag(
        """|@scope {
           |  :scope {
           |    padding-inline: 0.5em;
           |  }
           |}
        """.stripMargin
      ),
      date match {
        case Some(d) =>
          timeTag(
            d.format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))
          )
        case None =>
          span("None")
      }
    )

  val periods = List(
    Period(
      "Onboarding Period",
      LocalDate.of(2026, 3, 1),
      LocalDate.of(2026, 3, 15)
    ),
    Period(
      "Contributor Application Period",
      LocalDate.of(2026, 3, 16),
      LocalDate.of(2026, 3, 31)
    ),
    Period(
      "Proposal Review Period",
      LocalDate.of(2026, 4, 1),
      LocalDate.of(2026, 4, 21)
    ),
    Period(
      "Waiting for Announcement",
      LocalDate.of(2026, 4, 22),
      LocalDate.of(2026, 4, 30)
    ),
    Period(
      "Community Bonding Period",
      LocalDate.of(2026, 5, 1),
      LocalDate.of(2026, 5, 24)
    ),
    Period(
      "Coding Period (part 1)",
      LocalDate.of(2026, 5, 25),
      LocalDate.of(2026, 7, 5)
    ),
    Period(
      "Evaluations",
      LocalDate.of(2026, 7, 6),
      LocalDate.of(2026, 7, 10)
    ),
    Period(
      "Coding Period (part 2)",
      LocalDate.of(2026, 7, 11),
      LocalDate.of(2026, 8, 16)
    ),
    Period(
      "Final Evaluations",
      LocalDate.of(2026, 8, 17),
      LocalDate.of(2026, 8, 24)
    ),
    Period(
      "Extended Coding Period",
      LocalDate.of(2026, 8, 25),
      LocalDate.of(2026, 11, 2)
    ),
    Period(
      "Final Evaluations (extended deadlines)",
      LocalDate.of(2026, 11, 3),
      LocalDate.of(2026, 11, 9)
    )
  )

  val today = LocalDate.now(ZoneId.of("UTC"))
  val initialPeriod = periods.find(_.contains(today))

  SignallingRef[IO].of(initialPeriod).toResource.flatMap { period =>
    articleTag(
      h2("Greetings"),
      sectionTag(
        cls := "introduction",
        h3("About me"),
        addressTag(
          "I'm ",
          a(href := "https://github.com/kierinbell", "@kierinbell"),
          " on GitHub."
        ),
        p(
          "I like functional programming and web standards."
        ),
        p(
          "I agree to follow the Typelevel CoC and GSoC AI policy."
        )
      ),
      sectionTag(
        cls := "information",
        h3("Google Summer of Code"),
        form(
          idAttr := "period-tracker",
          fieldSet(
            legend(
              "Select period"
            ),
            label(
              forId := "current-date",
              "Date: "
            ),
            input.withSelf { self =>
              (
                idAttr := "date",
                typ := "date",
                value := today.toString,
                onInput --> (_.foreach(_ =>
                  self
                    .value
                    .get
                    .flatMap(value =>
                      val parsed = LocalDate.parse(value)
                      period.set(periods.find(_.contains(parsed))))))
              )
            },
            label(
              forId := "period-select",
              "Period: "
            ),
            select.withSelf { self =>
              (
                idAttr := "period-select",
                option(
                  disabled := true,
                  selected := true,
                  value := "",
                  "N/A"
                ),
                periods.map(period =>
                  option(
                    value := period.name,
                    period.name
                  )),
                value <-- period.map(_.map(_.name)),
                onChange --> (_.foreach(_ =>
                  self.value.get.flatMap(value => period.set(periods.find(_.name == value)))))
              )
            }
          ),
          fieldSet(
            legend(
              "Period status"
            ),
            label(
              forId := "period-start",
              "Start date: "
            ),
            period.map(p =>
              makeDateOutput("period-start", "date period-select", p.map(_.startDate))),
            label(
              forId := "period-end",
              "End date: "
            ),
            period
              .map(p => makeDateOutput("period-end", "date period-select", p.map(_.endDate))),
            period.map {
              case Some(p) if p.contains(today) =>
                val start = p.startDate
                val end = p.endDate
                val total = start.until(end, ChronoUnit.DAYS)
                val elapsed = start.until(today, ChronoUnit.DAYS)
                val progress = (elapsed.toDouble / total) * 100

                div(
                  label(
                    forId := "period-progress",
                    "Period in progress: "
                  ),
                  progressTag(
                    idAttr := "period-progress",
                    maxAttr := "100",
                    value := progress.toString,
                    s"${progress.toInt}%"
                  )
                )
              case _ =>
                p("Period not in progress.")
            }
          )
        )
      )
    )
  }
