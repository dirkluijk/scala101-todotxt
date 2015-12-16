package todo

import org.scalatest.{FlatSpec, MustMatchers}

import scala.util.Try

/**
  * @author Dirk Luijk <dirk@luijk.nl>
  */
class TestSpec extends FlatSpec with MustMatchers {

  "A simple text line" should "be a task" in {
    val task = parseTask("Call mom")

    task.isSuccess must be (right = true)
    task.get.description must be (Some("Call mom"))
  }

  "Priority" should "appear first" in {
    val tasks = parseTasks(
      "Lorem",
      "(C) Ipsum",
      "(b) dolor",
      "sid (a) amed"
    )

    tasks.isSuccess must be (true)

    tasks.get.head.priority must be (None)
    tasks.get(1).priority must be (Some("C"))
    tasks.get(2).priority must be (None)
    tasks.get(3).priority must be (None)
  }

  "Creation dates" should "appear after priority" in {
    val tasks = parseTasks(
      "Lorem",
      "(A) 1991-03-18 Ipsum",
      "(B) 231223-23 Lorem"
    )

    tasks.get.head.created must be (None)
    tasks.get(1).created must be (Some("1991-03-18 ")) // TODO: fix trailing space
    tasks.get(2).created must be (None)
  }

  "Contexts and projects" should "be recognized" in {
    val task = parseTask("(A) Call Mom +Family @iphone +PeaceLoveAndHappiness @phone")

    task.isSuccess must be (true)
    task.get.contexts must be (Set(Context("iphone"), Context("phone")))
    task.get.projects must be (Set(Project("Family"), Project("PeaceLoveAndHappiness")))
  }

  "A completed task" should "start with an x" in {
    val tasks = parseTasks(
      "x 2011-03-03 Call Mom",
      "xylophone lesson",
      "X 2012-01-01 Make resolutions",
      "A) x Find ticket prices"
    )

    tasks.get.head.completed must not be None
    tasks.get(1).completed must be (None)
    tasks.get(2).completed must be (None)
    tasks.get(3).completed must be (None)
  }

  "The completion date" should "appear directly after the x" in {
    val tasks = parseTasks(
      "x 2011-03-03 Call Mom",
      "xylophone lesson",
      "X 2012-01-01 Make resolutions",
      "A) x Find ticket prices"
    )

    tasks.get.head.completed must not be None
    tasks.get(1).completed must be (None)
    tasks.get(2).completed must be (None)
    tasks.get(3).completed must be (None)
  }

  private def parseTask(line: String): Try[Task] = {
    TodoParser(line).task.run()
  }

  private def parseTasks(lines: String*): Try[Seq[Task]] = {
    TodoParser(lines.mkString("\n")).tasks.run()
  }
}
