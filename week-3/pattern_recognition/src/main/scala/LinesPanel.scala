import scala.swing.Swing._
import scala.swing.{MainFrame, Panel}
import scala.swing.event._
import java.awt.{BasicStroke, Color, Graphics2D, geom}

/**
  * Created by droidman on 3/25/17.
  */
object LinesPanel {

  def ui(lines: Iterable[LineSegment], points: Set[Point]) = new Panel {

    background = Color.white
    preferredSize = (1920, 1080)

    override def paintComponent(g: Graphics2D) {
      super.paintComponent(g)
      g.setColor(Color.BLACK)
      g.scale(0.03, 0.03)
      val g2 = g.asInstanceOf[Graphics2D]
      g2.setStroke(new BasicStroke(3))
      for(p <- points) {
        g2.drawLine(p.x.toInt, p.y.toInt, p.x.toInt, p.y.toInt)
      }
      for(line <- lines){
        g2.drawLine(line.p.x.toInt, line.p.y.toInt, line.q.x.toInt, line.q.y.toInt)
      }
    }
  }
}

