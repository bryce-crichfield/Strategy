
case class Reader[T, A](g: T => A):
  def apply(d: T): A = g(d)
  def map [B] (f: A => B): Reader[T, B] =
    Reader(d => f(g(d)))
  def flatMap [B] (f: A => Reader[T, B]): Reader[T, B] =
    Reader(d => f(g(d))(d))

  def andThen[B](r: Reader[(T, A), B]): Reader[T, B] = Reader {
    t => r.apply((t, this.apply(t)))
  }


case class Size(w: Int, h: Int)
import java.awt.image.BufferedImage

case class Observable[A](next: () => Option[A], close: () => Unit)

type Closeable[A] = (A, () => Unit)
def Buffer: Reader[Size, Observable[BufferedImage]] = Reader { size =>
  var buffer = new BufferedImage(size.w, size.h,
    java.awt.image.BufferedImage.TYPE_INT_RGB)
  Observable(() => Option(buffer), () => buffer = null)
}

def Window: Reader[(Size, Observable[BufferedImage]), Observable[java.awt.Window]] = Reader { tuple =>
  val (size, obs) = tuple
  var window = new java.awt.Window(null) {
    override def paint(g: java.awt.Graphics): Unit =
      g.drawImage(obs.next().getOrElse(sys.error("Null Buffer")), 0, 0, null)
  }
  window.setVisible(true)
  window.setSize(size.w, size.h)
  Observable(() => Option(window), () => {
    obs.close()
    window.dispose()
    window = null
  })
}

def Screen: Reader[Size, Observable[java.awt.Window]] =
  Buffer.andThen(Window)




object Main extends App {
  import java.awt._

  def drawRect(x: Int, y: Int, w: Int, h: Int): Reader[Graphics, Unit] = Reader {
    graphics =>
      graphics.setColor(java.awt.Color(255, 0, 0))
      graphics.drawRect(x,y,w,h)
  }

  def drawLine(x1: Int, y1: Int, x2: Int, y2: Int): Reader[Graphics, Unit] = Reader {
    graphics =>
      graphics.setColor(java.awt.Color(0, 255, 0))
      graphics.drawLine(x1, y1, x2, y2)
  }

  // Note: from the user's perspective we cannot modify the graphics we receive
  // Ie: the left-hand value is constant through composition
  def graphicsProgram: Reader[Graphics, Unit] = for {
    _ <- drawRect(0, 0, 50, 50)
    _ <- drawRect(50, 50, 10, 10)
    _ <- drawLine(0, 0, 100, 100)
  } yield ()


  def runGraphicsProgram(graphics: Graphics): Unit =
    graphicsProgram.apply(graphics)


  def startScreen(size: Size): Observable[java.awt.Window] =
    Screen(size)


  val window = startScreen(Size(100, 100))

  println("Closing")
  window.close()
  while (true) { }
}
