import cats.data.{IndexedStateT, Reader, State, StateT}
import monocle.Lens
import scala.util.Left

import scala.language.postfixOps


type Error[A] = Either[String, A]
type Operation[S, A] = StateT[Error, S, A]


case class Context(stack: Stack, table: Table, code: Code)
type ContextOperation[A] = StateT[Error, Context, A]
object Context {

  def taps(f: => Unit): ContextOperation[Unit] =
    StateT[Error, Context, Unit] { context =>
      f
      Right(context -> ())
    }
  end taps

  def noop: ContextOperation[Unit] =
    StateT[Error, Context, Unit] { context =>
      Right(context -> ())
    }
  end noop

  def does[S, A](s: Operation[S, A])(using lens: Lens[Context, S])
    : ContextOperation[A] = StateT[Error, Context, A] { context =>
    s.run(lens.get(context)) match
      case Left(msg) => Left(msg)
      case Right((o, a)) =>
        Right(lens.replace(o)(context) -> a)
    }
  end does

  def stack[A](op: StackOperation[A]): ContextOperation[A] =
    import implicits.StackLens
    Context.does[Stack, A](op)
  end stack

  def table[A](op: TableOperation[A]): ContextOperation[A] =
    import implicits.TableLens
    Context.does[Table, A](op)
  end table

  def code[A](op: CodeOperation[A]): ContextOperation[A] =
    import implicits.CodeLens
    Context.does[Code, A](op)
  end code
}


type Table = Map[String, Int]
type TableOperation[A] = StateT[Error, Table, A]
object Table {
  def lookup(value: String): TableOperation[Int] =
    StateT[Error, Table, Int] { table =>
      table.get(value) match
        case None => Left(f"lookup : $value not found")
        case Some(out) => Right(table, out)
    }
  end lookup

  def rewrite(k: String)(v: Int): TableOperation[Unit] =
    StateT[Error, Table, Unit] { table =>
      Right((table.updated(k, v), ()))
    }
  end rewrite
}


type Code = (Int, Array[Int])
type CodeOperation[A] = StateT[Error, Code, A]
object Code {

}


type Stack = List[Int]
type StackOperation[A] = StateT[Error, Stack, A]
object Stack {
  def push(value: Int): StackOperation[Unit] =
    StateT.modify[Error, Stack] { stack => value::stack }
  end push

  def pop: StackOperation[Int] =
    StateT[Error, Stack, Int] { stack =>
      stack match
        case Nil => Left("pop : stack empty")
        case a :: as => Right((as, a))
    }
  end pop

  def top: StackOperation[Int] =
    StateT[Error, Stack, Int] { stack =>
      stack match
        case Nil => Left("pop : stack empty")
        case a :: as => Right((a::as, a))
    }
  end top

  def add: StackOperation[Unit] = for {
    a <- pop
    b <- pop
    _ <- push(a + b)
  } yield ()
  end add
}



object implicits {
  given TableLens: Lens[Context, Table] =
    Lens[Context, Table] { (c: Context) => c.table } {
      (t: Table) => (c: Context) => c.copy(table = t)
    }

  given StackLens: Lens[Context, Stack] =
    Lens[Context, Stack] { (c: Context) => c.stack } {
      (s: Stack) => (c: Context) => c.copy(stack = s)
    }

  given CodeLens: Lens[Context, Code] =
    Lens[Context, Code] { (c: Context) => c.code } {
      (d: Code) => (c: Context) => c.copy(code = d)
    }

  given Id: Lens[Context, Context] =
    Lens[Context, Context] { t => t} { t1 => t2 => t1 }
}


object Playground {

  import implicits.{StackLens, TableLens, Id}

  def readIntoStack(k: String): ContextOperation[Unit] = for {
    o <- Context.does(Table.lookup(k))
    _ <- Context.does(Stack.push(o))
  } yield ()

  def readIntoTable(k: String): ContextOperation[Unit] = for {
    o <- Context.does(Stack.pop)
    _ <- Context.does(Table.rewrite(k)(o))
  } yield ()

  def program: ContextOperation[Unit] =
    def loop: ContextOperation[Unit] =
      for {
        _ <- Context does Context.noop
//        _ <- Context taps println("Loop Start")
//        _ <- Context does Stack.push(1)
//        _ <- Context does Stack.add
//        o <- Context does Stack.top
//        _ <- if o == 0 then Context.noop else loop
      } yield ()
    end loop

    for {
      _ <- Context does Stack.push(-5)
      _ <- loop
    } yield ()
  end program

  @main def main(): Unit = {
    val ctx = Context(List(), Map(), (0, Array()))
    val ctx_out = program.run(ctx)
    println(ctx_out)
    println("DONE")
  }
}
//
//object Generate {
//  case class Conditional():
//    def gen: ContextOperation[Boolean] = ???
//    end gen
//  end Conditional
//
//
//  case class Statement():
//    def gen: ContextOperation[Unit] = ???
//    end gen
//  end Conditional
//
//  case IfElse(cond: Conditional[Boolean], If: Statement, Else: Statement):
//    def gen: ContextOperation[Unit] = for {
//      b <- cond.gen
//      _ <- if b then If.gen else Else.gen
//    } yield ()
//}