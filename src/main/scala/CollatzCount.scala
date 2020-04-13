import scala.collection.mutable

object CollatzCount {

  def main(args: Array[String]) = {
    val start = 32

    collatzFinalList(start, printer)

    collatzFinalStack(start, printer)

  }

  def collatzInnerList(n: Int, acc: List[Int]): List[Int] = {
    if (n == 1) n :: acc
    else if (n % 2 == 0) collatzInnerList(n/2, n :: acc )
    else collatzInnerList(3*n + 1, n :: acc )
  }

  def collatzFinalList(n: Int, fun: (Int, Int)=>Unit): Unit = {
    val acc = collatzInnerList(n, List())
    acc.foldLeft(0){ (ctr, e) =>
      fun(e, ctr)
      ctr + 1
    }
  }

  def collatzInnerStack(n: Int, stack: mutable.Stack[Int]): mutable.Stack[Int] = {
    if (n == 1) {
      stack.push(n)
      stack
    } else if (n % 2 == 0) {
      stack.push(n)
      collatzInnerStack(n/2, stack)
    } else {
      stack.push(n)
      collatzInnerStack(3*n + 1, stack)
    }
  }

  def popStack(ctr: Int, stack: mutable.Stack[Int], fun: (Int, Int)=>Unit): Unit = {
    if (stack.nonEmpty) {
      val popped = stack.pop
      fun(popped, ctr)
      popStack(ctr + 1, stack, fun)
    } else ()
  }


  def collatzFinalStack(n: Int, fun: (Int, Int) => Unit): Unit = {
    val stack = collatzInnerStack(n, mutable.Stack())
    popStack(0, stack, fun)
  }


  val printer = (x: Int, y: Int) => println("Num ->" + x + " " + " " + "Acc -> " + y)

}
