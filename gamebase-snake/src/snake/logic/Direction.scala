package snake.logic

// you can alter this file!

/** Given a direction `d` : you can decide
 * which one it is as follows:
 *
 * {{{
 * d match {
 *   case East() => ...
 *   case North() => ...
 *   case West() => ...
 *   case South() => ...
 * }
 * }}}
 */
sealed abstract class Direction {
  def opposite : Direction
  def toPoint : Point
}

case class East()   extends Direction  {
  def opposite : West   = West()
  def toPoint : Point = Point(1,0)
}
case class North()  extends Direction  {
  def opposite : South  = South()
  def toPoint : Point = Point(0,-1)
}
case class West()   extends Direction  {
  def opposite : Direction = East()
  def toPoint : Point = Point(-1,0)
}
case class South()  extends Direction  {
  def opposite : Direction= North()
  def toPoint : Point = Point(0,1)
}
