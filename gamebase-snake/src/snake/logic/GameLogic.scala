package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.logic.GameLogic._

/** To implement Snake, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``snake`` package.
 */
class GameLogic(val random: RandomGenerator,
                val gridDims : Dimensions) {

  val initFrame : GameFrame = GameFrame(waitForStep = false, gameOver = false, gridDims, null, 0, Point(2,0), East(), List(Point(1,0), Point(0,0)))

  private var frames : SStack[GameFrame] = SStack[GameFrame](initFrame)

  private def currentFrame: GameFrame = frames.top

  def gameOver: Boolean = currentFrame.gameOver

  def waitForStep: Boolean = currentFrame.waitForStep

  var reverse : Boolean = false

  frames = frames.push(GameFrame(waitForStep = false, gameOver = false, gridDims, newApple(), 0, Point(2,0), East(), List(Point(1,0), Point(0,0))))

  def step(): Unit = {
    if(!reverse){
      if(!gameOver){
        frames = frames.push(currentFrame.moveSnake())

        if (currentFrame.snakeHead == currentFrame.apple){
          val applePos : Point = newApple()
          frames = frames.push(GameFrame(waitForStep = false, gameOver = false, gridDims, applePos, currentFrame.growQueue + 3, currentFrame.snakeHead, currentFrame.snakeDirection, currentFrame.snakeTail))
        }
      }
    }
    else if(frames.size > 2){
      val oldFrames = frames.pop
      if (!oldFrames.top.waitForStep && (oldFrames.top.apple != oldFrames.top.snakeHead)){
        frames = oldFrames
      }
      else {
        frames = oldFrames.pop
      }
    }
  }

  def newApple(): Point= {
    val nrFreeSpots = currentFrame.countFreeSpots()

    if (nrFreeSpots > 0){
      val randomValue : Int = random.randomInt(nrFreeSpots)

      var currentValue : Int = 0
      var currentGridPosition : Int = 0

      var result : Point = null

      while(currentValue < nrFreeSpots){
        val x : Int = currentGridPosition % gridDims.width
        val y : Int = currentGridPosition / gridDims.width
        result = Point(x, y)

        if (currentValue == randomValue && currentFrame.isFree(result)){
          return result
        }
        else if (currentFrame.isFree(result)){
          currentValue += 1
        }
        currentGridPosition += 1
      }
      result
    }
    else {
      frames = frames.push(currentFrame.GameOver())
      null
    }
  }

  def setReverse(r: Boolean): Unit = {
    if(r) reverse = true else reverse = false
  }

  def changeDir(d: Direction): Unit = {
    if(d != currentFrame.snakeDirection.opposite && d != currentFrame.snakeDirection && !waitForStep && !gameOver){
      frames = frames.push(currentFrame.changeDir(d))
    }
  }

  def getCellType(p : Point): CellType = {
    currentFrame.getCellType(p)
  }
}

case class GameFrame(
                      waitForStep : Boolean,
                      gameOver : Boolean,
                      gridDims: Dimensions,
                      apple : Point,
                      growQueue : Int,
                      snakeHead : Point,
                      snakeDirection : Direction,
                      snakeTail : List[Point]){

    def moveSnake(): GameFrame ={
      var moveHeadTo : Point = (snakeHead + snakeDirection.toPoint)

      if (moveHeadTo.y == gridDims.height){
        moveHeadTo = Point(moveHeadTo.x, 0)
      }
      else if (moveHeadTo.y == -1){
        moveHeadTo = Point(moveHeadTo.x, gridDims.height - 1)
      }
      if (moveHeadTo.x == gridDims.width){
        moveHeadTo = Point(0, moveHeadTo.y)
      }
      else if (moveHeadTo.x == -1){
        moveHeadTo = Point(gridDims.width - 1, moveHeadTo.y)
      }

      if (growQueue == 0){
        var movedTail : List[Point] = List()

        var moveNextBodyTo: Point = snakeHead

        for(st <- snakeTail){
          movedTail = movedTail :+ moveNextBodyTo
          moveNextBodyTo = st
        }

        if (movedTail contains moveHeadTo){
          GameOver()
        }
        else GameFrame(waitForStep = false, gameOver = false, gridDims, apple, growQueue, moveHeadTo, snakeDirection, movedTail)

      }
      else {
        val newTail : List[Point] = snakeHead +: snakeTail

        val newGrowQueue : Int = growQueue - 1

        if (newTail contains moveHeadTo){
          GameOver()
        }
        else GameFrame(waitForStep = false, gameOver = false, gridDims, apple, newGrowQueue, moveHeadTo, snakeDirection, newTail)
      }
    }

    def changeDir(d: Direction): GameFrame ={
        GameFrame(waitForStep = true, gameOver = false, gridDims, apple, growQueue, snakeHead, d, snakeTail)
    }

    def countFreeSpots() : Int = {
      var count : Int = 0
      for(x <- 0 until gridDims.width){
        for(y <- 0 until gridDims.height){
          if (isFree(Point(x, y))){
            count += 1
          }
        }
      }
      count
    }

    def isFree(p : Point): Boolean ={
      if (p == snakeHead || (snakeTail contains p) || (p == apple)){
        false
      }
      else true
    }

    def GameOver() : GameFrame ={
      GameFrame(waitForStep = false, gameOver = true, gridDims, apple, growQueue, snakeHead, snakeDirection, snakeTail)
    }

    def getCellType(p : Point): CellType = {
      if (isHead(p))         SnakeHead(snakeDirection)
      else if (isBody(p))    SnakeBody()
      else if (isApple(p))   Apple()
      else                   Empty()
    }

    private def isHead(p : Point)   : Boolean = snakeHead == p
    private def isBody(p : Point)    : Boolean = snakeTail contains (p)
    private def isApple(p : Point) : Boolean = apple == p
  }


/** GameLogic companion object */
object GameLogic {

  val FramesPerSecond: Int = 5 // change this to increase/decrease speed of game

  val DrawSizeFactor = 1.0 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller

  // These are the dimensions used when playing the game.
  // When testing the game, other dimensions are passed to
  // the constructor of GameLogic.
  //
  // DO NOT USE the variable DefaultGridDims in your code!
  //
  // Doing so will cause tests which have different dimensions to FAIL!
  //
  // In your code only use gridDims.width and gridDims.height
  // do NOT use DefaultGridDims.width and DefaultGridDims.height
  val DefaultGridDims
    : Dimensions =
    Dimensions(width = 25, height = 25)  // you can adjust these values to play on a different sized board

}


