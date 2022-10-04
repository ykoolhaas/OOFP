package tetris.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.logic.SStack
import tetris.logic.TetrisLogic._

/** To implement Tetris, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``tetris`` package.
 */
class TetrisLogic(val randomGen: RandomGenerator,
                  val gridDims : Dimensions,
                  val initialBoard: Seq[Seq[CellType]]) {

  def this(random: RandomGenerator, gridDims : Dimensions) =
    this(random, gridDims, makeEmptyBoard(gridDims))

  def this() =
    this(new ScalaRandomGen(), DefaultDims, makeEmptyBoard(DefaultDims))

  val nrTetrominoes : Int = 7

  var currentGameState : GameState = GameState(gameOver = false, List(), gridDims, randomGen)
  currentGameState = currentGameState.newTetromino()

  def currentTetromino : Tetromino = currentGameState.tetrominoes.head

  def rotateLeft(): Unit = (currentGameState = currentGameState.rotateLeft())

  def rotateRight(): Unit = (currentGameState = currentGameState.rotateRight())

  def moveLeft(): Unit = (currentGameState = currentGameState.moveLeft())

  def moveRight(): Unit = (currentGameState = currentGameState.moveRight())

  def moveDown(): Unit = (currentGameState = currentGameState.moveDown())

  def doHardDrop(): Unit = (currentGameState = currentGameState.doHardDrop(currentTetromino))

  def isGameOver: Boolean = currentGameState.gameOver

  def getCellType(p : Point): CellType = {
    for(tetromino <- currentGameState.tetrominoes){
      for(square <- tetromino.squares){
        if((square + tetromino.anchorPoint) == p){
          return tetromino.cellType
        }
      }
    }
    Empty
  }
}

object TetrisLogic {

  val FramesPerSecond: Int = 5 // change this to speed up or slow down the game

  val DrawSizeFactor = 1.0 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller



  def makeEmptyBoard(gridDims : Dimensions): Seq[Seq[CellType]] = {
    val emptyLine = Seq.fill(gridDims.width)(Empty)
    Seq.fill(gridDims.height)(emptyLine)
  }


  // These are the dimensions used when playing the game.
  // When testing the game, other dimensions are passed to
  // the constructor of GameLogic.
  //
  // DO NOT USE the variable DefaultGridDims in your code!
  //
  // Doing so will cause tests which have different dimensions to FAIL!
  //
  // In your code only use gridDims.width and gridDims.height
  // do NOT use DefaultDims.width and DefaultDims.height


  val DefaultWidth: Int = 10
  val NrTopInvisibleLines: Int = 4
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines
  val DefaultDims : Dimensions = Dimensions(width = DefaultWidth, height = DefaultHeight)


  def apply() = new TetrisLogic(new ScalaRandomGen(),
    DefaultDims,
    makeEmptyBoard(DefaultDims))

}