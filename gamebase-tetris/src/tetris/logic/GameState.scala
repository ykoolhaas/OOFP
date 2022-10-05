package tetris.logic

import engine.random.RandomGenerator

case class GameState(
                      gameOver : Boolean,
                      tetrominoes : List[Tetromino],
                      gridDims : Dimensions,
                      randomGen: RandomGenerator
                    ) {

  def updateTetromino(updatedTetromino : Tetromino) : List[Tetromino] = {
    val tetrominoesAfterUpdate : List[Tetromino] = updatedTetromino +: tetrominoes.tail
    tetrominoesAfterUpdate
  }

  def newTetromino(): GameState ={
    val position : Point = Point((gridDims.width-1)/2, 1)
    randomGen.randomInt(7) match {
      case 0 => GameState(gameOver, I_Tetromino(ICell, position, List(Point(-1, 0),  Point(0, 0), Point(1, 0),  Point(2, 0))) +: tetrominoes, gridDims, randomGen)
      case 1 => GameState(gameOver, standardTetromino(JCell, position, List(Point(-1, -1), Point(-1, 0), Point(0, 0),  Point(1, 0))) +: tetrominoes, gridDims, randomGen)
      case 2 => GameState(gameOver, standardTetromino(LCell, position, List(Point(-1, 0),  Point(0, 0),  Point(1, 0),  Point(1, -1))) +: tetrominoes, gridDims, randomGen)
      case 3 => GameState(gameOver, O_Tetromino(OCell, position, List(Point(0, 0),   Point(0, -1), Point(1, -1), Point(1, 0))) +: tetrominoes, gridDims, randomGen)
      case 4 => GameState(gameOver, standardTetromino(SCell, position, List(Point(-1, 0),  Point(0, 0),  Point(0, -1), Point(1, -1))) +: tetrominoes, gridDims, randomGen)
      case 5 => GameState(gameOver, standardTetromino(TCell, position, List(Point(-1, 0),  Point(0, 0),  Point(0, -1), Point(1, 0))) +: tetrominoes, gridDims, randomGen)
      case 6 => GameState(gameOver, standardTetromino(ZCell, position, List(Point(-1, -1), Point(0, -1), Point(0, 0),  Point(1, 0))) +: tetrominoes, gridDims, randomGen)
    }
  }

  def handleFullRows(y : Int, gameState: GameState): GameState ={
    if (y >= gridDims.height){
      return gameState
    }
    if(rowIsFull(y)){
      handleFullRows(y + 1, gameState.removeFullRow(y))
    }
    else handleFullRows(y + 1, gameState)
  }

  def removeFullRow(y : Int) : GameState= {
    val newTetrominoes : List[Tetromino] = tetrominoes.map(tetromino => tetromino.removeRow(y))
    GameState(gameOver, newTetrominoes, gridDims, randomGen)
  }

  def rowIsFull(y : Int): Boolean = {
    for(x <- 0 until gridDims.width){
      val currentPoint : Point = Point(x, y)
      if (!isOccupied(currentPoint)){
        return false
      }
    }
    true
  }

  def isOccupied(p : Point): Boolean = {
    for(el <- tetrominoes.tail){
      val actualPoints : List[Point] = el.squares.map(point => point + el.anchorPoint)
      if(actualPoints contains p){
        return true
      }
    }
    false
  }

  def isOutOfBounds(p : Point): Boolean = {
    if(p.x >= gridDims.width || p.x < 0 || p.y >= gridDims.height || p.y < 0){
      true
    }
    else false
  }

  def rotateLeft(): GameState = {
    val newTetrominoes : List[Tetromino] = updateTetromino(tetrominoes.head.rotateLeft())
    for(p <- newTetrominoes.head.squares){
      val actualPoint : Point = p + newTetrominoes.head.anchorPoint
      if(isOccupied(actualPoint) || isOutOfBounds(actualPoint)) return this
    }
    GameState(gameOver, newTetrominoes, gridDims, randomGen)
  }

  def rotateRight(): GameState = {
    val newTetrominoes : List[Tetromino] = updateTetromino(tetrominoes.head.rotateRight())
    for(p <- newTetrominoes.head.squares){
      val actualPoint : Point = p + newTetrominoes.head.anchorPoint
      if(isOccupied(actualPoint) || isOutOfBounds(actualPoint)) return this
    }
    GameState(gameOver, newTetrominoes, gridDims, randomGen)
  }

  def moveLeft(): GameState = {
    val newTetrominoes : List[Tetromino] = updateTetromino(tetrominoes.head.moveLeft())
    for(p <- newTetrominoes.head.squares){
      val actualPoint : Point = p + newTetrominoes.head.anchorPoint
      if(isOccupied(actualPoint) || isOutOfBounds(actualPoint)) return this
    }
    GameState(gameOver, newTetrominoes, gridDims, randomGen)
  }

  def moveRight(): GameState = {
    val newTetrominoes : List[Tetromino] = updateTetromino(tetrominoes.head.moveRight())
    for(p <- newTetrominoes.head.squares){
      val actualPoint : Point = p + newTetrominoes.head.anchorPoint
      if(isOccupied(actualPoint) || isOutOfBounds(actualPoint)) return this
    }
    GameState(gameOver, newTetrominoes, gridDims, randomGen)
  }

  def moveDown(): GameState = {
    val newTetrominoes : List[Tetromino] = updateTetromino(tetrominoes.head.moveDown())
    for(p <- newTetrominoes.head.squares){
      val actualPoint : Point = p + newTetrominoes.head.anchorPoint
      if(isOccupied(actualPoint) || isOutOfBounds(actualPoint)) {
        return newTetromino()
      }
    }
    GameState(gameOver, newTetrominoes, gridDims, randomGen)
  }

  def doHardDrop(tetromino: Tetromino): GameState = {
    for(p <- tetromino.squares){
      val actualPoint : Point = p + tetromino.moveDown().anchorPoint
      if(isOccupied(actualPoint) || isOutOfBounds(actualPoint)) return GameState(gameOver, updateTetromino(tetromino), gridDims, randomGen)
    }
    doHardDrop(tetromino.moveDown())
  }
}