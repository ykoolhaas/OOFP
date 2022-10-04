package tetris.logic

abstract class Tetromino{
  val cellType : CellType
  val anchorPoint : Point
  val squares : List[Point]

  def rotateLeft(): Tetromino
  def rotateRight() : Tetromino
  def moveLeft() : Tetromino
  def moveRight() : Tetromino
  def moveDown() : Tetromino
}

case class standardTetromino(cellType: CellType, anchorPoint : Point, squares : List[Point]) extends Tetromino {
  def rotateLeft(): standardTetromino = {
    def rotate(p: Point) : Point = Point(p.y, -p.x)
    val squaresAfterRotation = squares.map(rotate)
    standardTetromino(cellType, anchorPoint, squaresAfterRotation)
  }
  def rotateRight() : standardTetromino = {
    def rotate(p: Point) : Point = Point(-p.y, p.x)
    val squaresAfterRotation = squares.map(rotate)
    standardTetromino(cellType, anchorPoint, squaresAfterRotation)
  }

  def moveLeft() : standardTetromino = standardTetromino(cellType, anchorPoint + Point(-1, 0), squares)
  def moveRight() : standardTetromino = standardTetromino(cellType, anchorPoint + Point(1, 0), squares)
  def moveDown() : standardTetromino =  standardTetromino(cellType, anchorPoint + Point(0, 1), squares)
}

case class O_Tetromino(cellType: CellType, anchorPoint : Point, squares : List[Point]) extends Tetromino {
  def rotateLeft(): O_Tetromino = this
  def rotateRight() : O_Tetromino = this

  def moveLeft() : O_Tetromino = O_Tetromino(cellType, anchorPoint + Point(-1, 0), squares)
  def moveRight() : O_Tetromino = O_Tetromino(cellType, anchorPoint + Point(1, 0), squares)
  def moveDown() : O_Tetromino = O_Tetromino(cellType, anchorPoint + Point(0, 1), squares)
}

case class I_Tetromino(cellType: CellType, anchorPoint : Point, squares : List[Point]) extends Tetromino {
  def rotateLeft(): I_Tetromino = {
    def rotate(p: Point) : Point = Point(p.y, -p.x + 1)
    val squaresAfterRotation = squares.map(rotate)
    I_Tetromino(cellType, anchorPoint, squaresAfterRotation)
  }
  def rotateRight() : I_Tetromino = {
    def rotate(p: Point) : Point = Point(-p.y + 1, p.x)
    val squaresAfterRotation = squares.map(rotate)
    I_Tetromino(cellType, anchorPoint, squaresAfterRotation)
  }

  def moveLeft() : I_Tetromino = I_Tetromino(cellType, anchorPoint + Point(-1, 0), squares)
  def moveRight() : I_Tetromino = I_Tetromino(cellType, anchorPoint + Point(1, 0), squares)
  def moveDown() : I_Tetromino = I_Tetromino(cellType, anchorPoint + Point(0, 1), squares)
}

