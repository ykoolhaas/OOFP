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

  def removeRow(y : Int): Tetromino
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

  def removeRow(y: Int): standardTetromino = {
    val squaresAfterRemoval : List[Point] = squares.filter(point => point.y != y)
    standardTetromino(cellType, anchorPoint, squaresAfterRemoval)
  }

  def moveLeft() : standardTetromino = standardTetromino(cellType, anchorPoint + Point(-1, 0), squares)
  def moveRight() : standardTetromino = standardTetromino(cellType, anchorPoint + Point(1, 0), squares)
  def moveDown() : standardTetromino =  standardTetromino(cellType, anchorPoint + Point(0, 1), squares)
}

case class O_Tetromino(cellType: CellType, anchorPoint : Point, squares : List[Point]) extends Tetromino {
  def rotateLeft(): O_Tetromino = this
  def rotateRight() : O_Tetromino = this

  def removeRow(y: Int): O_Tetromino = {
    val squaresAfterRemoval : List[Point] = squares.filter(point => point.y != y)
    O_Tetromino(cellType, anchorPoint, squaresAfterRemoval)
  }

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

  def removeRow(y: Int): I_Tetromino = {
    val squaresAfterRemoval : List[Point] = squares.filter(point => point.y != y)
    I_Tetromino(cellType, anchorPoint, squaresAfterRemoval)
  }

  def moveLeft() : I_Tetromino = I_Tetromino(cellType, anchorPoint + Point(-1, 0), squares)
  def moveRight() : I_Tetromino = I_Tetromino(cellType, anchorPoint + Point(1, 0), squares)
  def moveDown() : I_Tetromino = I_Tetromino(cellType, anchorPoint + Point(0, 1), squares)
}

