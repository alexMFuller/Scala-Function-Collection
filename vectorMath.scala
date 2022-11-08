object Main extends App {
  def vectorAdd(x: List[Int], y: List[Int]): List[Int] = {

    // val L = zip(x,y)
    // fun add x = map (fn (a,b) => a+b) x
    for {
      (a, b) <- x zip y
    } yield a + b
  }

  println(vectorAdd(List(1, 2, 3), List(4, 5, 6)))

  def svProduct(x: Int, y: List[Int]): List[Int] = {
    // fun svProduct x y =  map (fn c => c*x) y;
    for {
      i <- y
    } yield i * x

  }
  println(svProduct(2, List(1, 2, 3)))

  def vmProduct(x: List[Int], y: List[List[Int]]): List[Int] = {
    // fun vmProduct a b = reduce (fn (x,y) => vectorAdd x y) (map (fn (x,y) => svProduct x y) (zip(a, b)));

    val j = (for {
      (a, b) <- x zip y
      j <- List(svProduct(a, b))
    } yield j)

    val i = j.reduce((c, d) => vectorAdd(c, d))

    i
  }

  println(vmProduct(List(1, 2, 3), List(List(1, 1), List(2, 1), List(3, 1))))

  def matrixProduct(
      x: List[List[Int]],
      y: List[List[Int]]
  ): List[List[Int]] = {
    // fun matrixProduct L L2  = map (fn c => vmProduct c L2) L;

    for {
      v <- x
      i <- List(vmProduct(v, y))
    } yield i

  }

  println(
    matrixProduct(
      List(List(1, 2, 3), List(1, 1, 1)),
      List(List(1, 1), List(2, 1), List(3, 1))
    )
  )

}
