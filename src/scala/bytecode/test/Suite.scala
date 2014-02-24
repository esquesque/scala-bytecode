package scala.bytecode.test

trait Suite {
  def cases: List[Case[_]]
  def transforms: List[scala.bytecode.MethodInfo.Transform]

  def main(args: Array[String]) {
    val tfms = transforms
    val len = tfms.length
    val out = Array.fill(len + 1)(new java.io.ByteArrayOutputStream)
    val pnt = Array.tabulate(len + 1)(x => new java.io.PrintStream(out(x), true))
    def str(x: Int) = out(x).toString
    val results = for (c <- cases) yield {
      val method = c.method
      println(">>>>>"+ method)
      val result = try {
	for (x <- 0 until len) {
	  method.instructions.out(pnt(x))
	  method.apply(tfms(x))
	}
	val r = c.apply
	c match {
	  case astc: ASTCase => astc.tree.out(pnt(len), 2)
	  case _ =>
	}
	r
      } catch { case x: Throwable => x.printStackTrace; false }
      if (result) {
	println(">PASS\n.....")
	print(str(len))
      } else {
	println(">FAIL")
	for (x <- 0 until len) {
	  println(str(x))
	  println("....."+ tfms(x).getClass.getSimpleName +".....")
	}
	method.instructions.out()
	println(".....")
	print(str(len))
      }
      out foreach (_.reset)
      result
    }
    System.exit(if (results reduce (_ & _)) {
      println(">"+ results.length +" OK")
      0
    } else {
      println(">"+ (results filter (_ == true)).length +" PASSED")
      println(">"+ (results filter (! _)).length +" FAILED")
      1
    } )
  }
}
