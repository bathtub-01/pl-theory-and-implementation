// numbers
val ZERO = lambda_Function("s", lambda_Function("z", lambda_Name("z")))
val ONE = lambda_Function("s", lambda_Function("z", lambda_Application(lambda_Name("s"), lambda_Name("z"))))
val TWO = lambda_Function("s", lambda_Function("z", lambda_Application(lambda_Name("s"),
                                                                       lambda_Application(lambda_Name("s"), lambda_Name("z")))))

def num_gen(number: Int): lambda_Expr = {
  def rec(n:Int, tmp: lambda_Expr): lambda_Expr = {
    if n == 0 then tmp
      else rec(n - 1, lambda_Application(S, tmp))
  }
  cbv_driver(rec(number, ZERO))
}

// successor
val S = {
  val wy = lambda_Application(lambda_Name("w"), lambda_Name("y"))
  val wyx = lambda_Application(wy, lambda_Name("x"))
  val ywyx = lambda_Application(lambda_Name("y"), wyx)
  lambda_Function("w", lambda_Function("y", lambda_Function("x", ywyx)))
}

val ADD = {
  val xs = lambda_Application(lambda_Name("x"), S)
  val xsy = lambda_Application(S, lambda_Name("y"))
  lambda_Function("x", lambda_Function("y", xsy))
}
def add(input1: lambda_Expr, input2: lambda_Expr): lambda_Expr = 
  cbv_driver(lambda_Application(lambda_Application(ADD, input1), input2))

val MUL = {
  val yz = lambda_Application(lambda_Name("y"), lambda_Name("z"))
  val xyz = lambda_Application(lambda_Name("x"), yz)
  lambda_Function("x", lambda_Function("y", lambda_Function("z", xyz)))
}
def mul(input1: lambda_Expr, input2: lambda_Expr): lambda_Expr = 
  cbv_driver(lambda_Application(lambda_Application(MUL, input1), input2))                                                                                              

// conditionals
val TRUE = lambda_Function("x", lambda_Function("y", lambda_Name("x")))
val FALSE = lambda_Function("x", lambda_Function("y", lambda_Name("y")))

val AND = {
  val xy = lambda_Application(lambda_Name("x"), lambda_Name("y"))
  val xyf = lambda_Application(xy, FALSE)
  lambda_Function("x", lambda_Function("y", xyf))
}                                                            
def and(input1: lambda_Expr, input2: lambda_Expr): lambda_Expr = 
  cbv_driver(lambda_Application(lambda_Application(AND, input1), input2))

val OR = {
  val xt = lambda_Application(lambda_Name("x"), TRUE)
  val xty = lambda_Application(xt, lambda_Name("y"))
  lambda_Function("x", lambda_Function("y", xty))
}
def or(input1: lambda_Expr, input2: lambda_Expr): lambda_Expr = 
  cbv_driver(lambda_Application(lambda_Application(OR, input1), input2))

val NEG = {
  val xf = lambda_Application(lambda_Name("x"), FALSE)
  val xft = lambda_Application(xf, TRUE)
  lambda_Function("x", xft)
}
def neg(input: lambda_Expr): lambda_Expr = cbv_driver(lambda_Application(NEG, input))

// if is ZERO
val Z = {
  val xf = lambda_Application(lambda_Name("x"), FALSE)
  val xfn = lambda_Application(xf, NEG)
  val xfnf = lambda_Application(xfn, FALSE)
  lambda_Function("x", xfnf)
}
def isZero(input: lambda_Expr): lambda_Expr = cbv_driver(lambda_Application(Z, input))

// pair
val PAIR = {
  val za = lambda_Application(lambda_Name("z"), lambda_Name("a"))
  val zab = lambda_Application(za, lambda_Name("b"))
  lambda_Function("a", lambda_Function("b", lambda_Function("z", zab)))
}
                                                                                      
// predecessor
val P = {
  val pt = lambda_Application(lambda_Name("p"), TRUE)
  val spt = lambda_Application(S, pt)
  val phi = lambda_Function("p", lambda_Application(lambda_Application(PAIR, spt), pt))
  val nphi = lambda_Application(lambda_Name("n"), phi)
  val pzz = lambda_Application(lambda_Application(PAIR, ZERO), ZERO)
  val nphipzz = lambda_Application(nphi, pzz)
  lambda_Function("n", lambda_Application(nphipzz, FALSE))
}

// Y combinator
val Y = {
  val yxx = lambda_Application(lambda_Name("y"), lambda_Application(lambda_Name("x"), lambda_Name("x")))
  val fxyxx = lambda_Function("x", yxx)
  lambda_Function("y", lambda_Application(fxyxx, fxyxx))
}

// recursion example1 - add up the first n natural numbers
val R = {
  val pn = lambda_Application(P, lambda_Name("n"))
  val rpn = lambda_Application(lambda_Name("r"), pn)
  val ns = lambda_Application(lambda_Name("n"), S)
  val nsrpn = lambda_Application(ns, rpn)
  val zn = lambda_Application(Z, lambda_Name("n"))
  val zn0 = lambda_Application(zn, ZERO)
  lambda_Function("r", lambda_Function("n", lambda_Application(zn0, nsrpn)))
}

// recursion example2 - Fibonacci Number
val FIB = {
  val pn = lambda_Application(P, lambda_Name("n"))
  val twopn = lambda_Application(lambda_Application(TWO, P), lambda_Name("n"))
  val rtwopn = lambda_Application(lambda_Name("r"), twopn)
  val rpn = lambda_Application(lambda_Name("r"), pn)
  val rpns = lambda_Application(rpn, S)
  val rpnsrtwopn = lambda_Application(rpns, rtwopn)
  val zpn = lambda_Application(Z, pn)
  val zpnone = lambda_Application(zpn, ONE)
  lambda_Function("r", lambda_Function("n", lambda_Application(zpnone, rpnsrtwopn)))
}
