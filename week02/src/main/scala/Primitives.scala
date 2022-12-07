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
val S = lambda_Function("w", lambda_Function("y", lambda_Function("x", 
                                                                  lambda_Application(lambda_Name("y"),
                                                                                     lambda_Application(lambda_Application(lambda_Name("w"), lambda_Name("y")), lambda_Name("x"))))))

def add(input1: lambda_Expr, input2: lambda_Expr): lambda_Expr = 
  cbv_driver(lambda_Application(lambda_Application(input1, S), input2))

val MUL = lambda_Function("x", lambda_Function("y", lambda_Function("z",
                                                                    lambda_Application(lambda_Name("x"),
                                                                                       lambda_Application(lambda_Name("y"),
                                                                                                          lambda_Name("z"))))))
def mul(input1: lambda_Expr, input2: lambda_Expr): lambda_Expr = 
  cbv_driver(lambda_Application(lambda_Application(MUL, input1), input2))                                                                                              

// conditionals
val TRUE = lambda_Function("x", lambda_Function("y", lambda_Name("x")))
val FALSE = lambda_Function("x", lambda_Function("y", lambda_Name("y")))

val AND = lambda_Function("x", lambda_Function("y", lambda_Application(lambda_Application(lambda_Name("x"),
                                                                                          lambda_Name("y")),
                                                                       FALSE)))
def and(input1: lambda_Expr, input2: lambda_Expr): lambda_Expr = 
  cbv_driver(lambda_Application(lambda_Application(AND, input1), input2))

val OR = lambda_Function("x", lambda_Function("y", lambda_Application(lambda_Application(lambda_Name("x"), TRUE),
                                                                      lambda_Name("y"))))
def or(input1: lambda_Expr, input2: lambda_Expr): lambda_Expr = 
  cbv_driver(lambda_Application(lambda_Application(OR, input1), input2))                                                              

val NEG = lambda_Function("x", lambda_Application(lambda_Application(lambda_Name("x"), 
                                                                     FALSE), 
                                                  TRUE))
def neg(input: lambda_Expr): lambda_Expr = cbv_driver(lambda_Application(NEG, input))

// if is ZERO
val Z = lambda_Function("x", lambda_Application(lambda_Application(lambda_Application(lambda_Name("x"), FALSE), NEG), 
                                                FALSE))
def isZero(input: lambda_Expr): lambda_Expr = cbv_driver(lambda_Application(Z, input))

// pair
val PAIR = lambda_Function("a", lambda_Function("b", lambda_Function("z", 
                                                                     lambda_Application(lambda_Application(lambda_Name("z"), 
                                                                                                           lambda_Name("a")), 
                                                                                        lambda_Name("b")))))

// predecessor
val P = {
  val pt = lambda_Application(lambda_Name("p"), TRUE)
  val spt = lambda_Application(S, pt)
  val phi = lambda_Function("p", lambda_Application(lambda_Application(PAIR, spt), pt))
  lambda_Function("n", lambda_Application(lambda_Application(lambda_Application(lambda_Name("n"), phi),
                                                             lambda_Application(lambda_Application(PAIR, ZERO), ZERO)),
                                          FALSE))
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
