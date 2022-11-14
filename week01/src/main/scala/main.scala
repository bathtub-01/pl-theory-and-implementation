package MultiLayerIR

// The 'higher level' IR

abstract class hIR_Expr
case class hIR_Number(n: Int) extends hIR_Expr
case class hIR_Sum(e1: hIR_Expr, e2: hIR_Expr) extends hIR_Expr
case class hIR_Mul(e1: hIR_Expr, e2: hIR_Expr) extends hIR_Expr
case class hIR_Val(name: String) extends hIR_Expr
case class hIR_Let(name: String, e1: hIR_Expr, e2: hIR_Expr) extends hIR_Expr

type hIR_Env = List[(String, Int)]

def hIR_Eval(e: hIR_Expr, env: hIR_Env = Nil): Int = {
  def assoc(name: String, env: hIR_Env): Int = env match{
    case Nil => throw new RuntimeException("Val not found.")
    case _ => if env.head._1 == name then env.head._2 else assoc(name, env.tail)
  }
  e match
  case hIR_Number(n) => n
  case hIR_Sum(e1, e2) => hIR_Eval(e1, env) + hIR_Eval(e2, env)
  case hIR_Mul(e1, e2) => hIR_Eval(e1, env) * hIR_Eval(e2, env)
  case hIR_Val(name) => assoc(name, env)
  case hIR_Let(name, e1, e2) => hIR_Eval(e2, (name, hIR_Eval(e1, env)) :: env)
}

// The 'lower level' IR

abstract class lIR_Expr
case class lIR_Number(n: Int) extends lIR_Expr
case class lIR_Sum(e1: lIR_Expr, e2: lIR_Expr) extends lIR_Expr
case class lIR_Mul(e1: lIR_Expr, e2: lIR_Expr) extends lIR_Expr
case class lIR_Val(nth: Int) extends lIR_Expr
case class lIR_Let(e1: lIR_Expr, e2: lIR_Expr) extends lIR_Expr

type lIR_Env = List[Int]

def lIR_Eval(e: lIR_Expr, env: lIR_Env = Nil): Int = e match {
  case lIR_Number(n) => n
  case lIR_Sum(e1, e2) => lIR_Eval(e1, env) + lIR_Eval(e2, env)
  case lIR_Mul(e1, e2) => lIR_Eval(e1, env) * lIR_Eval(e2, env)
  case lIR_Val(nth) => env(nth)
  case lIR_Let(e1, e2) => lIR_Eval(e2, lIR_Eval(e1, env) :: env)
}

// Comipler for hIR -> lIR transformation

type h2l_Env = List[String]

def h2l_compiler(e: hIR_Expr, env: h2l_Env = Nil): lIR_Expr = e match {
  case hIR_Number(n) => lIR_Number(n)
  case hIR_Sum(e1, e2) => lIR_Sum(h2l_compiler(e1, env), h2l_compiler(e2, env))
  case hIR_Mul(e1, e2) => lIR_Mul(h2l_compiler(e1, env), h2l_compiler(e2, env))
  case hIR_Val(name) => lIR_Val(env.indexOf(name))
  case hIR_Let(name, e1, e2) => lIR_Let(h2l_compiler(e1, env), h2l_compiler(e2, name :: env))
}

// The "stack machine level" IR

abstract class smIR_Instr
case class smIR_Const(n: Int) extends smIR_Instr
case class smIR_Sum() extends smIR_Instr
case class smIR_Mul() extends smIR_Instr
case class smIR_Val(nth: Int) extends smIR_Instr
case class smIR_Pop() extends smIR_Instr
case class smIR_Swap() extends smIR_Instr

// Compiler for lIR -> smIR transformation

type stack = List[Int]
type insts = List[smIR_Instr]
type depth = List[Int]

def l2sm_compiler(e: lIR_Expr, d: depth = Nil): insts = {
  def update(d:depth): depth = d.map(i => i + 1)
  e match
  case lIR_Number(n) => List(smIR_Const(n))
  case lIR_Sum(e1, e2) => l2sm_compiler(e1, d) ::: l2sm_compiler(e2, update(d)) ::: List(smIR_Sum())
  case lIR_Mul(e1, e2) => l2sm_compiler(e1, d) ::: l2sm_compiler(e2, update(d)) ::: List(smIR_Mul())
  case lIR_Val(nth) => List(smIR_Val(nth + d(nth)))
  case lIR_Let(e1, e2) => l2sm_compiler(e1, d) ::: l2sm_compiler(e2, 0 :: d) ::: List(smIR_Swap(), smIR_Pop())
}

// The "stack machine" simulator

def sm_simulator(ins: insts, stk: stack = Nil): stack = ins match {
  case Nil => stk
  case _ => ins.head match {
    case smIR_Const(n) => sm_simulator(ins.tail, n :: stk)
    case smIR_Sum() => sm_simulator(ins.tail, (stk(0) + stk (1)) :: stk.drop(2))
    case smIR_Mul() => sm_simulator(ins.tail, (stk(0) * stk (1)) :: stk.drop(2))
    case smIR_Val(nth) => sm_simulator(ins.tail, stk(nth) :: stk)
    case smIR_Pop() => sm_simulator(ins.tail, stk.drop(1))
    case smIR_Swap() => sm_simulator(ins.tail, List(stk(1), stk(0)) ::: stk.drop(2))
  }
}

// object Main extends App {
//   println("Hello, World!")
// }

val state1 = hIR_Let("x", hIR_Number(2), hIR_Let("y", hIR_Number(3), hIR_Sum(hIR_Val("y"), hIR_Mul(hIR_Val("x"), hIR_Val("x")))))
val state2 = hIR_Let("x", hIR_Let("x", hIR_Number(5), hIR_Sum(hIR_Val("x"), hIR_Val("x"))), hIR_Mul(hIR_Val("x"), hIR_Val("x")))
