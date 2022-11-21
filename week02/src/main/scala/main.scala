//package LambdaCalculus

abstract class lambda_Expr {
  def toString(): String
}
case class lambda_Name(n: String) extends lambda_Expr {
  override def toString(): String = n
}
case class lambda_Function(head: String, body: lambda_Expr) extends lambda_Expr {
  override def toString(): String = "lambda " + head + "." + body.toString()
}
case class lambda_Application(e1: lambda_Expr, e2: lambda_Expr) extends lambda_Expr {
  override def toString(): String = "(" + e1.toString() + ")" + "(" + e2.toString() + ")"
}

def isFree(v: String, e: lambda_Expr): Boolean = e match {
  case lambda_Name(_) => true
  case lambda_Function(head, body) => (v != head) && isFree(v, body)
  case lambda_Application(e1, e2) => isFree(v, e1) || isFree(v, e2)
}

def isBound(v: String, e: lambda_Expr): Boolean = e match {
  case lambda_Name(_) => false
  case lambda_Function(head, body) => (v == head) || isBound(v, body)
  case lambda_Application(e1, e2) => isBound(v, e1) || isBound(v, e2)
}

def freeList(e: lambda_Expr): List[String] = e match {
  case lambda_Name(n) => n :: Nil
  case lambda_Function(head, body) => freeList(body).filter(_ != head)
  case lambda_Application(e1, e2) => {
    val l1 = freeList(e1)
    val l2 = freeList(e2)
    l1 ::: l2.filter(!l1.contains(_))
  }
}

def alpha_equiv(f: lambda_Function): lambda_Function = {
  def squote(s: String) = s + "'"
  def rec(e: lambda_Expr, name: String): lambda_Expr = {
    if isFree(name, e) then e match {
      case lambda_Name(n) => if n == name then lambda_Name(squote(n)) else e
      case lambda_Function(head, body) => lambda_Function(head, rec(body, name))
      case lambda_Application(e1, e2) => lambda_Application(rec(e1, name), rec(e2, name))
    }
    else e
  }
  lambda_Function(squote(f.head), rec(f.body, f.head))
}

def rename(e1: lambda_Expr, e2: lambda_Expr): lambda_Expr = {
  def rec(e: lambda_Expr, name: String): lambda_Expr = e match {
    case lambda_Name(_) => e
    case lambda_Function(head, body) => 
      if head == name then rec(alpha_equiv(lambda_Function(head, body)), name) else lambda_Function(head, rec(body, name))
    case lambda_Application(e1, e2) => lambda_Application(rec(e1, name), rec(e2, name))
  }
  def iter(tmp: lambda_Expr, fl: List[String]): lambda_Expr = {
    if fl.isEmpty then tmp
    else iter(rec(tmp, fl.head), fl.tail)
  }
  iter(e1, freeList(e2))
}

def substitution(from: String, to: lambda_Expr, in: lambda_Expr): lambda_Expr = {
  if isFree(from, in) then {
    in match {
      case lambda_Name(n) => if n == from then to else in
      case lambda_Function(head, body) => lambda_Function(head, substitution(from, to, body))
      case lambda_Application(e1, e2) => lambda_Application(substitution(from, to, e1), substitution(from, to, e2))
    }
  }
  else in
}

def eval(e: lambda_Expr): lambda_Expr = e match {
  case lambda_Name(_) => throw new RuntimeException("input term isn't closed.")
  case lambda_Function(_, _) => e
  case lambda_Application(e1, e2) => {
    val func = eval(e1) match // any better way to avoid using asInstanceOf[lambda_Function]?
      case lambda_Function(head, body) => lambda_Function(head, body)
    val arg = eval(e2)
    eval(substitution(func.head, arg, func.body))
  }
}

val identity = lambda_Function("x", lambda_Name("x"))
val expr1 = lambda_Application(lambda_Function("y", lambda_Application(lambda_Function("y", lambda_Name("y")), lambda_Function("x", lambda_Application(lambda_Name("x"), lambda_Name("y"))))), identity)
val expr2 = lambda_Application(lambda_Function("y", lambda_Name("y")), lambda_Function("x", lambda_Application(lambda_Name("x"), lambda_Name("y"))))
val f1 = lambda_Function("y", lambda_Application(lambda_Name("y"), lambda_Function("y", lambda_Name("y"))))
val f2 = lambda_Function("y", lambda_Application(lambda_Name("y"), lambda_Function("x", lambda_Application(lambda_Name("x"), lambda_Name("y")))))
val a1 = lambda_Application(lambda_Name("x"), lambda_Name("y"))