//package LambdaCalculus

abstract class lambda_Expr {
  def toString(): String
}
case class lambda_Name(n: String) extends lambda_Expr {
  override def toString(): String = n
}
case class lambda_Function(head: String, body: lambda_Expr) extends lambda_Expr {
  override def toString(): String = "(lambda " + head + "." + body.toString() +")"
}
case class lambda_Application(e1: lambda_Expr, e2: lambda_Expr) extends lambda_Expr {
  override def toString(): String = "(" + e1.toString() + e2.toString() + ")"
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

def nameList(e: lambda_Expr): List[String] = e match {
  case lambda_Name(n) => n :: Nil
  case lambda_Function(head, body) => head :: freeList(body).filter(_ != head)
  case lambda_Application(e1, e2) => {
    val l1 = freeList(e1)
    val l2 = freeList(e2)
    l1 ::: l2.filter(!l1.contains(_))
  }
}

def alpha_equiv(f: lambda_Function): lambda_Function = {
  val unique: String = {
    val nl = nameList(f)
    def squote(s: String) = s + "'"
    def find_unique(n: String): String = if nl.contains(n) then find_unique(squote(n)) else n
    find_unique(squote(f.head))
  }
  def rec(e: lambda_Expr): lambda_Expr = {
    if isFree(f.head, e) then e match {
      case lambda_Name(n) => if n == f.head then lambda_Name(unique) else e
      case lambda_Function(head, body) => lambda_Function(head, rec(body))
      case lambda_Application(e1, e2) => lambda_Application(rec(e1), rec(e2))
    }
    else e
  }
  lambda_Function(unique, rec(f.body))
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

def closed_eval(e: lambda_Expr): lambda_Expr = e match {
  case lambda_Name(_) => throw new RuntimeException("input term isn't closed.")
  case lambda_Function(_, _) => e
  case lambda_Application(e1, e2) => {
    val func = closed_eval(e1) match // any better way to avoid using asInstanceOf[lambda_Function]?
      case lambda_Function(head, body) => lambda_Function(head, body)
    val arg = closed_eval(e2)
    closed_eval(substitution(func.head, arg, func.body))
  }
}

def beta_reduc(e: lambda_Expr): lambda_Expr = e match {
  case lambda_Name(_) => e
  case lambda_Function(_, _) => e
  case lambda_Application(e1, e2) => {
    val func = rename(e1, e2) match 
      case lambda_Function(head, body) => lambda_Function(head, body)
    substitution(func.head, e2, func.body)
  }
}

def isStucked(e: lambda_Expr): Boolean = e match {
  case lambda_Name(_) => true
  case lambda_Function(_, body) => isStucked(body)
  case lambda_Application(e1, e2) => (e1 match {
    case lambda_Name(_) => true
    case lambda_Function(_, _) => false
    case lambda_Application(_, _) => isStucked(e1) //?
  }) && isStucked(e2)
}

// call-by-value
def cbv_driver(e: lambda_Expr): lambda_Expr = e match {
  case lambda_Name(_) => e
  case lambda_Function(head, body) => if isStucked(body) then e else lambda_Function(head, cbv_driver(body))
  case lambda_Application(e1, e2) => {
    val sub1 = if isStucked(e1) then e1 else cbv_driver(e1)
    val sub2 = if isStucked(e2) then e2 else cbv_driver(e2)
    val tmp = lambda_Application(sub1, sub2)
    if isStucked(tmp) then tmp
      else cbv_driver(beta_reduc(tmp))}
}

def expansion(e: lambda_Expr): lambda_Expr = e match {
  case lambda_Name(_) => e
  case lambda_Function(head, body) => lambda_Function(head, expansion(body))
  case lambda_Application(lambda_Function(_, _), e2) => beta_reduc(e)
  case lambda_Application(lambda_Application(sub1, sub2), e2) => 
    lambda_Application(expansion(lambda_Application(sub1, sub2)), e2)
  case lambda_Application(lambda_Name(n), e2) => 
    lambda_Application(lambda_Name(n), expansion(e2))
}

// call-by-name
def cbn_driver(e: lambda_Expr): lambda_Expr = {
  if isStucked(e) then e else e match {
    case lambda_Function(head, body) => lambda_Function(head, cbn_driver(body))
    case lambda_Application(e1, e2) => cbn_driver(expansion(e))
  }
}

// expression example
val identity = lambda_Function("x", lambda_Name("x"))
val expr1 = lambda_Application(lambda_Function("y", lambda_Application(lambda_Function("y", lambda_Name("y")), lambda_Function("x", lambda_Application(lambda_Name("x"), lambda_Name("y"))))), identity)
val expr2 = lambda_Application(lambda_Function("y", lambda_Name("y")), lambda_Function("x", lambda_Application(lambda_Name("x"), lambda_Name("y"))))
val f1 = lambda_Function("y", lambda_Application(lambda_Name("y"), lambda_Function("y", lambda_Name("y"))))
val f2 = lambda_Function("y", lambda_Application(lambda_Name("y"), lambda_Function("x", lambda_Application(lambda_Name("x"), lambda_Name("y")))))