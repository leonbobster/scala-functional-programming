object PatternMatching {
    trait Expr

    case class Number(n: Int) extends Expr
    case class Sum(e1: Expr, e2: Expr) extends Expr
    case class Prod(e1: Expr, e2: Expr) extends Expr

    def eval(e: Expr): Int = e match {
        case Number(n) => n
        case Sum(e1, e2) => eval(e1) + eval(e2)
        case Prod(e1, e2) => eval(e1) * eval(e2)
    }

    def show(e: Expr): String = e match {
        case Number(n) => n.toString
        case Sum(e1, e2) => show(e1) + " + " +  show(e2)
        case Prod(e1, e2) => {
          ((e1, e2) match {
            case (Sum(_, _), _) => "(%s) * %s"
            case (_, Sum(_, _)) => "%s * (%s)"
            case (_, _) => "%s * %s"
          }).format(show(e1), show(e2))
        }
    }
  
    val e = Prod( Sum(Number(1), Number(3)), Number(5) )
  
    println(show(e) + " = " + eval(e))
}
