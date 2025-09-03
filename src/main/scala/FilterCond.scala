import scala.language.implicitConversions

trait FilterCond {
  def eval(r: Row): Option[Boolean]

  // TODO 2.2
  def ===(other: FilterCond): FilterCond = Equal(this, other)
  def &&(other: FilterCond): FilterCond = And(this, other)
  def ||(other: FilterCond): FilterCond = Or(this, other)
  def unary_! : FilterCond = Not(this)
}

case class Field(colName: String, predicate: String => Boolean) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = r.get(colName) match
    case Some(value) => Some(predicate(value))
    case None => Some(false)
}

case class Compound(op: (Boolean, Boolean) => Boolean, conditions: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    val results = conditions.flatMap(_.eval(r)) 
    if (results.isEmpty) Some(false) 
    else Some(results.reduce(op))
  }
}

case class Not(f: FilterCond) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = f.eval(r).map(!_)
}

def And(f1: FilterCond, f2: FilterCond): FilterCond = Compound((a, b) => a && b, List(f1, f2))
def Or(f1: FilterCond, f2: FilterCond): FilterCond = Compound((a, b) => a || b, List(f1, f2))
def Equal(f1: FilterCond, f2: FilterCond): FilterCond = Compound((a, b) => a == b, List(f1, f2))

case class Any(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    val results = fs.flatMap(_.eval(r)) 
    Some(results.contains(true))
  }
}

case class All(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    fs.flatMap(_.eval(r)).reduceOption(_ && _).orElse(Some(false))
  }
}