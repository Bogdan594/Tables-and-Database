object Queries {
  def query_1(db: Database, ageLimit: Int, cities: List[String]): Option[Table] = {
    db.tables.find(_.name == "Customers").map(_.filter(Field("age", _.toIntOption.exists(_ > ageLimit)) && Field("city", cities.contains)).sort("id"))
  }

  def query_2(db: Database, date: String, employeeID: Int): Option[Table] = {
    db.tables.find(_.name == "Orders").map(_.filter(Field("date", _ > date) && Field("employee_id", _ != employeeID.toString)).select(List("order_id", "cost")).sort("cost", ascending = false))
  }

  def query_3(db: Database, minCost: Int): Option[Table] = {
    db.tables.find(_.name == "Orders").map(_.filter(Field("cost", v => v.trim.toIntOption.exists(_ > minCost))).select(List("order_id", "employee_id", "cost")).sort("employee_id"))
  }
}