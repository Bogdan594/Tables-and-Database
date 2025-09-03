case class Database(tables: List[Table]) {
  // TODO 3.0
  override def toString: String = tables.map(_.toString).mkString("\n")

  // TODO 3.1
  def insert(tableName: String): Database = {
    val found = tables.foldLeft(false) {(acc, table) =>
      acc || (table.name == tableName)
    }
    if(found) this
    else Database(tables :+ Table(tableName , Nil))
   }

  // TODO 3.2
  def update(tableName: String, newTable: Table): Database = {
    val (found, updatedTables) = tables.foldLeft((false, List[Table]())) {
      case ((foundAcc, acc), table) =>
        if (table.name == tableName)
          (true, acc :+ newTable)
        else
          (foundAcc, acc :+ table)
    }

    if (found) Database(updatedTables)
    else this
  }
  // TODO 3.3
  def delete(tableName: String): Database = {
    val(found, updatedTables) = tables.foldLeft((false, List[Table]())) {
      case ((foundAcc, acc), table) =>
        if(table.name == tableName)
          (true, acc)
        else (foundAcc, acc :+ table)
    }
    if (found) Database(updatedTables)
    else this
  }

  // TODO 3.4
  def selectTables(tableNames: List[String]): Option[Database] = {
    val selected = tables.foldLeft(List[Table]()) {
      case (acc, table) =>
        if (tableNames.contains(table.name)) acc :+ table
        else acc
    }
    if (selected.length == tableNames.length) Some(Database(selected))
    else None
  }

  // TODO 3.5
  // Implement indexing here
  def apply(index: Int): Table = tables(index)

}
