
type Row = Map[String, String]
type Tabular = List[Row]

case class Table (tableName: String, tableData: Tabular) {
  
  // TODO 1.0
  def header: List[String] = tableData.head.keys.toList
  def data: Tabular = tableData
  def name: String = tableName


  // TODO 1.1
  override def toString: String = {
    val h = header.foldLeft("") {
      case ("", col) => col
      case (acc, col) => acc + "," + col
    }

    val lines = data.foldLeft("") {
      case ("", row) =>
        row.values.foldLeft("") {
          case ("", value) => value
          case (acc, value) => acc + "," + value
        }
      case (accLines, row) =>
        val line = row.values.foldLeft("") {
          case ("", value) => value
          case (acc, value) => acc + "," + value
        }
        accLines + "\n" + line
    }
     h + "\n" + lines
  }

  // TODO 1.3
  def insert(row: Row): Table = {
    if(!data.contains(row)){
      val newData = data :+ row
      Table(name, newData)
    } else {
      Table(name, data)
    }
  }

  // TODO 1.4
  def delete(row: Row): Table = {
    def compareRow(row1: Row, row2: Row): Boolean = {
      if (row1.size != row2.size) false
      else {
        (row1.toList, row2.toList) match {
          case (Nil, Nil) => true
          case ((k1, v1) :: r1, (k2, v2) :: r2) if (k1 == k2 && v1 == v2)
          => compareRow(r1.toMap, r2.toMap)
          case ((_, _) :: _, Nil) => false
          case (Nil, (_ , _) :: _) => false
          case ((k1, v1) :: _, (k2, v2) :: _) if k1 != k2 || v1 != v2 => false
          case _ => false
        }
      }
    }

    val newRows = data.foldLeft(List[Row]()) {
      case (acc, r) =>
        if (!compareRow(r, row)) r :: acc
        else acc
    }.reverse
    
    Table(name, newRows)
  }

  // TODO 1.5
  def sort(column: String, ascending: Boolean = true): Table = {
    val sortedData = data.sortWith { (row1, row2) =>
      val value1 = row1(column)
      val value2 = row2(column)

      if (ascending) {
        value1 < value2
      } else {
        value1 > value2
      }
    }

    Table(name, sortedData)
  }

  // TODO 1.6
  def select(columns: List[String]): Table = {
    val selected = data.foldLeft(List[Row]()){
      case (acc, row) =>
        val selectFromRow = row.foldLeft(Map[String, String]()){
          case (newRow, (k,v))  =>
            if (columns.contains(k)) newRow + (k -> v)
            else newRow
        }
        acc :+ selectFromRow
    }

    Table(name, selected)
  }

  // TODO 1.7
  // Construiti headerele noi concatenand numele tabelei la headere
  def cartesianProduct(otherTable: Table): Table = {
    val otherTableHeader = otherTable.header
    val otherTableNewHeader = otherTableHeader.map(n => otherTable.name + "." + n)
    val thisTableNewHeader = header.map(n => name + "." + n)
    val thisTableNewData = data.foldLeft(List[Row]()){
      case (acc, row) =>
        val newRow = row.map{
          case (key, value) => (name + "." + key) -> value
        }
        newRow :: acc
    }.reverse

    val otherTableNewData = otherTable.data.foldLeft(List[Row]()){
      case (acc, row) =>
        val newRow = row.map {
          case (key, value) => (otherTable.name + "." + key) -> value
        }
        newRow :: acc
    }.reverse


    val finalHeader = otherTableNewHeader ++ thisTableNewHeader

    def conc(r: Row, l:List[Row]): List[Row] = {
      l.map(row => row ++ r)
    }

    val finalData = thisTableNewData.flatMap(row => conc(row, otherTableNewData))
    
    Table(name, finalData)
  }

  // TODO 1.8
  def join(other: Table)(col1: String, col2: String): Table = {
    def getValue(s: String, r: Row): String = {
      r.foldLeft("") {
        case (acc, (k, v)) =>
          if (k == s) v
          else acc
      }
    }

    val both = data.foldLeft(List[(Row, Row)]()) { (acc, row1) =>
      val matchingRows = other.data.foldLeft(List[(Row, Row)]()) { (acc2, row2) =>
        if (getValue(col1, row1) == getValue(col2, row2))
          (row1, row2) :: acc2
        else
          acc2
      }
      matchingRows ++ acc
    }

    val onlyFirst = data.foldLeft(List[Row]()) { (acc, row1) =>
      val foundMatch = other.data.foldLeft(false) { (found, row2) =>
        if (getValue(col1, row1) == getValue(col2, row2)) true
        else found
      }
      if (!foundMatch) row1 :: acc
      else acc
    }.reverse

    val onlySecond = other.data.foldLeft(List[Row]()) { (acc, row2) =>
      val foundMatch = data.foldLeft(false) { (found, row1) =>
        if (getValue(col1, row1) == getValue(col2, row2)) true
        else found
      }

      if (!foundMatch) row2 :: acc
      else acc
    }.reverse

    val header1 = header
    val header2 = other.header

    def combineRows(row1: Row, row2: Row): Row = {
      val allKeys = (row1.keys ++ row2.keys).toSet

      allKeys.map { key =>
        val value1 = row1.getOrElse(key, "")
        val value2 = row2.getOrElse(key, "")

        if (value1 == value2) {
          key -> value1
        } else {
          key -> (if (value1.isEmpty) value2 else if (value2.isEmpty) value1 else value1 + ";" + value2)

        }
      }.toMap
    }

    val combinedRows = both.map {
      case (row1, row2) => combineRows(row1, row2)
    }

    def findDif(header1: List[String], header2: List[String]): (List[String], List[String]) = {
      val diff1 = header1.foldLeft(List[String]()) { (acc, h1) =>
        if (!header2.contains(h1)) h1 :: acc
        else acc
      }

      val diff2 = header2.foldLeft(List[String]()) { (acc, h2) =>
        if (!header1.contains(h2)) h2 :: acc
        else acc
      }

      (diff1.reverse, diff2.reverse)
    }

    val (dif1, dif2) = findDif(header1, header2)
    // dif1 e ceea ce header2 nu are si dif2 e ceea ce header1 nu are
    val extras2 = dif1.map(s => (s, "")).toMap
    val extras1 = dif2.map(s => (s, "")).toMap

    val firstListFull = onlyFirst.map(row => row ++ extras1)
    val secondListFull = onlySecond.map(row => row ++ extras2)

    val combRowsFinal = combinedRows.map(row => row - col2).reverse
    val secondListFullFinal = secondListFull.map{ row =>
      val col2value = row.getOrElse(col2, "")
      val updatedRow = row -col1 -col2
      val finalRow = updatedRow + (col1 -> col2value)
      finalRow
    }
    val firstListFullFinal = firstListFull.map { row => row - col2}

    val fullData = combRowsFinal ++ firstListFullFinal ++ secondListFullFinal
    
    Table(name, fullData)
  }

  // TODO 2.3
  def filter(f: FilterCond): Table = {
    val newData = data.filter(row => f.eval(row).getOrElse(false))
    
    Table(name, newData)
  }
  
  // TODO 2.4
  def update(f: FilterCond, updates: Map[String, String]): Table = {
    val updatedRows = data.map { row =>
      if (f.eval(row).getOrElse(false)) {
        val updatedRow = row ++ updates
        updatedRow
      } else {
        row
      }
    }

    Table(name, updatedRows)
  }



  // TODO 3.5
  // Implement indexing
  def apply(index: Int): Row = data(index)

}

object Table {
  // TODO 1.2
  def fromCSV(csv: String): Table = {
    val lines = csv.split("\n").toList
    val header = lines.head.split(",").toList
    val data = lines.tail.map { line =>
      val values = line.split(",").toList
      header.zip(values).toMap
    }
    
    Table("idk", data)
  }

  // TODO 1.9
  def apply(name: String, s: String): Table = {
    val lines = s.split("\n").toList
    val header = lines.head.split(",").toList
    val data = lines.tail.map { line =>
      val values = line.split(",").toList
      header.zip(values).toMap
    }
    
    Table(name, data)
  }
}