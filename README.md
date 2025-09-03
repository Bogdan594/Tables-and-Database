# Tables and Database

## Part one: Tables

It contains all the functions need for a table such as: sort, select, delete(row), insert(row). The table is a class, with a field for name, data and header. The data is stored as list of rows, where a row si a map[String,String].

## Part two : Filters

An ADT for filtering tables. 

## Part three: Database

A class that has a list of tables. The functions implemented are: insert, update, delete and selectTables.

## Part four: Querries

It contains three querries that are requested. They all return Option[Table], for the cases where there are no tables in the database that match the given conditions.

