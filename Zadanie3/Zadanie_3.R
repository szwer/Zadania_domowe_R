#######
#1. Utworz funkcje: rankAccount <- function(dataFrame,n, colName,groupName,valueSort,num)
#ktora bedzie zwracala dla danej tabeli(dataFrame) n wierszy posiadajace najwieksze wartosci(sortowanie po kolumnie valueSort)
#dla wybranej grupy(konkretna wartosc komorki , np. "NAUCZYCIEL) z kolumny(colName) np. occupation-zawod.
#######

rankAccount <- function(dataFrame,n, colName,groupName,valueSort,num){
  data <- read.table(dataFrame, nrows = n, sep = ",", header = TRUE)
  data <- data[order(data[[valueSort]], decreasing=TRUE),]
  head(data[data[[colName]] == groupName,], num)
}

 
#######
#2.Tak jak w 1 tylko z uzyciem datachunku.
#######
library("tidyverse")

rankAccountChunk <- function(dataFrame,n, colName,groupName,valueSort,num){
  data <- read.table(dataFrame, nrows = n, sep = ",", header = TRUE)
  data%>%arrange(desc(.data[[valueSort]]))%>%filter(.data[[colName]] == groupName)%>%head(num)
}


#######
#3. SPRAWIDZIC CZY DA SIE ZROBIC TO SAMO W zapytaniu SQL dla takich wartosci jak: tabelaZbazyDanych,occupation, nauczyciel, saldo
######

library(DBI)
library(RSQLite)
library(RPostgres)

readToBase <- function(filepath, dbpath, tablename, size, header = TRUE, sep = ",", deleteTable = TRUE){
  if (deleteTable){
    ap = FALSE
    ov = TRUE
  }else{
    ap = TRUE
    ov = FALSE}
  
  fileConnection <- file(description = filepath, open = "r")
  dbConn <- dbConnect(SQLite(), dbpath)
  data <- read.table(fileConnection, nrows = size, header = header, fill = TRUE, sep = sep)
  print(nrow(data))
  columnsNames <- names(data)
  dbWriteTable(conn = dbConn, name = tablename, data, append = ap, overwrite = ov)

}



rankAccountSql <- function(dataFrame,n, colName,groupName,valueSort,num, dbp, tablename){
  readToBase(dataFrame, dbp, tablename, size = n, sep = ",", deleteTable = TRUE)
  con=dbConnect(SQLite(),dbp)
  dbGetQuery(con, paste0("SELECT * FROM ",tablename, " WHERE ", colName, "='", groupName, "' ORDER BY ",  valueSort, " DESC LIMIT ", num, ";" ) )
  
}


#### TESTOWANIE ####

dataFrame = 'konta.csv'
n = 1000
valueSort = "saldo"
colName = "occupation"
groupName = "NAUCZYCIEL"
num = 10
dbp="baza_konta_zadanie"
tablename="konta_zadanie"

wyn <- rankAccount(dataFrame,n, colName,groupName,valueSort,num)
wyn
wyn2 <- rankAccountChunk(dataFrame,n, colName,groupName,valueSort,num)
wyn2
wyn3<- rankAccountSql(dataFrame,n, colName,groupName,valueSort,num, dbp, tablename)
wyn3
