library(data.table)
library(DBI)
library(RPostgres)

load_data <- function(entity, years) {
  quarters = c('Q1', 'Q2', 'Q3', 'Q4')
  data <- data.table()
  for (year in years) {
    for (quarter in quarters) {
      filename <- paste0('data/ASCII/', entity, year, quarter, '.txt')
      if (file.exists(filename)) {
        data <- rbind(data, fread(filename, sep="$"))
      }
    }
  }
  return(data)
}

entities = c('THER', 'RPSR', 'REAC', 'OUTC', 'INDI', 'DRUG', 'DEMO')
years = c('20', '21', '22', '23')  

con <- dbConnect(RPostgres::Postgres(), 
                 dbname = "shiny",
                 host = "localhost", 
                 port = 5433,
                 user = "shiny",
                 password = "baum1234")

for (i in entities){
  data_table <- load_data(i, years)
  dbWriteTable(con, i, data_table)
}

dbDisconnect(con)



