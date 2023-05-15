library(data.table)
library(DBI)
library(RPostgres)

load_data <- function(entity, years) {
  quarters = c('Q1','Q2','Q3','Q4')
  data <- data.table()
  for (year in years) {
    for (quarter in quarters) {
      filename <- paste0('../../data/ASCII/', entity, year, quarter, '.txt')
      if (file.exists(filename)) {
        new_data <- fread(filename, sep="$")
        if (entity == 'DRUG') {
          new_data[, YearQuarter := paste(year, quarter, sep = "-")] 
        }
        data <- rbind(data, new_data)
      }
    }
  }
  return(data)
}

entities = c('THER', 'RPSR', 'REAC', 'OUTC', 'INDI', 'DRUG', 'DEMO')
years = c('22','23')

THER <- load_data("THER", years)
OUTC <- load_data("OUTC", years)
INDI <- load_data("INDI", years)
DRUG <- load_data("DRUG", years)
DEMO <- load_data("DEMO", years)

REAC <- load_data("REAC", years)
RPSR <- load_data("RPSR", years)

data_tables <- list(THER, RPSR, OUTC, INDI, DRUG, DEMO)

setnames(THER, "dsg_drug_seq", "drug_seq") 
setnames(INDI, "indi_drug_seq", "drug_seq")


########################################

join_data <- function(v_drugname){
  
  # Create a data table with the selected_drug details
  selected_drug <- unique(DRUG[drugname == v_drugname, .(primaryid, caseid, drug_seq, drugname, route, YearQuarter)])
  
  # Create a data table with the selected_patients details
  selected_patients <- unique(DEMO[primaryid %in% selected_drug$primaryid, .(primaryid, caseid,age,sex,wt, reporter_country)])
  
  # Create a data table with the selected_therapies details
  selected_therapies <- unique(THER[primaryid %in% selected_drug$primaryid, .(primaryid, caseid, drug_seq, start_dt, end_dt, dur)])
  
  # Create a data table with the selected_indications details
  selected_indications <- unique(INDI[primaryid %in% selected_drug$primaryid])
  
  # Create a data table with the selected_outcomes details
  selected_outcomes <- OUTC[primaryid %in% selected_drug$primaryid, .(primaryid, caseid, outc_cod)]
  selected_outcomes <- selected_outcomes[selected_outcomes[, .I[which.max(.I)], by = .(primaryid, caseid)]$V1]
  
  
  # Merge all the data tables
  final_data <- merge(selected_drug, selected_outcomes, by = c("primaryid","caseid"), all.x = TRUE)
  final_data <- merge(final_data, selected_patients, by = c("primaryid","caseid"), all.x = TRUE)
  final_data <- merge(final_data, selected_therapies, by = c("primaryid", "drug_seq","caseid"), all.x = TRUE)
  final_data <- merge(final_data, selected_indications, by = c("primaryid", "drug_seq","caseid"), all.x = TRUE)
  
  # Ensure only unique rows in the final data
  final_data <- unique(final_data)
  
  return(final_data)
}

# Run the function
final_data <- join_data("LITHIUM")
View(final_data)

#Funktionen

#Liste der Medikamente

unique_drugs <- unique(DRUG$drugname)
unique_drugs

#Liste aller beobachteten Sequenzen an Medikamenten


#Statistiken
num_reports_per_quarter <- function(data){
  reports <- data[, .N, by = YearQuarter]
  return(reports)
}

num_reports_per_sequence <- function(data){
  reports <- data[, .N, by = drug_seq]
  return(reports)
}

hist_therapy_duration <- function(data){
  hist_data <- hist(data$dur, plot = FALSE) 
  return(hist_data) 
}

top_indications <- function(data){
  indications <- data[, .N, by = drug_seq] 
  top_indications <- indications[order(-N)][1:10]
  return(top_indications)
}

outcome_distribution <- function(data){
  data_complete_therapy <- data[!is.na(end_dt)] # Assuming 'end_dt' indicates the end of therapy
  outcome_dist <- data_complete_therapy[, .N, by = outc_cod]
  return(outcome_dist)
}



#Aufruf
library(ggplot2)

# Plotting number of reports per quarter:
reports_per_quarter <- num_reports_per_quarter(final_data)
ggplot(reports_per_quarter, aes(x = YearQuarter, y = N)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Reports per Quarter", x = "Quarter", y = "Number of Reports")

#Plotting number of reports per sequence:
reports_per_sequence <- num_reports_per_sequence(final_data)
ggplot(reports_per_sequence, aes(x = factor(drug_seq), y = N)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Reports per Sequence", x = "Sequence", y = "Number of Reports")

#Plotting histogram of therapy duration:
hist_data <- hist_therapy_duration(final_data)
ggplot(hist_data, aes(x = mids, y = counts)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Therapy Duration", x = "Duration", y = "Frequency")

#Plotting top 10 indications:
top_indications_data <- top_indications(final_data)
ggplot(top_indications_data, aes(x = reorder(drug_seq, -N), y = N)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Indications", x = "Indication", y = "Number of Reports") 

#Plotting outcome distribution:
outcome_distribution_data <- outcome_distribution(final_data)
ggplot(outcome_distribution_data, aes(x = outc_cod, y = N)) +
  geom_bar(stat = "identity") +
  labs(title = "Outcome Distribution", x = "Outcome Code", y = "Number of Outcomes")






