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
        print(paste("Loading file:", filename)) # This will print the name of the file being loaded
        
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


# THIS RUNS EVERY TIME app.R IS RUNNING, EXTRACT THIS OR WRITE SOMETHING TO EXECUTE IT ONCE
########################################
entities = c('THER', 'RPSR', 'REAC', 'OUTC', 'INDI', 'DRUG', 'DEMO')
years = c('22','23')

for (entity in entities) {
  assign(entity, load_data(entity, years))
}
##############

setnames(THER, "dsg_drug_seq", "drug_seq")
setnames(INDI, "indi_drug_seq", "drug_seq")


# Konvertierung von Therapie duration zu Monaten
conversion_factors <- list(
  SEC = 60 * 60 * 24 * 30,   # Seconds to month: 60 sec/min * 60 min/hr * 24 hr/day * 30 day/month
  MIN = 60 * 24 * 30,        # Minutes to month: 60 min/hr * 24 hr/day * 30 day/month
  HR = 24 * 30,              # Hours to month: 24 hr/day * 30 day/month
  DAY = 30,                  # Days to month: 30 day/month
  WK = 4.348,                # Weeks to month: average 4.348 weeks/month
  MON = 1,                   # Months to month: already in correct unit
  YR = 1/12                  # Years to month: 1 year/12 months
)

# Convert `dur` to numeric month values
#THER$dur <- as.numeric(THER$dur)

THER[, dur_converted := NA_real_]
non_empty_dur_cod <- THER$dur_cod != ""
factors <- conversion_factors[THER$dur_cod[non_empty_dur_cod]]
THER$dur_converted[non_empty_dur_cod] <- round(THER$dur[non_empty_dur_cod] / unlist(factors),2)
non_empty_dur_cod <- NULL

#Konvertierung alter
conversion_factors <- list(
  DEC = 10,
  YR = 1,
  MON = 1/12,
  WK = 1/52.143,
  DY = 1/365.25,
  HR = 1/8766
)

DEMO$age <- as.numeric(DEMO$age)
non_empty_age_cod <- DEMO$age_cod != ""
factors <- conversion_factors[DEMO$age_cod[non_empty_age_cod]]

# Create a logical index for non-empty age_cod values
non_empty_age <- DEMO$age_cod[non_empty_age_cod]

# Create a vector of conversion factors aligned with the non-empty age_cod values
age_factors <- unlist(factors)[non_empty_age]

# Calculate age in years for non-empty age_cod values
DEMO$age[non_empty_age_cod] <- round(DEMO$age[non_empty_age_cod] * rep(age_factors, length.out = sum(non_empty_age_cod)),2)

non_empty_age <- NULL
non_empty_age_cod <- NULL
age_factors <- NULL
DEMO$age_cod <- NULL
########################################

join_data <- function(v_drugname, v_sex, v_age_min, v_age_max) {
  
  # Create a data table with the selected_drug details
  selected_drug <- unique(DRUG[drugname == v_drugname, .(primaryid, caseid, drug_seq, drugname, route, YearQuarter)])
  
  # Create a data table with the selected_patients details
  selected_patients <- unique(DEMO[primaryid %in% selected_drug$primaryid & sex == v_sex & age >= v_age_min & age <= v_age_max,
                                   .(primaryid, age, sex, wt, reporter_country)])
  
  # Create a data table with the selected_therapies details
  selected_therapies <- unique(THER[primaryid %in% selected_drug$primaryid, .(primaryid, drug_seq, dur_converted)])
  
  # Create a data table with the selected_indications details
  selected_indications <- unique(INDI[primaryid %in% selected_drug$primaryid, .(primaryid, drug_seq, indi_pt)])
  
  # Create a data table with the selected_outcomes details
  selected_outcomes <- OUTC[primaryid %in% selected_drug$primaryid, .(primaryid, outc_cod)]
  selected_outcomes <- selected_outcomes[selected_outcomes[, .I[which.max(.I)], by = .(primaryid)]$V1]
  
  # Merge all the data tables
 # final_data <- merge(selected_drug, selected_outcomes, by = "primaryid", all.x = TRUE)
  #final_data <- merge(final_data, selected_patients, by = "primaryid", all.x = TRUE)
 # final_data <- merge(final_data, selected_therapies, by = c("primaryid", "drug_seq"), all.x = TRUE)
 # final_data <- merge(final_data, selected_indications, by = c("primaryid", "drug_seq"), all.x = TRUE)
  final_data <- data.table(selected_drug)[selected_outcomes, on = "primaryid", nomatch = 0, allow.cartesian = TRUE]
  final_data <- final_data[selected_patients, on = "primaryid", nomatch = 0, allow.cartesian = TRUE]
  final_data <- final_data[selected_therapies, on = c("primaryid", "drug_seq"), nomatch = 0, allow.cartesian = TRUE]
  final_data <- final_data[selected_indications, on = c("primaryid", "drug_seq"), nomatch = 0, allow.cartesian = TRUE]
  
  # Ensure only unique rows in the final data
  final_data <- unique(final_data)
  
  return(final_data)
}


# Run the function
 final_data <- join_data("IBUPROFEN", "M" ,0 ,120)
 View(final_data)

#Funktionen

#Liste der Medikamente

#unique_drugs <- unique(DRUG$drugname)
# unique_drugs

#Liste aller beobachteten Sequenzen an Medikamenten


#Funktionen für plots
num_reports_per_quarter <- function(data){
  reports <- data[, .N, by = YearQuarter]
  return(reports)
}

num_reports_per_sequence <- function(data){
  reports <- data[drug_seq < 50, .N, by = drug_seq]
  #reports <- data[, .N, by = drug_seq]
  return(reports)
}

calc_therapy_duration2 <- function(data){

  ther_data <- data[!is.na(start_dt) & !is.na(end_dt), .(start_dt, end_dt)]
  ther_data[, `:=`(start_dt = as.character(start_dt), end_dt = as.character(end_dt))]
  ther_data[start_dt %like% "^\\d{6}$", start_dt := paste0(start_dt, "01")]
  ther_data[end_dt %like% "^\\d{6}$", end_dt := paste0(end_dt, "01")]
  ther_data[, `:=`(start_dt = as.Date(start_dt, format = "%Y%m%d"),
                   end_dt = as.Date(end_dt, format = "%Y%m%d"))]
  durations <- as.numeric(ther_data$end_dt - ther_data$start_dt)
  durations[durations < 0] <- 
  return(durations)
}

calc_therapy_duration <- function(data){
  ther_data <- data[!is.na(dur_converted)]
  return(ther_data[dur_converted])
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



# #erstellung plots
# library(ggplot2)
# 
# # Plotting number of reports per quarter:
# reports_per_quarter <- num_reports_per_quarter(final_data)
# ggplot(reports_per_quarter, aes(x = YearQuarter, y = N)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Number of Reports per Quarter", x = "Quarter", y = "Number of Reports")
# 
# #Plotting number of reports per sequence:
# reports_per_sequence <- num_reports_per_sequence(final_data)
# ggplot(reports_per_sequence, aes(x = factor(drug_seq), y = N)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Number of Reports per Sequence", x = "Sequence", y = "Number of Reports")
# 
#Plotting histogram of therapy duration:
 therapy_durations <- calc_therapy_duration(final_data)
 hist(therapy_durations)
 ggplot(hist_data, aes(x = mids, y = counts)) +
   geom_bar(stat = "identity") +
   labs(title = "Distribution of Therapy Duration", x = "Duration", y = "Frequency")

# #Plotting top 10 indications:
# top_indications_data <- top_indications(final_data)
# ggplot(top_indications_data, aes(x = reorder(drug_seq, -N), y = N)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Top 10 Indications", x = "Indication", y = "Number of Reports")
# 
# #Plotting outcome distribution:
# outcome_distribution_data <- outcome_distribution(final_data)
# ggplot(outcome_distribution_data, aes(x = outc_cod, y = N)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Outcome Distribution", x = "Outcome Code", y = "Number of Outcomes")

# View(hist_data)




