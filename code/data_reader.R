library(data.table)
library(magrittr)

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
          new_data[, year := as.numeric(paste0(20,year))] 
          new_data[, quarter := quarter] 
        }
        data <- rbind(data, new_data)
      }
    }
  }
  return(data)
}

# THIS RUNS EVERY TIME app.R IS RUNNING, EXTRACT THIS OR WRITE SOMETHING TO EXECUTE IT ONCE
########################################
entities = c('THER', 'REAC', 'OUTC', 'INDI', 'DRUG', 'DEMO')
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

conversion_factors <- list(
  SEC = 60 * 60 * 24,  # Seconds to day: 1 day/ (60 sec/min * 60 min/hr * 24 hr/day)
  MIN = 60 * 24,       # Minutes to day: 1 day/ (60 min/hr * 24 hr/day)
  HR = 24,               # Hours to day: 1 day/24 hr
  DAY = 1,                   # Days to day: already in correct unit
  WK = 1 / 7,                # Weeks to day: 1 day/7 days
  MON = 1 / 30.4375,         # Months to day: 1 day/30.4375 days (average month length)
  YR = 1 / 365.25            # Years to day: 1 day/365.25 days (average year length)
)

# Convert `dur` to numeric month values
#THER$dur <- as.numeric(THER$dur)

THER[, dur_converted := NA_real_]
non_empty_dur_cod <- THER$dur_cod != ""
factors <- conversion_factors[THER$dur_cod[non_empty_dur_cod]]
THER$dur_converted[non_empty_dur_cod] <- round(THER$dur[non_empty_dur_cod] / unlist(factors),2)
non_empty_dur_cod <- NULL
factors <- NULL


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

outcome_lookup <- data.frame(
  CODE = c("DE", "LT", "HO", "DS", "CA", "RI", "OT"),
  MEANING_TEXT = c("Death", 
                   "Life-Threatening", 
                   "Hospitalization - Initial or Prolonged", 
                   "Disability", 
                   "Congenital Anomaly", 
                   "Required Intervention to Prevent Permanent Impairment/Damage", 
                   "Other Serious (Important Medical Event)")
)
OUTC[, outcome_decoded := outcome_lookup$MEANING_TEXT[match(outc_cod, outcome_lookup$CODE)]]

# Reaction (REAC) reduced to only those which as well occured
REAC <-REAC[drug_rec_act!=""]

########################################

join_data <- function(v_drugname = NULL, v_sex = NULL, v_age_min = NULL, v_age_max = NULL, v_year = NULL) {
  
  # If variables are NULL, select all
  v_drugname <- ifelse(is.null(v_drugname), unique(DRUG$drugname), v_drugname)
  
  if (is.null(v_sex) | v_sex == "All") {
    v_sex <- unique(DEMO$sex)
  }
  if (is.null(v_year) | v_year == "All") {
    v_year <- unique(DRUG$year)
  }
  v_age_min <- ifelse(is.null(v_age_min), min(DEMO$age, na.rm = TRUE), v_age_min)
  v_age_max <- ifelse(is.null(v_age_max), max(DEMO$age, na.rm = TRUE), v_age_max)
  
  
  selected_drug <- unique(DRUG[drugname == v_drugname & year %in%  as.numeric(v_year), 
                               .(primaryid, caseid, drug_seq, drugname, route, year, quarter)])
  
  selected_patients <- unique(DEMO[primaryid %in% selected_drug$primaryid & sex %in% v_sex & age >= v_age_min & age <= v_age_max,
                                   .(primaryid, age, sex, wt, reporter_country)])
  
  selected_therapies <- unique(THER[primaryid %in% selected_drug$primaryid, 
                                    .(primaryid, caseid, drug_seq, dur_converted, end_dt)])
  
  selected_indications <- unique(INDI[primaryid %in% selected_drug$primaryid, 
                                      .(primaryid, caseid, drug_seq, indi_pt)])
  
  
  selected_indications <- unique(INDI[primaryid %in% selected_drug$primaryid, 
                                      .(primaryid, caseid, drug_seq, indi_pt)])
  
  selected_outcomes <- OUTC[primaryid %in% selected_drug$primaryid, 
                            .(primaryid, outc_cod, outcome_decoded)]
  selected_outcomes <- selected_outcomes[selected_outcomes[, .I[which.max(.I)], by = .(primaryid)]$V1]
  
# Merge all the data tables
  final_data <- merge(selected_drug, selected_patients, by = "primaryid", all.x = TRUE)
  final_data <- merge(final_data, selected_outcomes, by = "primaryid", all.x = TRUE)
  final_data <- merge(final_data, selected_therapies, by = c("primaryid", "drug_seq", "caseid"), all.x = TRUE)
  final_data <- merge(final_data, selected_indications, by = c("primaryid", "drug_seq", "caseid"), all.x = TRUE)
  final_data <- merge(final_data, REAC, by = c("primaryid", "caseid"), all.x = TRUE)

  # Ensure only unique rows in the final data
  final_data <- unique(final_data)
  
  return(final_data)
}


################################ Testing
# Run the function
final_data <- join_data("IBUPROFEN", "All" ,0 ,120, "All")
 View(final_data)
 
 unique(DEMO$occr_country)
 
##################################################
# Aufbereitungen für PLots und Listen

#Liste der Medikamente

#unique_drugs <- unique(DRUG$drugname)
unique_drugs <- DRUG$drugname %>% table %>% sort(decreasing = TRUE) %>% names %>% .[1:30]
#unique_drugs

#Liste aller beobachteten Sequenzen an Medikamenten


#Funktionen für plots
num_reports_per_quarter <- function(data, v_year = NULL){
  if (v_year == "All") {
    reports <- data[, .N, by = quarter]
  } else {
    reports <- data[year == as.numeric(v_year), .N, by = quarter]
  }
  return(reports)
}

num_reports_per_sequence <- function(data){
  reports <- data[drug_seq < 50, .N, by = drug_seq]
  #reports <- data[, .N, by = drug_seq]
  return(reports)
}


calc_therapy_duration <- function(data) {
  ther_data <- data[!is.na(dur_converted) & dur_converted > 0]
  upper_bound <- quantile(ther_data$dur_converted, 0.90)
  ther_data$dur_converted[ther_data$dur_converted > upper_bound] <- upper_bound
  return(ther_data$dur_converted)
}

top_indications <- function(data){
  indications <- data[, .N, by = indi_pt] 
  top_indications <- indications[order(-N)][1:10]
  return(top_indications)
}

outcome_distribution <- function(data){
  data_complete_therapy <- data[!is.na(end_dt)] # Assuming 'end_dt' indicates the end of therapy
  outcome_dist <- data_complete_therapy[, .N, by = outcome_decoded]
  return(outcome_dist)
}


library(ggplot2)

# #erstellung plots
# library(ggplot2)
# 
# # Plotting number of reports per quarter:
# reports_per_quarter <- num_reports_per_quarter(final_data, "2022")
# ggplot(reports_per_quarter, aes(x = quarter, y = N)) +
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
# therapy_durations <- calc_therapy_duration(final_data)
# data <- data.frame(duration = therapy_durations)
#ggplot(data, aes(x=duration)) +
#  geom_histogram() +
#  labs(x="Therapy Duration (days)", y="Count", title="Histogram of Therapy Durations") +
#  theme_minimal()

 
 

# #Plotting top 10 indications:
#top_indications_data <- top_indications(final_data)
# ggplot(top_indications_data, aes(x = reorder(indi_pt, -N), y = N)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Top 10 Indications", x = "Indication", y = "Number of Reports")
# 
# #Plotting outcome distribution:
# outcome_distribution_data <- outcome_distribution(final_data)
# ggplot(outcome_distribution_data, aes(x = outc_cod, y = N)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Outcome Distribution", x = "Outcome Code", y = "Number of Outcomes")

# View(hist_data)




