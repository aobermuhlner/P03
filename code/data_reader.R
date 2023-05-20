library(data.table)

# Loading in

DRUG <- fread("../../data/processed_data/DRUG.csv")
DEMO <- fread("../../data/processed_data/DEMO.csv")
THER <- fread("../../data/processed_data/THER.csv")
INDI <- fread("../../data/processed_data/INDI.csv")
OUTC <- fread("../../data/processed_data/OUTC.csv")
REAC <- fread("../../data/processed_data/REAC.csv")

# creates and index data.table which is called in join_Data
idx <- data.table(drugname = unlist(strsplit(DRUG$drugname, "\\\\")),
                  rownum = rep(1:nrow(DRUG), times = lengths(strsplit(DRUG$drugname, "\\\\"))),
                  key = "drugname")

# Join_data function parameters come from shiny

join_data <- function(v_drugname = NULL, v_sex = NULL, v_age_min = NULL, v_age_max = NULL, v_year = NULL) {
  
  # if (is.null(v_sex) | v_sex == "All") {
  #   v_sex <- unique(DEMO$sex)
  # }
  if (is.null(v_year) | v_year == "All") {
    v_year <- unique(DRUG$year)
  }
  v_age_min <- ifelse(is.null(v_age_min), min(DEMO$age, na.rm = TRUE), v_age_min)
  v_age_max <- ifelse(is.null(v_age_max), max(DEMO$age, na.rm = TRUE), v_age_max)
  
  selected_drug <- DRUG[idx[J(v_drugname), unique(rownum)]]
  selected_drug <- unique(selected_drug[year %in%  as.numeric(v_year),])
  if (is.null(v_sex) | v_sex == "All") {
    selected_patients <- unique(DEMO[primaryid %in% selected_drug$primaryid & 
                                       age >= v_age_min & age <= v_age_max,
                                     .(primaryid, age, sex, wt, reporter_country)])
  } else {
    selected_patients <- unique(DEMO[primaryid %in% selected_drug$primaryid & 
                                       sex %in% v_sex & 
                                       age >= v_age_min & age <= v_age_max,
                                     .(primaryid, age, sex, wt, reporter_country)])
  }
  selected_therapies <- unique(THER[primaryid %in% selected_drug$primaryid,])
  selected_indications <- unique(INDI[primaryid %in% selected_drug$primaryid,])
  selected_outcomes <- OUTC[primaryid %in% selected_drug$primaryid, 
                            .(primaryid, outc_cod, outcome_decoded)]
  selected_outcomes <- selected_outcomes[selected_outcomes[, .I[which.max(.I)], by = .(primaryid)]$V1]
  
  # Merge all the data tables
  final_data <- merge(selected_drug, selected_patients, by = "primaryid", all.x = ifelse((is.null(v_sex) | v_sex == "All"), TRUE, FALSE))
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
#final_data <- join_data("ZYRTEC", "All" ,0 ,120, "All")
#View(final_data)

##################################################

# Aufbereitungen für PLots und Listen

unique_drugs <- DRUG$drugname %>% table %>% sort(decreasing = TRUE) %>% names %>% .[1:10]


#Funktionen für plots
num_reports_per_quarter <- function(data){
  reports <- data[, .N, by = quarter]
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

calc_therapy_duration_relative <- function(data, therapy_length = "All") {
  ther_data <- data[!is.na(dur_converted) & dur_converted > 0]
  
  if (therapy_length == "short term") {
    ther_data <- ther_data[dur_converted <= 30]
  } else if (therapy_length == "medium term") {
    ther_data <- ther_data[dur_converted > 30 & dur_converted <= 365]
  } else if (therapy_length == "long term") {
    ther_data <- ther_data[dur_converted > 365]
  } # If therapy_length is "All", no additional filtering is needed
  
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

manufactorer_distribution <- function(data){
  data_complete_mfr <- data[!is.na(mfr_sndr)]
  manufactorer_dist <- data_complete_mfr[, .N, by = mfr_sndr]
  manufactorer_dist <- manufactorer_dist[order(-N)][1:10]  # Order by count (N) in descending order and take the top 10
  return(manufactorer_dist)
}

prod_ai_distribution <- function(data){
  data_complete_prod_ai <- data[!is.na(prod_ai)]
  prod_ai_dist <- data_complete_prod_ai[, .N, by = prod_ai]
  prod_ai_dist <- prod_ai_dist[order(-N)][1:10]  # Order by count (N) in descending order and take the top 10
  return(prod_ai_dist)
}


library(ggplot2)

unique(final_data$prod_ai)

prod_ai_distribution(final_data)

# #erstellung plots
# library(ggplot2)

# Drug mix by group
plot_prod_ai_distribution <- function(data) {
  prod_ai_dist <- prod_ai_distribution(data)
  p <- ggplot(prod_ai_dist, aes(x = reorder(prod_ai, -N), y = N)) + 
    geom_col() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("Drug Combinations") +
    ylab("Count") +
    ggtitle("Top 10 Drug Combinations in 'prod_ai'")
  print(p)
}
plot_prod_ai_distribution(final_data)




# Manufactorer_distribution
plot_manufactorer_distribution <- function(data) {
  manufactorer_dist <- manufactorer_distribution(data)
  manufactorer_dist <- manufactorer_dist[order(-N)]  # Order the data.table from highest to lowest count
  p <- ggplot(manufactorer_dist, aes(x = reorder(mfr_sndr, -N), y = N)) +  # Also reorder the x-axis to match
    geom_col() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("Manufacturer Sender") +
    ylab("Count") +
    ggtitle("Outcome Distribution by Top 10 Manufacturer Senders")
  print(p)
}
plot_manufactorer_distribution(final_data)

# Therapy length
therapy_durations <- calc_therapy_duration_relative(final_data, "long term")
data <- data.frame(duration = therapy_durations)
ggplot(data, aes(x=duration)) +
  geom_histogram() +
  labs(x="Therapy Duration (days)", y="Count", title="Histogram of Therapy Durations") +
  theme_minimal()



# 
# Plotting number of reports per quarter:
# reports_per_quarter <- num_reports_per_quarter(final_data)
# ggplot(reports_per_quarter, aes(x = quarter, y = N)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Number of Reports per Quarter", x = "Quarter", y = "Number of Reports")

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



