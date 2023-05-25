library(data.table)
library(dplyr)
library(ggplot2)

# Loading in
options(scipen=999)

DRUG <- fread("../../data/processed_data/DRUG.csv")
DEMO <- fread("../../data/processed_data/DEMO.csv")
THER <- fread("../../data/processed_data/THER.csv")
INDI <- fread("../../data/processed_data/INDI.csv")
OUTC <- fread("../../data/processed_data/OUTC.csv")
REAC <- fread("../../data/processed_data/REAC.csv")

idx <- data.table(drugname = unlist(strsplit(DRUG$drugname, "\\\\")),
                  rownum = rep(1:nrow(DRUG), times = lengths(strsplit(DRUG$drugname, "\\\\"))),
                  key = "drugname")

join_data_drug <- function(v_drugname = NULL) {
  
  # Merge all the data tables
  drug_data <- DRUG[idx[.(v_drugname), unique(rownum)]] |>
    merge(y = THER, by = c("primaryid", "caseid", "drug_seq"), all.x = TRUE) |>
    merge(y = INDI, by = c("primaryid", "caseid", "drug_seq"), all.x = TRUE) |>
    merge(y = REAC, by = c("primaryid"), all.x = TRUE) |>
    merge(y = DEMO, by = "primaryid", all.x = TRUE) |>
    merge(y = OUTC, by = "primaryid", all.x = TRUE) |>
    mutate(caseid = caseid.x) |>
    select(-c(caseid.x , caseid.y)) |>
    unique()
  
  # nur benötigte Spalten Selectieren 
  
  return(drug_data)
}

filter_data <- function(data, v_sex = NULL, v_age_min = NULL, v_age_max = NULL, v_year = NULL, v_sequence_min = NULL, v_sequence_max = NULL) {
  
  v_age_min <- ifelse(is.null(v_age_min), min(data$age, na.rm = TRUE), v_age_min)
  v_age_max <- ifelse(is.null(v_age_max), max(data$age, na.rm = TRUE), v_age_max)
  
  v_sequence_min <- ifelse(is.null(v_sequence_min), min(data$drug_seq, na.rm = TRUE), v_sequence_min)
  v_sequence_max <- ifelse(is.null(v_sequence_max), max(data$drug_seq, na.rm = TRUE), v_sequence_max)
  
  if (is.null(v_year) | v_year == "All"){
    v_year <- unique(data$year)
  }
  
  if (is.null(v_sex) | v_sex == "All") {
    filtered_data <- data[age >= v_age_min & age <= v_age_max & drug_seq >= v_sequence_min & drug_seq <= v_sequence_max,]
  } else {
    filtered_data <- data[sex %in% v_sex & age >= v_age_min & age <= v_age_max & drug_seq >= v_sequence_min & drug_seq <= v_sequence_max,]
  }
  
  filtered_data <- filtered_data[year %in% v_year]
  return(filtered_data)
}

########################################## Testing
# Run the function
final_data <- join_data_drug("IBUPROFEN")
y <- filter_data(final_data, 'All', 0, 120, 'All', 'All')


#View(y)
##################################################

# Aufbereitungen für PLots und Listen

# unique_drugs <- DRUG$drugname %>% table %>% sort(decreasing = TRUE) %>% names %>% .[1:10]
num_reports_per_quarter <- function(data){
  reports <- data[, .N, by = .(quarter)]
  return(reports)
}

num_reports_per_sequence <- function(data){
  reports <- data[, .N, by = drug_seq]
  return(reports)
}

calc_therapy_duration <- function(data) {
  ther_data <- data[!is.na(dur_converted) & dur_converted > 0]
  upper_bound <- quantile(ther_data$dur_converted, 0.90)
  ther_data$dur_converted[ther_data$dur_converted > upper_bound] <- upper_bound
  return(ther_data$dur_converted)
}

calc_therapy_duration_relative <- function(data, therapy_length) {
  ther_data <- data[!is.na(dur_converted) & dur_converted > 0]
  print(therapy_length)
  if (therapy_length == "Short term") {
    ther_data <- ther_data[dur_converted <= 30]
  } else if (therapy_length == "Medium term") {
    ther_data <- ther_data[dur_converted > 30 & dur_converted <= 365]
  } else if (therapy_length == "Long term") {
    ther_data <- ther_data[dur_converted > 365]
  } # If therapy_length is "All", no additional filtering is needed
  
  return(ther_data$dur_converted)
}

top_indications <- function(data){
  data <- data[!is.na(indi_pt)]
  indications <- data[, .N, by = indi_pt]
  return(indications)
}

outcome_distribution <- function(data){
  data <- data[!is.na(outcome_decoded) & !is.na(end_dt)]
  outcome_dist <- data[, .N, by = outcome_decoded]
  return(outcome_dist)
}

manufactorer_distribution <- function(data){
  data_complete_mfr <- data[!is.na(mfr_sndr)]
  manufactorer_dist <- data_complete_mfr[, .N, by = mfr_sndr]
  manufactorer_dist <- na.omit(manufactorer_dist)
  manufactorer_dist <- manufactorer_dist[mfr_sndr != "",]
 return(manufactorer_dist)
}

prod_ai_distribution <- function(data){
  data_complete_prod_ai <- data[!is.na(prod_ai)]
  prod_ai_dist <- data_complete_prod_ai[, .N, by = prod_ai]
  prod_ai_dist <- na.omit(prod_ai_dist)
  prod_ai_dist <- prod_ai_dist[prod_ai != "",]
  prod_ai_dist <- prod_ai_dist[order(-N)][1:10]  # Order by count (N) in descending order and take the top 10
  return(prod_ai_dist)
}

drug_react_distribution <- function(data){
  data_complete_drug_reaction <- data[!is.na(drug_rec_act)]
  drug_react <- data_complete_drug_reaction[, .N, by = drug_rec_act]
  drug_react <- na.omit(drug_react)
  drug_react <- drug_react[drug_rec_act != "",]
  drug_react <- drug_react[order(-N)][1:10]
  return(drug_react)
}

# Plots

plot_reports_per_quarter <- function(data){
  reports_per_quarter <- num_reports_per_quarter(data)
  
  # Calculate relative values
  total_reports <- sum(reports_per_quarter$N)
  reports_per_quarter$N_relative <- reports_per_quarter$N / total_reports * 100
  
  ggplot(reports_per_quarter, aes(x = "Quartals", y = N_relative, fill = quarter)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("#4e5d6c","#2B3E50", "#4e5d6c", "#2B3E50")) +
    labs(title = "", x = "Quartals", y = "Percentage of Reports") +
    theme_minimal() +
    coord_flip() +
    geom_text(aes(label = paste0(quarter, "\n", round(N_relative, 1), "%")), 
              position = position_stack(vjust = 0.5), 
              colour = "white") +
    theme(legend.position = "none")
}


plot_reports_per_sequence <- function(data){
  reports_per_sequence <- num_reports_per_sequence(data)
  reports_per_sequence <- dplyr::filter(reports_per_sequence, drug_seq <= 50)
  ggplot(reports_per_sequence, aes(x = factor(drug_seq), y = N)) +
    geom_bar(stat = "identity", fill="#2B3E50") +
    labs(title = "", x = "Sequence", y = "Number of Reports") +
    theme_minimal()
}

plot_therapy_durations <- function(data, therapy_filter){
  therapy_durations <- calc_therapy_duration_relative(data, therapy_filter)
  
  # Create a data frame from the vector of therapy durations
  therapy_df <- data.frame(duration = therapy_durations)
  
  ggplot(therapy_df, aes(x = duration)) +
    geom_histogram( fill="#2B3E50") +   # You might need to adjust binwidth
    labs(title = "", x = "Duration", y = "Frequency") +
    theme_minimal()
}


plot_top_indications <- function(data){
  top_indications_data <- top_indications(data)
  top_indications_data <- top_indications_data[order(top_indications_data$N, decreasing = TRUE),][1:10,]
  ggplot(top_indications_data, aes(x = reorder(indi_pt, -N), y = N)) +
    geom_bar(stat = "identity", fill="#2B3E50") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # Add label order distance
    labs(title = "", x = "Indication", y = "Number of Reports") +
    theme_minimal()
}



plot_outcome_distribution <- function(data){
  outcome_distribution_data <- outcome_distribution(data)
  outcome_distribution_data <- outcome_distribution_data[order(outcome_distribution_data$N, decreasing = TRUE),][1:10,]
  ggplot(outcome_distribution_data, aes(x = reorder(outcome_decoded, -N), y = N)) +
    geom_bar(stat = "identity", fill="#2B3E50") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # Add label order distance
    labs(title = "", x = "Outcome", y = "Number of Outcomes") +
    theme_minimal()
}

# Drug mix by group
plot_prod_ai_distribution <- function(data) {
  prod_ai_dist <- prod_ai_distribution(data)
  p <- ggplot(prod_ai_dist, aes(x = reorder(prod_ai, -N), y = N)) +
    geom_col(fill="#2B3E50") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Drug Combinations") +
    ylab("Count") +
    theme_minimal()
  print(p)
}


# Manufactorer_distribution
plot_manufactorer_distribution <- function(data) {
  manufactorer_dist <- manufactorer_distribution(data)
  manufactorer_dist <- manufactorer_dist[order(manufactorer_dist$N, decreasing = TRUE),][1:10,]
  p <- ggplot(manufactorer_dist, aes(x = reorder(mfr_sndr, -N), y = N)) +  # Also reorder the x-axis to match
    geom_col(fill="#2B3E50") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # Add label order distance
    xlab("Manufacturer") +
    ylab("Count")+
    theme_minimal()
  print(p) 
}

plot_drug_reaction <- function(data) {
  reaction <- drug_react_distribution(data)
  p <- ggplot(reaction, aes(x = reorder(drug_rec_act, -N), y = N)) +
    geom_col(fill="#2B3E50") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # Add label order distance
    geom_text(aes(label = N), vjust = -0.5) +  # Add numbers on top of the bars
    xlab("Top 10 Reactions") +
    ylab("Count")+
    theme_minimal()
  
  print(p)
}


# plot_drug_reaction(final_data)


