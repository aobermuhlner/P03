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
    merge(y = REAC, by = c("primaryid", "caseid"), all.x = TRUE) |>
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
#final_data <- join_data_drug("IBUPROFEN")
#y <- filter_data(final_data, 'All', 0, 120, 'All', 'All')
#View(y)
##################################################


# Function Report per quarter
num_reports_per_quarter <- function(data){
  reports <- data[, .N, by = .(quarter)]
  return(reports)
}

# Function Report per sequence
num_reports_per_sequence <- function(data){
  reports <- data[, .N, by = drug_seq]
  return(reports)
}


# Function Therapy
calc_therapy_duration_relative <- function(data, therapy_length) {
  ther_data <- data[!is.na(dur_converted) & dur_converted > 0]
  print(therapy_length)
  if (therapy_length == "Short term") {
    ther_data <- ther_data[dur_converted <= 30]
  } else if (therapy_length == "Medium term") {
    ther_data <- ther_data[dur_converted > 30 & dur_converted <= 365]
  } else if (therapy_length == "Long term") {
    ther_data <- ther_data[dur_converted > 365]
  } 
  return(ther_data$dur_converted)
}

# Function Indication
top_indications <- function(data){
  data <- data[!is.na(indi_pt)]
  indications <- data[, .N, by = indi_pt]
  return(indications)
}


# Function Outcome
outcome_distribution <- function(data){
  data <- data[!is.na(outcome_decoded) & !is.na(end_dt)]
  outcome_dist <- data[, .N, by = outcome_decoded]
  return(outcome_dist)
}


# Function Manufacturer
manufactorer_distribution <- function(data){
  data_complete_mfr <- data[!is.na(mfr_sndr)]
  manufactorer_dist <- data_complete_mfr[, .N, by = mfr_sndr]
 return(manufactorer_dist)
}

# Function Drug mix
prod_ai_distribution <- function(data){
  data_complete_prod_ai <- data[!is.na(prod_ai)]
  prod_ai_dist <- data_complete_prod_ai[, .N, by = prod_ai]
  return(prod_ai_dist)
}

# Function Reaction
drug_react_distribution <- function(data){
  data_complete_drug_reaction <- data[!is.na(pt)]
  drug_react <- data_complete_drug_reaction[, .N, by = pt]
  return(drug_react)
}

drug_react_distribution(final_data)


# Plots
# Plot report per quarter
plot_reports_per_quarter <- function(data){
  reports_per_quarter <- num_reports_per_quarter(data)
  
  # Calculate relative values
  total_reports <- sum(reports_per_quarter$N)
  reports_per_quarter$N_relative <- reports_per_quarter$N / total_reports * 100
  
  ggplot(reports_per_quarter, aes(x = "Quartals", y = N_relative, fill = quarter)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("#4e5d6c","#2B3E50", "#4e5d6c", "#2B3E50")) +
    labs(title = "", x = "", y = "Percentage of Reports") +
    theme_minimal() +
    coord_flip() +
    geom_text(aes(label = paste0(quarter, "\n", round(N_relative, 1), "%")), 
              position = position_stack(vjust = 0.5), 
              colour = "white") +
    theme(legend.position = "none")
}

# reports per sequence

# Plot Reports per sequence
plot_reports_per_sequence <- function(data){
  reports_per_sequence <- num_reports_per_sequence(data)
  reports_per_sequence <- dplyr::filter(reports_per_sequence, drug_seq <= 50)
  ggplot(reports_per_sequence, aes(x = factor(drug_seq), y = N)) +
    geom_bar(stat = "identity", fill="#2B3E50") +
    labs(title = "", x = "Sequence", y = "Number of Reports") +
    theme_minimal()
}

# Plot Therapy duration
plot_therapy_durations <- function(data, therapy_filter){
  therapy_durations <- calc_therapy_duration_relative(data, therapy_filter)
  therapy_df <- data.frame(duration = therapy_durations)
  
  bin_width <- switch(therapy_filter,
                      "Short term" = 1,
                      "Medium term" = 7,
                      "Long term" = 365,
                      "All" = 365)
  
  ggplot(therapy_df, aes(x = duration)) +
    geom_histogram(binwidth = bin_width, fill="#2B3E50") +
    labs(title = "", x = "Duration", y = "Frequency") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(round(min(therapy_df$duration)), round(max(therapy_df$duration)), by = bin_width))
}

plot_therapy_durations(final_data, "Medium term")

# plot indications

# Plot Indication
plot_top_indications <- function(data){
  indications_data <- top_indications(data)
  indications_data <- indications_data[order(indications_data$N, decreasing = TRUE),][1:10,]
  indications_data <- indications_data[!is.na(indications_data$N)]
  ggplot(indications_data, aes(x = reorder(indi_pt, -N), y = N)) +
    geom_bar(stat = "identity", fill="#2B3E50") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # Add label order distance
    labs(title = "", x = "", y = "Number of Reports") +
    theme_minimal()
}


# Plot Outcome
plot_outcome_distribution <- function(data){
  outcome_data <- outcome_distribution(data)
  outcome_data <- outcome_data[order(outcome_data$N, decreasing = TRUE),][1:10,]
  outcome_data <- outcome_data[!is.na(outcome_data$N)]
  ggplot(outcome_data, aes(x = reorder(outcome_decoded, -N), y = N)) +
    geom_bar(stat = "identity", fill="#2B3E50") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # Add label order distance
    labs(title = "", x = "", y = "Number of Outcomes") +
    theme_minimal()
}


# Plot Medication mix
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



# Manufactorer distribution
plot_manufactorer_distribution <- function(data) {
  manufactorer_dist <- manufactorer_distribution(data)
  manufactorer_dist <- manufactorer_dist[order(manufactorer_dist$N, decreasing = TRUE),][1:10,]
  manufactorer_dist <- manufactorer_dist[!is.na(manufactorer_dist$N)]
  p <- ggplot(manufactorer_dist, aes(x = reorder(mfr_sndr, -N), y = N)) +  # Also reorder the x-axis to match
    geom_col(fill="#2B3E50") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # Add label order distance
    labs(title = "", x = "", y = "Count") +
    theme_minimal()
  print(p) 
}


# Plot Drug Reaction
plot_drug_reaction <- function(data) {
  reaction <- drug_react_distribution(data)
  reaction <- reaction[order(reaction$N, decreasing = TRUE),][1:10,]
  reaction <- reaction[!is.na(reaction$N)]
  p <- ggplot(reaction, aes(x = reorder(pt, -N), y = N)) +
    geom_col(fill="#2B3E50") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # Add label order distance
    labs(title = "", x = "", y = "Count") +
    theme_minimal()
  
  print(p)
}
