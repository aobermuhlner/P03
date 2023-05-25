library(data.table)

# Loading in
options(scipen=999)

DRUG <- fread("../../data/processed_data/DRUG.csv")
DEMO <- fread("../../data/processed_data/DEMO.csv")
THER <- fread("../../data/processed_data/THER.csv")
INDI <- fread("../../data/processed_data/INDI.csv")
OUTC <- fread("../../data/processed_data/OUTC.csv")
REAC <- fread("../../data/processed_data/REAC.csv")

data_vec <- list(
  "DRUG" = DRUG,
  "DEMO" = DEMO,
  "THER" = THER,
  "INDI" = INDI,
  "OUTC" = OUTC,
  "REAC" = REAC
)

idx <- data.table(drugname = unlist(strsplit(DRUG$drugname, "\\\\")),
                  rownum = rep(1:nrow(DRUG), times = lengths(strsplit(DRUG$drugname, "\\\\"))),
                  key = "drugname")

join_data_drug <- function(v_drugname = NULL) {
  
  # Merge all the data tables
  drug_data <- data_vec[["DRUG"]][idx[.(v_drugname), unique(rownum)]] |>
    merge(y = data_vec[["THER"]], by = c("primaryid", "caseid", "drug_seq"), all.x = TRUE) |>
    merge(y = data_vec[["INDI"]], by = c("primaryid", "caseid", "drug_seq"), all.x = TRUE) |>
    merge(y = data_vec[["REAC"]], by = c("primaryid", "caseid"), all.x = TRUE) |>
    merge(y = data_vec[["DEMO"]], by = "primaryid", all.x = TRUE) |>
    merge(y = data_vec[["OUTC"]], by = "primaryid", all.x = TRUE) |>
    mutate(caseid = caseid.x) |>
    select(-c(caseid.x , caseid.y)) |>
    unique()
  
  # nur benötigte Spalten Selectieren 
  
  return(drug_data)
}

filter_data <- function(data, v_sex = NULL, v_age_min = NULL, v_age_max = NULL, v_year = NULL, v_sequenz = NULL) {
  
  v_age_min <- ifelse(is.null(v_age_min), min(data$age, na.rm = TRUE), v_age_min)
  v_age_max <- ifelse(is.null(v_age_max), max(data$age, na.rm = TRUE), v_age_max)
  
  if (is.null(v_year) | v_year == "All"){
    v_year <- unique(data$year)
  }
  if (is.null(v_sequenz) | v_sequenz == "All"){
    v_sequenz <- unique(data$drug_seq)
  }
  if (is.null(v_sex) | v_sex == "All") {
    filtered_data <- data[age >= v_age_min & age <= v_age_max,]
  } else {
    filtered_data <- data[sex %in% v_sex & age >= v_age_min,]
  }
  filtered_data <- data[drug_seq %in% v_sequenz & year %in% v_year]
  return(filtered_data)
}


################################ Testing
# Run the function
final_data <- join_data_drug("IBUPROFEN")
y <- filter_data(final_data, 'All', 0, 120, 'All', 'All')
View(y)
#View(final_data)
##################################################

# Aufbereitungen für PLots und Listen

# unique_drugs <- DRUG$drugname %>% table %>% sort(decreasing = TRUE) %>% names %>% .[1:10]


#Funktionen für plots
num_reports_per_quarter <- function(data){
  reports <- data[, .N, by = .(year, quarter)]
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
  top_indications <- indications[order(-N)][1:10]
  return(top_indications)
}


outcome_distribution <- function(data){
  data <- data[!is.na(outcome_decoded)]
  data_complete_therapy <- data[!is.na(end_dt)] # Assuming 'end_dt' indicates the end of therapy
  outcome_dist <- data_complete_therapy[, .N, by = outcome_decoded]
  ourcome_dist <- outcome_dist[order(-N)][1:10]
  return(outcome_dist)
}

manufactorer_distribution <- function(data){
  data_complete_mfr <- data[!is.na(mfr_sndr)]
  manufactorer_dist <- data_complete_mfr[, .N, by = mfr_sndr]
  manufactorer_dist <- na.omit(manufactorer_dist)
  manufactorer_dist <- manufactorer_dist[mfr_sndr != "",]
  manufactorer_dist <- manufactorer_dist[order(-N)][1:10]  # Order by count (N) in descending order and take the top 10
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


library(ggplot2)
# 
# unique(final_data$prod_ai)
# 
# prod_ai_distribution(final_data)

# #erstellung plots
plot_reports_per_quarter <- function(data){
  reports_per_quarter <- num_reports_per_quarter(data)
  ggplot(reports_per_quarter, aes(x = as.factor(year), y = N, fill = quarter)) +
    geom_bar(stat = "identity") +
    labs(title = "", x = "Year", y = "Number of Reports") +
    theme_minimal() +
    coord_flip() +
    geom_text(aes(label = N), position = position_stack(vjust = 0.5))
}

plot_reports_per_sequence <- function(data){
  reports_per_sequence <- num_reports_per_sequence(data)
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
    geom_histogram() +   # You might need to adjust binwidth
    labs(title = "", x = "Duration", y = "Frequency") +
    theme_minimal()
}


plot_top_indications <- function(data){
  top_indications_data <- top_indications(data)
  ggplot(top_indications_data, aes(x = reorder(indi_pt, -N), y = N)) +
    geom_bar(stat = "identity", fill="#2B3E50") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # Add label order distance
    geom_text(aes(label = N), vjust = -0.5) +  # Add numbers on top of the bars
    labs(title = "", x = "Indication", y = "Number of Reports") +
    theme_minimal()
}


plot_outcome_distribution <- function(data){
  outcome_distribution_data <- outcome_distribution(data)
  ggplot(outcome_distribution_data, aes(x = reorder(outcome_decoded, -N), y = N)) +
    geom_bar(stat = "identity", fill="#2B3E50") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # Add label order distance
    geom_text(aes(label = N), vjust = -0.5) +  # Add numbers on top of the bars
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
  p <- ggplot(manufactorer_dist, aes(x = reorder(mfr_sndr, -N), y = N)) +  # Also reorder the x-axis to match
    geom_col(fill="#2B3E50") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # Add label order distance
    geom_text(aes(label = N), vjust = -0.5) +  # Add numbers on top of the bars
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


