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

DRUG[DRUG$drugname %in% 'XANAX']
drug <- join_data_drug('XANAX')
x <- filter_data(drug, 'All', 0, 120, 'All', 'All')
View(x)
