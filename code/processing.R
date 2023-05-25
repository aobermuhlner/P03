library(data.table)
library(magrittr)

load_data <- function(entity, years) {
  quarters = c('Q1','Q2','Q3','Q4')
  data <- data.table()
  for (year in years) {
    for (quarter in quarters) {
      filename <- paste0('../../data/ASCII/', entity, year, quarter, '.txt')
      if (file.exists(filename)) {
        print(paste("Loading file:", filename)) 
        
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

entities = c('THER', 'REAC', 'OUTC', 'INDI', 'DRUG', 'DEMO')
years = c('22','23')

for (entity in entities) {
  assign(entity, load_data(entity, years))
}

# renames columns
setnames(THER, "dsg_drug_seq", "drug_seq")
setnames(INDI, "indi_drug_seq", "drug_seq")

#Konvertierung Therapie duration
conversion_factors <- list(
  SEC = 60 * 60 * 24,  # Seconds to day: 1 day/ (60 sec/min * 60 min/hr * 24 hr/day)
  MIN = 60 * 24,       # Minutes to day: 1 day/ (60 min/hr * 24 hr/day)
  HR = 24,               # Hours to day: 1 day/24 hr
  DAY = 1,                   # Days to day: already in correct unit
  WK = 1 / 7,                # Weeks to day: 1 day/7 days
  MON = 1 / 30.4375,         # Months to day: 1 day/30.4375 days (average month length)
  YR = 1 / 365.25            # Years to day: 1 day/365.25 days (average year length)
)

THER[, dur_converted := NA_real_]
non_empty_dur_cod <- THER$dur_cod != ""
factors <- conversion_factors[THER$dur_cod[non_empty_dur_cod]]
THER$dur_converted[non_empty_dur_cod] <- round(THER$dur[non_empty_dur_cod] / unlist(factors),2)


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

non_empty_age <- DEMO$age_cod[non_empty_age_cod]
age_factors <- unlist(factors)[non_empty_age]
DEMO$age[non_empty_age_cod] <- round(DEMO$age[non_empty_age_cod] * rep(age_factors, length.out = sum(non_empty_age_cod)),2)
DEMO[age > 120, age := NA]

# Codierung Outcome
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
REAC <- REAC[!is.na(REAC$pt)]


# Deletes all rows which start with a \ (only 1)
DRUG <- DRUG[!grepl("^\\\\", drugname)]

# OUTC gefiltert auf nur aktuellsten outcome
OUTC <- OUTC[OUTC[, .I[which.max(.I)], by = .(primaryid)]$V1]

# REAC gefiltert auf nur aktuellsten reaction
REAC <- REAC[REAC[, .I[which.max(.I)], by = .(primaryid, caseid)]$V1]

# Indiator gefiltert auf nur den letzten indicator
INDI <- INDI[INDI[, .I[which.max(.I)], by = .(primaryid)]$V1]


# select only needed columns
DRUG <- DRUG[, .(primaryid, caseid, drug_seq, drugname, prod_ai, route, year, quarter)]
DEMO <- DEMO[, .(primaryid, age, sex, wt, reporter_country, mfr_sndr)]
THER <- THER[, .(primaryid, caseid, drug_seq, dur_converted, start_dt, end_dt)]
INDI <- INDI[, .(primaryid, caseid, drug_seq, indi_pt)]
OUTC <- OUTC[, .(primaryid, caseid, outc_cod, outcome_decoded)]
REAC <- REAC[, .(primaryid, caseid, pt, drug_rec_act)]


fwrite(DRUG, "../../data/processed_data/DRUG.csv")
fwrite(DEMO, "../../data/processed_data/DEMO.csv")
fwrite(THER, "../../data/processed_data/THER.csv")
fwrite(INDI, "../../data/processed_data/INDI.csv")
fwrite(OUTC, "../../data/processed_data/OUTC.csv")
fwrite(REAC, "../../data/processed_data/REAC.csv")

