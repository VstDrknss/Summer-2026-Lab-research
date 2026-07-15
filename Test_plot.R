library(readxl)
library(minpack.lm)
library(ggplot2)

file_path <- "~/Downloads/2026-06-04 pkc ser lis ser lis AX.xlsx"
data <- read_excel(file_path, sheet = 1)

drug_rows <- list(
  drug1 = 2:9,
  drug2 = 13:20,
  drug3 = 24:31,
  drug4 = 35:42
)

# -------------------------
# 2. Function to compute mean table
# -------------------------
make_mean_table <- function(df) {
  data.frame(
    Basal = rowMeans(df[, 1:3], na.rm = TRUE),
    T1    = rowMeans(df[, 4:6], na.rm = TRUE),
    T2    = rowMeans(df[, 7:9], na.rm = TRUE),
    T3    = rowMeans(df[, 10:12], na.rm = TRUE),
    T4    = rowMeans(df[, 13:15], na.rm = TRUE),
    T5    = rowMeans(df[, 16:18], na.rm = TRUE),
    T6    = rowMeans(df[, 19:21], na.rm = TRUE),
    T7    = rowMeans(df[, 22:24], na.rm = TRUE),
    T8    = rowMeans(df[, 25:27], na.rm = TRUE),
    T9    = rowMeans(df[, 28:30], na.rm = TRUE),
    T10   = rowMeans(df[, 31:33], na.rm = TRUE)
  )
}

# -------------------------
# 3. Storage lists
# -------------------------
drug_raw  <- list()
drug_clean <- list()
drug_mean <- list()

# -------------------------
# 4. Loop through drugs
# -------------------------
for (name in names(drug_rows)) {
  
  rows <- drug_rows[[name]]
  
  # raw extraction
  raw <- data[rows, 117:149]
  
  # clean numeric + round
  clean <- as.data.frame(lapply(raw, as.numeric))
  clean <- round(clean, 3)
  
  # mean table
  mean_table <- make_mean_table(clean)
  mean_table <- round(mean_table, 3)
  
  # store
  drug_raw[[name]]   <- raw
  drug_clean[[name]] <- clean
  drug_mean[[name]]  <- mean_table
}

# -------------------------
# 5. View results
# -------------------------
View(drug_raw$drug1)
View(drug_clean$drug1)
View(drug_mean$drug1)
