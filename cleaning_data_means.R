library(readr)

input_file <- file.choose()

output_file <- "~/Downloads/SER1_cleaned.csv"


data <- read_csv(
  input_file
)

# select columns

keep_columns <- c(
  1,
  seq(
    from = 2,
    to = ncol(data),
    by = 2
  )
)


cleaned_data <- data[, keep_columns]

colnames(cleaned_data) <- c(
  "Concentration",
  "basal",
  "2.5 min",
  "5 min",
  "7.5 min",
  "10 min",
  "12.5 min",
  "15 min",
  "17.5 min",
  "20 min",
  "22.5 min",
  "25 min"
)

write_csv(
  cleaned_data,
  output_file
)


print("CSV cleaned successfully")
