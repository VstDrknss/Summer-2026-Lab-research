library(readr)

input_file <- file.choose()

filename <- basename(input_file)

experiment_date <- sub(
  ".*(\\d{4}-\\d{2}-\\d{2}).*",
  "\\1",
  filename
)


output_file <- paste0(
  "~/Downloads/SER2_cleaned_",
  experiment_date,
  ".csv"
)


data <- read_csv(
  input_file
)


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
