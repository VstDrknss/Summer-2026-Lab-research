

##testing for claening files form a folder
library(readr)
library(rstudioapi)

input_folder <- selectDirectory(
  caption = "Select folder containing raw CSV files"
)

output_folder <- selectDirectory(
  caption = "Select folder to save cleaned CSV files"
)


files <- list.files(
  input_folder,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)


print(paste(length(files), "CSV files found"))


#looping through all csv files
for (input_file in files) {
  
  print(paste("Processing:", basename(input_file)))
  
  filename <- basename(input_file)
  
  
  # Extract drug name (first word before space)
  drug_name <- sub(
    " .*",
    "",
    filename
  )
  
  
  # Extract date
  experiment_date <- sub(
    ".*(\\d{4}-\\d{2}-\\d{2}).*",
    "\\1",
    filename
  )
  
  
  # Extract sensor (word after date)
  sensor <- sub(
    ".*\\d{4}-\\d{2}-\\d{2}\\s+([^ ]+).*",
    "\\1",
    filename
  )
  
  
  data <- read_csv(
    input_file,
    show_col_types = FALSE
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
  
  
  output_file <- file.path(
    output_folder,
    paste0(
      drug_name,
      "_cleaned_",
      sensor,
      "_",
      experiment_date,
      ".csv"
    )
  )
  
  write_csv(
    cleaned_data,
    output_file
  )
  
  
  print(
    paste(
      "Saved:",
      output_file
    )
  )
}


print("All CSV files cleaned successfully!")
