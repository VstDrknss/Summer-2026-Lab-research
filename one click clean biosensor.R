library(readr)
library(rstudioapi)

main_folder <- selectDirectory(
  caption = "Select general folder (e.g. PKC)"
)

# Find all immediate drug folders
drug_folders <- list.dirs(
  main_folder,
  full.names = TRUE,
  recursive = FALSE
)

excluded_folders <- c(
  "Excel"
)

drug_folders <- drug_folders[
  !basename(drug_folders) %in% excluded_folders
]

for(drug_folder in drug_folders){
  
  cleaned_folder <- file.path(
    drug_folder,
    "CLEANED"
  )
  
  
  if(!dir.exists(cleaned_folder)){
    
    dir.create(
      cleaned_folder
    )
    
  }
  
  # Only look directly inside the drug folder
  files <- list.files(
    drug_folder,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  # Exclude:
  # - files already named _cleaned.csv
  # - combined files
  files <- files[
    !grepl(
      "_cleaned\\.csv$|combined_means",
      basename(files),
      ignore.case = TRUE
    )
  ]

  # Skip folder if there are no raw CSV files
  if(length(files) == 0){
    
    next
    
  }
  
  for(input_file in files){
    
    cat("Processing:", basename(input_file), "\n")
    
    filename <- basename(input_file)

    drug_name <- sub(
      " .*",
      "",
      filename
    )
    
    experiment_date <- sub(
      ".*(\\d{4}-\\d{2}-\\d{2}).*",
      "\\1",
      filename
    )
    
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

    output_filename <- sub(
      "\\.csv$",
      "_cleaned.csv",
      filename,
      ignore.case = TRUE
    )
    
    output_file <- file.path(
      cleaned_folder,
      output_filename
    )
    
    write_csv(
      cleaned_data,
      output_file
    )
    
    
    cat(
      "Saved to CLEANED:",
      basename(output_file),
      "\n"
    )
    
  }
  
  
  cat("\n")
  cat(
    "Finished folder:",
    basename(drug_folder),
    "\n"
  )
  
}

cat("ALL FOLDERS HAVE BEEN PROCESSED\n")