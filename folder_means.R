library(readr)
library(rstudioapi)

input_folder <- selectDirectory(
  caption = "Select folder containing cleaned CSV files"
)


input_files <- list.files(
  input_folder,
  pattern = "\\.csv$",
  full.names = TRUE
)


cat(
  "Found",
  length(input_files),
  "files\n"
)


combined_file <- file.choose()

drug_name <- basename(dirname(combined_file))

output_file <- file.path(
  dirname(combined_file),
  paste0("Combined_means_of_", drug_name, ".csv")
)


combined <- read_csv(
  combined_file,
  name_repair = "minimal",
  na = c("", "NA"),
  col_types = cols(.default = col_character())
)


colnames(combined) <- trimws(colnames(combined))

colnames(combined) <- make.unique(
  colnames(combined)
)


combined_conc <- grep(
  "concentration",
  colnames(combined),
  ignore.case = TRUE
)


colnames(combined)[combined_conc] <- "Concentration"


combined$Concentration <- as.numeric(
  combined$Concentration
)


date_row <- 1


combined <- rbind(
  combined[1,],
  combined
)


combined[1,] <- NA

combined$Concentration[1] <- NA


check_rows <- which(
  !is.na(combined$Concentration)
)


timepoint_map <- c(
  "Basal" = "basal",
  "Timepoint 1" = "2.5 min",
  "Timepoint 2" = "5 min",
  "Timepoint 3" = "7.5 min",
  "Timepoint 4" = "10 min",
  "Timepoint 5" = "12.5 min",
  "Timepoint 6" = "15 min",
  "Timepoint 7" = "17.5 min",
  "Timepoint 8" = "20 min",
  "Timepoint 9" = "22.5 min",
  "Timepoint 10" = "25 min"
)


# looping through all csv files
for(input_file in input_files){
  
  cat(
    "\nProcessing:",
    basename(input_file),
    "\n"
  )
  
  experiment_date <- sub(
    ".*(\\d{4}-\\d{2}-\\d{2}).*",
    "\\1",
    basename(input_file)
  )
  
  cat(
    "Experiment date:",
    experiment_date,
    "\n"
  )
  
  data <- read_csv(
    input_file,
    name_repair = "minimal",
    na = c("", "NA"),
    col_types = cols(.default = col_character())
  )
  
  
  colnames(data) <- trimws(
    colnames(data)
  )
  
  
  data_conc <- grep(
    "concentration",
    colnames(data),
    ignore.case = TRUE
  )
  
  colnames(data)[data_conc] <- "Concentration"
  
  
  data$Concentration <- as.numeric(
    data$Concentration
  )
  
  
  colnames(data)[
    colnames(data) %in% names(timepoint_map)
  ] <-
    timepoint_map[
      colnames(data)[colnames(data) %in% names(timepoint_map)]
    ]
  
  # CHECK IF EXPERIMENT ALREADY EXISTS (SAME DATE + SAME DATA)
  
  duplicate_found <- FALSE
  
  # Find columns with the same experiment date
  same_date_cols <- which(
    unlist(combined[date_row, ]) == experiment_date
  )
  
  # Only check duplicates if the date already exists
  if(length(same_date_cols) > 0){
    
    all_timepoints_match <- c()
    
    for(col in 2:ncol(data)){
      
      timepoint <- colnames(data)[col]
      
      new_values <- as.character(
        data[[col]]
      )
      
      existing_cols <- intersect(
        grep(
          paste0("^", timepoint),
          colnames(combined)
        ),
        same_date_cols
      )
      
      timepoint_match <- FALSE
      
      for(c in existing_cols){
        
        existing_values <- as.character(
          combined[check_rows, c]
        )
        
        if(identical(
          existing_values,
          new_values
        )){
          
          timepoint_match <- TRUE
          break
          
        }
      }
      
      all_timepoints_match <- c(
        all_timepoints_match,
        timepoint_match
      )
      
    }
    
    
    if(all(all_timepoints_match)){
      
      duplicate_found <- TRUE
      
    }
  }
  
  
  if(duplicate_found){
    
    cat(
      "Skipping duplicate file:",
      basename(input_file),
      "\n"
    )
    
    next
    
  }
  
  for(col in 2:ncol(data)){
    
    timepoint <- colnames(data)[col]
    
    values <- data[[col]]
    
    
    cat(
      "Adding:",
      timepoint,
      "\n"
    )
    
    
    # find matching columns
    time_cols <- grep(
      paste0("^", timepoint),
      colnames(combined)
    )
    
    
    target_col <- NA
    
    
    # find empty replicate
    for(c in time_cols){
      
      if(all(
        is.na(combined[check_rows,c]) |
        combined[check_rows,c] == ""
      )){
        
        target_col <- c
        
        break
        
      }
      
    }
    
    
    # create new column
    if(is.na(target_col)){
      
      insert_position <- max(time_cols)
      
      new_column <- rep(
        NA_character_,
        nrow(combined)
      )
      
      new_df <- data.frame(
        new_column
      )
      
      colnames(new_df) <- timepoint
      
      
      left <- combined[,1:insert_position,drop=FALSE]
      
      
      if(insert_position < ncol(combined)){
        
        right <- combined[
          ,
          (insert_position+1):ncol(combined),
          drop=FALSE
        ]
        
        combined <- cbind(
          left,
          new_df,
          right
        )
        
      } else {
        
        combined <- cbind(
          combined,
          new_df
        )
        
      }
      
      target_col <- insert_position + 1
      
      cat(
        "Created new column:",
        timepoint,
        "\n"
      )
      
    }
    

    # ADD DATE UNDER TIMEPOINT
    combined[date_row, target_col] <- experiment_date
    
    
    # inserting values
    for(i in seq_len(nrow(data))){
      
      row <- match(
        data$Concentration[i],
        combined$Concentration
      )
      
      if(!is.na(row)){
        
        combined[row,target_col] <- values[i]
        
      }
      
    }
    
    
  }
  
  
}

# ORDER COLUMNS BY TIMEPOINT THEN DATE

timepoints <- c(
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

ordered_columns <- c()

for(tp in timepoints){
  
  # find columns belonging to this timepoint
  cols <- grep(
    paste0("^", tp),
    colnames(combined)
  )
  
  # remove Concentration if matched
  cols <- cols[
    colnames(combined)[cols] != "Concentration"
  ]
  
  if(length(cols) > 0){
    
    # extract dates
    dates <- as.Date(
      unlist(combined[1, cols]),
      format = "%Y-%m-%d"
    )
    
    # sort by date
    cols <- cols[
      order(dates, na.last = TRUE)
    ]
    
    ordered_columns <- c(
      ordered_columns,
      colnames(combined)[cols]
    )
  }
}


# keep Concentration first

combined <- combined[
  ,
  c("Concentration", ordered_columns),
  drop = FALSE
]

write_csv(
  combined,
  output_file,
  na=""
)

cat("Finished merging all files!")
