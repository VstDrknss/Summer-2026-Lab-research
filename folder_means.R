library(readr)
library(rstudioapi)

input_folder <- selectDirectory(
  caption = "Select folder containing cleaned CSV files")

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

drug_name <- basename(
  dirname(combined_file)
)

output_file <- file.path(
  dirname(combined_file),
  paste0(
    "Combined_means_of_",
    drug_name,
    ".csv"
  )
)


combined <- read_csv(
  combined_file,
  name_repair = "minimal",
  na = c("", "NA"),
  col_types = cols(.default = col_character())
)

colnames(combined) <- trimws(
  colnames(combined)
)

# Make duplicate column names unique
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
  combined[1, ],
  combined
)

combined[1, ] <- NA

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


find_timepoint_columns <- function(
    column_names,
    timepoint
){
  
  escaped <- gsub(
    "([.|()\\^$*+?{}\\[\\]\\\\])",
    "\\\\\\1",
    timepoint
  )
  
  which(
    grepl(
      paste0(
        "^",
        escaped,
        "(\\.\\d+)?$"
      ),
      column_names
    )
  )
  
}


is_completely_empty <- function(
    x,
    rows
){
  
  values <- as.character(
    x[rows]
  )
  
  all(
    is.na(values) |
      trimws(values) == ""
  )
  
}

for(input_file in input_files){
  
  cat(
    "Processing:",
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

  #concentration
  data_conc <- grep(
    "concentration",
    colnames(data),
    ignore.case = TRUE
  )
  
  colnames(data)[data_conc] <- "Concentration"
  
  data$Concentration <- as.numeric(
    data$Concentration
  )
  

  #renamign inputs
  for(i in seq_along(colnames(data))){
    
    current_name <- colnames(data)[i]
    
    if(current_name %in% names(timepoint_map)){
      
      colnames(data)[i] <-
        timepoint_map[current_name]
      
    }
    
  }
  
  
  for(i in seq_along(colnames(data))){
    
    cat(
      i,
      ":",
      colnames(data)[i],
      "\n"
    )
    
  }
  

  
  duplicate_found <- FALSE
  
  same_date_cols <- which(
    as.character(
      combined[date_row, ]
    ) == experiment_date
  )
  
  if(length(same_date_cols) > 0){
    
    all_timepoints_match <- TRUE
    
    for(col in 2:ncol(data)){
      
      timepoint <- colnames(data)[col]
      
      new_values <- as.character(
        data[[col]]
      )
      
      # Find exact timepoint columns
      time_cols <- find_timepoint_columns(
        colnames(combined),
        timepoint
      )
      
      # Keep only columns with this date
      existing_cols <- intersect(
        time_cols,
        same_date_cols
      )
      
      timepoint_match <- FALSE
      
      for(existing_col in existing_cols){
        
        existing_values <- as.character(
          combined[
            check_rows,
            existing_col
          ]
        )
        
        if(
          length(existing_values) ==
          length(new_values) &&
          isTRUE(
            all.equal(
              existing_values,
              new_values,
              check.attributes = FALSE
            )
          )
        ){
          
          timepoint_match <- TRUE
          
          break
          
        }
        
      }
      
      if(!timepoint_match){
        
        all_timepoints_match <- FALSE
        
        break
        
      }
      
    }
    
    if(all_timepoints_match){
      
      duplicate_found <- TRUE
      
    }
    
  }

  #skipping dupes
  if(duplicate_found){
    
    cat(
      "Skipping duplicate file:",
      basename(input_file),
      "\n"
    )
    
    next
    
  }
  
  
  #adding each timepoint
  for(col in 2:ncol(data)){
    
    timepoint <- colnames(data)[col]
    
    values <- data[[col]]
    
    cat(
      "\nAdding:",
      timepoint,
      "\n"
    )
    
    
    time_cols <- find_timepoint_columns(
      colnames(combined),
      timepoint
    )
    
    
    target_col <- NA_integer_

    #check for timepoint +date
    if(length(time_cols) > 0){
      
      for(c in time_cols){
        
        existing_date <- as.character(
          combined[
            date_row,
            c
          ]
        )
        
        if(
          !is.na(existing_date) &&
          existing_date == experiment_date
        ){
          
          target_col <- c
          
          cat(
            "Found existing column for:",
            timepoint,
            "+",
            experiment_date,
            "-> column",
            c,
            "\n"
          )
          
          break
          
        }
        
      }
      
    }
    
    #looking for empty column
    if(is.na(target_col)){
      
      if(length(time_cols) > 0){
        
        for(c in time_cols){
          
          empty <- is_completely_empty(
            combined[[c]],
            check_rows
          )
          
          existing_date <- as.character(
            combined[
              date_row,
              c
            ]
          )
          
          no_date <- (
            is.na(existing_date) ||
              existing_date == ""
          )
          
          if(
            empty &&
            no_date
          ){
            
            target_col <- c
            
            cat(
              "Using existing empty column:",
              c,
              "for",
              timepoint,
              "\n"
            )
            
            break
            
          }
          
        }
        
      }
      
    }
    
    
    #creating new column if needed
    if(is.na(target_col)){
      
      tp_index <- match(
        timepoint,
        timepoints
      )
      
      insert_position <- ncol(combined)
      
      
      # Find the first later timepoint already present and insert before it.
      if(
        !is.na(tp_index) &&
        tp_index < length(timepoints)
      ){
        
        later_timepoints <- timepoints[
          (tp_index + 1):
            length(timepoints)
        ]
        
        for(next_tp in later_timepoints){
          
          next_cols <- find_timepoint_columns(
            colnames(combined),
            next_tp
          )
          
          if(length(next_cols) > 0){
            
            insert_position <- min(
              next_cols
            ) - 1
            
            break
            
          }
          
        }
        
      }
      
      
      new_column <- rep(
        NA_character_,
        nrow(combined)
      )
      
      new_df <- data.frame(
        new_column,
        stringsAsFactors = FALSE
      )
      
      colnames(new_df) <- timepoint
      
      #insert column
      if(
        insert_position < ncol(combined)
      ){
        
        left <- combined[
          ,
          seq_len(insert_position),
          drop = FALSE
        ]
        
        right <- combined[
          ,
          (insert_position + 1):
            ncol(combined),
          drop = FALSE
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
      
      # Actual physical column created
      target_col <-
        insert_position + 1
      
      
      cat(
        "Created NEW column:",
        target_col,
        "with timepoint:",
        timepoint,
        "\n"
      )
      
    }
    
    #experiment date
    combined[
      date_row,
      target_col
    ] <- experiment_date
    
    
    cat(
      "Assigned date",
      experiment_date,
      "to column",
      target_col,
      "\n"
    )

    #add values
    for(i in seq_len(nrow(data))){
      
      row <- match(
        data$Concentration[i],
        combined$Concentration
      )
      
      if(!is.na(row)){
        
        combined[
          row,
          target_col
        ] <- values[i]
        
      }
      
    }
  
}


ordered_indices <- integer(0)

for(tp in timepoints){
  
  # Find actual physical columns belonging to this
  # exact timepoint
  cols <- find_timepoint_columns(
    colnames(combined),
    tp
  )
  
  if(length(cols) > 0){
    
    # Get dates from those exact physical columns
    dates <- as.Date(
      as.character(
        combined[
          date_row,
          cols
        ]
      ),
      format = "%Y-%m-%d"
    )
    
    # Sort the ACTUAL COLUMN INDICES by date
    cols <- cols[
      order(
        dates,
        na.last = TRUE
      )
    ]
    
    # Store the physical column positions,
    # NOT their names.
    ordered_indices <- c(
      ordered_indices,
      cols
    )
    
  }
  
}


# KEEP CONCENTRATION FIRST
combined <- combined[
  ,
  c(
    1,
    ordered_indices
  ),
  drop = FALSE
]


write_csv(
  combined,
  output_file,
  na = ""
)

cat("\nFinished merging all files!\n")
