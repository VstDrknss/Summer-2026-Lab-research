library(readr)


# File paths
input_file <- "~/Downloads/SER1_cleaned.csv"
combined_file <- "~/Downloads/ser_combined_updated.csv"
output_file <- "~/Downloads/ser_combined_updated.csv"


experiment_date <- readline(
  prompt = "Enter experiment date (YYYY-MM-DD): "
)


# Read files
data <- read_csv(
  input_file,
  name_repair = "minimal",
  na = c("", "NA"),
  col_types = cols(.default = col_character())
)

combined <- read_csv(
  combined_file,
  name_repair = "minimal",
  na = c("", "NA"),
  col_types = cols(.default = col_character())
)


# Clean column names
colnames(data) <- trimws(colnames(data))
colnames(combined) <- trimws(colnames(combined))


# Find concentration column
data_conc <- grep(
  "concentration",
  colnames(data),
  ignore.case = TRUE
)

combined_conc <- grep(
  "concentration",
  colnames(combined),
  ignore.case = TRUE
)


colnames(data)[data_conc] <- "Concentration"
colnames(combined)[combined_conc] <- "Concentration"


# Timepoint conversion
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



# Rename input columns
colnames(data)[colnames(data) %in% names(timepoint_map)] <-
  timepoint_map[colnames(data)[colnames(data) %in% names(timepoint_map)]]


# Rename combined columns
for(i in seq_along(colnames(combined))){
  
  base <- sub("\\.\\d+$", "", colnames(combined)[i])
  
  if(base %in% names(timepoint_map)){
    
    colnames(combined)[i] <- sub(
      base,
      timepoint_map[base],
      colnames(combined)[i],
      fixed = TRUE
    )
    
  }
}


# Convert concentration
data$Concentration <- as.numeric(
  data$Concentration
)

combined$Concentration <- as.numeric(
  combined$Concentration
)


# Locate date row
date_row <- which(
  is.na(combined$Concentration)
)[1]

check_rows <- which(
  !is.na(combined$Concentration)
)


# Merge data
for(col in 2:ncol(data)){
  
  timepoint <- sub(
    "\\.\\d+$",
    "",
    colnames(data)[col]
  )
  
  values <- data[[col]]
  
  cat("Processing:", timepoint, "\n")
  
  
  # Find replicate columns
  time_cols <- grep(
    paste0("^", timepoint, "($|\\.)"),
    colnames(combined)
  )
  
  target_col <- NA
  
  
  # Find empty replicate
  for(c in time_cols){
    
    if(all(
      is.na(combined[check_rows,c]) |
      combined[check_rows,c] == ""
    )){
      
      target_col <- c
      break
      
    }
    
  }
  
  
  # Create new replicate if needed
  if(is.na(target_col)){
    
    insert_position <- max(time_cols)
    
    new_column <- rep(
      NA_character_,
      nrow(combined)
    )
    
    new_column[date_row] <- experiment_date

    new_name <- paste0(
      timepoint,
      ".new"
    )
    
    new_df <- data.frame(
      new_column
    )
    
    colnames(new_df) <- new_name
    
    left <- combined[,1:insert_position, drop=FALSE]
    
    if(insert_position < ncol(combined)){
      
      right <- combined[,
                        (insert_position+1):ncol(combined),
                        drop=FALSE]
      
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
    
    cat("Created:", new_name, "\n")
    
  }
  

  # Insert values using concentration
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


#clean headers
colnames(combined) <- sub(
  "\\.\\d+$",
  "",
  colnames(combined)
)

write_csv(
  combined,
  output_file,
  na=""
)

cat("Finished merging!\n")
