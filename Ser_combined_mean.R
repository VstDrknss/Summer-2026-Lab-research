library(readr)

input_file <- "~/Downloads/SER1_cleaned.csv"
combined_file <- file.choose()
output_file <- "~/Downloads/ser_combined_updated.csv"

experiment_date <- readline(
  prompt = "Enter experiment date (YYYY-MM-DD): "
)

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

# Clean data

colnames(data) <- trimws(colnames(data))
colnames(combined) <- make.unique(trimws(colnames(combined)))

#converting str to numerical
data$Concentration <- as.numeric(trimws(data$Concentration))
combined$Concentration <- as.numeric(trimws(combined$Concentration))

#finding date row
date_row <- which(is.na(combined$Concentration))[1]
check_rows <- which(!is.na(combined$Concentration))


#find rows with conc
for(col in 2:ncol(data)){
  
  timepoint <- colnames(data)[col]
  values <- data[[col]]
  
  #####cat("Processing", timepoint, "\n")
  
  # Find all columns for this timepoint
  time_cols <- grep(
    paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", timepoint)),
    colnames(combined)
  )
  
  target_col <- NA
  
  # Look for an empty replicate
  for(c in time_cols){
    
    if(all(is.na(combined[check_rows, c]) |
           combined[check_rows, c] == "")){
      
      target_col <- c
      break
      
    }
    
  }
  
  # if none are empty, create a new column
  if(is.na(target_col)){
    
    insert_position <- max(time_cols)
    
    new_column <- rep(NA_character_, nrow(combined))
    new_column[date_row] <- experiment_date
    
    if(insert_position == ncol(combined)){
      
      combined[[paste0(timepoint, ".new")]] <- new_column
      target_col <- ncol(combined)
      
    } else {
      
      left <- combined[, 1:insert_position, drop = FALSE]
      right <- combined[, (insert_position + 1):ncol(combined), drop = FALSE]
      
      combined <- cbind(
        left,
        setNames(data.frame(new_column), paste0(timepoint, ".new")),
        right
      )
      
      target_col <- insert_position + 1
    }
    
    #####cat("Created new column:", colnames(combined)[target_col], "\n")
    
  }
  
  # Fill values by concentration
  for(i in seq_len(nrow(data))){
    
    row_match <- match(data$Concentration[i], combined$Concentration)
    
    if(!is.na(row_match))
      combined[row_match, target_col] <- values[i]
    
  }
  
  #####cat("Filled", colnames(combined)[target_col], "\n\n")
  
}


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


for(timepoint in timepoints){
  
  
  cols <- grep(
    paste0("^", timepoint),
    colnames(combined)
  )
  
  
  if(length(cols) > 0){
    
    colnames(combined)[cols] <- timepoint
    
  }
  
}

write_csv(combined, output_file, na = "")

cat("Finished merging!\n")
