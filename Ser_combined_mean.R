library(readr)

experiment_date <- readline(
  prompt = "Enter experiment date (YYYY-MM-DD): "
)


input_file <- "~/Downloads/SER2_cleaned.csv"

combined_file <- "~/Downloads/ser_combined_means.csv"

output_file <- "~/Downloads/ser_combined_updated.csv"



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



data$Concentration <- as.character(
  data$Concentration
)

combined$Concentration <- as.character(
  combined$Concentration
)



if(nrow(combined) < 2){
  
  date_row <- as.data.frame(
    matrix(
      NA,
      nrow = 1,
      ncol = ncol(combined)
    )
  )
  
  colnames(date_row) <- colnames(combined)
  
  combined <- rbind(
    combined[1,],
    date_row,
    combined[-1,]
  )
  
}



for(col in 2:ncol(data)){
  

  # Get timepoint and values
  
  timepoint <- colnames(data)[col]
  
  values <- data[[col]]
  
  
  # Find existing columns for this timepoint

  
  time_cols <- which(
    colnames(combined) == timepoint
  )
  

  # If timepoint does not exist, create it

  if(length(time_cols) == 0){
    
    
    combined[[ncol(combined)+1]] <- NA
    
    target_col <- ncol(combined)
    
    colnames(combined)[target_col] <- timepoint
    
    
  } else {
    
    
    
    dates <- combined[2,time_cols]
    
    
    filled_dates <- which(
      !is.na(dates) &
        dates != ""
    )
    
    
    
    if(length(filled_dates) > 0){
      
      
      # last used replicate
      last_used <- max(filled_dates)
      
      
      # next replicate
      target_col <- time_cols[last_used] + 1
      
      
      #creating new column if none empty
      
      if(
        target_col > ncol(combined) ||
        colnames(combined)[target_col] != timepoint
      ){
        
        
        combined[[ncol(combined)+1]] <- NA
        
        target_col <- ncol(combined)
        
        colnames(combined)[target_col] <- timepoint
        
      }
      
      
    } else {
      
      
      # No previous dates, use first replicate
      target_col <- time_cols[1]
      
    }
    
  }
  
  
  
  combined[2,target_col] <- experiment_date
  
  #inserting selon conc
  
  for(i in seq_along(values)){
    
    
    conc <- data$Concentration[i]
    
    
    row_match <- which(
      combined$Concentration == conc
    )
    
    
    if(length(row_match) == 1){
      
      
      combined[row_match,target_col] <- values[i]
      
      
    }
    
  }
  
  
  
  print(
    paste(
      "Added",
      timepoint,
      "date:",
      experiment_date,
      "column:",
      target_col
    )
  )
  
}



write_csv(
  combined,
  output_file,
  na = ""
)

