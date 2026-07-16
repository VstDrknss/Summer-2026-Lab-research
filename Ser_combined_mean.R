library(readr)

input_file <- "~/Downloads/SER2_cleaned.csv"

combined_file <- "~/Downloads/Data 1.csv"

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


colnames(data) <- trimws(colnames(data))
colnames(combined) <- trimws(colnames(combined))
colnames(combined) <- make.unique(colnames(combined))


data$Concentration <- as.numeric(
  trimws(data$Concentration)
)


combined$Concentration <- as.numeric(
  trimws(combined$Concentration)
)
#
date_row <- which(is.na(combined$Concentration))[1]

timepoints <- colnames(data)[2:ncol(data)]


check_rows <- which(
  !is.na(combined$Concentration)
)


for(timepoint in timepoints){
  
  print(
    paste("Checking:", timepoint)
  )
  
  time_cols <- grep(
    paste0("^", timepoint),
    colnames(combined)
  )
  
  all_full <- TRUE
  
  for(c in time_cols){
    
    empty <- any(
      is.na(combined[[c]][check_rows]) |
        combined[[c]][check_rows] == ""
    )
    
    if(empty){
      
      all_full <- FALSE
      
    }
    
  }
  
  
  if(all_full){
    
    insert_position <- max(time_cols)
    
    ##
    new_column <- data.frame(
      empty = rep(
        NA,
        nrow(combined)
      ),
      stringsAsFactors = FALSE
    )
    
    colnames(new_column) <- paste0(
      timepoint,
      ".new"
    )
    
    # Put the experiment date in the header row
    new_column[date_row, 1] <- experiment_date
    #####
    
    if(insert_position == ncol(combined)){
      
      combined <- cbind(
        combined,
        new_column
      )
      
    } else {
      
      combined <- cbind(
        combined[,1:insert_position, drop = FALSE],
        new_column,
        combined[,(insert_position+1):ncol(combined), drop = FALSE]
      )
      
    }
    
    print(
      paste(
        "Added empty column:",
        paste0(timepoint, ".new")
      )
    )
    
  }
  
  
}


for(col in 2:ncol(data)){
  
  timepoint <- colnames(data)[col]
  
  values <- data[[col]]
  
  print(
    paste(
      "Adding values for:",
      timepoint
    )
  )
  
  
  time_cols <- grep(
    paste0("^", timepoint),
    colnames(combined)
  )
  
  
  target_col <- NA
  
  
  for(c in time_cols){
    
    empty <- any(
      is.na(combined[[c]][check_rows]) |
        combined[[c]][check_rows] == ""
    )
    
    if(empty){
      
      target_col <- c
      
      break
      
    }
    
  }
  
  
  if(is.na(target_col)){
    
    print(
      paste(
        "No empty column found for",
        timepoint
      )
    )
    
    next
    
  }
  
  
  for(i in seq_along(values)){
    
    conc <- data$Concentration[i]
    
    row_match <- which(
      combined$Concentration == conc
    )
    
    
    if(length(row_match) == 1){
      
      combined[row_match, target_col] <- values[i]
      
    } else {
      
      print(
        paste(
          "No concentration match:",
          conc
        )
      )
      
    }
    
  }
  
  
  print(
    paste(
      "Filled:",
      colnames(combined)[target_col]
    )
  )
  
}


write_csv(
  combined,
  output_file,
  na = ""
)


print("Finished merging!")
