library(readxl)
library(minpack.lm)
library(ggplot2)


file_path <- file.choose()
data <- read_excel(file_path, sheet = 1)

drug_rows <- list(
  Drug1 = 2:9,
  Drug2 = 13:20,
  Drug3 = 24:31,
  Drug4 = 35:42
)

timepoints <- c(
  "Basal","2.5","5","7.5","10",
  "12.5","15","17.5","20","22.5","25"
)

logConc <- -12:-5

drug_fit <- list()
drug_plot <- list()

# NEW: corrected results
drug_fit_corrected <- list()
drug_plot_corrected <- list()

for(drug in names(drug_rows)){
  
  raw <- data[drug_rows[[drug]], 117:149]
  clean <- as.data.frame(lapply(raw, as.numeric))
  clean <- round(clean, 3)
  
  plot_all <- data.frame()
  curves_all <- data.frame()
  parameter_table <- data.frame()
  
  # NEW: corrected storage
  plot_all_corrected <- data.frame()
  curves_all_corrected <- data.frame()
  parameter_table_corrected <- data.frame()
  
  for(i in seq_along(timepoints)){
    
    tp <- timepoints[i]
    cols <- ((i-1)*3+1):((i-1)*3+3)

    ## Mean + SEM from raw data
    mean_y <- rowMeans(clean[, cols], na.rm = TRUE)
    sem_y  <- apply(clean[, cols], 1, sd, na.rm = TRUE) / sqrt(3)
    
    plot_all <- rbind(
      plot_all,
      data.frame(
        Concentration = logConc,
        Response = mean_y,
        SEM = sem_y,
        Timepoint = tp
      )
    )
    
    df <- data.frame(
      X = logConc,
      Y = mean_y
    )
    
    fit <- try(
      nlsLM(
        Y ~ Bottom + (Top-Bottom)/(1+10^(LogEC50-X)),
        data = df,
        start = list(
          Bottom = min(df$Y),
          Top = max(df$Y),
          LogEC50 = median(df$X)
        )
      ),
      silent = TRUE
    )
    
    if(!inherits(fit, "try-error")){
      
      xnew <- seq(min(df$X), max(df$X), length.out = 300)
      
      ynew <- predict(
        fit,
        newdata = data.frame(X = xnew)
      )
      
      curves_all <- rbind(
        curves_all,
        data.frame(
          Concentration = xnew,
          Response = ynew,
          Timepoint = tp
        )
      )
      
      parameter_table <- rbind(
        parameter_table,
        data.frame(
          Timepoint = tp,
          Bottom = coef(fit)[1],
          Top = coef(fit)[2],
          LogEC50 = coef(fit)[3]
        )
      )
    }
    
    #basekine correctiong
    baseline <- as.numeric(clean[1, cols])
    
    corrected <- sweep(
      clean[, cols],
      2,
      baseline,
      FUN = "-"
    )
    
    mean_y_corrected <- rowMeans(
      corrected,
      na.rm = TRUE
    )
    
    sem_y_corrected <- apply(
      corrected,
      1,
      sd,
      na.rm = TRUE
    ) / sqrt(3)
    
    plot_all_corrected <- rbind(
      plot_all_corrected,
      data.frame(
        Concentration = logConc,
        Response = mean_y_corrected,
        SEM = sem_y_corrected,
        Timepoint = tp
      )
    )
    
    df_corrected <- data.frame(
      X = logConc,
      Y = mean_y_corrected
    )
    
    fit_corrected <- try(
      nlsLM(
        Y ~ Bottom + (Top-Bottom)/(1+10^(LogEC50-X)),
        data = df_corrected,
        start = list(
          Bottom = min(df_corrected$Y),
          Top = max(df_corrected$Y),
          LogEC50 = median(df_corrected$X)
        )
      ),
      silent = TRUE
    )
    
    if(!inherits(fit_corrected, "try-error")){
      
      xnew <- seq(
        min(df_corrected$X),
        max(df_corrected$X),
        length.out = 300
      )
      
      ynew <- predict(
        fit_corrected,
        newdata = data.frame(X = xnew)
      )
      
      curves_all_corrected <- rbind(
        curves_all_corrected,
        data.frame(
          Concentration = xnew,
          Response = ynew,
          Timepoint = tp
        )
      )
      
      parameter_table_corrected <- rbind(
        parameter_table_corrected,
        data.frame(
          Timepoint = tp,
          Bottom = coef(fit_corrected)[1],
          Top = coef(fit_corrected)[2],
          LogEC50 = coef(fit_corrected)[3]
        )
      )
    }
  }
  
  plot_all$Timepoint <- factor(
    plot_all$Timepoint,
    levels = timepoints
  )
  
  curves_all$Timepoint <- factor(
    curves_all$Timepoint,
    levels = timepoints
  )
  
  plot_all_corrected$Timepoint <- factor(
    plot_all_corrected$Timepoint,
    levels = timepoints
  )
  
  curves_all_corrected$Timepoint <- factor(
    curves_all_corrected$Timepoint,
    levels = timepoints
  )
  
  
  p <- ggplot(plot_all, aes(Concentration, Response, colour = Timepoint)) +
    
    geom_errorbar(
      aes(ymin = Response - SEM, ymax = Response + SEM),
      width = 0.1,
      alpha = 0.5
    ) +
    
    geom_point(size = 2.2) +
    
    geom_line(
      data = curves_all,
      aes(Concentration, Response, colour = Timepoint),
      linewidth = 0.5
    ) +
    
    theme_classic(base_size = 15) +
    
    labs(
      title = drug,
      x = "log10(Agonist Concentration)",
      y = "Response"
    )
  
  
  p_corrected <- ggplot(
    plot_all_corrected,
    aes(Concentration, Response, colour = Timepoint)
  ) +
    
    geom_errorbar(
      aes(ymin = Response - SEM, ymax = Response + SEM),
      width = 0.1,
      alpha = 0.5
    ) +
    
    geom_point(size = 2.2) +
    
    geom_line(
      data = curves_all_corrected,
      aes(Concentration, Response, colour = Timepoint),
      linewidth = 0.5
    ) +
    
    theme_classic(base_size = 15) +
    
    labs(
      title = paste(drug, "- Baseline Corrected"),
      x = "log10(Agonist Concentration)",
      y = "Response"
    )
  
  drug_plot[[drug]] <- p
  drug_fit[[drug]] <- parameter_table
  
  # NEW
  drug_plot_corrected[[drug]] <- p_corrected
  drug_fit_corrected[[drug]] <- parameter_table_corrected
}

drug_plot$Drug1
drug_plot$Drug2
drug_plot$Drug3
drug_plot$Drug4

drug_plot_corrected$Drug1
drug_plot_corrected$Drug2
drug_plot_corrected$Drug3
drug_plot_corrected$Drug4

drug_fit$Drug1
drug_fit$Drug2
drug_fit$Drug3
drug_fit$Drug4

drug_fit_corrected$Drug1
drug_fit_corrected$Drug2
drug_fit_corrected$Drug3
drug_fit_corrected$Drug4