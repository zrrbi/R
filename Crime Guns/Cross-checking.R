library(readxl);library(tidyverse)

folder_path <- ""  

excel_files <- list.files(path = folder_path, pattern = ".xlsx", full.names = TRUE)
timestamp <- zoo::as.yearmon(substr(excel_files,11,14),"%m%y")

timestampN <- sort(timestamp)
excel_files <- excel_files[match(timestampN,timestamp)]

added_cases <- list()
disappeared_cases <- list()

common_cases <- list()

# Iterate through the Excel files to compare data
for (i in 2:length(excel_files)) {
  current_month_data <- read_excel(excel_files[i]) %>% mutate(timestamp = timestampN[i])
  previous_month_data <- read_excel(excel_files[i - 1]) %>% mutate(timestamp = timestampN[i-1])
  
  # Identify added cases (appear in the current month but not in the previous month)
  added <- current_month_data[!current_month_data$LIC_SEQN %in% previous_month_data$LIC_SEQN, ]
  
  # Identify disappeared cases (appear in the previous month but not in the current month)
  disappeared <- previous_month_data[!previous_month_data$LIC_SEQN %in% current_month_data$LIC_SEQN, ]
  
  disappeared_cases[[i-1]] <- disappeared 
  
  # Find common cases between added and disappeared
  common <- added[added$LIC_SEQN %in% disappeared$LIC_SEQN, ]
  if (nrow(common) > 0) {
    common_cases[[i]] <- common
  }
}

disappeared_cases_df <- dplyr::bind_rows(disappeared_cases)

if (length(common_cases) > 0) {
  cat("Common Cases between Added and Disappeared (Dealers that opened and closed during this period):\n")
  for (i in 2:length(excel_files)) {
    if (!is.null(common_cases[[i]])) {
      cat(paste("Between", basename(excel_files[i - 1]), "and", basename(excel_files[i]), ":\n"))
      print(common_cases[[i]])
    }
  }
} else {
  cat("No common cases between Added and Disappeared.\n")
}

search_input = "05671"
close_date <- disappeared_cases_df$timestamp[ which(disappeared_cases_df$LIC_SEQN == search_input) ] 
close_date 
