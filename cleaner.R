library(purrr)
library(dplyr)
library(readr)

clean_and_combine <- function(x) {
  cleaned <- sapply(x, function(item) gsub("[^0-9/]", "", item))
  cleaned <- cleaned[cleaned != ""]
  if (length(cleaned) > 0) {
    combined <- paste(cleaned, collapse = "")
    if (grepl("^[0-9]+$", combined)) {
      as.numeric(combined)
    } else {
      combined
    }
  } else {
    NA
  }
}

extract_property_data <- function(file_path) {
  x <- scan(file_path, what = "", sep = "\n")
  
  x <- gsub('&nbsp;', '', x, fixed = TRUE)
  x <- gsub('</span>', ',', x, fixed = TRUE)
  x <- gsub("<[^<>]*>", "", x)  
  
  y <- strsplit(x, ",")
  
  Location <- trimws(y[[202]][1])
  TotalAssessed <- clean_and_combine(y[[237]])
  TotalAppraised <- clean_and_combine(y[[244]])
  PID <- clean_and_combine(y[[251]])
  SalesPrice <- clean_and_combine(y[[380]])
  SaleDate <- clean_and_combine(y[[390]])
  grade_indices <- which(sapply(y, function(x) any(grepl("Grade", x))))
  TempGrade <- y[[grade_indices]]
  Grade <- gsub(".*Grade[^a-zA-Z0-9]*", "", TempGrade)
  
  data.frame(
    File = basename(file_path),
    Location,
    TotalAssessed,
    TotalAppraised,
    PID,
    SalesPrice,
    SaleDate,
    Grade,
    stringsAsFactors = FALSE
  )
}


safe_extract <- function(file_path) {
  tryCatch(
    {
      extract_property_data(file_path)
    },
    error = function(e) {
      message("Skipping file: ", file_path, " (", e$message, ")")
      return(NULL)
    }
  )
}

files <- list.files("Hamden_Sept2025", pattern = "\\.html$", full.names = TRUE)

all_data <- do.call(rbind, lapply(files, safe_extract))

write.csv(all_data, "Cleaned_data.csv", row.names = FALSE)

data <- read.csv("Cleaned_data.csv")
