library(purrr)
library(dplyr)
library(readr)
options(timeout = 600)

############## SCRAPES AND CLEANS HTML ##################

zip_url <- "https://yaleedu-my.sharepoint.com/:u:/g/personal/sonam_wangchuk_yale_edu/EV1nde_U8F1HjPFQS2pOX5wBKXvNWUI1kRLeNEdZpM4KbA?e=dxyX1w&download=1"
zip_file <- "Hamden_Sept2025.zip"

download.file(zip_url, destfile = zip_file, mode = "wb")
unzip(zip_file, exdir = "Hamden_Sept2025")

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

files <- list.files("Hamden_Sept2025/Hamden_Sept2025", pattern = "\\.html$", full.names = TRUE)

all_data <- do.call(rbind, lapply(files, safe_extract))

write.csv(all_data, "HTML_scraped_data.csv", row.names = FALSE)

######### RECEIVE AND CLEAN THE LARGE PROPERTY DATASET TO JUST HAVE HAMDEN ##########

url <- "https://yaleedu-my.sharepoint.com/:x:/g/personal/sonam_wangchuk_yale_edu/EZUM1auDjVRFmu_CLWL6Y48BcLzGnN_XBhNJydLW2WiwKw?e=gjv95Y&download=1"
data <- read.csv(url)
data_hamden <- data %>% filter(Town_Name == "Hamden")
write.csv(data_hamden, "Hamden_unmerged.csv", row.names = FALSE)

################# MERGE THE TWO DATASETS ####################

Joint_data <- left_join(all_data, data_hamden, by = "Location")

# Deletes all of the rows that didn't have a data_hamden equivalent
Joint_data <- Joint_data[!is.na(Joint_data$OBJECTID), ]

Final_data <- select(Joint_data, c("Location", "TotalAssessed", "TotalAppraised",
                                   "SalesPrice", "SaleDate", "Grade", "Unit_Type",
                                   "Land.Acres", "Living.Area", "Effective.Area",
                                   "Total.Rooms", 
                                   "Number.of.Bedroom", "Number.of.Baths",
                                   "ayb", "eyb"))
Final_data <- filter(Final_data, SalesPrice >= 100000)
write.csv(Final_data, "Final_Data_Valuation_in_2024.csv", row.names = FALSE)

################# MERGE THE 2009 VALUATIONS ############################

data2009 <- read.csv("Hamden2009.csv")
HTML2009 <- all_data[(grepl("2009", all_data$SaleDate)), ]
data2009$Location <- paste(data2009$prefix, data2009$street, sep = " ")
Joint2009 <- left_join(final2009, data2009, by = "Location")
Joint2009 <- Joint2009[!is.na(Joint2009$assessedvalue), ]
Joint2009 <- filter(Joint2009, SalesPrice >= 50000)
write.csv(Joint2009, "Final_Data_Valuation_in_2009.csv", row.names = FALSE)

################# RDS Centroids for the leaflet plot ###################

url <- "https://yaleedu-my.sharepoint.com/:u:/g/personal/sonam_wangchuk_yale_edu/ET9PD_gWrUxCrpOulBlxiREB5hOvzM0MWjOtQDuedUHz-w?e=YiYcrZ&download=1"
local_file <- "centroids.no.geometry.rds"
download.file(url, destfile = local_file, mode = "wb")
