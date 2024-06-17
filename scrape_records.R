library("rvest")
library("tidyverse")
library("readr")


scrape_records_page <- function(page_number) {
  # Construct the URL for the specific page
  url <- paste0("https://worldathletics.org/records/all-time-toplists/sprints/100-metres/all/men/senior",
                "?regionType=world&timing=all&windReading=regular&page=", page_number,
                "&bestResultsOnly=true&firstDay=1900-01-01&lastDay=2024-06-17&maxResultsByCountry=all&eventId=10229630&ageCategory=senior")
  
  # Read HTML content from the URL
  page <- read_html(url)
  
  # Extract table headers
  headers <- page %>%
    html_nodes(".records-table thead th") %>%
    html_text()
  
  # Extract table rows
  rows <- page %>%
    html_nodes(".records-table tbody tr")
  
  # Extract data from each row
  data <- lapply(rows, function(row) {
    row_data <- row %>%
      html_nodes("td") %>%
      html_text()
    return(row_data)
  })
  
  # Convert list to data frame
  data_df <- as.data.frame(do.call(rbind, data))
  
  # Set column names from headers
  colnames(data_df) <- headers
  
  return(data_df)
}

all_data <- list()

for (page in 1:109) {
  data <- scrape_records_page(page)
  all_data[[page]] <- data
  cat("Scraped page", page, "\n")
}

# Combine all data frames into a single data frame
combined_data <- do.call(rbind, all_data)

# Inspect the combined data frame
head(combined_data)

# Save the data to a CSV file
write.csv(combined_data, "100m_sprints_men_records.csv", row.names = FALSE)


# Remove leading and trailing whitespace from headers
colnames(combined_data) <- trimws(colnames(combined_data))

# Remove leading and trailing whitespace from values in the data frame
combined_data <- data.frame(lapply(combined_data, function(x) trimws(as.character(x))), stringsAsFactors = FALSE)

# Optionally, you can convert numeric columns back to numeric type
# combined_data <- type.convert(combined_data, as.is = TRUE)

# Inspect the cleaned data
head(combined_data)

# Save the cleaned data to a CSV file
write.csv(combined_data, "cleaned_100m_sprints_men_records.csv", row.names = FALSE)


# Create a new column "year" based on the last four characters of "Date"
combined_data$year <- substr(combined_data$Date, nchar(combined_data$Date) - 3, nchar(combined_data$Date))

# remove weird column that's blank (artifact from scraping)
combined_data <- combined_data %>% select(-c.............................................................)

# Inspect the updated data frame
head(combined_data)

# Save the updated data to a CSV file
write.csv(combined_data, "updated_100m_sprints_men_records.csv", row.names = FALSE)



