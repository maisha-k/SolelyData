## code to prepare `animalshelter` dataset goes here
url <- "https://data.kingcounty.gov/api/views/yaai-7frk/rows.csv?accessType=DOWNLOAD"
raw_data <- read.csv(url)
head(raw_data)

# Data Wrangling
## Filtered and keep only relevant columns
animalshelter <- raw_data |>
  dplyr::select(Animal_ID, Record_Type, Current_Location, Animal_Name, animal_type,Age,Animal_Gender,Animal_Breed, Animal_Color, Date, Date_Type, Obfuscated_Address, City, State, Zip, jurisdiction, obfuscated_latitude, obfuscated_longitude)
animalshelter$Date <- as.Date(animalshelter$Date, format = "%b %d, %Y")
usethis::use_data(animalshelter, overwrite = TRUE)

