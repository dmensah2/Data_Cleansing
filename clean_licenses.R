#Author: Deidre
#Clean BCC licsense information for geocoding
#Date: 7/2/2020

#read in our tidying libraries
library(tidyr)
library(tidyverse)
library(dplyr)
library(stringr)
library (magrittr)
library(ggmap)
library(stringi)

#register Google API Key
register_google("YOUR API KEY")

#read in csv of bcc liceses as data frame 
licenses <- read.csv("C:/Weedmaps/Regional_Health/2020_08_07/tables/excel/BCC_Storefront.csv")

#write the number of record we have before dropping NA values
paste("You have", as.character(nrow(licenses)) , "total records.")

#loop through each records in the original address column and assign NA values
for (i in 1:length(licenses$Premise.Address)){
  #if a record is an emptry string, we assign NA to it
  if (licenses$Premise.Address[i] == "") {
    licenses$Premise.Address[i] <- NA
  }
}

#drop NA vaues
licenses_clean<- licenses%>%  
  na.omit() %>% 
  separate("Premise.Address", into = c("Address","County"), sep = "County:")

#drop values that have licenses number and no address information
#licenses_clean <- licenses[licenses$License.Number != "License Number"] 

#return total count of records to console after dropping NA values
#paste("You now have", as.character(nrow(licenses_clean)),"records after removing NA values.")
 
#add column of 9 digit zipcodes and clean
licenses_clean %<>% 
  mutate(str_sub(licenses_clean$Address,-11,-1) 
         %>% as.numeric() %>% as.character()) 

#rename zipcode column
names(licenses_clean)[13] <- "Zipcode_long"

#clean up zipcodes column and create new column for addresses stripped of zipcodes
for (i in 1:nrow(licenses_clean)) {
  if (!is.na(licenses_clean$Zipcode_long[i])) {
    stri_sub(licenses_clean$Zipcode_long[i], 6, 5) <- "-"
    licenses_clean$Address1[i] <- substr(licenses_clean$Address[i],1,nchar(licenses_clean$Address[i])-10)
  } else if (is.na(licenses_clean$Zipcode_long[i])) {
    licenses_clean$Zipcode_long[i] <- str_sub(licenses_clean$Address[i],-6,-1)
    licenses_clean$Address1[i] <- substr(licenses_clean$Address[i],1,nchar(licenses_clean$Address[i])-6)
  }
}

#create final address column by merging cleaned address column and zipcode column
licenses_clean$Full_Address <- paste0(licenses_clean$Address1, licenses_clean$Zipcode_long)

# Loop through the addresses to get the latitude/longitude and google address of 
# each address and add it to the
# data frame in new columns lat and lon
for(i in 1:nrow(licenses_clean))
{
  result <- geocode(licenses_clean$Full_Address[i], output = "latlona", source = "google")
  licenses_clean$lon[i] <- as.numeric(result[1])
  licenses_clean$lat[i] <- as.numeric(result[2])
  licenses_clean$geoAddress[i] <- as.character(result[3])
}

#download dataframe to working directory
write.csv(licenses_clean, "C:/Weedmaps/Regional_Health/2020_08_07/tables/excel/BCC_Storefront_cleaned.csv")
