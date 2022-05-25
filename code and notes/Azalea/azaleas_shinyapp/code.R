library(dplyr)
library(ggplot2)
library(ggbeeswarm)
library(viridis)
library(readxl)
## inputting the datasets
#####################################
### NOTE: destfile location may need updating

url <- "https://happiness-report.s3.amazonaws.com/2021/DataForFigure2.1WHR2021C2.xls"
destfile <- "C:/Users/Public/tmp" ## replace with your location
curl::curl_download(url, destfile)
DataForFigure2_1WHR2021C2 <- read_excel(destfile)

url <- "https://happiness-report.s3.amazonaws.com/2021/MortalityDataWHR2021C2.xlsx"
destfile <- "C:/Users/Public/tmp2" ## replace with your location
curl::curl_download(url, destfile)
MortalityDataWHR2021C2 <- read_excel(destfile)


df <- merge(DataForFigure2_1WHR2021C2, MortalityDataWHR2021C2)
names(df) <- gsub(" ", "_", names(df))
names(df) <- gsub(",", "", names(df))
names(df) <- gsub("-", "", names(df))

########################################
regionCountryDeath <- df[, c("Regional_indicator", "Country_name","COVID19_deaths_per_100000_population_in_2020", "Index_of_exposure_to_COVID19__infections_in_other_countries_as_of_March_31")]

names(regionCountryDeath)[names(regionCountryDeath)=="Index_of_exposure_to_COVID19__infections_in_other_countries_as_of_March_31"] <- "Exposure"
names(regionCountryDeath)[names(regionCountryDeath)=="Country_name"] <- "Country"
names(regionCountryDeath)[names(regionCountryDeath)=="COVID19_deaths_per_100000_population_in_2020"] <- "Deaths_per_100k"
names(regionCountryDeath)[names(regionCountryDeath)=="Regional_indicator"] <- "Region"

view(regionCountryDeath)



