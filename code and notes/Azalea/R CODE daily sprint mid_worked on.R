library(dplyr)
library(ggplot2)
library(ggbeeswarm)
## inputting the datasets
DataForFigure2_1WHR2021C2 <- read_excel("~/DePaul/465/project/DataForFigure2.1WHR2021C2.xls")
MortalityDataWHR2021C2 <- read_excel("~/DePaul/465/project/MortalityDataWHR2021C2.xlsx")

## merge and rename headers for clarity
df <- merge(DataForFigure2_1WHR2021C2, MortalityDataWHR2021C2)
names(df) <- gsub(" ", "_", names(df))
names(df) <- gsub(",", "", names(df))
names(df) <- gsub("-", "", names(df))

########################################
################### 1 ##################
########################################
## creating new df for starplot using group_by & summarise
starplot <- df %>% 
  group_by(Regional_indicator) %>% 
  summarise(across(c(Logged_GDP_per_capita, 
                     Gini_coefficient_of_income, 
                     COVID19_deaths_per_100000_population_in_2020), 
                   mean))
## cleaning up starplot
starplot <- na.omit(starplot)
## making the names clearer
names(starplot)[names(starplot)=="Logged_GDP_per_capita"] <- "GDP"
names(starplot)[names(starplot)=="COVID19_deaths_per_100000_population_in_2020"] <- "COVID-19 Deaths"
names(starplot)[names(starplot)=="Gini_coefficient_of_income"] <- "Gini Coefficient"
## changing first column into rowname
starplot <- starplot %>% remove_rownames %>% column_to_rownames(var = "Regional_indicator")
## just some more tidying
rownames(starplot) <- gsub("and", "&", rownames(starplot))
## palette!
palette(colors <- c("#7fc7a5",
                    "#cb907b",
                    "firebrick4"))
## creating the starplot 
stars(starplot, 
      draw.segments=TRUE,
      key.loc = c(4.5, 0.2), # add the key in lower right (12,2 are coordinates)
      main = "\n\nGDP per Capita, Gini Income Coeffiecient,\nand COVID-19 Deaths per 100k people \nPer Global Region\n", 
      full = FALSE, # can use just half the circle
)

########################################
################### 2 ##################
########################################

regionCountryDeath <- df[, c("Regional_indicator", "Country_name","COVID19_deaths_per_100000_population_in_2020", "Index_of_exposure_to_COVID19__infections_in_other_countries_as_of_March_31")]
names(regionCountryDeath)[names(regionCountryDeath)=="Index_of_exposure_to_COVID19__infections_in_other_countries_as_of_March_31"] <- "Exposure"

regionCountryDeath %>% 
  ggplot(
    aes(
        x=factor(Regional_indicator),
        y=COVID19_deaths_per_100000_population_in_2020, 
        color = Exposure)
        ) +
  geom_beeswarm(size=10, alpha=.7, cex = .75)+
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))+
  labs(title = "COVID-19 Deaths per 100k Population and Initial Index of Exposure in Every Country, Seperated by Region",
       x= "Region",
       y = "COVID-19 Deaths per 100k Population")+
  theme(text = element_text(size = 20))+
  scale_color_viridis()





