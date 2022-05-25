library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)
library(viridis)
install.packages("fmsb")
library(fmsb)
library(GGally)
library(ggiraphExtra)
library(ggradar)
library(qdap)
library(tidyverse)
library(gganimate)
library(gifski)
library(gapminder)
library(RColorBrewer)
library(scales)
library(ggpubr)
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))
#################################
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
names(regionCountryDeath)[names(regionCountryDeath)=="Country_name"] <- "Country"
names(regionCountryDeath)[names(regionCountryDeath)=="COVID_Deaths_per_100k"] <- "Deaths_per_100k"
names(regionCountryDeath)[names(regionCountryDeath)=="Regional_indicator"] <- "Region"

regionCountryDeath %>% 
  ggplot(
    aes(
      x=factor(Regional_indicator),
      y=COVID19_deaths_per_100000_population_in_2020, 
      color = Exposure)
  ) +
  geom_beeswarm(size=10, alpha=.7, cex = .75)+
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))+
  labs(title = "COVID-19 Deaths per 100k Population and Initial Index of Exposure in Every Country,\n Separated by Region",
       x= "Region",
       y = "COVID-19 Deaths per 100k Population")+
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        legend.title = element_text(size=14),
        legend.text = element_text(size = 12),
        legend.key.width= unit(3, 'cm'))+
  scale_color_viridis(option = "F", begin = 0, end = .8, direction = -1)


######################################################################################
###########################    Excess DEATHS Study    ################################
######################################################################################
df %>% ggplot(aes(x= Excess_deaths_in_2020_per_100000_population_relative_to_20172019_average, 
                  y= COVID19_deaths_per_100000_population_in_2020, 
                  color = Regional_indicator))+
  geom_point(size=5, ) +
  geom_smooth(color = "black",method=lm)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8))+
  scale_color_brewer(palette = "Paired", name = "Region")+
    labs(title = 
           "Excess Deaths per Country Relative to Previous 3 Years vs COVID-19 Deaths in 2020",
         subtitle = "Deaths noted by number per 100,000 population",
         x = "Excess Deaths",
         y ="COVID-19 Deaths")

######################################################################################
###########    GDP per Capita vs COVID-19 Deaths per 100,000 People    ##############
######################################################################################
df %>% ggplot(aes(Logged_GDP_per_capita, COVID19_deaths_per_100000_population_in_2020, 
                  color = Regional_indicator))+
  geom_point(size=5) +
  scale_color_brewer(palette = "Paired", name = "Region")+
  scale_y_continuous(trans = "log", breaks = extended_breaks())+
  geom_smooth(color = "black")+
  labs(title = "GDP per Capita vs COVID-19 Deaths per 100,000 People",
       x = "GDP per Capita",
       y = "Log Base_10: Covid Deaths per 100k People",
       color = "Region")+
  theme(legend.position = "bottom")

######################################################################################
######### Perceptions of Corruption per Country in the Last 10 Years ##############
######################################################################################
tenyears <- filter(dfyears, year > 2010)

tenyears %>% ggplot(aes(x=factor(year), 
                       y=`Perceptions of corruption.x`))+
  geom_violin(size =1.5 ) +
  geom_point(
             size=4, 
             alpha=0.8, 
             position = position_jitterdodge(jitter.width = 2, dodge.width = 0),
             aes(
               color =  factor(`Regional indicator`))
             )+
  scale_color_brewer(palette = "Paired")+
  labs( title = "Perceptions of Corruption per Country in the Last 10 Years, Colored by Region",
        x = "Year\n Region",
        y = "Perceptions of Corruption")+
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 22),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12))

######################################################################################
######### Perceptions of Corruption vs Healthy Life Expectancy at Birth ##############
######################################################################################
plot1 <- df %>% 
  ggplot(aes(x=Perceptions_of_corruption,
             y=Healthy_life_expectancy,
             color = Regional_indicator))+
  geom_point(size = 5,show.legend = F)+
  geom_smooth(color = "grey30")+
  scale_color_brewer(palette = "Paired", name = "Region")+
  labs(title = "vs Healthy Life Expectancy at Birth",
       x = "Perceptions of Corruption",
       y = "Healthy Life Expectancy at Birth")

######################################################################################
######### Perceptions of Corruption vs Healthy Life Expectancy at Birth ##############
######################################################################################
plot2 <- df %>% 
  ggplot(aes(x=Perceptions_of_corruption,
             y=Generosity,
             color = Regional_indicator))+
  geom_point(size = 5, show.legend = F)+
  geom_smooth(color = "grey30")+
  scale_color_brewer(palette = "Paired", name = "Region")+
  labs(title = "vs Generosity",
       y = "Generosity",
       x = "Perceptions of Corruption")

######################################################################################
############# Perceptions of Corruption vs Exposure to COVID-19 ################
######################################################################################
plot3 <- df %>% 
  ggplot(aes(x=Perceptions_of_corruption,
             y=Index_of_exposure_to_COVID19__infections_in_other_countries_as_of_March_31,
             color = Regional_indicator))+
  geom_point(size = 5, show.legend = F)+
  geom_smooth(color = "grey30")+
  scale_y_log10()+
  scale_color_brewer(palette = "Paired", name = "Region")+
  labs(title = "vs Exposure to COVID-19",
       x = "Perceptions of Corruption",
       y = "LOG_Base_10 Exposure to COVID-19")

######################################################################################
########### Perceptions of Corruption vs Freedom to Make Life Choices ##############
######################################################################################
plot4 <- df %>% 
  ggplot(aes(x=Perceptions_of_corruption,
             y=Freedom_to_make_life_choices,
             color = Regional_indicator))+
  geom_point(size = 5, show.legend = F)+
  geom_smooth(color = "grey30")+
  scale_color_brewer(palette = "Paired", name = "Region")+
  labs(title = "vs Freedom to Make Life Choices",
       x = "Perceptions of Corruption",
       y = "Freedom to Make Life Choices")

######################################################################################
######### Perceptions of Corruption vs gini ##############
######################################################################################

plot5 <- df %>% 
  ggplot(aes(x=Perceptions_of_corruption,
             y=Gini_coefficient_of_income,
             color = Regional_indicator))+
  geom_point(size = 5, show.legend = F)+
  geom_smooth(color = "grey30")+
  scale_color_brewer(palette = "Paired", name = "Region")+
  labs(title = "vs Income Disparity",
       subtitle = "Greater Gini Coefficient shows a higher degree\nof inequality in the distribution of family income.",
       x = "Perceptions of Corruption",
       y = "Gini Coefficient of Income")+
  theme(plot.subtitle = element_text(size = 10))

ggarrange(plot1,plot2,plot3,plot4,plot5, 
                       ncol=2, nrow=3, common.legend = TRUE,legend="bottom")

annotate_figure(plot, top = text_grob("Perceptions of Corruption vs ...", 
                                      color = "black", face = "bold", size = 20))

######################################################################################
########### Perceptions of Corruption vs Female Head of Government ##############
######################################################################################
df %>%
  ggplot(aes(y=Perceptions_of_corruption,
             x=Female_head_of_government))+
  geom_violin(size=1.3)+
  geom_point(
    size=4, 
    alpha=0.9, show.legend = F,
    position = position_jitterdodge(jitter.width = 3, dodge.width = 0),
    aes(
      color =  factor(Regional_indicator))
  )+
  scale_color_brewer(palette = "Paired", name = "Region")+
  labs(title = "Perceptions of Corruption vs Having a Female Head of Government",
       y = "Perceptions of Corruption",
       x = "Female Head of Government")

######################################################################################
    ############ GDP per Capita vs Healthy Life Expectancy at Birth ##############
######################################################################################
df %>% 
  ggplot(aes(x=Logged_GDP_per_capita,
             y=Healthy_life_expectancy,
             color = Regional_indicator))+
  geom_point(size = 5)+
  scale_color_brewer(palette = "Paired", name = "Region")+
  labs(title = "GDP per Capita vs Healthy Life Expectancy at Birth,\nOrganized by Region",
       y = "Healthy Life Expectancy",
       x = "Logged GDP per Capita")+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))


years <- inner_join(DataPanelWHR2021C2, DataForFigure2_1WHR2021C2, by = "Country name")

dfyears <- years[, c("Country name",
                    "year",
                    "Regional indicator",
                    "Life Ladder",
                    "Log GDP per capita",
                    "Social support.x",
                    "Healthy life expectancy at birth",
                    "Freedom to make life choices.x",
                    "Generosity.x",
                    "Perceptions of corruption.x",
                    "Positive affect",
                    "Negative affect")]

######################################################################################
####### Median Age per Country vs COVID-19 Deaths per 100k Population, 2020 ##########
######################################################################################
df %>% ggplot( 
              aes(x=Median_age, 
                  y=COVID19_deaths_per_100000_population_in_2020, 
                  color=Regional_indicator))+
  geom_point(size=5)+
  geom_smooth(color = "grey30")+
  scale_y_continuous(trans = "log", breaks = extended_breaks())+
  labs( title = "Median Age per Country vs COVID-19 Deaths per 100k Population, 2020",
        x= "Median Age",
        y = "Log_Base10 COVID-19 Deaths")+
  scale_color_brewer(palette = "Paired", name = "Region")

################################
ggggg <- read.csv("~/DePaul/465/project/ggggg.csv")
ggggg <- ggggg %>% 
  remove_rownames %>%
  column_to_rownames(var = "Country")
ggggg <- na.omit(ggggg)
topHead <-  head(ggggg,10) 
topHead <- topHead %>% 
  remove_rownames %>%
  column_to_rownames(var = "Country")

##
palette(colors <- c(
                    "#440154ff",                  
                    "#FDE725FF",
                    "#55C667FF",
                    "#39568CFF",
                    "#95D860FF",
                    "#5b8313"))
stars(topHead, 
      draw.segments=TRUE,
      key.loc = c(7,0), # add the key in lower right (12,2 are coordinates)
      main = "\nHappiness Factors of the 10 Countries with\nthe Highest Percentage of Covid Fatalities",
      full = FALSE, # can use just half the circle
)


#############
LOWtail <-  tail(ggggg,10) 

p2 <- stars(LOWtail, 
      draw.segments=TRUE,
      key.loc = c(8, 2), # add the key in lower right (12,2 are coordinates)
      main = "\nHappiness Factors of the 10 Countries with \ntheLowest Percentage of Covid Fatalities",
      full = FALSE, # can use just half the circle
)

ggarrange(p1,p2, common.legend = TRUE)




