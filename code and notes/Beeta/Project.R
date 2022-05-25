packages = c("sf","tmap","tidyverse","plotly","ggthemes","heatmaply","RColorBrewer")

Happy <- read_csv("~/Grad School/DSC465/Data2020.csv", 
                              col_types = cols(`Perceptions of corruption` = col_double()))

colnames(Happy) <- make.names(colnames(Happy), unique = TRUE)

names(Happy)

#-------------------------------------------------Remove columns I don't need---------------------------------------------------

Happy<-Happy[!is.na(Happy$Ladder.score),] %>% 
  select(Country.name, 
          Regional.indicator, 
          Ladder.score, 
          Logged.GDP.per.capita, 
          Social.support, 
          Healthy.life.expectancy, 
          Freedom.to.make.life.choices, 
          Generosity, 
          Perceptions.of.corruption)

#--------------------------------------Violin Plot of the Regions against Ladder Score------------------------------


Happy_box<-ggplot(Happy, aes(x=Regional.indicator, y = Ladder.score)) + 
  geom_boxplot() + 
  theme_minimal() + 
  geom_violin(aes(fill = Regional.indicator)) + 
  stat_summary(geom = "point", fun = "mean", color = "red") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  theme(axis.line = element_line(colour = "black"), 
        axis.title.x = element_text(colour = "darkcyan", size = 12), 
        axis.title.y = element_text(colour = "darkcyan", size = 12)) + 
  scale_fill_brewer(palette="Set3") + 
  labs(title = "Regional Ladder Score", x = "World Regions", y = "Ladder Score")

ggplotly(Happy_box)

#..................................Heatmap showing p-value from correlation mapped to point size.......................................#

Happy_corr<-as.data.frame(Happy)

Happy_corr <- cor(Happy[, 3:9])

cor.test.p <- function(x){
  FUN <- function(x, y) cor.test(x, y)[["p.value"]]
  z <- outer(
    colnames(x), 
    colnames(x), 
    Vectorize(function(i,j) FUN(x[,i], x[,j]))
  )
  dimnames(z) <- list(colnames(x), colnames(x))
  z
}  

p <- cor.test.p(Happy_corr)

heatmaply_cor(
  Happy_corr,
  node_type = "scatter",
  point_size_mat = -log10(p), 
  point_size_name = "-log10(p-value)",
  label_names = c("x", "y", "Correlation"), colors = colorRampPalette(brewer.pal(3, "Spectral"))(256), 
  xlab = "Happiness Indicators", 
  ylab = "Happiness Indicators", main = "Correlation Heatmap with p-value to Point Size")


#got this warning message, even though heat map plotted:
#Warning messages:
#1: `guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> = "none")` instead. 
#2: `guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> = "none")` instead. 
#3: `guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> = "none")` instead. 
#4: `guides(<scale> = FALSE)` is deprecated. Please use `guides(<scale> = "none")` instead.


#-------------------------------------------bottom 10 countries based on region-----------------------------------------


packages = c('sf', 'tmap', 'tidyverse','plotly','ggthemes','heatmaply','RColorBrewer')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

Unhappy <- read_csv("~/Grad School/DSC465/Data2020.csv", 
                        col_types = cols(`Perceptions of corruption` = col_double()))


colnames(Unhappy) <- make.names(colnames(Unhappy), unique = TRUE)


Unhappy <- Unhappy[!is.na(Unhappy$Ladder.score),] %>%
  select(Country.name,Regional.indicator,Ladder.score,Standard.error.of.ladder.score,upperwhisker,
         lowerwhisker,Freedom.to.make.life.choices,
         Generosity,Perceptions.of.corruption)


bottom20<- tail(Unhappy,10) 

p3 <- ggplot(bottom20, aes(x= reorder(Country.name,-Ladder.score),
                           y=Ladder.score, fill=Regional.indicator))+
  geom_point( color="#F5D300", size=4, shape=18) +
  geom_segment( aes(x=reorder(Country.name,-Ladder.score),
                    xend=reorder(Country.name,-Ladder.score), 
                    y=0, yend=Ladder.score), color="grey") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_line(colour = "#01535F"),
    panel.border = element_blank(), 
    axis.title = element_text(colour = "#FFACFC"),
    axis.ticks.x = element_blank(), 
    axis.text = element_text(colour = "#09FBD3"), 
    legend.text = element_text(size = 9, colour = "#09FBD3"), 
    legend.title = element_text(face = "bold", colour = "#FFACFC", size = 12), 
    plot.title = element_text(size = rel(2), colour = "#F5D300"),  
    panel.background = element_rect(fill = "#01535F", colour="#01535F"), 
    plot.background = element_rect(fill = "#01535F")
  ) +
  scale_fill_brewer(palette = "Set2")+
  xlab("Country") +
  ylab("Ladder score")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) + 
  ggtitle("Regions with the Bottom 10 Countries")

ggplotly(p3)

display.brewer.all()


#--------------------------------------------------map-------------------------------------



