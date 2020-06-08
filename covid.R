######################################################################################
#COVID UPDATE AND GRAPH 
##################################################################################
#install.packages("transformr")
library(transformr)
library(dplyr)
library(scales)

df <- read.csv(file = '~/Downloads/coronavirus.politologue.com-pays-2020-06-05.csv', sep= ';')

#month variable
df$Pays = as.character(df$Pays)
df$Pays[df$Pays == "Espagne"] <- "Spain"
df$Pays[df$Pays == "Italie"] <- "Italy"
df$Pays[df$Pays == "Allemagne"] <- "Germany"

df$Date = as.Date(df$Date, "%d/%m/%y")
head(df)

df2 = df %>%
  filter(Pays == 'France' |Pays == 'Spain'| Pays == 'Italy'| Pays == 'Germany'| Pays == 'Portugal') %>%
  group_by(Pays, Date) %>%
  summarise(TauxInfection = mean(TauxInfection) / 100, Infections = sum(Infections)) 

#rename col names and values
names(df2)[1]<-paste("Countries")
names(df2)[4]<-paste("Total_Covid_Cases")

#shadow_mark() 
#+ guides(size = FALSE)
ggplot(df2[df2$Date>='2020-03-01',], 
  aes(x = Date, y=TauxInfection, size = Total_Covid_Cases, colour = Countries)) +
  geom_point(show.legend = TRUE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  transition_time(Date) +
  labs(title = "Day: {frame_time}") +
  labs(x = "Month", y = "Infection Rate") + 
  guides(color = guide_legend(order = 1)) + 
  scale_y_continuous(labels = percent) 


