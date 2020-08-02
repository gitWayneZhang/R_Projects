library(dplyr)
#library(tmap)
#library(sf)
#library(geojsonio)
library(tidyr)
library(ggplot2)
library(gganimate)
library(lubridate)
library(gifski)
library(animation)

covid.zipcase <- read.csv(file="C:/projects/rstudioPrj/covid19/data/CDPH_Data/cobbcasenew.csv")
covid.zipcase[1,]
covid.zipcase[is.na(covid.zipcase)] <- 0

covid.zipcase <-gather(covid.zipcase, "yearDate","case",8:46)
covid.zipcase$yearDate <- gsub("X","",covid.zipcase$yearDate)

covid.zipcase$yearDate <- gsub("\\.",'/',covid.zipcase$yearDate)
covid.zipcase$yearDate

covid.zipcase <- covid.zipcase%>%
  mutate(yearDate=mdy(yearDate))
head(covid.zipcase)

covid.zipcase <- covid.zipcase %>% 
  group_by(yearDate)%>%
  mutate(rank=rank(-case),
         valuelbl=paste0(" ", case)) %>%
  group_by(ZIPCODE) %>% 
  filter(rank<=10)%>%
  ungroup()

g <- ggplot(covid.zipcase,aes(rank,group=ZIPCODE,
                              fill=as.factor(ZIPCODE),color=as.factor(ZIPCODE)))+
  geom_tile(aes(y=case/2,height=case,width=0.9),alpha=0.8,color=NA)+
  geom_text(aes(y=0,label=paste(ZIPCODE," ")), vjust =0.2,hjust=1,size=9)+
  geom_text(aes(y=case,label = valuelbl, hjust=0),size=10) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=15, hjust=0.5, face="italic", color="black"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))
anim <-  g + transition_states(yearDate, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Date : {closest_state}',  
       subtitle  =  "Top 10 ZIP Code Cases",
       caption  = "Covid-19 Cases in Cobb by Zip code")
animate(anim, 200, fps = 8,  width = 1000, height = 800, 
        renderer = gifski_renderer("gganimBar.gif"))
