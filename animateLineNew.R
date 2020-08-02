library(dplyr)
#library(tmap)
library(sf)
#library(geojsonio)
library(tidyr)
library(ggplot2)
library(gganimate)
library(lubridate)
library(gifski)
library(animation)
# library(arcgisbinding)
# arc.check_product()
# enrich_df <- arc.open(path = 'C:/projects/rstudioPrj/covid19/data/cobbzip.gdb/cobbzip')
# enrich_select_df <- arc.select(object = enrich_df, fields = c('OBJECTID', 'ZIP_CODE', 'PO_NAME', 'POPULATION', 'SQMI', 'ZIP'))
# spdf <- arc.data2sp(enrich_select_df)
# 
# #spdf <- geojson_read("C:/projects/rstudioPrj/covid19/data/final_map.geojson", what = "sp")
# #spdf <- geojson_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes.geojson",  what = "sp")
# nrow(spdf)
# spdfCobb <- spdf[grep("Cobb County", spdf$label), ]
# #spdfCobb <- spdf[spdf$label == "Cobb County,",]
# str(spdfCobb)
# head(spdfCobb)

covid.zipcase <- read.csv(file="C:/projects/rstudioPrj/covid19/data/CDPH_Data/cobbcasenew.csv")
covid.zipcase[1,]
covid.zipcase[is.na(covid.zipcase)] <- 0

head(covid.zipcase)

#covid.zipcase <- covid.zipcase[grep("Cobb County", covid.zipcase$Feature.Label),]
#nrow(covid.zipcase)

#cobb.mergecase <-  merge(spdfCobb,covid.zipcase,by="Zip.Code",duplicateGeoms = FALSE)

covid.zipcase <-gather(covid.zipcase, "yearDate","case",8:47)
covid.zipcase$yearDate <- gsub("X","",covid.zipcase$yearDate)

covid.zipcase$yearDate <- gsub("\\.",'/',covid.zipcase$yearDate)
covid.zipcase$yearDate
#covid.zipcase$yearDate <- strptime(covid.zipcase$yearDate, "%m/%d/%Y")
#covid.zipcase$yearDate <- as.POSIXct(covid.zipcase$yearDate)
#covid.zipcase$yearDate <- as.numeric(covid.zipcase$yearDate)
covid.zipcase <- covid.zipcase%>%
  mutate(yearDate=mdy(yearDate))
head(covid.zipcase)
nrow(covid.zipcase)
# cobb.mergecase <-  merge(spdf,covid.zipcase,by.x="ZIP",by.y="Zip.Code",duplicateGeoms = TRUE)
# nrow(cobb.mergecase)
# head(cobb.mergecase)

cobb.dataFrame <- covid.zipcase %>%
  group_by(NAME,yearDate)%>%
  arrange(yearDate)
#ungroup()
nrow(cobb.dataFrame)
cobb.dataFrame <- na.omit(cobb.dataFrame)
#arcgis_df <- arc.sp2data(cobb.mergecase)
#class(arcgis_df)
#head(arcgis_df)
anim <- cobb.dataFrame %>% 
  group_by(NAME,yearDate)%>%
  summarise(case=median(case))%>%
  ggplot(aes(x=yearDate,y=case,group=NAME,colour=NAME))+
  geom_line(size=1)+
  geom_text(aes(label=NAME),color="black", vjust = -1.5, size=2.8) +  
  geom_point(size=1.5)+
  # geom_text(aes(label=PO_NAME),color="dodgerblue",nudge_x=3,hjust=0)+
  labs(title = "City Covid-19 Cases in Cobb", x = "Date", y = "Covid-19 Cases",size=15)+
  transition_reveal(yearDate)+
  theme(axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15))
animate(anim, 200, fps = 20,  width = 800, height = 800, 
        renderer = gifski_renderer("gganimBar.gif"))
