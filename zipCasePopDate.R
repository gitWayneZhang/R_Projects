library(dplyr)
#library(tmap)
library(sf)
#library(geojsonio)
library(tidyr)
library(ggplot2)
library(gganimate)
library(lubridate)
library(gifski)
#library(animation)
covid.zipcase <- read.csv(file="C:/projects/rstudioPrj/covid19/data/CDPH_Data/cobbcasenew.csv")
covid.zipcase[1,]
covid.zipcase[is.na(covid.zipcase)] <- 0

head(covid.zipcase)

#covid.zipcase <- covid.zipcase[grep("Cobb County", covid.zipcase$Feature.Label),]
#nrow(covid.zipcase)

#cobb.mergecase <-  merge(spdfCobb,covid.zipcase,by="Zip.Code",duplicateGeoms = FALSE)

covid.zipcase <-gather(covid.zipcase, "yearDate","case",8:46)
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
#arcgis_df <- arc.sp2data(cobb.mergecase)
#class(arcgis_df)
#head(arcgis_df)

#arc.write('C:/projects/rstudioPrj/covid19/data/cobbzip.gdb/cobbcases', arcgis_df, shape_info = arc.shapeinfo(enrich_df))
cobb.dataFrame <- covid.zipcase%>%
  group_by(ZIPCODE,yearDate)%>%
  arrange(yearDate)
nrow(cobb.dataFrame)

ggplot(cobb.dataFrame, aes(POP2015, case, size = POP2015, colour = as.factor(ZIPCODE))) +
  geom_point(alpha = 0.7, show.legend = TRUE) +
  geom_text(aes(label=ZIPCODE),color="dodgerblue",nudge_x=1,hjust=1.2,size=4)+
  scale_color_viridis_d() +
  labs(size="Population",col="ZIP Code")+
  #  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_continuous(limits = c(10000, 50000)) +  
  #  labs(x = "ZIP code", y = "Case")+
  transition_manual(yearDate) +
  labs(title = "Date: {current_frame}",x = "Population", y = "Case")
#  coord_flip()
