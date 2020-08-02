library(dplyr)
library(tmap)
library(sf)
#library(geojsonio)
library(tidyr)
library(ggplot2)
library(gganimate)
library(lubridate)
library(gifski)
library(animation)
library(arcgisbinding)
arc.check_product()
enrich_df <- arc.open(path = 'C:/projects/rstudioPrj/covid19/data/cobbzip.gdb/cobbzip')
enrich_select_df <- arc.select(object = enrich_df, fields = c('OBJECTID', 'ZIP_CODE', 'PO_NAME', 'POPULATION', 'SQMI', 'ZIP'))
spdf <- arc.data2sp(enrich_select_df)
#spdf <- geojson_read("C:/projects/rstudioPrj/covid19/data/final_map.geojson", what = "sp")
#spdf <- geojson_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes.geojson",  what = "sp")
nrow(spdf)
head(spdf)
#spdfCobb <- spdf[grep("Cobb County", spdf$label), ]
#spdfCobb <- spdf[spdf$label == "Cobb County,",]
#str(spdfCobb)
#head(spdfCobb)
#nrow(spdfCobb)
par(mar=c(0,0,0,0))
plot(spdf, col="blue")
covid.zipcase <- read.csv(file="C:/projects/rstudioPrj/covid19/data/CDPH_Data/concat.csv")
covid.zipcase[1,]
covid.zipcase[is.na(covid.zipcase)] <- 0

head(covid.zipcase)

covid.zipcase <- covid.zipcase[grep("Cobb County", covid.zipcase$Feature.Label),]
nrow(covid.zipcase)

#cobb.mergecase <-  merge(spdfCobb,covid.zipcase,by="Zip.Code",duplicateGeoms = FALSE)


# covid.zipcase <-gather(covid.zipcase, "yearDate","case",4:40)
# covid.zipcase$yearDate <- gsub("X","",covid.zipcase$yearDate)
# 
# covid.zipcase$yearDate <- gsub("\\.",'/',covid.zipcase$yearDate)
# covid.zipcase$yearDate

#covid.zipcase$yearDate <- strptime(covid.zipcase$yearDate, "%m/%d/%Y")
#covid.zipcase$yearDate <- as.POSIXct(covid.zipcase$yearDate)
#covid.zipcase$yearDate <- as.numeric(covid.zipcase$yearDate)
covid.zipcase <- covid.zipcase%>%
  mutate(yearDate=mdy(yearDate))
head(covid.zipcase)

cobb.mergecase <-  merge(spdf,covid.zipcase,by.x="ZIP",by.y="Zip.Code",duplicateGeoms = TRUE)
nrow(cobb.mergecase)
head(cobb.mergecase)

cobb.dataFrame <- cobb.mergecase %>%
  group_by(ZIP,yearDate)%>%
  arrange(yearDate)
  summarise(case=median(case))
#ungroup()
nrow(cobb.dataFrame)
cobb.dataFrame <- na.omit(cobb.dataFrame)

cobbzip_anim <- tm_shape(spdf)+ 
                tm_polygons() +
  tm_shape(cobb.mergecase)+
        tmap_options(max.categories = 38)+
        tm_fill(col='X6.23.2020', title = 'COBB',
        palette = "viridis")+
#       labels = ZIP_CODE) +
  tm_borders("grey") +
  tm_facets(along = "ZIP_CODE", free.coords = FALSE) +
  tm_layout(main.title.size = 1)

#tmap_save(cobbzip_anim, "cobbzip_anim.png",height=9,width=12,dpi=72)

tmap_animation(cobbzip_anim, filename = "cobbzip_anim.gif",
               height=9,width=12,dpi=72,delay = 200, restart.delay = 200)
#arcgis_df <- arc.sp2data(cobb.mergecase)
#class(arcgis_df)
# #head(arcgis_df)
# cobb.dataFrame %>% 
#   group_by(PO_NAME,yearDate)%>%
#   summarise(case=median(case))%>%
#   ggplot(aes(x=yearDate,y=case,colour=PO_NAME))+
#   geom_line(size=1)+
#   geom_point(size=1.5)+
#   labs(title = "City case in Cobb", x = "Date", y = "Case")+
#   transition_reveal(yearDate)