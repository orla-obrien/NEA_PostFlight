library(tidyverse)
library(leaflet)
library(sf)
library(mapview)
library(webshot)
library(htmltools)
library(lubridate)
library(htmlwidgets)
library(leaflegend)
library(leaflet.providers)
#webshot::install_phantomjs(force=TRUE)
#install.packages("leaflet.providers")

## General, Directed, or Condensed survey? ##

# Write "G", "D", or "C" 
SurveyType <- "G"

Survey_Area <- "Jordan Basin"
# NOAA Flight?

NOAA_Yes_No <- "Yes"

setwd("~/Documents/R_Work_Directory/NEA_PostFlight")
target_dir <- paste(getwd(), "/output/", sep="")
filenames <- list.files(".//data//", pattern="*URI.csv")
date <- substr(filenames, 1, 8)
date <- max(date)

filename <- paste(".//data//", date, "_URI.csv", sep="")
dat <- read.csv(filename)

dat <- dat %>%
  filter(dat$long < 0) #%>%
  #filter(dat$legtype != 0) 

min_long <- min(dat$long)
min_lat <- min(dat$lat)
max_long <- max(dat$long)
max_lat <- max(dat$lat)
mid_lat <- mean(c(min(dat$lat), max(dat$lat)))
mid_long <- mean(c(min(dat$long), max(dat$long)))

dat.date <- dplyr::select(dat, year, month, day)

dat.date <- dat.date %>%
  mutate(date = make_date(year, month, day))

dat.date$date <- as.Date(dat.date$date[1])
surveyday <- weekdays(unique(dat.date$date, na.rm = TRUE))
dat.date$date <- as.Date(dat.date$date[1], format = "%Y-%m-%d")
text.date <- unique(format(dat.date$date, "%B %d, %Y"))
text.date.short <- unique(format(dat.date$date, "%B %d"))



## use this url: https://apps-nefsc.fisheries.noaa.gov/cgi-bin/mammalmaps/xmlgenDMA.pl
url_dma <- 'https://services2.arcgis.com/C8EMgrsFcRFL6LrL/arcgis/rest/services/NEFSC_Dynamic_Management_Areas/FeatureServer/0/query/?where=1%3D1&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&relationParam=&returnGeodetic=false&outFields=*&returnGeometry=true&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&defaultSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token='

# read in dma data
dma <- st_read(url_dma, quiet = TRUE)

if(nrow(dma)>0){
  
  # check expiration times
  e_dates <- as.Date(as.POSIXct(as.character(dma$EXPDATE), format = "%d-%b-%Y %H:%M:%S", tz = 'UTC'))
  
  # if bad DMAs exist, remove them
  if(TRUE %in% c(e_dates<Sys.Date())){
    
    # indices of bad DMAs
    i_bad <- which(e_dates<Sys.Date())
    
    # number of expired DMAs
    n_bad <- length(i_bad)
    
    # names of bad DMAs
    names_bad <- as.character(dma$NAME[i_bad])
    
    # issue warning
    message("The following ", n_bad, " DMA(s) were detected:")
    message(paste(names_bad, collapse = '\n'))
    message("These will be removed from WhaleMap")
    
    # remove expired DMAs
    dma <- dma[-i_bad,]
    
  }
}

## copy any relevant DMAs below
dma1_lat <- c(0,
              0,
              0,
              0) %>%
  as.data.frame() %>%
  rename("lat" = ".")

dma1_long <- c(-0,
               -0,
               -0,
               -0)
dma1 <- cbind(dma1_lat, dma1_long) %>%
  as.data.frame() #

dma2_lat <- c(0,
              0,
              0,
              0) %>%
  as.data.frame() %>%
  rename("lat" = ".")

dma2_long <- c(-0,
               -0,
               -0,
               -0)
dma2 <- cbind(dma2_lat, dma2_long) %>%
  as.data.frame()

dma3_lat <- c(0,
              0,
              0,
              0) %>%
  as.data.frame() %>%
  rename("lat" = ".")

dma3_long <- c(-0,
               -0,
               -0,
               -0)
dma3 <- cbind(dma3_lat, dma3_long) %>%
  as.data.frame()



SMA_live <- if (dat$month[1] == 11 ) {
  st_read(".//shapefiles//SNE_SMA.shp")
}else if (dat$month[1] == 12 ) {
  st_read(".//shapefiles//SNE_SMA.shp")
}else if (dat$month[1] == 1 ) {
  st_read(".//shapefiles//SNE_SMA.shp")
}else if (dat$month[1] == 2 ) {
  st_read(".//shapefiles//SNE_SMA.shp")
}else if (dat$month[1] == 3 ) {
  st_read(".//shapefiles//SNE_SMA.shp")
}else if (dat$month[1] == 4 ) {
  st_read(".//shapefiles//SNE_SMA.shp")
}else if (dat$month[1] == 5 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 6 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 7 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 8 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 9 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 10 ) {
  st_read(".//shapefiles//sma_fake.shp")
}

CCB_SMA <- if (dat$month[1] == 11 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 12 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 1 ) {
  st_read(".//shapefiles//CCB_SMA.shp")
}else if (dat$month[1] == 2 ) {
  st_read(".//shapefiles//CCB_SMA.shp")
}else if (dat$month[1] == 3 ) {
  st_read(".//shapefiles//CCB_SMA.shp")
}else if (dat$month[1] == 4 ) {
  st_read(".//shapefiles//CCB_SMA.shp")
}else if ((dat$month[1] == 5 & dat$day[1] < 16)) {
  st_read(".//shapefiles//CCB_SMA.shp")
}else if ((dat$month[1] == 5 & dat$day[1] > 16)) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 6 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 7 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 8 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 9 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 10 ) {
  st_read(".//shapefiles//sma_fake.shp")
}

RP_SMA <- if (dat$month[1] == 11 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 12 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 1 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 2 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 3 ) {
  st_read(".//shapefiles//RacePointSMA.shp")
}else if (dat$month[1] == 4 ) {
  st_read(".//shapefiles//RacePointSMA.shp")
}else if (dat$month[1] == 5 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 6 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 7 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 8 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 9 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 10 ) {
  st_read(".//shapefiles//sma_fake.shp")
}

GSC_SMA <- if (dat$month[1] == 11 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 12 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 1 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 2 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 3 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 4 ) {
  st_read(".//shapefiles//GSC_SMA.shp")
}else if (dat$month[1] == 5 ) {
  st_read(".//shapefiles//GSC_SMA.shp")
}else if (dat$month[1] == 6 ) {
  st_read(".//shapefiles//GSC_SMA.shp")
}else if (dat$month[1] == 7 ) {
  st_read(".//shapefiles//GSC_SMA.shp")
}else if (dat$month[1] == 8 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 9 ) {
  st_read(".//shapefiles//sma_fake.shp")
}else if (dat$month[1] == 10 ) {
  st_read(".//shapefiles//sma_fake.shp")
}


#### replace Kenney SPECCODE with common names and NO scientific names ####
kenney2commonNoLatin <- function(SPECCODE){
  str_replace_all(SPECCODE, 
                  c("BLWH" = "Blue Whale", 
                    "BOWH" = "Bowhead Whale", 
                    "BRWH" = "Bryde's Whale", 
                    "FIWH" = "Fin Whale",
                    "GRWH" = "Gray Whale", 
                    "HUWH" = "Humpback Whale",
                    "RIWH" = "Right Whale", 
                    "SEWH" = "Sei Whale",
                    "SPWH" = "Sperm Whale", 
                    "SRWH" = "Southern Right Whale",
                    "UNBA" = "Unidentified *Balaenoptera*",
                    "UNBS" = "Unidentified Bryde's or Sei Whale", 
                    "UNFS" = "Unidentified Fin or Sei Whale", 
                    "UNLW" = "Unidentified Large Whale", 
                    "UNRO" = "Unidentified Rorqual", 
                    "UNWH" = "Unidentified Whale",
                    "BEWH" = "Beaked Whale", 
                    "BLBW" = "Blainville's Beaked Whale", 
                    "GEBW" = "Gervais' Beaked Whale", 
                    "GOBW" = "Cuvier's Beaked Whale", 
                    "KIWH" = "Killer Whale", 
                    "MIWH" = "Minke Whale", 
                    "NBWH" = "Northern Bottlenose Whale", 
                    "SOBW" = "Sowerby's Beaked Whale", 
                    "TRBW" = "True's Beaked Whale", 
                    "UNBW" = "Unidentified Beaked Whale", 
                    "UNMW" = "Unidentified Medium Whale", 
                    "ASDO" = "Atlantic Spotted Dolphin", 
                    "BELU" = "Beluga",
                    "BODO" = "Bottlenose Dolphin", 
                    "CLDO" = "Clymene Dolphin", 
                    "DSWH" = "Dwarf Sperm Whale", 
                    "FKWH" = "False Killer Whale", 
                    "FRDO" = "Fraser's Dolphin", 
                    "GRAM" = "Rissos Dolphin",
                    "HAPO" = "Harbor Porpoise", 
                    "LFPW" = "Long-finned Pilot Whale", 
                    "MHWH" = "Melon-Headed Whale", 
                    "OBDO" = "Offshore Bottlenose Dolphin", 
                    "PIWH" = "Pilot Whale", 
                    "PSDO" = "Pan-Tropical Spotted Dolphin", 
                    "PSWH" = "Pygmy Sperm Whale",
                    "PYKW" = "Pygmy Killer Whale", 
                    "RTDO" = "Rough-Toothed Dolphin", 
                    "SADO" = "Common Dolphin", 
                    "SFPW" = "Short-finned Pilot Whale", 
                    "SNDO" = "Spinner Dolphin", 
                    "SPDO" = "Spotted Dolphin", 
                    "STDO" = "Striped Dolphin", 
                    "UNBD" = "Unidentified Beaked Dolphin",
                    "UNBF" = "Unidentified Blackfish",
                    "UNCW" = "Unidentified Common or White-sided Dolphin",
                    "UNDO" = "Unidentified Dolphin/Porpoise",
                    "UNGD" = "Spotted or Bottlenose Dolphin",
                    "UNKO" = "Pygmy or Dwarf Sperm Whale", 
                    "UNLD" = "Unidentified *Lagenorhynchus*",
                    "UNSB" = "Unidentified Small Blackfish", 
                    "UNST" = "Unidentified *Stenella*",
                    "WBDO" = "White-Beaked Dolphin",
                    "WSDO" = "Atlantic White-Sided Dolphin", 
                    "BESE" = "Bearded Seal", 
                    "GRSE" = "Gray Seal", 
                    "HASE" = "Harbor Seal", 
                    "HGSE" = "Harp or Gray Seal",
                    "HOSE" = "Hooded Seal", 
                    "HPSE" = "Harp Seal", 
                    "MANA" = "Manatee", 
                    "PINN" = "Unidentified Pinniped", 
                    "POBE" = "Polar Bear", 
                    "RISE" = "Ringed Seal", 
                    "UNSE" = "Unidentified Seal", 
                    "WALR" = "Walrus", 
                    "GRTU" = "Green Turtle", 
                    "HATU" = "Hawksbill Turtle", 
                    "LETU" = "Leatherback Turtle", 
                    "LOTU" = "Loggerhead Turtle", 
                    "ORTU" = "Olive Ridley Sea Turtle", 
                    "RITU" = "Kemp's Ridley Turtle", 
                    "UNTU" = "Unidentified Turtle", 
                    "ANSH" = "Angel Shark", 
                    "BASH" = "Basking Shark", 
                    "BLSH" = "Blue Shark", 
                    "DUSH" = "Dusky Shark", 
                    "GHSH" = "Great Hammerhead Shark", 
                    "HHSH" = "Hammerhead Shark", 
                    "LMSH" = "Long-finned Mako Shark", 
                    "SDOG" = "Spiny Dogfish", 
                    "SMSH" = "Short-finned Mako Shark",
                    "THSH" = "Thresher Shark", 
                    "TISH" = "Tiger Shark", 
                    "UNSH" = "Unidentified/Other Shark", 
                    "WHSH" = "Whale Shark", 
                    "WTSH" = "White Shark", 
                    "BFTU" = "Bluefin Tuna", 
                    "BLFI" = "Bluefish", 
                    "CDRA" = "Chilean Devil Ray", 
                    "CNRA" = "Cow-Nosed Ray",  
                    "FLFI" = "Flying Fish", 
                    "MAHI" = "Mahi-mahi/Dolphin-fish", 
                    "MARA" = "Manta Ray", 
                    "MOBU" = "Mobulid ray, not identified to species", 
                    "OCSU" = "Ocean Sunfish", 
                    "OTBI" = "Other Billfish", 
                    "SCFI" = "Fish School", 
                    "SCRA" = "School of Rays", 
                    "SWFI" = "Swordfish", 
                    "TUNS" = "Unidentified Tuna", 
                    "UNFI" = "Unidentified/Other Fish", 
                    "UNRA" = "Unidentified/Other Ray", 
                    "WHMA" = "White Marlin", 
                    "YFTU" = "Yellowfin Tuna", 
                    "AMAL" = "American Alligator", 
                    "JELL" = "Jellyfish", 
                    "LMJE" = "Lion's-Mane Jellyfish",
                    "PMOW" = "Portuguese Man of War", 
                    "UNCE" = "Unidentified Cetacean", 
                    "UNID" = "Unidentified Animal", 
                    "UNMM" = "Unidentified Marine Mammal"))
}
riwh_surv1 <- dat %>%
  filter(speccode == "RIWH" | speccode == "HUWH" | speccode == "MIWH" |
           speccode == "UNLW" | speccode == "SADO" | speccode == "WSDO" |
           speccode == "UNDO" | speccode == "UNSE" | speccode == "FIWH" | 
           speccode == "BEWH" | speccode == "BLWH" | speccode == "BODO" | 
           speccode == "GRAM" | speccode == "GRSE" | speccode == "HAPO" |
           speccode == "HASE" | speccode == "KIWH" | speccode == "LETU" |
           speccode == "LOTU" | speccode == "MANA" | speccode == "UNTU" |
           speccode == "PINN" | speccode == "PIWH" | speccode == "SEWH" |
           speccode == "SFPW" | speccode == "SPWH" | speccode == "UNFS" |
           speccode == "GRWH"| speccode == "GOBW"| speccode == "TRBW" |
           speccode == "UNBW") %>%
  mutate(new.speccode = kenney2commonNoLatin(speccode)) %>%
  arrange(desc(number))




#### color scheme ####
mm_levs <-  
  c("Cuvier's Beaked Whale",
    "True's Beaked Whale",
    "Unidentified Beaked Whale",
    'Right Whale',
    'Humpback Whale',
    'Fin Whale',
    'Minke Whale',
    'Sei Whale',
    'Sperm Whale',
    'Blue Whale',
    'Unidentified Large Whale',
    'Unidentified Fin or Sei Whale',
    'Common Dolphin', 
    'Bottlenose Dolphin',
    'Atlantic White-Sided Dolphin',
    'Pilot Whale',
    'Harbor Porpoise',
    'Striped Dolphin',
    'Rissos Dolphin',
    'Unidentified Dolphin/Porpoise', 
    'Gray Seal', 
    'Harbor Seal',
    'Unidentified Seal',
    'Leatherback Turtle',
    'Loggerhead Turtle',
    'Kemps Ridley Turtle',
    'Unidentified Turtle')

mm_pal <-  
  c("#b15928",
    "#E3FA20",
    "#252525",
    "#FF0B0B",
    "#0156FF",
    "#068100",
    "#E3FA20",
    "#D089F4",
    "#ae017e",
    "#80b1d3",
    "#252525",
    "#252525",
    "#FF850B",
    "slategrey",
    "#ae017e",
    "#6a3d9a",
    "#b15928",
    "#fb9a99",
    "#33FFFF",
    "#f0f0f0",
    "#33FFFF",
    "#662506",
    "#C0BDC1",
    "darkseagreen3",
    "#ae017e",
    "#9ebcda",
    "brown" 
    
  )


sightColors <-colorFactor(palette = mm_pal, domain = mm_levs, ordered = TRUE) 


#### Map code ####
#statesf <- st_read(".//shapefiles//NAmerica_Union.shp")
leasesf <- st_read(".//shapefiles//BOEMWIndLeaseOutlines_12_6_2022.shp")
shiplanessf <- st_read(".//shapefiles//MORIS_ShipLanes.shp")


surveymap <- 
  leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta = 0.25, zoomControl=FALSE)) %>%

setMaxBounds(
  lng1 = min_long,
  lat1 = min_lat,
  lng2 = max_long,
  lat2 = max_lat
) %>%
  
  setView(lng = mid_long, lat = mid_lat, zoom = 9)  %>%
  
  addPolygons(data = dma, color = "yellow", weight = 0.5, fillColor = "yellow") %>%
  addPolygons(lng=dma1$dma1_long, lat=dma1$lat, color="yellow", weight=0.5, fillColor="yellow")%>%


  addPolygons(lng=dma2$dma2_long, lat=dma2$lat, color="yellow", weight=0.5, fillColor="yellow")%>%

  addPolygons(lng=dma3$dma3_long, lat=dma3$lat, color="yellow", weight=0.5, fillColor="yellow")%>%

  addPolygons(data=leasesf, color = "white", weight=2, fill=FALSE) %>%
  
#  addPolygons(data=shiplanessf, color = "black", weight = 2, fillColor="darkgrey") %>%

  addPolygons(data=SMA_live, color="red", weight=0.5, fillColor="red") %>%
  addPolygons(data=RP_SMA, color="red", weight=0.5, fillColor="red") %>%
  
  addPolygons(data=GSC_SMA, color="red", weight=0.5, fillColor="red") %>%
  
    addPolylines(data = dat, 
                 lng = dat$long,
                 lat = dat$lat,
                 fillColor = "black",
                 weight = 1.75,
                 opacity = 0.5, 
                 stroke = TRUE,
                 color = "black") %>% 
  addPolygons(data=CCB_SMA, color="red", weight=0.5, fillColor="red") %>%
  # addPolylines(data = dat, 
  #              lng = dat$long,
  #              lat = dat$lat,
  #              fillColor = "black",
  #              weight = 1.25,
  #              opacity = 0.5, 
  #              stroke = TRUE,
  #              color = "black") %>%  
    addCircleMarkers(data = riwh_surv1, 
                     lng = riwh_surv1$long, 
                     lat = riwh_surv1$lat,
                     weight = 0.5,
                     fillColor = ~sightColors(new.speccode),
                     color = "black",
                     radius = 9,
                     fillOpacity = 1,
                     stroke = TRUE,
                     # radius = NUMBER,
                     label = ~paste0(number, ' ', new.speccode),
                     group = 'Points') %>%

  
  addLegendFactor(pal = sightColors, values = riwh_surv1$new.speccode,
                  orientation = "vertical", shape="circle", width = 20, height = 20, 
                  labelStyle= 'font-size: 18px; font-family: Arial', position= 'topright') %>%
  # add ocean basemap
  addProviderTiles("Esri.OceanBasemap", options = providerTileOptions(
    variant= "Ocean/World_Ocean_Base")) %>%
  
  # add another layer with place names
  
  addProviderTiles("Esri.OceanBaseMap", 
                   options = providerTileOptions(
                     variant = "Ocean/World_Ocean_Reference"
                   )) %>%
  addScaleBar(position = 'bottomright') 



saveWidget(surveymap, paste0(target_dir, "/", text.date, "webshot.html"), selfcontained = FALSE)
webshot(paste0(target_dir, "/", text.date, "webshot.html"), file = paste0(target_dir, "/", text.date, "leaflet_map.png"), 
        vwidth = 1400, vheight = 1200)

