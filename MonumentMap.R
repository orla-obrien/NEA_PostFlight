library(tidyverse)
library(leaflet)
library(sf)
library(mapview)
library(webshot)
library(htmltools)
library(lubridate)
library(htmlwidgets)
library(leaflegend)
library(raster)
#webshot::install_phantomjs(force=TRUE)


setwd("C:\\Users\\oobrien\\Documents\\R_Work_Directory\\NEA_PostFlight")
target_dir <- paste(getwd(), "/output/", sep="")
filenames <- list.files(".//data//", pattern="*URI.csv")
date <- substr(filenames, 1, 8)
date <- max(date)

filename <- paste(".//data//", date, "_URI.CSV", sep="")
dat <- read.csv(filename)

# dat <- read.csv(".//data//20230514_URI.CSV")
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

dat.date$date <- as.Date(dat.date$date)
surveyday <- weekdays(unique(dat.date$date, na.rm = TRUE))
dat.date$date <- as.Date(dat.date$date, format = "%Y-%m-%d")
text.date <- unique(format(dat.date$date, "%B %d, %Y"))
text.date.short <- unique(format(dat.date$date, "%B %d"))


## replace Kenney SPECCODE with common names and NO scientific names
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
           speccode == "LOTU" | speccode == "MARA" | speccode == "UNTU" |
           speccode == "CDRA" | speccode == "PIWH" | speccode == "SEWH" |
           speccode == "GOBW" | speccode == "SPWH" | speccode == "UNFS" | 
       #   speccode == "BASH" | speccode == "TRBW" |
         #  speccode == "OCSU" | speccode == "WHSH" |
           speccode == "ASDO" | speccode == "STDO" | speccode == "UNBW" ) %>%
  mutate(new.speccode = kenney2commonNoLatin(speccode)) %>%
  arrange(desc(number))




mm_levs <-  
  c(    'Humpback Wahel',
        'Chilean Devil Ray',
    'Common Dolphin', 
    'Bottlenose Dolphin',
    'Sperm Whale',
    'Rissos Dolphin',
    'Striped Dolphin',
    'Fin Whale', 
    'Unidentified Dolphin/Porpoise'
    )
mm_pal <-  
  c("#08519c",
    'brown',
    "#FF850B",
    "#b2df8a",
    "#6a3d9a",
    "#ffff99",
    "#31bbe9",
    "#068100", 
    "white"
  )


sightColors <-colorFactor(palette = mm_pal, domain = mm_levs, ordered = TRUE) 



statesf <- st_read("C://Users//oobrien//Documents//R_Work_Directory//NEA_PostFlight//shapefiles//States_DOT_unkwn.shp")
bathy <- st_read("C://Users//oobrien//Documents//R_Work_Directory//NEA_PostFlight//shapefiles//BATHYMGM_POLY.shp")
MNM_boundary<- st_read("C://Users//oobrien//Documents//R_Work_Directory//NEA_PostFlight//shapefiles//MNM_SurveyArea.shp")
bathy <- st_transform(bathy, "+proj=longlat +ellps=WGS84 +datum=WGS84")

MNM_lat <- c(40.122, 
             40.527, 
             40.600, 
             40.207) %>%
  as.data.frame() %>%
  rename("lat" = ".")

MNM_long <- c(-68.212, 
               -68.268, 
               -67.628, 
               -67.578)
MNM_bound <- cbind(MNM_lat, MNM_long) %>%
  as.data.frame() 




surveymap <- 
  leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta = 0.25, zoomControl=FALSE)) %>%
   addPolygons(lng=MNM_bound$MNM_long, lat=MNM_bound$lat, color = "black", fill = "transparent", weight = 3, fillOpacity = 0) %>%

  setMaxBounds(
    lng1 = min_long,
    lat1 = min_lat - .15,
    lng2 = max_long + .5,
    lat2 = max_lat 
  ) %>%
 
  addPolylines(data = dat, 
               lng = dat$long,
               lat = dat$lat,
               fillColor = "black",
               weight = 1.5,
               opacity = 0.5, 
               stroke = TRUE,
               dashArray = 5,
               color = "black") %>%  
  addCircleMarkers(data = riwh_surv1, 
                   lng = riwh_surv1$long, 
                   lat = riwh_surv1$lat,
                   weight = 0.5,
                   fillColor = ~sightColors(new.speccode),
                   color = "black",
                   radius = ~ifelse(new.speccode == "Ocean Sunfish", 3, 4),
                   fillOpacity = 1,
                   stroke = TRUE,
                   # radius = NUMBER,
                   label = ~paste0(number, ' ', new.speccode),
                   group = 'Points') %>%
  
  addScaleBar(position = 'topleft') %>%
  addLegendFactor(pal = sightColors, values = riwh_surv1$new.speccode,
                  orientation = "vertical", shape="circle", width = 8, height = 8, 
                  labelStyle= 'font-size: 12px; font-family: Arial', position= 'topright') %>%
  addProviderTiles("Esri.OceanBasemap", options = providerTileOptions(
    variant= "Ocean/World_Ocean_Base")) %>%
  
  # add another layer with place names
  
  addProviderTiles("Esri.OceanBaseMap", 
                   options = providerTileOptions(
                     variant = "Ocean/World_Ocean_Reference"
                   )) %>%

  setView(lng = mid_long - .5, lat = mid_lat, zoom = 9) 

surveymap

saveWidget(surveymap, paste0(target_dir, "/", text.date, "webshot.html"), selfcontained = FALSE)
webshot(paste0(target_dir, "/", text.date, "webshot.html"), file = paste0(target_dir, "/", text.date, "leaflet_map.png"))

