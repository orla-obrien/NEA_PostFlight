
library(tidyverse)
library(viridis)
library(sf)
library(sp)
library(raster)
library(rgdal)
library(glue)
library(janitor)
library(gt)
library(RColorBrewer)
library(ggforce)
library(readxl)

##WEA lease shapefile
wea.shp <- st_read("C://Users//oobrien//Documents//GIS//BOEM_Renewable_Energy_Areas_Shapefile_4_13_2020 (1)//BOEM_Lease_Areas_4_13_2020.shp") %>%
  filter(State == "Rhode Island / Massachusetts" |
           State == "Massachusetts")

wea.shp1 <- st_read("C://Users//oobrien//Documents//GIS//BOEM_Renewable_Energy_Areas_Shapefile_4_13_2020 (1)//BOEM_Lease_Areas_4_13_2020.shp") 

## Mass coast shapefile
mass.shp <- st_read("C://Users//oobrien//Documents//GIS//States_DOT_unkwn.shp")

## Bathymetry
dat <- raster("C://Users//oobrien//Documents//GIS//gom15dd0")
gom_bathy <- st_read("C://Users//oobrien//Documents//GIS//bathymgm//BATHYMGM_POLY.shp")
#dat <- st_read("C://Users//oobrien//Documents//GIS//bathymgm//BATHYMGM_POLY.shp")
## project the data to that of the bathy data
# mass.project <- spTransform(mass.shp,
                            # crs(dat))
mass.project <- mass.shp
rm(mass.shp)
wea.project <- st_as_sf(wea.shp, crs(dat))

## WEA with out individual lease boundaries
wea.sf <- wea.project %>% 
  st_union() %>% 
  st_as_sf()

dat <- as.data.frame(dat, xy= TRUE) %>%
  filter(gom15dd0 <200) %>%
  filter(y < 43 & y > 40.2) %>%
  filter(x < -69.0 & x > -71.5)

## set colors for maps
sightColors <-
  setNames(c('#F8766D', 
             '#00BFC4', 
             '#7CAE00', 
             'red', 
             'magenta', 
             '#C77CFF', 
             'orange', 
             'yellow', 
             'purple', 
             'green4', 
             'grey78',
             'blue',
             'mediumpurple1'),
           
           c('North Atlantic Right Whale', 
             'Unidentified Dolphin/Porpoise', 
             'Gray Seal', 
             'Common Dolphin', 
             'Humpback Whale', 
             'Leatherback Turtle', 
             'Unidentified Seal', 
             'Fin Whale', 
             'Minke Whale', 
             'Harbor Porpoise', 
             'Unidentified Large Whale', 
             'Bottlenose Dolphin', 
             'Sei Whale'))


## base map for study area includes bathy, wea lease sites, massachusetts
base_wea_map <- function(){
  
  ggplot() +
    
    
    
    
    
    ## add bathy
    geom_raster(data = dat,
                aes(x = x, y = y, fill = gom15dd0),
                alpha = 0.9, show.legend = FALSE) +
    
    
    ## add coastline
    # geom_polygon(data = mass.project,
    #              aes(x = long, y = lat,
    #                  group = group), fill = "bisque2") +
    # 
    ## WEA wiht dissolved lease areas
    geom_sf(data = wea.sf, 
            
            alpha = 0.05, 
            color = "white", 
            fill = "transparent") +
    
    ## add in WEA lease sites
    # geom_polygon(data = wea.sf,
    #              aes(x = long, y = lat,
    #                  group = group), alpha = 0.05,
    #              color = "white", fill = "transparent") +
    # 
    
    coord_sf(xlim=c(-71.4,-69.3),
             ylim=c(40.3, 41.6)) +
    
    
    labs(x = "Longitude",
         y = "Latitude") +
    
    
    theme(panel.border = element_rect(colour = "black",
                                      fill=NA, size = 0.2),
          text = element_text(size=12), ## axis text size
          legend.text = element_text(size = 7), ## legend text size
          legend.title = element_blank(), ## remove legend title
          legend.position = "bottom", ## put the legend at the bottom
          plot.margin = unit(c(.1, .1, .1, .1), "cm"), 
          axis.text.x = element_text(angle = 75, 
                                     hjust = 1, 
                                     size =  7),
          axis.text.y = element_text(size =  7)) +
    
    guides(colour = guide_legend(override.aes = list(size = 5))) +
    scale_size_continuous(range = c(7, 50),
                          breaks = c(1, 5, 100, 500), 
                          limits = c(1, 1000), 
                          labels = c("1", "5", "100", "500+"), 
                          guide = guide_legend(direction = "horizontal")) 
  
}
base_wea_noBathy <- function(){
  
  ggplot() +
    
    
    coord_sf(xlim=c(-71.4,-69.3),
             ylim=c(40.3, 41.6)) +
    
    
    ## add coastline
    geom_polygon(data = mass.project,
                 aes(x = long, y = lat,
                     group = group), fill = "bisque2") +
    
    
    ## add in WEA lease sites
    geom_polygon(data = wea.project,
                 aes(x = long, y = lat,
                     group = group), alpha = 0.05,
                 color = "black", fill = "transparent") +
    
    
    
    labs(x = "Longitude",
         y = "Latitude") +
    
    
    theme(panel.border = element_rect(colour = "black",
                                      fill=NA, size = 0.2),
          text = element_text(size=12), ## axis text size
          legend.text = element_text(size = 7), ## legend text size
          legend.title = element_blank(), ## remove legend title
          legend.position = "bottom", ## put the legend at the bottom
          plot.margin = unit(c(.1, .1, .1, .1), "cm"), 
          axis.text.x = element_text(angle = 75, 
                                     hjust = 1, 
                                     size =  7)) +
    
    guides(colour = guide_legend(override.aes = list(size=5))) 
  
}
two_data_wea_map <- function(data1, data2, mapping1, mapping2){
  if(missing(data2)) {
    
    ggplot() +
      
      
      coord_quickmap(xlim=c(-71.4,-69.75),
                     ylim=c(40.55, 41.6)) +
      
      
      ## add bathy
      geom_raster(data = dat,
                  aes(x = x, y = y, fill = gom15dd0),
                  alpha = 0.9, show.legend = FALSE) +
      
      
      ## add coastline
      geom_polygon(data = mass.project,
                   aes(x = long, y = lat,
                       group = group), fill = "bisque2") +
      
      
      ## add in WEA lease sites
      geom_polygon(data = wea.project,
                   aes(x = long, y = lat,
                       group = group), alpha = 0.05,
                   color = "white", fill = "transparent") +
      
      
      ## add data1 
      geom_point(data = data1,
                 mapping = mapping1,
                 size = 0.05) +
      
      labs(x = "Longitude",
           y = "Latitude") +
      
      
      theme(panel.border = element_rect(colour = "black",
                                        fill=NA, size = 0.2),
            text = element_text(size=12), ## axis text size
            legend.text = element_text(size = 9), ## legend text size
            legend.title = element_blank(), ## remove legend title
            legend.position = "bottom", ## put the legend at the bottom
            plot.margin = unit(c(.1, .1, .1, .1), "cm")) +
      
      guides(colour = guide_legend(override.aes = list(size=5))) 
  } else {
    ggplot() +
      
      
      coord_quickmap(xlim=c(-71.4,-69.75),
                     ylim=c(40.55, 41.6)) +
      
      
      ## add bathy
      geom_raster(data = dat,
                  aes(x = x, y = y, fill = gom15dd0),
                  alpha = 0.9, show.legend = FALSE) +
      
      
      ## add coastline
      geom_polygon(data = mass.project,
                   aes(x = long, y = lat,
                       group = group), fill = "bisque2") +
      
      
      ## add in WEA lease sites
      geom_polygon(data = wea.project,
                   aes(x = long, y = lat,
                       group = group), alpha = 0.05,
                   color = "white", fill = "transparent") +
      
      
      ## add data1 
      geom_point(data = data1,
                 mapping = mapping1, 
                 size = 0.05) +
      
      ## add data2
      geom_point(data = data2,
                 mapping = mapping2, 
                 size = 0.05) +
      
      labs(x = "Longitude",
           y = "Latitude") +
      
      
      theme(panel.border = element_rect(colour = "black",
                                        fill=NA, size = 0.2),
            text = element_text(size=12), ## axis text size
            legend.text = element_text(size = 9), ## legend text size
            legend.title = element_blank(), ## remove legend title
            legend.position = "bottom", ## put the legend at the bottom
            plot.margin = unit(c(.1, .1, .1, .1), "cm")) +
      
      guides(colour = guide_legend(override.aes = list(size=5))) 
  }
  
}
base_calibration_map <- function(){
  
  ggplot() +
    
    
    coord_quickmap(xlim=c(-70.6, -70),
                   ylim=c(42, 42.75)) +
    
    
    ## add bathy
    geom_raster(data = dat,
                aes(x = x, y = y, fill = gom15dd0),
                alpha = 0.9, show.legend = FALSE) +
    
    
    ## add coastline
    geom_polygon(data = mass.project,
                 aes(x = long, y = lat,
                     group = group), fill = "bisque2") +
    
    
    labs(x = "Longitude",
         y = "Latitude",
         fill = "Species") +
    
    facet_wrap(~setalt) +
    
    theme(strip.background = element_rect(fill = "white", 
                                          color = "black")) +
    
    theme(panel.border = element_rect(colour = "black",
                                      fill=NA, size = 0.2),
          text = element_text(size=12), ## axis text size
          legend.text = element_text(size = 7), ## legend text size
          legend.title = element_blank(), ## remove legend title
          legend.position = "bottom", ## put the legend at the bottom
          plot.margin = unit(c(.1, .1, .1, .1), "cm"), 
          axis.text.x = element_text(angle = 75, 
                                     hjust = 1, 
                                     size =  7),
          axis.text.y = element_text(size =  7)) +
    
    guides(colour = guide_legend(override.aes = list(size=5))) +
    scale_size_continuous(range = c(7, 50),
                          breaks = c(1, 5, 100, 500), 
                          limits = c(1, 1000), 
                          labels = c("1", "5", "100", "500+"), 
                          guide = guide_legend(direction = "horizontal")) 
  
  
}
wea_calibration_map <- function(){
  
  ggplot() +
    
    
    coord_sf(xlim=c(-71.4, -69.3),
             ylim=c(40.3, 41.6)) +
    
    
    
    ## add bathy
    geom_raster(data = dat,
                aes(x = x, y = y, fill = gom15dd0),
                alpha = 0.9, show.legend = FALSE) +
    
    ## add in WEA lease sites
    geom_polygon(data = wea.project,
                 aes(x = long, y = lat,
                     group = group), alpha = 0.05,
                 color = "white", fill = "transparent") +
    
    ## add coastline
    geom_polygon(data = mass.project,
                 aes(x = long, y = lat,
                     group = group), fill = "bisque2") +
    
    
    labs(x = "Longitude",
         y = "Latitude",
         fill = "Species") +
    
    facet_wrap(~setalt) +
    
    theme(strip.background = element_rect(fill = "white", 
                                          color = "black")) +
    
    theme(panel.border = element_rect(colour = "black",
                                      fill=NA, size = 0.2),
          text = element_text(size=12), ## axis text size
          legend.text = element_text(size = 7), ## legend text size
          legend.title = element_blank(), ## remove legend title
          legend.position = "bottom", ## put the legend at the bottom
          plot.margin = unit(c(.1, .1, .1, .1), "cm"), 
          axis.text.x = element_text(angle = 75, 
                                     hjust = 1, 
                                     size =  7),
          axis.text.y = element_text(size =  7)) +
    
    guides(colour = guide_legend(override.aes = list(size=5))) +
    scale_size_continuous(range = c(7, 50),
                          breaks = c(1, 5, 100, 500), 
                          limits = c(1, 1000), 
                          labels = c("1", "5", "100", "500+"), 
                          guide = guide_legend(direction = "horizontal")) 
  
  
}

## replace Kenney SPECCODE with common names and scientific names
kenney2common <- function(SPECCODE){
  str_replace_all(SPECCODE, 
                  c("BLWH" = "Blue Whale", 
                    "BOWH" = "Bowhead Whale", 
                    "BRWH" = "Bryde's Whale", 
                    "FIWH" = "Fin Whale (*Balaenoptera physalus*)",
                    "GRWH" = "Gray Whale", 
                    "HUWH" = "Humpback Whale (*Megaptera novaeangliae*)",
                    "RIWH" = "North Atlantic Right Whale (*Eubalaena glacialis*)", 
                    "SEWH" = "Sei Whale (*Balaenoptera borealis*)",
                    "SPWH" = "Sperm Whale", 
                    "SRWH" = "Southern Right Whale",
                    "UNBA" = "Unidentified *Balaenoptera*",
                    "UNBS" = "Unidentified Bryde's or Sei Whale", 
                    "UNFS" = "Unidentified Fin or Sei Whale", 
                    "UNLW" = "Unidentified Large Whale", 
                    "UNRO" = "Unidentified Rorqual (Balaenopteridae)", 
                    "UNWH" = "Unidentified Whale",
                    "BEWH" = "Beaked Whale", 
                    "BLBW" = "Blainville's Beaked Whale", 
                    "GEBW" = "Gervais' Beaked Whale", 
                    "GOBW" = "Cuvier's Beaked Whale", 
                    "KIWH" = "Killer Whale", 
                    "MIWH" = "Minke Whale (*Balaenoptera acutorastrata*)", 
                    "NBWH" = "Northern Bottlenose Whale", 
                    "SOBW" = "Sowerby's Beaked Whale", 
                    "TRBW" = "True's Beaked Whale", 
                    "UNBW" = "Unidentified Beaked Whale", 
                    "UNMW" = "Unidentified Medium Whale", 
                    "ASDO" = "Atlantic Spotted Dolphin", 
                    "BELU" = "Beluga",
                    "BODO" = "Bottlenose Dolphin (*Tursiops truncatus*)", 
                    "CLDO" = "Clymene Dolphin", 
                    "DSWH" = "Dwarf Sperm Whale", 
                    "FKWH" = "False Killer Whale", 
                    "FRDO" = "Fraser's Dolphin", 
                    "GRAM" = "Risso's Dolphin",
                    "HAPO" = "Harbor Porpoise (*Phocoena phocoena*)", 
                    "LFPW" = "Long-finned Pilot Whale", 
                    "MHWH" = "Melon-Headed Whale", 
                    "OBDO" = "Offshore Bottlenose Dolphin", 
                    "PIWH" = "Pilot Whale", 
                    "PSDO" = "Pan-Tropical Spotted Dolphin", 
                    "PSWH" = "Pygmy Sperm Whale",
                    "PYKW" = "Pygmy Killer Whale", 
                    "RTDO" = "Rough-Toothed Dolphin", 
                    "SADO" = "Common Dolphin (*Delphinus delphis*)", 
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
                    "WSDO" = "Atlantic White-Sided Dolphin (*Lagenorhynchus acutus*)", 
                    "BESE" = "Bearded Seal", 
                    "GRSE" = "Gray Seal (*Halichoerus grypus*)", 
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
                    "LETU" = "Leatherback Turtle (*Dermochelys coriacea*)", 
                    "LOTU" = "Loggerhead Turtle", 
                    "ORTU" = "Olive Ridley Sea Turtle", 
                    "RITU" = "Kemp's Ridley Turtle", 
                    "UNTU" = "Unidentified Turtle", 
                    "ANSH" = "Angel Shark", 
                    "BASH" = "Basking Shark (*Cetorhinus maximus*)", 
                    "BLSH" = "Blue Shark (*Prionace glauca*)", 
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
                    "OCSU" = "Ocean Sunfish (*Mola mola*)", 
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

## replace Kenney SPECCODE with common names and NO scientific names
kenney2commonNoLatin <- function(SPECCODE){
  str_replace_all(SPECCODE, 
                  c("BLWH" = "Blue Whale", 
                    "BOWH" = "Bowhead Whale", 
                    "BRWH" = "Bryde's Whale", 
                    "FIWH" = "Fin Whale",
                    "GRWH" = "Gray Whale", 
                    "HUWH" = "Humpback Whale",
                    "RIWH" = "North Atlantic Right Whale", 
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
                    "GRAM" = "Risso's Dolphin",
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

##replace digit month with written out month
num2month <- function(month){
  str_replace_all(month, 
                  c("01" = "January", 
                    "02" = "February",
                    "03" = "March", 
                    "04" = "April",
                    "05" = "May", 
                    "06" = "June", 
                    "07" = "July",
                    "08" = "August", 
                    "09" = "September", 
                    "10" = "October", 
                    "11" = "November", 
                    "12" = "December"))
}

## convert degrees minutes seconds to decimal degrees
angle2dec <- function(angle) {
  angle <- as.character(angle)
  angle <- ifelse(grepl("S|W", angle), paste0("-", angle), angle)
  angle <- trimws(gsub("[^- +.0-9]", "", angle))
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    (abs(y[1]) + y[2]/60 + y[3]/3600) * sign(y[1])
  })
  return(x)
}

## convert strut from strut number to meters
strut2meters <- function(strip){
  
  
  
  dist_off_track_m = as.numeric(recode(strip, "0" = "0", 
                                       '1'  = "231.5", 
                                       '2' = "231.5", 
                                       '3' = "463",
                                       '4' = "463",
                                       '5' = "926",
                                       '6' = "926",
                                       '7' = "1852",  
                                       '8' = "1852", 
                                       '9' = "3704", 
                                       '10' = "3704", 
                                       '11' = "7408", 
                                       '12' = "7408", 
                                       '13' = "7409", 
                                       '14' = "7409"))
}

## pull out marine mammals, sharks, fish
megafauna <- function(data, speccode){
  
  data %>%
    filter(speccode == "BLWH" | speccode == "BOWH" | speccode == "BRWH" |
             speccode == "FIWH" | speccode == "GRWH" | speccode == "HUWH" |
             speccode == "RIWH" | speccode == "SEWH" | speccode == "SPWH" |
             speccode == "SRWH" | speccode == "UNBA" | speccode == "UNBS" |
             speccode == "UNFS" | speccode == "UNLW" | speccode == "UNRO" |
             speccode == "UNWH" | speccode == "BEWH" | speccode == "BLBW" |
             speccode == "GEBW" | speccode == "GOBW" | speccode == "KIWH" |
             speccode == "MIWH" | speccode == "NBWH" | speccode == "SOBW" |
             speccode == "TRBW" | speccode == "UNBW" | speccode == "UNMW" |
             speccode == "ASDO" | speccode == "BELU" | speccode == "BODO" |
             speccode == "CLDO" | speccode == "DSWH" | speccode == "FKWH" |
             speccode == "FRDO" | speccode == "GRAM" | speccode == "HAPO" |
             speccode == "LFPW" | speccode == "MHWH" | speccode == "OBDO" |
             speccode == "PIWH" | speccode == "PSDO" | speccode == "PSWH" |
             speccode == "PYKW" | speccode == "RTDO" | speccode == "SADO" |
             speccode == "SFPW" | speccode == "SNDO" | speccode == "SPDO" |
             speccode == "STDO" | speccode == "UNBD" | speccode == "UNBF" |
             speccode == "UNCW" | speccode == "UNDO" | speccode == "UNGD" |
             speccode == "UNKO" | speccode == "UNLD" | speccode == "UNSB" |
             speccode == "UNST" | speccode == "WBDO" | speccode == "WSDO" |
             speccode == "BESE" | speccode == "GRSE" | speccode == "HASE" |
             speccode == "HGSE" | speccode == "HOSE" | speccode == "HPSE" |
             speccode == "MANA" | speccode == "PINN" | speccode == "POBE" |
             speccode == "RISE" | speccode == "UNSE" | speccode == "WALR" |
             speccode == "GRTU" | speccode == "HATU" | speccode == "LETU" |
             speccode == "LOTU" | speccode == "ORTU" | speccode == "RITU" |
             speccode == "UNTU" | speccode == "ANSH" | speccode == "BASH" |
             speccode == "BLSH" | speccode == "DUSH" | speccode == "GHSH" |
             speccode == "HHSH" | speccode == "LMSH" | speccode == "MKSH" |
             speccode == "SDOG" | speccode == "SMSH" | speccode == "THSH" |
             speccode == "TISH" | speccode == "UNSH" | speccode == "WHSH" |
             speccode == "WTSH" | speccode == "BFTU" | speccode == "BLFI" |
             speccode == "CDRA" | speccode == "CNRA" | speccode == "FLFI" |
             speccode == "MAHI" | speccode == "MARA" | speccode == "MOBU" |
             speccode == "OCSU" | speccode == "OTBI" | speccode == "SCFI" |
             speccode == "SCRA" | speccode == "SWFI" | speccode == "TUNS" |
             speccode == "UNFI" | speccode == "UNRA" | speccode == "WHMA" |
             speccode == "YFTU" | speccode == "AMAL" | speccode == "JELL" |
             speccode == "LMJE" | speccode == "PMOW" | speccode == "UNCE" |
             speccode == "UNMM")
  
  
}

## if the number is greater than 10 put it in numerics, 
## if it is less than ten spell it out.
num2word <- function(dat){
  
  
  
  dat <- if_else(dat[1] <= 10, 
                 numbers2words(dat[1]),
                 as.character(dat))
  
  
}

## pull out large whales
LargeWhales <- function(data, species){
  
  data %>% 
    filter(species == "Fin Whale" |
             species == "Humpback Whale" |
             species == "Minke Whale" |
             species == "North Atlantic Right Whale" |
             species == "Unidentified Mysticete Whale" |
             species == "Unidentified Whale" |
             species == "Sei Whale")
}


## pull out odontocetes 
odont <- function(data, species) {
  
  data %>% 
    filter(species == "Atlantic White-Sided Dolphin" |
             species == "Bottlenose Dolphin" |
             species == "Bottlenose Dolphin, Atlantic White-Sided Dolphin" |
             species == "Harbor Porpoise" |
             species == "Short-beaked Common Dolphin" |
             species == "Common Dolphin" |
             species == "Short-Finned Pilot Whale" |
             species == "Long-Finned Pilot Whale" |
             species == "Long-finned Pilot Whale" |
             species == "Striped Dolphin" |
             species == "Unidentified Dolphin" |
             species == "Unidentified Dolphin or Porpoise" |
             species == "Whale, Pilot Spp." |
             species == "Pilot Whale" |
             species == "White-Beaked Dolphin"|
             species == "Sperm Whale"|
             species == "Unidentified  (Tursiops, Delphinus, Lagenorhyncus) Dolphin")
}

## function that will take start and end points and convert
## to a linestring
make_line <- function(end_long_dec, start_lat_dec, 
                      start_long_dec, end_lat_dec) {
  st_linestring(matrix(c(end_long_dec, start_long_dec, 
                         end_lat_dec, start_lat_dec), 2, 2))
}



## get rid of bad lat/longs from WEA
bad_wea_posits <- function(data, lat, long) {
  data %>% 
    filter(lat > 40.5 & lat < 42) %>% 
    filter(long < -69.5 & long > -72)
}


bad_calibration_posits <- function(data, lat, long) {
  data %>% 
    filter(lat > 42 & lat < 43) %>% 
    filter(long < -70 & long > -70.7)
  
}
