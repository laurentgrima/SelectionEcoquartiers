library("tidyverse")
library("sf")
setwd("C:/Users/Grima/Desktop/Terroiko")
sites <- st_read("data/input/perimetresQuartiers.gpkg")
             
donnees_naturalistes <- read_delim(paste0("C:/Users/Grima/Desktop/Terroiko/data/output/data/",NomSite,".csv"), ",")


donnees_naturalistes$taxon <- donnees_naturalistes$Classe
donnees_naturalistes$taxon <- donnees_naturalistes$taxon %>% replace_na("Squamata")

donnees_naturalistes <- donnees_naturalistes %>%
  filter(Regne == "Animalia")
# nb de données par regne
donnees_naturalistes %>% 
  group_by(taxon) %>%
  tally() %>%
  ggplot(aes(x=taxon, y=n)) +
  geom_bar(stat="identity", fill="#7FA9CB") +
  labs(x ="Taxon", y = "Nombre d'observations") +
  geom_text(aes(label = n), vjust = -0.3)+
  theme_minimal()

# nb de données par origine
donnees_naturalistes %>% 
  group_by(LibelleJeuDonnees) %>%
  tally() %>%
  ggplot(aes(x=LibelleJeuDonnees, y=n)) +
  geom_bar(stat="identity", fill="#7FA9CB") +
  labs(x ="", y = "Source des données") +
  coord_flip() +
  theme_minimal()

# nb de données par origine
donnees_naturalistes %>% 
  group_by(annee = lubridate::floor_date(DateDebut, "year"), taxon) %>%
  tally() %>%
  ggplot() +
  geom_bar(stat="identity",aes(x=annee, y=n, fill=taxon)) +
  labs(x ="", y = "nombre d'observations") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  coord_cartesian(clip = 'off')+
  theme_minimal()

#Tableaux

nbParSource <- 
  donnees_naturalistes %>%
  group_by(donnees_naturalistes$LibelleJeuDonnees) %>%
  summarise(count = length(LibelleJeuDonnees))

write.csv(x=nbParSource, file=paste0("data/output/Tables/nbParSource",NomSite,".csv"))

library(tidyverse)
library(sf)
library(raster)
library(mapview)
library(MASS)

setwd("C:/Users/Grima/Desktop/Terroiko")
OCS_2020_CESBIO <- raster("data/input/OCS_2020_CESBIO.tif")
emprise <- st_read("data/input/contourMontpellierVille.geojson")
perimetre <- st_read("data/input/emprises/perimetreCaylus.geojson")
degreeUrba <- raster("data/input/degreeUrba.tif")
dataObsNat <- read_delim("data/input/dataObsNat.csv", ";")
dataObsNat <- dataObsNat %>% drop_na(Latitude)
dataObsNatSF <- st_as_sf(dataObsNat,coords=c("Longitude","Latitude"),crs=4326)
lonlat <- cbind(c(as.numeric(dataObsNat$Longitude)),c(as.numeric(dataObsNat$Latitude)))
dataObsNatLambert <- SpatialPoints(coords = lonlat, proj4string = CRS("+init=epsg:4326"))
dataObsNatLambert <- st_as_sf(dataObsNatLambert)
dataObsNatLambert <- st_transform(dataObsNatLambert,CRS("+init=epsg:2154"))
k = kde2d(dataObsNat$Longitude,dataObsNat$Latitude,h=0.001,n=25)
r = raster(k)
crs(dataObsNatLambert)
mapview(emprise, alpha.regions=0.1)+
  mapview(perimetre, alpha.regions=0.1)+
  mapview(dataObsNatSF,zcol = "Classe")

st_area(perimetre)/1000
intersection <- st_intersection(x = perimetre, y = dataObsNatLambert)

mapview(emprise, alpha.regions=0.1)+
  mapview(perimetre, alpha.regions=0.1)+
  mapview(intersection, alpha.regions=0.1)

cropped <- crop(OCS_2018_CESBIO,perimetre)


mapview(emprise, alpha.regions=0.1)+
  mapview(perimetre, alpha.regions=0.1)+
  mapview(cropped, alpha.regions=0.5)

"https://console.cloud.google.com/storage/browser/gcp-public-data-sentinel-2/tiles/31/T/EJ?pageState=(%22StorageObjectListTable%22:(%22f%22:%22%255B%255D%22))&prefix=&forceOnObjectsSortingFiltering=false"

library(gdalUtils)
library(rgdal)
#utm 25831
gdal_translate("C:/Users/Grima/Desktop/Terroiko/data/input/Sentinelle2/tiles_31_T_EJ_S2A_MSIL1C_20150716T105026_N0204_R051_T31TEJ_20160816T205328.SAFE_GRANULE_S2A_OPER_MSI_L1C_TL_EPA__20160816T141242_A000334_T31TEJ_N02.04_IMG_DATA_S2A_OPER_MSI_L1C_TL_EPA__20160816T141242_A000334_T31TEJ_B02.jp2", "new_band_name.tif")
band1 <- readGDAL("new_band_name.tif")
memory.limit(size=99900)
plot(band1)
band1 <- raster(band1)
writeRaster(band1,'test.tif')

###

dataObsNat %>% 
  group_by(Regne) %>%
  tally() %>%
  ggplot(aes(x=Regne, y=n)) +
  geom_bar(stat="identity", fill="#7FA9CB") +
  labs(x ="", y = "nb d'observations") +
  theme_minimal()
