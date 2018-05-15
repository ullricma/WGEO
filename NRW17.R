#setwd("C:/Users/Sarah/Desktop/Master Sozio√∂konomie/Semester 4/WiGeo/Daten")
setwd("C:/Users/Dietmar/Desktop/Master SozOek/Semester II/WRGEO/Semester2/Data")
getwd()

library(rgdal)
library(spdep)
library(dplyr)
library(readr)

#install.packages("raster")

# shp <- readOGR(".", "STATISTIK_AUSTRIA_GEM_20170101")
# plot(shp)
# tail(shp$ID)
# 
# 
# nrw17 <- read.csv("./nrw2017.csv", header = TRUE, sep = ",",
#          dec = ".", fill = TRUE, comment.char = "")
# names(nrw17)
# 
# nrw17.v1 <- nrw17[,c("GKZ","Name","Wahlberechtigte","abgegeben","g√.ltig","ung√.ltig","fpoe")]
# 
# nrw17.v1$fpoe.anteil <- nrw17.v1$fpoe/nrw17.v1$g√.ltig
# 
# UR <- read.csv("./Urban_Rural_Daten.csv", header=TRUE, sep = ";", dec = ".", fill=F,comment.char = "") 
# 
# Statcube <- read.csv("./Statcube_Daten.csv", header=TRUE, sep = ";", dec = ".", fill=F,comment.char = "", stringsAsFactors = FALSE) 
# names(Statcube)
# 
# Statcube$G.AT     <- Statcube$G.AT.M + Statcube$G.AT.W 
# Statcube$G.EU     <- Statcube$G.EU.M + Statcube$G.EU.W 
# Statcube$G.JUGOSL <- Statcube$G.JUGOSL.M + Statcube$G.JUGOSL.W 
# Statcube$G.TR     <- Statcube$G.TR.M + Statcube$G.TR.W 
# Statcube$G.SONST  <- Statcube$G.SONST.M + Statcube$G.SONST.W 
# Statcube$S.AT     <- Statcube$S.AT.M + Statcube$S.AT.W 
# Statcube$S.EU     <- Statcube$S.EU.M + Statcube$S.EU.W
# Statcube$S.JUGOSL <- Statcube$S.JUGOSL.M + Statcube$S.JUGOSL.W
# Statcube$S.TR     <- Statcube$S.TR.M + Statcube$S.TR.W
# Statcube$S.SONST  <- Statcube$S.SONST.M + Statcube$S.SONST.W
# 
# 
# #Datens√§tze Wahldaten+Urbanit√§t zusammenf√ºgen
# 
# d <- nrw17.v1 %>% left_join(UR, by="GKZ")
# names(d)
# 
# d1 <- d[,c("GKZ","GEMNAME", "Wahlberechtigte", "abgegeben", "g√.ltig", "ung√.ltig", "fpoe", "fpoe.anteil", "UR_TYP", "Tourismus")]
#   
# #Auswahl (im Moment) wichtiger Variablen in Statcube Daten, d.h. ohne Geburtsland/Staatsangehoerigkeit nach Geschlecht
# names(Statcube)
# Statcube1 <- Statcube[,c("GKZ", "M",                        
#                     "W"                ,         "Anteil.M",                 
#                     "A.U15"   ,                  "A.15.29" ,                 
#                     "A.30.49"       ,            "A.50.64" ,                 
#                     "A.65.84"     ,              "A.ue85" ,                  
#                      "B.Pflichtschule" ,          "B.Lehrabschluss" ,         
#                      "B.Mittlere.hoehere.Schule", "B.Hochschule.Akademie" ,   
#                      "B.Entfaellt"   ,             "G.AT"  ,                   
#                      "G.EU"     ,                 "G.JUGOSL"   ,              
#                      "G.TR"    ,                  "G.SONST"   ,               
#                      "S.AT"    ,                  "S.EU"  ,                   
#                      "S.JUGOSL"    ,              "S.TR"    ,                 
#                      "S.SONST")]
# 
# #Zusammenf¸gen d1 (UR+NRW17) und Statcube Daten
# d2 <- d1 %>% left_join(Statcube1, by="GKZ")
# save(d2, file="WiGeo_Daten.RDA")

load("./WiGeo_Daten.RDA")

#Graphik mit Shapefile von Oesterreich
library(ggplot2)

fpo <- nrw17.v1[,c("GKZ", "fpoe.anteil")]

shp.fpo <- merge(shp, fpo, by.x="ID", by.y="GKZ", all.x=TRUE)

shp.points <- fortify(shp, region="ID")

shp.points <- merge(shp.points, fpo, by.x="id", by.y="GKZ", all.x=TRUE) %>% arrange(order)
names(shp.points)

ggplot(shp.points) + aes(x=long, y=lat, group=group, fill=fpoe.anteil) +  geom_polygon() + 
  scale_fill_gradientn(colours = rev(rainbow(7)),
                       breaks = c(0.1, 0.2, 0.3, 0.4, 0.5),
                       trans = "log10")

summary(fpo$Stimmenanteil)
