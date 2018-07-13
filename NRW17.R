setwd()
getwd()

library(rgdal)
library(spdep)
library(dplyr)
library(readr)
library(maptools)
#if (!require(gpclib)) install.packages("gpclib", type="source")
#gpclibPermit()

list.files()

shp <- readOGR(dsn = ".", layer = "STATISTIK_AUSTRIA_GEM_20170101")


# ##### Initial Steps to create our dataset #####
# # plot(shp)
# # tail(shp$ID)
# #
# # 
# nrw17 <- read.csv("./nrw2017.csv", header = TRUE, sep = ",",
#          dec = ".", fill = TRUE, comment.char = "")
# names(nrw17)
# 
# nrw17.v1 <- nrw17[,c("GKZ","Name","Wahlberechtigte","gueltig","fpoe")]
# 
# nrw17.v1$fpoe.anteil <- nrw17.v1$fpoe/nrw17.v1$gueltig
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
# #Datensätze Wahldaten+Urbanität zusammenfügen
# 
# d <- nrw17.v1 %>% left_join(UR, by="GKZ")
# names(d)
# 
# d1 <- d[,c("GKZ","GEMNAME", "Wahlberechtigte", "gueltig", "fpoe", "fpoe.anteil", "UR_TYP", "Tourismus")]
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
#                      "S.SONST", "AL")]
# 
# #Zusammenf?gen d1 (UR+NRW17) und Statcube Daten
# d2 <- d1 %>% left_join(Statcube1, by="GKZ")
#save(d2, file="WiGeo_Daten.RDA")

##### Load finished dataset #####
load("./WiGeo_Daten.RDA")

#Graphik mit Shapefile von Oesterreich
# library(ggplot2)
# 
# fpo <- d2[,c("GKZ", "fpoe.anteil")]
# 
# shp.fpo <- merge(shp, fpo, by.x="ID", by.y="GKZ", all.x=TRUE)
# 
# shp.points <- fortify(shp, region="ID")
# 
# shp.points <- merge(shp.points, fpo, by.x="id", by.y="GKZ", all.x=TRUE) %>% arrange(order)
# names(shp.points)
# 
# ggplot(shp.points) + aes(x=long, y=lat, group=group, fill=fpoe.anteil) +  geom_polygon() + 
#   scale_fill_gradientn(colours = rev(rainbow(7)),
#                        breaks = c(0.1, 0.2, 0.3, 0.4, 0.5),
#                        trans = "log10")
# 
# names(shp)
# str(d2)



### Unabh?ngige Variablen f?r Regressionsmodell ###

### ANT.JUNG - Anteil der 15-29-J?hrigen an der Gesamtbev?lkerung 
d2$ANT.JUNG <- (d2$A.15.29)/(d2$A.U15+d2$A.15.29+d2$A.30.49+d2$A.50.64+d2$A.65.84+d2$A.ue85)
summary(d2$ANT.JUNG)
### ANT.ALT: Anteil der 65+ J?hrigen an der Gesamtbev?lkerung 
d2$ANT.ALT <- (d2$A.65.84+d2$A.ue85)/(d2$A.U15+d2$A.15.29+d2$A.30.49+d2$A.50.64+d2$A.65.84+d2$A.ue85)
summary(d2$ANT.ALT)
### ANT.NQ: Anteil der Niedrigqualifizierten (Personen mit max. Pflichtschulabschluss) an der Gesamtbev?lkerung 
d2$ANT.NQ <- d2$B.Pflichtschule/(d2$B.Pflichtschule+d2$B.Lehrabschluss+d2$B.Mittlere.hoehere.Schule+d2$B.Hochschule.Akademie+d2$B.Entfaellt)
### ANT.MIG: Anteil der MigrantInnen (Geburtsland nicht ?sterreich) an der Gesamtbev?lkerung
a <- d2$B.Pflichtschule+d2$B.Lehrabschluss+d2$B.Mittlere.hoehere.Schule+d2$B.Hochschule.Akademie+d2$B.Entfaellt
b <- d2$G.AT
c <- 1-(b/a)
d2$ANT.MIG <- c


### "Back-up"-Variablen ### 
## ANT.MIG-DRITT: Anteil der MigrantInnen aus "Drittstaaten" (Geburtsland nicht ?sterreich oder EU)
d2$ANT.MIG.DRITT <- (d2$G.JUGOSL+d2$G.TR+d2$G.SONST)/(d2$G.AT+d2$G.EU+d2$G.JUGOSL+d2$G.TR+d2$G.SONST)
## ANT.AKAD: Anteil der Personen mit Hochschulabschluss an der Gesamtbev?lkerung
d2$ANT.AKAD <- d2$B.Hochschule.Akademie/(d2$B.Pflichtschule+d2$B.Lehrabschluss+d2$B.Mittlere.hoehere.Schule+d2$B.Hochschule.Akademie+d2$B.Entfaellt)


####### Vorbereitung Urbanit?tsgrad #####
# TYP	Bezeichnung	
# 101	Urbane Gro?zentren	st?dtisch
# 102	Urbane Mittelzentren	st?dtisch
# 103	Urbane Kleinzentren	st?dtisch ###
# 210	Regionale Zentren, zentral	l?ndlich
# 220	Regionale Zentren, intermedi?r	l?ndlich ###
# 310	L?ndlicher Raum im Umland von Zentren, zentral	l?ndlich
# 320	L?ndlicher Raum im Umland von Zentren, intermedi?r	l?ndlich
# 330	L?ndlicher Raum im Umland von Zentren, peripher	l?ndlich ###
# 410	L?ndlicher Raum, zentral	l?ndlich
# 420	L?ndlicher Raum, intermedi?r	l?ndlich
# 430	L?ndlicher Raum, peripher	l?ndlich ###
d2$UR_TYP2 <- factor(d2$UR_TYP, levels= c(101, 102, 103, 210, 220, 310, 320, 330, 410, 420, 430), 
                                labels=c("11", "10", "9", "8", "7", "6", "5", "4", "3", "2", "1"))

library(car)
d2$UR_TYP3 <- recode(d2$UR_TYP2,"'11'='4'; '10'='4'; '9'='4';'8'='3'; '7'='3';'6'='2';'5'='2';'4'='2';'3'='1';'2'='1';'1'='1'")

is.factor(d2$UR_TYP2)

##Get Overview of data
d2 %>% select(UR_TYP, UR_TYP2,UR_TYP3, GEMNAME, Wahlberechtigte) %>%  
  group_by(UR_TYP3)  %>%
  summarize(mean= mean(Wahlberechtigte))


##### Deskriptive Statistik der beiden Hauptvariablen
table(d2$UR_TYP3)
names <- c("Rural", "Rural bei Zentren","Regionale Zentren", "Urban")
par(mfrow=c(1,2))
p.ur <- barplot(table(d2$UR_TYP3),
                main="Urbanit?tsgrad",
                col = "steelblue", 
                names.arg=names,
                border=NA,
                axes=TRUE)
p.fpo <- hist(d2$fpoe.anteil, 
              col="steelblue", 
              main="FP? Anteil",
              xlab=NA, 
              ylab=NA)
dev.off()

#p.bi <- d2 %>% group_by(UR_TYP3) %>% summarise(mean=mean(fpoe.anteil))

m <- tapply(shp_merged$fpoe.anteil, shp_merged$UR_TYP3, mean)
barplot(m,col = "steelblue", 
        names.arg=names, 
        ylim = c(0,0.35), 
        cex.names=1.6,
        cex.axis = 1.6,
        border=NA,
        axes=TRUE)

###merge data file with data 

shp_merged <- merge(shp, d2, by.x="ID", by.y="GKZ")

##Kick Missing Values - Wird für uns vlt. noch wichtig, aber haben keine Missing Values f?r FP? Stimmenanteil
shp_merged <- subset(shp_merged, !is.na(shp_merged$fpoe.anteil))
shp_merged <- subset(shp_merged, !is.na(shp_merged$ANT.MIG))
shp_merged <- subset(shp_merged, !is.na(shp_merged$ANT.JUNG))
shp_merged <- subset(shp_merged, !is.na(shp_merged$ANT.ALT))
shp_merged <- subset(shp_merged, !is.na(shp_merged$ANT.NQ))
shp_merged <- subset(shp_merged, !is.na(shp_merged$AL))
shp_merged <- subset(shp_merged, !is.na(shp_merged$UR_TYP))


d2[,c("fpoe.anteil", "UR_TYP3", "AL", "ANT.MIG", "ANT.JUNG", "ANT.ALT", "ANT.NQ")]

#shp$AL, shp$ANT.MIG, shp$ANT.JUNG, "ANT.ALT", "ANT.NQ"
##Centroids
coords <- coordinates(shp_merged)

############## First: Spatial Weights (Matrix) ############
# PROBLEM: R?umliche Regressionen sind nur so gut wie die W-Matrix, die wir bestimmt haben 
# Wenn man es nicht gut bestimmen kann welche Matrix Sinn macht, am besten mehrere Matrizen nehmen

## Queen contiguity
## Gemeinsame Grenze - Polygon to Neighbour. Welches Polygon h?ngt mit anderem zusammen
## Poly2nb erstellt Nachbarmatrix

queen_nb <- poly2nb(shp_merged, row.names = shp_merged$ID, queen = T)

##Nachbarschaftsliste nach list W konvertieren. 
##2 Schritte, die man eig. immer braucht
##Von Shapefile zu Nachbarschaft
##Von Nachbarschaftsdefintionen zu sch?ner W Matrix
W.list.queen <- nb2listw(queen_nb, style = "W", zero.policy = TRUE)

### Second-order Contiguity
### The nblag function takes an existing neighbour list and returns a list of list
### from first to maxlag order of neighbours
### The nblag function creates higher order neighbour lists, where higher order neighbours are only lags links from each other 
### nblag_cumul cumulates neighbour lists to a single neighbour list ("nb" object)
queen_nb_lags <- nblag(queen_nb, maxlag = 2)
cumlqueen2_nb <- nblag_cumul(queen_nb_lags)
W.list.queen2 <- nb2listw(cumlqueen2_nb, style = "W", zero.policy = TRUE)

### did this actually work?

# What if there are no neighbours?
# summary(W.list.queen, zero.policy=TRUE)
##### k-nearest neighbour ###
## KNN Neighbours. 5 n?hesten Nachbarn. Wer sind f?nf n?chsten Nachbarn
k5.near <- knearneigh(coords, k=5) # Man verwendet coords (die im Grunde die centroids darstellen)
k5 <- knn2nb(k5.near)
W.list.k5 <- nb2listw(k5, style = "W", zero.policy = FALSE)
## KNN Neighbours. 3 n?hesten Nachbarn. Wer sind f?nf n?chsten Nachbarn
k3.near <- knearneigh(coords, k=3) # Man verwendet coords (die im Grunde die centroids darstellen)
k3 <- knn2nb(k3.near)
W.list.k3 <- nb2listw(k3, style = "W", zero.policy = FALSE)


## Distance bands
## What is the maximum distance between two obs (so that each obs has at least one neighbour?)
k1 <- knearneigh(coords, k=1)
k1 <- knn2nb(k1)
link.max <- max(unlist(nbdists(k1, coords=coords)))
link.max

## Mach Nachbarschaftsmatrix, wo jeder zwischen 0 und Maximaler Distanz zwischen L?ndern (link.max)
#distw.max <- dnearneigh(coords, 0, link.max, row.names=shp_merged@data$Id) 
#W.list <- nb2listw(distw.max, style="W", zero.policy=FALSE)

## Inverse distance weights - 1/Anzahl der Kilometer --> je weiter weg desto unwichtiger
# distw.tot <- dnearneigh(coords, 0, Inf, row.names=shp_merged$Id)
# dnbdist.tot<-nbdists(distw.tot, coords)
#  gl.tot<-lapply(dnbdist.tot, function(x) 1/x) 
# W.list.inv<-nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE)

## Mach Nachbarschaftsmatrix mit 50km und 25 km
distw.50 <- dnearneigh(coords, 0, 50000, row.names=shp_merged@data$Id) 
W.list50 <- nb2listw(distw.50, style="W", zero.policy=FALSE)

distw.25 <- dnearneigh(coords, 0, 25000, row.names=shp_merged@data$Id) 
W.list25 <- nb2listw(distw.25, style="W", zero.policy=FALSE)

# Before we regress, check if there is spatial structure in the data
# Gibt es ?berhaupt r?umliche Struktur? Wie ?hnlich sind die Nachbarn? Wenn sehr ?hnlich, dann vermutlich keine
# r?umliche Struktur
## Spatial Lags
#lag.listw gibt nach Nachbarschaftsdefinition durchschnittliches Wachstum aller nachbarn
#aus WS Uebung:
#shp$lag.pr80b <- lag.listw(W.list, shp$pr80b)
#shp$lag.pr103b <- lag.listw(W.list, shp$pr103b)
#shp$lag.growth <- lag.listw(W.list, shp$growth)
#plot(shp$pr80b, shp$lag.pr80b, xlab="Productivity", ylab="Lag of Prod.")

#shp_merged$lag.fpoe.anteil.inv <- lag.listw(W.list.inv, shp_merged$fpoe.anteil)
shp_merged$lag.fpoe.anteil.k3 <- lag.listw(W.list.k3, shp_merged$fpoe.anteil)
shp_merged$lag.fpoe.anteil.k5 <- lag.listw(W.list.k5, shp_merged$fpoe.anteil)
shp_merged$lag.fpoe.anteil.q <- lag.listw(W.list.queen, shp_merged$fpoe.anteil)
shp_merged$lag.fpoe.anteil.25 <- lag.listw(W.list25, shp_merged$fpoe.anteil)
shp_merged$lag.fpoe.anteil.50 <- lag.listw(W.list50, shp_merged$fpoe.anteil)
shp_merged$lag.fpoe.anteil.q2 <- lag.listw(W.list.queen2, shp_merged$fpoe.anteil)

#plot(shp_merged$fpoe.anteil, shp_merged$lag.fpoe.anteil.inv, xlab="FPOE Anteil", ylab="FPOE Anteil Lag", main="inv")
plot(shp_merged$fpoe.anteil, shp_merged$lag.fpoe.anteil.k3, xlab="FPOE Anteil", ylab="FPOE Anteil Lag", main="k3")
plot(shp_merged$fpoe.anteil, shp_merged$lag.fpoe.anteil.k5, xlab="FPOE Anteil", ylab="FPOE Anteil Lag", main="k5")
plot(shp_merged$fpoe.anteil, shp_merged$lag.fpoe.anteil.q, xlab="FPOE Anteil", ylab="FPOE Anteil Lag", main="q")
plot(shp_merged$fpoe.anteil, shp_merged$lag.fpoe.anteil.25, xlab="FPOE Anteil", ylab="FPOE Anteil Lag", main="25")
plot(shp_merged$fpoe.anteil, shp_merged$lag.fpoe.anteil.50, xlab="FPOE Anteil", ylab="FPOE Anteil Lag", main="50")
plot(shp_merged$fpoe.anteil, shp_merged$lag.fpoe.anteil.q2, xlab="FPOE Anteil", ylab="FPOE Anteil Lag", main="second-order queen")

## Spatial Autocorrelation tests
## Wie stark ist r?umlicher Zusammenhang? --> P Wert Signifikant --> Daten haben r?umliche Struktur 
## --> Daten haben r?umliche Struktur --> Welcher Zusammenhang also?
## f?r uns h
#moran.test(shp_merged$fpoe.anteil, listw = W.list.inv, alternative = "greater")
moran.test(shp_merged$fpoe.anteil, listw = W.list.k3, alternative = "greater")
moran.test(shp_merged$fpoe.anteil, listw = W.list.k5, alternative = "greater")
moran.test(shp_merged$fpoe.anteil, listw = W.list.queen, alternative = "greater")
moran.test(shp_merged$fpoe.anteil, listw = W.list25, alternative = "greater")
moran.test(shp_merged$fpoe.anteil, listw = W.list50, alternative = "greater")
moran.test(shp_merged$fpoe.anteil, listw = W.list.queen2, alternative = "greater")


#moran.plot(shp_merged$fpoe.anteil, listw = W.list.inv)
moran.plot(shp_merged$fpoe.anteil, listw = W.list.k3)
moran.plot(shp_merged$fpoe.anteil, listw = W.list.k5)
moran.plot(shp_merged$fpoe.anteil, listw = W.list.queen)
moran.plot(shp_merged$fpoe.anteil, listw = W.list25)
moran.plot(shp_merged$fpoe.anteil, listw = W.list50)
moran.plot(shp_merged$fpoe.anteil, listw = W.list.queen2)


#### Regression
## Start from Linear OLS Model
names(d2)
#1. mit AL
# d.reg <- d2[,c("fpoe.anteil", "UR_TYP3", "AL", "ANT.MIG", "ANT.JUNG", "ANT.ALT", "ANT.NQ")]
# d.reg <- na.omit(d.reg)

lin.urby1 <- lm(fpoe.anteil ~ UR_TYP3 + AL, data=shp_merged)
summary(lin.urby1)
confint(lin.urby1)
shp_merged$lmresiduals1 <- residuals(lin.urby1)

#2. mit AL und Migrationshintergrund
lin.urby2 <- lm(fpoe.anteil ~ UR_TYP3 + AL + ANT.MIG, data=shp_merged)
summary(lin.urby2)
confint(lin.urby2)
shp_merged$lmresiduals2 <- residuals(lin.urby2)

#3. mit AL und Migrationshintergrund und Alter
lin.urby3 <- lm(fpoe.anteil ~ UR_TYP3 + AL + ANT.MIG + ANT.JUNG +ANT.ALT, data=shp_merged)
summary(lin.urby3)
confint(lin.urby3)
shp_merged$lmresiduals3 <- residuals(lin.urby3)

#4. mit AL und Migrationshintergrund und Alter und Bildung
#I(substr(ID, 1,3))
library(stargazer)
lin.urby4 <- lm(fpoe.anteil ~ UR_TYP3 + I(AL*100) + I(ANT.MIG*100) + I(ANT.JUNG*100) + I(ANT.ALT*100) + I(ANT.NQ*100), data=shp_merged)
summary(lin.urby4)
confint(lin.urby4)
shp_merged$lmresiduals4 <- residuals(lin.urby4)


lin.urby5 <- lm(fpoe.anteil ~ UR_TYP3 + I(AL*100) + I(ANT.MIG*100) + I(ANT.JUNG*100) + I(ANT.ALT*100) + I(ANT.NQ*100) + I(substr(ID, 1,3)), data=shp_merged)
summary(lin.urby5)
confint(lin.urby5)
shp_merged$lmresiduals5 <- residuals(lin.urby5)


x <- substr(shp_merged$ID,1,3)
y <- setdiff(x, c("101","102","103","923"))


## Is there spatial dependence in the OLS errors?
## Wenn ich wei?, dass es falsch ist, interessiert mach, was richtig ist: Lag Modell oder Error Modell?

lm.morantest(lin.urby4, W.list.inv, resfun=rstudent)
lm.morantest(lin.urby4, W.list.k3, resfun=rstudent)
lm.morantest(lin.urby4, W.list.k5, resfun=rstudent)
lm.morantest(lin.urby4, W.list.queen, resfun=rstudent)
lm.morantest(lin.urby4, W.list.queen2, resfun=rstudent)
lm.morantest(lin.urby4, W.list25, resfun=rstudent)
lm.morantest(lin.urby4, W.list50, resfun=rstudent)

moran.plot(shp_merged$lmresiduals4, listw2U(W.list.inv))
moran.plot(shp_merged$lmresiduals4, listw2U(W.list.k3))
moran.plot(shp_merged$lmresiduals4, listw2U(W.list.k5))
moran.plot(shp_merged$lmresiduals4, listw2U(W.list.queen))
moran.plot(shp_merged$lmresiduals4, listw2U(W.list.queen2))
moran.plot(shp_merged$lmresiduals4, listw2U(W.list25))
moran.plot(shp_merged$lmresiduals4, listw2U(W.list50))


x <- W.list25


### Which model should we choose instead? LM Tests
## Alle M?glichkeiten testen, die es gibt
#lm test for spatial dependence in OLS residuals
## Welches Model verbessert unsere Schätzung? --> Alle au0er RLMlag?
res <- lm.LMtests(lin.urby5, x, test="all")
tres <- t(sapply(res, function(x) c(x$statistic, x$parameter, x$p.value)))
colnames(tres) <- c("Statistc", "df", "p-value")


##Errormodell sieht gut aus, LAGMODELL sieht gut aus. NUR IM FALL "Beide sind gut" 
##Robuste Statistiken anschauen (RLMerr)

printCoefmat(tres)


# Jetzt: Modell sch?tzen
# Spatial LAG Model (SAR)
# lagsarlm = (Lagmodell)

sar <- lagsarlm(formula=formula(lin.urby5), listw=x,data=shp_merged)
summary(sar, correlation=FALSE)

# Impacts - Aufsplitten in indirekte und direkte Effekte
sar.impacts<-impacts(sar, listw = x)
x <- sar.impacts
x[] <- lapply(x, round,4)
x

stargazer(lin.urby4, lin.urby5, sar, type = "text", out ="./output/FE-SL-Regressions.txt",
          dep.var.labels=c("Share FPÖ", "Share FPÖ"),
          covariate.labels=c("Urbanity degree 2","Urbanity degree 3","urbanity degree 4","Unemployment","Share migrants","Share young people (15-29)","Share old people (>65)","Share low education","Rust(Burgenland)","Eisenstadt-Umgebund(Burgenland)","Wien 23.(Liesing) [113 omitted]"), 
          omit = y, title = "Comparison: LM vs LM (fixed effects) vs. Spatial Lag", single.row = TRUE)


###TestSpace###

# Spatial Durbin Model (SDM) --> auch "lagsarm", aber mit type="mixed"

durbin <- lagsarlm(formula = formula(lin.Solow), 
                   listw = W.list,
                   type="mixed", data=shp)
summary(durbin, correlation=FALSE)


# Spatial Error Model (SEM) - 2. Modell: Error Modell

error <- errorsarlm(formula=formula(lin.Solow), listw=W.list, data=shp)
summary(error, correlation=FALSE)
