library(rgdal)
library(spdep)
library(dplyr)
library(readr)
library(maptools)
#if (!require(gpclib)) install.packages("gpclib", type="source")
#gpclibPermit()

list.files()

shp <- readOGR(dsn = ".", layer = "STATISTIK_AUSTRIA_GEM_20170101")

###############################################
##### Initial Steps to create our dataset #####
###############################################

nrw17 <- read.csv("./nrw2017.csv", header = TRUE, sep = ",",
         dec = ".", fill = TRUE, comment.char = "")
names(nrw17)

nrw17.v1 <- nrw17[,c("GKZ","Name","Wahlberechtigte","gueltig","fpoe")]

nrw17.v1$fpoe.anteil <- nrw17.v1$fpoe/nrw17.v1$gueltig

UR <- read.csv("./Urban_Rural_Daten.csv", header=TRUE, sep = ";", dec = ".", fill=F,comment.char = "")

Statcube <- read.csv("./Statcube_Daten.csv", header=TRUE, sep = ";", dec = ".", fill=F,comment.char = "", stringsAsFactors = FALSE)
names(Statcube)

Statcube$G.AT     <- Statcube$G.AT.M + Statcube$G.AT.W
Statcube$G.EU     <- Statcube$G.EU.M + Statcube$G.EU.W
Statcube$G.JUGOSL <- Statcube$G.JUGOSL.M + Statcube$G.JUGOSL.W
Statcube$G.TR     <- Statcube$G.TR.M + Statcube$G.TR.W
Statcube$G.SONST  <- Statcube$G.SONST.M + Statcube$G.SONST.W
Statcube$S.AT     <- Statcube$S.AT.M + Statcube$S.AT.W
Statcube$S.EU     <- Statcube$S.EU.M + Statcube$S.EU.W
Statcube$S.JUGOSL <- Statcube$S.JUGOSL.M + Statcube$S.JUGOSL.W
Statcube$S.TR     <- Statcube$S.TR.M + Statcube$S.TR.W
Statcube$S.SONST  <- Statcube$S.SONST.M + Statcube$S.SONST.W

#Combine voting data with urbanity

d <- nrw17.v1 %>% left_join(UR, by="GKZ")
names(d)

d1 <- d[,c("GKZ","GEMNAME", "Wahlberechtigte", "gueltig", "fpoe", "fpoe.anteil", "UR_TYP", "Tourismus")]

#Choose most important data from statcube without country of birth and gender
names(Statcube)
Statcube1 <- Statcube[,c("GKZ", "M",
                    "W"                ,         "Anteil.M",
                    "A.U15"   ,                  "A.15.29" ,
                    "A.30.49"       ,            "A.50.64" ,
                    "A.65.84"     ,              "A.ue85" ,
                     "B.Pflichtschule" ,          "B.Lehrabschluss" ,
                     "B.Mittlere.hoehere.Schule", "B.Hochschule.Akademie" ,
                     "B.Entfaellt"   ,             "G.AT"  ,
                     "G.EU"     ,                 "G.JUGOSL"   ,
                     "G.TR"    ,                  "G.SONST"   ,
                     "S.AT"    ,                  "S.EU"  ,
                     "S.JUGOSL"    ,              "S.TR"    ,
                     "S.SONST", "AL")]

#Combine d1 (UR+NRW17) and Statcube data
d2 <- d1 %>% left_join(Statcube1, by="GKZ")
save(d2, file="WiGeo_Daten.RDA")

###############################################
##### Load finished dataset #####
###############################################

load("./WiGeo_Daten.RDA")

#graphic with Shapefile from Austria
library(ggplot2)

fpo <- d2[,c("GKZ", "fpoe.anteil")]

shp.fpo <- merge(shp, fpo, by.x="ID", by.y="GKZ", all.x=TRUE)

shp.points <- fortify(shp, region="ID")

shp.points <- merge(shp.points, fpo, by.x="id", by.y="GKZ", all.x=TRUE) %>% arrange(order)
names(shp.points)

ggplot(shp.points) + aes(x=long, y=lat, group=group, fill=fpoe.anteil) +  geom_polygon() +
  scale_fill_gradientn(colours = rev(rainbow(7)),
                       breaks = c(0.1, 0.2, 0.3, 0.4, 0.5),
                       trans = "log10")

names(shp)
str(d2)


##############################################################
##### Create independent variables for regression modell #####
##############################################################

### ANT.JUNG - Share of age 15-29 in population
d2$ANT.JUNG <- (d2$A.15.29)/(d2$A.U15+d2$A.15.29+d2$A.30.49+d2$A.50.64+d2$A.65.84+d2$A.ue85)
summary(d2$ANT.JUNG)
### ANT.ALT: Share of age 65+ in population
d2$ANT.ALT <- (d2$A.65.84+d2$A.ue85)/(d2$A.U15+d2$A.15.29+d2$A.30.49+d2$A.50.64+d2$A.65.84+d2$A.ue85)
summary(d2$ANT.ALT)
### ANT.NQ: Share of low educated persons (maximum compulsory school) in population
d2$ANT.NQ <- d2$B.Pflichtschule/(d2$B.Pflichtschule+d2$B.Lehrabschluss+d2$B.Mittlere.hoehere.Schule+d2$B.Hochschule.Akademie+d2$B.Entfaellt)
### ANT.MIG: Share of migrants (country of birth not in Austria) in population
a <- d2$B.Pflichtschule+d2$B.Lehrabschluss+d2$B.Mittlere.hoehere.Schule+d2$B.Hochschule.Akademie+d2$B.Entfaellt
b <- d2$G.AT
c <- 1-(b/a)
d2$ANT.MIG <- c


### "Back-up"-Variables ### 
## ANT.MIG-DRITT: Share of migrants from "third countries" (country of birth =/= Austria OR EU) 
d2$ANT.MIG.DRITT <- (d2$G.JUGOSL+d2$G.TR+d2$G.SONST)/(d2$G.AT+d2$G.EU+d2$G.JUGOSL+d2$G.TR+d2$G.SONST)
## ANT.AKAD: Share of People with higher education in complete population
d2$ANT.AKAD <- d2$B.Hochschule.Akademie/(d2$B.Pflichtschule+d2$B.Lehrabschluss+d2$B.Mittlere.hoehere.Schule+d2$B.Hochschule.Akademie+d2$B.Entfaellt)


####### Preparing urbanity degree ####
d2$UR_TYP2 <- factor(d2$UR_TYP, levels= c(101, 102, 103, 210, 220, 310, 320, 330, 410, 420, 430), 
                                labels=c("11", "10", "9", "8", "7", "6", "5", "4", "3", "2", "1"))

library(car)
d2$UR_TYP3 <- recode(d2$UR_TYP2,"'11'='4'; '10'='4'; '9'='4';'8'='3'; '7'='3';'6'='2';'5'='2';'4'='2';'3'='1';'2'='1';'1'='1'")

is.factor(d2$UR_TYP2)

##Get Overview of data
d2 %>% select(UR_TYP, UR_TYP2,UR_TYP3, GEMNAME, Wahlberechtigte) %>%  
  group_by(UR_TYP3)  %>%
  summarize(mean= mean(Wahlberechtigte))


##### descriptive statistics of both main variables
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

##Kick Missing Values - Maybe this will be important for us later on
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


##############################################################
##### First: Spatial Weights (Matrix)  #####
##############################################################

## Queen contiguity

queen_nb <- poly2nb(shp_merged, row.names = shp_merged$ID, queen = T)

##convert neighbour list to weights matrix (step1: create neighbour matrix, step2: create weights matrix. 

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
## KNN Neighbours. 5 nearest neighbours. 
k5.near <- knearneigh(coords, k=5) # use coords (more or less centroids in this case)
k5 <- knn2nb(k5.near)
W.list.k5 <- nb2listw(k5, style = "W", zero.policy = FALSE)
## KNN Neighbours. 3 nearest neighbours. 
k3.near <- knearneigh(coords, k=3) 
k3 <- knn2nb(k3.near)
W.list.k3 <- nb2listw(k3, style = "W", zero.policy = FALSE)

## Distance bands
## What is the maximum distance between two obs (so that each obs has at least one neighbour?)
k1 <- knearneigh(coords, k=1)
k1 <- knn2nb(k1)
link.max <- max(unlist(nbdists(k1, coords=coords)))
link.max

## create neighbour matrix with 50km und 25 km
distw.50 <- dnearneigh(coords, 0, 50000, row.names=shp_merged@data$Id) 
W.list50 <- nb2listw(distw.50, style="W", zero.policy=FALSE)

distw.25 <- dnearneigh(coords, 0, 25000, row.names=shp_merged@data$Id) 
W.list25 <- nb2listw(distw.25, style="W", zero.policy=FALSE)

# Before we regress, check if there is spatial structure in the data

## Spatial Lags
#lag.listw gives us depending on neighbour definition from above average growth of all the neighbours

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


##############################################################
##### Performing the Regression  #####
##############################################################

## Start from Linear OLS Model
names(d2)
#1. with AL
# d.reg <- d2[,c("fpoe.anteil", "UR_TYP3", "AL", "ANT.MIG", "ANT.JUNG", "ANT.ALT", "ANT.NQ")]
# d.reg <- na.omit(d.reg)

lin.urby1 <- lm(fpoe.anteil ~ UR_TYP3 + AL, data=shp_merged)
summary(lin.urby1)
confint(lin.urby1)
shp_merged$lmresiduals1 <- residuals(lin.urby1)

#2. with AL and migration background
lin.urby2 <- lm(fpoe.anteil ~ UR_TYP3 + AL + ANT.MIG, data=shp_merged)
summary(lin.urby2)
confint(lin.urby2)
shp_merged$lmresiduals2 <- residuals(lin.urby2)

#3. with AL and migration background and age
lin.urby3 <- lm(fpoe.anteil ~ UR_TYP3 + AL + ANT.MIG + ANT.JUNG +ANT.ALT, data=shp_merged)
summary(lin.urby3)
confint(lin.urby3)
shp_merged$lmresiduals3 <- residuals(lin.urby3)

#4. with AL and migration background and age and education
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

##############################################################
##### Is there spatial dependence in the OLS errors?  #####
##############################################################


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
#lm test for spatial dependence in OLS residuals
res <- lm.LMtests(lin.urby5, x, test="all")
tres <- t(sapply(res, function(x) c(x$statistic, x$parameter, x$p.value)))
colnames(tres) <- c("Statistc", "df", "p-value")


##Errormodell looks good, LAGMODELL looks good --> robust test (RLMerr)

printCoefmat(tres)

# Now: estimate model
# Spatial LAG Model (SAR)
# lagsarlm = (Lagmodell)

sar <- lagsarlm(formula=formula(lin.urby5), listw=x,data=shp_merged)
summary(sar, correlation=FALSE)

# Impacts - splitting in indirect and direct effects
sar.impacts<-impacts(sar, listw = x)
x <- sar.impacts
x[] <- lapply(x, round,4)
x

##############################################################
##### Regression Output  #####
##############################################################
stargazer(lin.urby4, lin.urby5, sar, type = "text", out ="./output/FE-SL-Regressions.txt",
          dep.var.labels=c("Share FPÖ", "Share FPÖ"),
          covariate.labels=c("Urbanity degree 2","Urbanity degree 3","urbanity degree 4","Unemployment","Share migrants","Share young people (15-29)","Share old people (>65)","Share low education","Rust(Burgenland)","Eisenstadt-Umgebund(Burgenland)","Wien 23.(Liesing) [113 omitted]"), 
          omit = y, title = "Comparison: LM vs LM (fixed effects) vs. Spatial Lag", single.row = TRUE)
