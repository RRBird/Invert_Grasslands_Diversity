
#Author: Rhiannon Bird
#Written under version R 4.5.1


options(scipen = 999) #So R doesn't use scientific notation

#Libraries----

library("AICcmodavg")
library("glmmTMB")
library('lme4')

#ALL SPECIES RICHNESS----

head(TaxModel)
str(TaxModel)


#Include Day?----

Rich_null <- glmmTMB(Species_Rich ~ 1 + (1 | Property), family = nbinom2, data = TaxModel)
Rich_Day <- glmmTMB(Species_Rich ~ Day_Sampled + (1 | Property), family = nbinom2, data = TaxModel)


aictab(list("Null" = Rich_null,"Day" = Rich_null))
#no need to include day in next step

#Environmental Models ----

##Single----

head(TaxModel)

Rich_E <- glmmTMB(Species_Rich ~ Elevation + (1 | Property), family = nbinom2, data = TaxModel)

Rich_H <- glmmTMB(Species_Rich ~ Plant_Height + (1 | Property), family = nbinom2, data = TaxModel)

Rich_GC <- glmmTMB(Species_Rich ~ Ground_Cover + (1 | Property), family = nbinom2, data = TaxModel)

Rich_GGC <- glmmTMB(Species_Rich ~ Prop_Green_GC + (1 | Property), family = nbinom2, data = TaxModel)

Rich_WE <- glmmTMB(Species_Rich ~ Weed_Estimate + (1 | Property), family = nbinom2, data = TaxModel)

Rich_GS <- glmmTMB(Species_Rich ~ Grass_Status + (1 | Property), family = nbinom2, data = TaxModel)

Rich_C <- glmmTMB(Species_Rich ~ Cropping_500m + (1 | Property), family = nbinom2, data = TaxModel)

Rich_LS <- glmmTMB(Species_Rich ~ X500m.Simspson + (1 | Property), family = nbinom2, data = TaxModel)

Rich_LC <- glmmTMB(Species_Rich ~ X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = TaxModel)

richmodlist <- list("null" = Rich_null, "Elevation" = Rich_E,
                    "Height"=Rich_H, "GC" = Rich_GC, 
                    "Green GC" = Rich_GGC, "Weed Estimate" = Rich_WE,
                    "Grass Status" = Rich_GS, "Crops" = Rich_C,
                    "Lanscape Simpson" = Rich_LS, 
                    "Landscape Class" = Rich_LC)

aictab(richmodlist)
#Green GC and Elevation

##Additive----

head(TaxModel)
names(TaxModel)


Rich_E_H <- glmmTMB(Species_Rich ~ Elevation + Plant_Height + (1 | Property), family = nbinom2, data = TaxModel)
Rich_E_GC <- glmmTMB(Species_Rich ~ Elevation + Ground_Cover + (1 | Property), family = nbinom2, data = TaxModel)
Rich_E_GGC <- glmmTMB(Species_Rich ~ Elevation + Prop_Green_GC + (1 | Property), family = nbinom2, data = TaxModel)
Rich_E_WE <- glmmTMB(Species_Rich ~ Elevation + Weed_Estimate + (1 | Property), family = nbinom2, data = TaxModel)
Rich_E_GS <- glmmTMB(Species_Rich ~ Elevation + Grass_Status + (1 | Property), family = nbinom2, data = TaxModel)
Rich_E_C <- glmmTMB(Species_Rich ~ Elevation + Cropping_500m + (1 | Property), family = nbinom2, data = TaxModel)
Rich_E_LS <- glmmTMB(Species_Rich ~ Elevation + X500m.Simspson + (1 | Property), family = nbinom2, data = TaxModel)
Rich_E_LC <- glmmTMB(Species_Rich ~ Elevation + X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = TaxModel)


Rich_H_GC <- glmmTMB(Species_Rich ~ Plant_Height + Ground_Cover + (1 | Property), family = nbinom2, data = TaxModel)
Rich_H_GGC <- glmmTMB(Species_Rich ~ Plant_Height + Prop_Green_GC + (1 | Property), family = nbinom2, data = TaxModel)
Rich_H_WE <- glmmTMB(Species_Rich ~ Plant_Height + Weed_Estimate + (1 | Property), family = nbinom2, data = TaxModel)
Rich_H_GS <- glmmTMB(Species_Rich ~ Plant_Height + Grass_Status + (1 | Property), family = nbinom2, data = TaxModel)
Rich_H_C <- glmmTMB(Species_Rich ~ Plant_Height + Cropping_500m + (1 | Property), family = nbinom2, data = TaxModel)
Rich_H_LS <- glmmTMB(Species_Rich ~ Plant_Height + X500m.Simspson + (1 | Property), family = nbinom2, data = TaxModel)
Rich_H_LC <- glmmTMB(Species_Rich ~ Plant_Height + X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = TaxModel)


Rich_GC_GGC <- glmmTMB(Species_Rich ~ Ground_Cover + Prop_Green_GC + (1 | Property), family = nbinom2, data = TaxModel)
Rich_GC_WE <- glmmTMB(Species_Rich ~ Ground_Cover + Weed_Estimate + (1 | Property), family = nbinom2, data = TaxModel)
Rich_GC_GS <- glmmTMB(Species_Rich ~ Ground_Cover + Grass_Status + (1 | Property), family = nbinom2, data = TaxModel)
Rich_GC_C <- glmmTMB(Species_Rich ~ Ground_Cover + Cropping_500m + (1 | Property), family = nbinom2, data = TaxModel)
Rich_GC_LS <- glmmTMB(Species_Rich ~ Ground_Cover + X500m.Simspson + (1 | Property), family = nbinom2, data = TaxModel)
Rich_GC_LC <- glmmTMB(Species_Rich ~ Ground_Cover + X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = TaxModel)

Rich_GGC_WE <- glmmTMB(Species_Rich ~ Prop_Green_GC + Weed_Estimate + (1 | Property), family = nbinom2, data = TaxModel)
Rich_GGC_GS <- glmmTMB(Species_Rich ~ Prop_Green_GC + Grass_Status + (1 | Property), family = nbinom2, data = TaxModel)
Rich_GGC_C <- glmmTMB(Species_Rich ~ Prop_Green_GC + Cropping_500m + (1 | Property), family = nbinom2, data = TaxModel)
Rich_GGC_LS <- glmmTMB(Species_Rich ~ Prop_Green_GC + X500m.Simspson + (1 | Property), family = nbinom2, data = TaxModel)
Rich_GGC_LC <- glmmTMB(Species_Rich ~ Prop_Green_GC + X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = TaxModel)

Rich_WE_GS <- glmmTMB(Species_Rich ~ Weed_Estimate + Grass_Status + (1 | Property), family = nbinom2, data = TaxModel)
Rich_WE_C <- glmmTMB(Species_Rich ~ Weed_Estimate + Cropping_500m + (1 | Property), family = nbinom2, data = TaxModel)
Rich_WE_LS <- glmmTMB(Species_Rich ~ Weed_Estimate + X500m.Simspson + (1 | Property), family = nbinom2, data = TaxModel)
Rich_WE_LC <- glmmTMB(Species_Rich ~ Weed_Estimate + X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = TaxModel)

Rich_GS_C <- glmmTMB(Species_Rich ~ Grass_Status + Cropping_500m + (1 | Property), family = nbinom2, data = TaxModel)
Rich_GS_LS <- glmmTMB(Species_Rich ~ Grass_Status + X500m.Simspson + (1 | Property), family = nbinom2, data = TaxModel)
Rich_GS_LC <- glmmTMB(Species_Rich ~ Grass_Status + X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = TaxModel)

Rich_C_LS <- glmmTMB(Species_Rich ~ Cropping_500m + X500m.Simspson + (1 | Property), family = nbinom2, data = TaxModel)
Rich_C_LC <- glmmTMB(Species_Rich ~ Cropping_500m + X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = TaxModel)

Rich_LS_LC <- glmmTMB(Species_Rich ~ X500m.Simspson + X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = TaxModel)


Modnames <- c("Rich_null","Rich_E_H","Rich_E_GC",
              "Rich_E_GGC","Rich_E_WE","Rich_E_GS",
              "Rich_E_C","Rich_E_LS","Rich_E_LC","Rich_H_GC",
              "Rich_H_GGC","Rich_H_WE","Rich_H_GS",
              "Rich_H_C","Rich_H_LS","Rich_H_LC",
              "Rich_GC_GGC","Rich_GC_WE","Rich_GC_GS",
              "Rich_GC_C","Rich_GC_LS","Rich_GC_LC",
              "Rich_GGC_WE","Rich_GGC_GS","Rich_GGC_C",
              "Rich_GGC_LS","Rich_GGC_LC","Rich_WE_GS", 
              "Rich_WE_C","Rich_WE_LS","Rich_WE_LC", 
              "Rich_GS_C","Rich_GS_LS","Rich_GS_LC", 
              "Rich_C_LS","Rich_C_LC","Rich_LS_LC")
richmodlist2 <- mget(Modnames)

aictab(richmodlist2)
#Elevation + Landscape class
#Elevation + Landscape Simpson
#Green GC + Grass status
#Elevation + Green GC
#Height + Green GC


##Interactive----

head(TaxModel)
names(TaxModel)

TaxModel$GC_Scaled <- scale(TaxModel$Ground_Cover)


Rich_ExH <- glmmTMB(Species_Rich ~ Elevation * Plant_Height + (1 | Property), family = nbinom2, data = TaxModel)
Rich_ExGC <- glmmTMB(Species_Rich ~ Elevation * Ground_Cover + (1 | Property), family = nbinom2, data = TaxModel)
Rich_ExGGC <- glmmTMB(Species_Rich ~ Elevation * Prop_Green_GC + (1 | Property), family = nbinom2, data = TaxModel)
Rich_ExWE <- glmmTMB(Species_Rich ~ Elevation * Weed_Estimate + (1 | Property), family = nbinom2, data = TaxModel)
Rich_ExGS <- glmmTMB(Species_Rich ~ Elevation * Grass_Status + (1 | Property), family = nbinom2, data = TaxModel)
Rich_ExC <- glmmTMB(Species_Rich ~ Elevation * Cropping_500m + (1 | Property), family = nbinom2, data = TaxModel)
Rich_ExLS <- glmmTMB(Species_Rich ~ Elevation * X500m.Simspson + (1 | Property), family = nbinom2, data = TaxModel)
Rich_ExLC <- glmmTMB(Species_Rich ~ Elevation * X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = TaxModel)

Rich_HxGC <- glmmTMB(Species_Rich ~ Plant_Height * Ground_Cover + (1 | Property), family = nbinom2, data = TaxModel)
Rich_HxGGC <- glmmTMB(Species_Rich ~ Plant_Height * Prop_Green_GC + (1 | Property), family = nbinom2, data = TaxModel)
Rich_HxWE <- glmmTMB(Species_Rich ~ Plant_Height * Weed_Estimate + (1 | Property), family = nbinom2, data = TaxModel)
Rich_HxGS <- glmmTMB(Species_Rich ~ Plant_Height * Grass_Status + (1 | Property), family = nbinom2, data = TaxModel)
Rich_HxC <- glmmTMB(Species_Rich ~ Plant_Height * Cropping_500m + (1 | Property), family = nbinom2, data = TaxModel)
Rich_HxLS <- glmmTMB(Species_Rich ~ Plant_Height * X500m.Simspson + (1 | Property), family = nbinom2, data = TaxModel)
Rich_HxLC <- glmmTMB(Species_Rich ~ Plant_Height * X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = TaxModel)

Rich_GCxGGC <- glmmTMB(Species_Rich ~ Ground_Cover * Prop_Green_GC + (1 | Property), family = nbinom2, data = TaxModel)
Rich_GCxWE <- glmmTMB(Species_Rich ~ GC_Scaled * Weed_Estimate + (1 | Property), family = nbinom2, data = TaxModel) #Didn't converge (even with scaled variable)
Rich_GCxGS <- glmmTMB(Species_Rich ~ Ground_Cover * Grass_Status + (1 | Property), family = nbinom2, data = TaxModel)
Rich_GCxC <- glmmTMB(Species_Rich ~ Ground_Cover * Cropping_500m + (1 | Property), family = nbinom2, data = TaxModel)
Rich_GCxLS <- glmmTMB(Species_Rich ~ Ground_Cover * X500m.Simspson + (1 | Property), family = nbinom2, data = TaxModel)
Rich_GCxLC <- glmmTMB(Species_Rich ~ Ground_Cover * X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = TaxModel)

Rich_GGCxWE <- glmmTMB(Species_Rich ~ Prop_Green_GC * Weed_Estimate + (1 | Property), family = nbinom2, data = TaxModel)
Rich_GGCxGS <- glmmTMB(Species_Rich ~ Prop_Green_GC * Grass_Status + (1 | Property), family = nbinom2, data = TaxModel)
Rich_GGCxC <- glmmTMB(Species_Rich ~ Prop_Green_GC * Cropping_500m + (1 | Property), family = nbinom2, data = TaxModel)
Rich_GGCxLS <- glmmTMB(Species_Rich ~ Prop_Green_GC * X500m.Simspson + (1 | Property), family = nbinom2, data = TaxModel)
Rich_GGCxLC <- glmmTMB(Species_Rich ~ Prop_Green_GC * X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = TaxModel)

Rich_WExC <- glmmTMB(Species_Rich ~ Weed_Estimate * Cropping_500m + (1 | Property), family = nbinom2, data = TaxModel)
Rich_WExLS <- glmmTMB(Species_Rich ~ Weed_Estimate * X500m.Simspson + (1 | Property), family = nbinom2, data = TaxModel)

Rich_GSxC <- glmmTMB(Species_Rich ~ Grass_Status * Cropping_500m + (1 | Property), family = nbinom2, data = TaxModel)
Rich_GSxLS <- glmmTMB(Species_Rich ~ Grass_Status * X500m.Simspson + (1 | Property), family = nbinom2, data = TaxModel)

Rich_CxLS <- glmmTMB(Species_Rich ~ Cropping_500m * X500m.Simspson + (1 | Property), family = nbinom2, data = TaxModel)
Rich_CxLC <- glmmTMB(Species_Rich ~ Cropping_500m * X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = TaxModel)

Rich_LSxLC <- glmmTMB(Species_Rich ~ X500m.Simspson * X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = TaxModel)


Modnames2 <- c("Rich_null","Rich_ExH","Rich_ExGC",
              "Rich_ExGGC","Rich_ExWE","Rich_ExGS",
              "Rich_ExC","Rich_ExLS","Rich_ExLC","Rich_HxGC",
              "Rich_HxGGC","Rich_HxWE","Rich_HxGS",
              "Rich_HxC","Rich_HxLS","Rich_HxLC",
              "Rich_GCxGGC","Rich_GCxGS",
              "Rich_GCxC","Rich_GCxLS","Rich_GCxLC",
              "Rich_GGCxWE","Rich_GGCxGS","Rich_GGCxC",
              "Rich_GGCxLS","Rich_GGCxLC", "Rich_WExC",
              "Rich_WExLS", "Rich_GSxC","Rich_GSxLS", 
              "Rich_CxLS","Rich_CxLC","Rich_LSxLC")
richmodlist2 <- mget(Modnames2)

aictab(richmodlist2)
#Elevation X Ground Cover
#Elevation X Landscape Simpson
#Height X Green GC
#Elevation X Green GC
#Elevation x crops

##Final AICC----

Modnames3<- c("Rich_null", "Rich_E","Rich_GGC", "Rich_E_LC",
              "Rich_E_LS","Rich_GGC_GS", "Rich_E_GGC", "Rich_H_GGC",
              "Rich_ExGC","Rich_ExLS","Rich_HxGGC","Rich_ExGGC")
richmodlist_Final <- mget(Modnames3)
aictab(richmodlist_Final)

#Top model = Elevation + Landscape Class

#Equivalent models:
##Elevation + Landscape Simpson
##Green Ground Cover + Grass Status
##Elevation + Green Ground Cover
##Height + Green Ground Cover
## Elevation x Ground Cover
##Green Ground Cover


#Predictions----

##Elevation + Landscape Class
summary(Rich_E_LC)

Predictions_Elevation <- seq(min(TaxModel$Elevation),max(TaxModel$Elevation),length.out=20)

toprichpred <- expand.grid(Elevation = Predictions_Elevation, X500m.Dominant.Landscape.Class = unique(TaxModel$X500m.Dominant.Landscape.Class))
head(toprichpred);dim(toprichpred)

toprichpred1 <- predict(object = Rich_E_LC,newdata= toprichpred,se.fit = T, type = "link",re.form = NA)

toprichpred2<-data.frame(toprichpred,fit.link=toprichpred1$fit,se.link=toprichpred1$se.fit)

toprichpred2$lci.link<-toprichpred2$fit.link-
  (1.96*toprichpred2$se.link)
toprichpred2$uci.link<-toprichpred2$fit.link+
  (1.96*toprichpred2$se.link)

toprichpred2$fit<-exp(toprichpred2$fit.link)
toprichpred2$se<-exp(toprichpred2$se.link)
toprichpred2$lci<-exp(toprichpred2$lci.link)
toprichpred2$uci<-exp(toprichpred2$uci.link)

head(toprichpred2);dim(toprichpred2)


#Elevation + Landscape Simpson
summary(Rich_E_LS)

Predictions_LS <- seq(min(TaxModel$X500m.Simspson),max(TaxModel$X500m.Simspson),length.out=20)

second_richpred <- expand.grid(Elevation = Predictions_Elevation, X500m.Simspson = Predictions_LS)
head(second_richpred);dim(second_richpred)

second_richpred1 <- predict(object = Rich_E_LS,newdata= second_richpred,se.fit = T, type = "link",re.form = NA)

second_richpred2<-data.frame(second_richpred,fit.link=second_richpred1$fit,se.link=second_richpred1$se.fit)

second_richpred2$lci.link<-second_richpred2$fit.link-
  (1.96*second_richpred2$se.link)
second_richpred2$uci.link<-second_richpred2$fit.link+
  (1.96*second_richpred2$se.link)

second_richpred2$fit<-exp(second_richpred2$fit.link)
second_richpred2$se<-exp(second_richpred2$se.link)
second_richpred2$lci<-exp(second_richpred2$lci.link)
second_richpred2$uci<-exp(second_richpred2$uci.link)

head(second_richpred2);dim(second_richpred2)

#Green GC + Grass Status
summary(Rich_GGC_GS)

Predictions_GGC <- seq(min(TaxModel$Prop_Green_GC),max(TaxModel$Prop_Green_GC),length.out=20)

Third_richpred <- expand.grid(Prop_Green_GC = Predictions_GGC, Grass_Status  = unique(TaxModel$Grass_Status))
head(Third_richpred);dim(Third_richpred)

Third_richpred1 <- predict(object = Rich_GGC_GS,newdata= Third_richpred,se.fit = T, type = "link",re.form = NA)

Third_richpred2<-data.frame(Third_richpred,fit.link=Third_richpred1$fit,se.link=Third_richpred1$se.fit)

Third_richpred2$lci.link<-Third_richpred2$fit.link-
  (1.96*Third_richpred2$se.link)
Third_richpred2$uci.link<-Third_richpred2$fit.link+
  (1.96*Third_richpred2$se.link)

Third_richpred2$fit<-exp(Third_richpred2$fit.link)
Third_richpred2$se<-exp(Third_richpred2$se.link)
Third_richpred2$lci<-exp(Third_richpred2$lci.link)
Third_richpred2$uci<-exp(Third_richpred2$uci.link)

head(Third_richpred2);dim(Third_richpred2)

#Elevation + Green GC
summary(Rich_E_GGC)

Fourth_richpred <- expand.grid(Elevation  = Predictions_Elevation, Prop_Green_GC = Predictions_GGC)
head(Fourth_richpred);dim(Fourth_richpred)

Fourth_richpred1 <- predict(object = Rich_E_GGC,newdata= Fourth_richpred,se.fit = T, type = "link",re.form = NA)

Fourth_richpred2<-data.frame(Fourth_richpred,fit.link=Fourth_richpred1$fit,se.link=Fourth_richpred1$se.fit)

Fourth_richpred2$lci.link<-Fourth_richpred2$fit.link-
  (1.96*Fourth_richpred2$se.link)
Fourth_richpred2$uci.link<-Fourth_richpred2$fit.link+
  (1.96*Fourth_richpred2$se.link)

Fourth_richpred2$fit<-exp(Fourth_richpred2$fit.link)
Fourth_richpred2$se<-exp(Fourth_richpred2$se.link)
Fourth_richpred2$lci<-exp(Fourth_richpred2$lci.link)
Fourth_richpred2$uci<-exp(Fourth_richpred2$uci.link)

head(Fourth_richpred2);dim(Fourth_richpred2)

#Height + Green GC
summary(Rich_H_GGC)

Predictions_Height <- seq(min(TaxModel$Plant_Height),max(TaxModel$Plant_Height),length.out=20)

Fifth_richpred <- expand.grid(Plant_Height  = Predictions_Height, Prop_Green_GC = Predictions_GGC)
head(Fifth_richpred);dim(Fifth_richpred)

Fifth_richpred1 <- predict(object = Rich_H_GGC,newdata= Fifth_richpred,se.fit = T, type = "link",re.form = NA)

Fifth_richpred2<-data.frame(Fifth_richpred,fit.link=Fifth_richpred1$fit,se.link=Fifth_richpred1$se.fit)

Fifth_richpred2$lci.link<-Fifth_richpred2$fit.link-
  (1.96*Fifth_richpred2$se.link)
Fifth_richpred2$uci.link<-Fifth_richpred2$fit.link+
  (1.96*Fifth_richpred2$se.link)

Fifth_richpred2$fit<-exp(Fifth_richpred2$fit.link)
Fifth_richpred2$se<-exp(Fifth_richpred2$se.link)
Fifth_richpred2$lci<-exp(Fifth_richpred2$lci.link)
Fifth_richpred2$uci<-exp(Fifth_richpred2$uci.link)

head(Fifth_richpred2);dim(Fifth_richpred2)

#Elevation + GC
summary(Rich_E_GC)

Predictions_GC <- seq(min(TaxModel$Ground_Cover),max(TaxModel$Ground_Cover),length.out=20)

Sixth_richpred <- expand.grid(Elevation  = Predictions_Elevation, Ground_Cover = Predictions_GC)
head(Sixth_richpred);dim(Sixth_richpred)

Sixth_richpred1 <- predict(object = Rich_E_GC,newdata= Sixth_richpred,se.fit = T, type = "link",re.form = NA)

Sixth_richpred2<-data.frame(Sixth_richpred,fit.link=Sixth_richpred1$fit,se.link=Sixth_richpred1$se.fit)

Sixth_richpred2$lci.link<-Sixth_richpred2$fit.link-
  (1.96*Sixth_richpred2$se.link)
Sixth_richpred2$uci.link<-Sixth_richpred2$fit.link+
  (1.96*Sixth_richpred2$se.link)

Sixth_richpred2$fit<-exp(Sixth_richpred2$fit.link)
Sixth_richpred2$se<-exp(Sixth_richpred2$se.link)
Sixth_richpred2$lci<-exp(Sixth_richpred2$lci.link)
Sixth_richpred2$uci<-exp(Sixth_richpred2$uci.link)

head(Sixth_richpred2);dim(Sixth_richpred2)

#Green Ground Cover
summary(Rich_GGC)

Seventh_richpred <- data.frame(Prop_Green_GC  = Predictions_GGC)
head(Seventh_richpred);dim(Seventh_richpred)

Seventh_richpred1 <- predict(object = Rich_GGC,newdata= Seventh_richpred,se.fit = T, type = "link",re.form = NA)

Seventh_richpred2<-data.frame(Seventh_richpred,fit.link=Seventh_richpred1$fit,se.link=Seventh_richpred1$se.fit)

Seventh_richpred2$lci.link<-Seventh_richpred2$fit.link-
  (1.96*Seventh_richpred2$se.link)
Seventh_richpred2$uci.link<-Seventh_richpred2$fit.link+
  (1.96*Seventh_richpred2$se.link)

Seventh_richpred2$fit<-exp(Seventh_richpred2$fit.link)
Seventh_richpred2$se<-exp(Seventh_richpred2$se.link)
Seventh_richpred2$lci<-exp(Seventh_richpred2$lci.link)
Seventh_richpred2$uci<-exp(Seventh_richpred2$uci.link)

head(Seventh_richpred2);dim(Seventh_richpred2)


#Main Figure----

#Elevation + Landscape Class
summary(Rich_E_LC)
head(toprichpred2);dim(toprichpred2)

AA <- toprichpred2$X500m.Dominant.Landscape.Class == "NTV_Woody_Closed"
A_A <- toprichpred2$Elevation == Predictions_Elevation[10]

raw_x <- ifelse(TaxModel$X500m.Dominant.Landscape.Class ==
                  "NTV_Woody_Closed", 1, 
                ifelse(TaxModel$X500m.Dominant.Landscape.Class ==
                         "NTV_Herbaceous_Open", 2, 
                       ifelse(
                         TaxModel$X500m.Dominant.Landscape.Class ==
                                "NTV_Woody_Open", 3, NA)))


dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = TaxModel$Elevation,y = TaxModel$Species_Rich,xlab = expression("Elevation (m)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = 60,'a)',cex=1.1)

polygon(x = c(toprichpred2$Elevation[AA],rev(toprichpred2$Elevation[AA])), y = c(toprichpred2$lci[AA],rev(toprichpred2$uci[AA])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=toprichpred2$Elevation[AA],y = toprichpred2$fit[AA],lwd = 2,col = 'grey30')


plot(x = 1:3,y = toprichpred2$fit [A_A],xlab = " ",ylab = 'Species Richness', type = 'p',pch = 16,cex =2.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,4),ylim = c(0,16))
axis(side=1,at=1:3,labels=c(' ',' ', " "))
arrows(x0=1:3, y0=toprichpred2$lci [A_A],x1=1:3, y1=toprichpred2$uci[A_A],angle=90,length=0.2, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.3,'b)',cex=1.1)
mtext(side=1,line=1.5,at = 0.8,'Woody\nClosed',cex=1.1)
mtext(side=1,line=1.5,at = 2,'Herbaceous\nOpen',cex=1.1)
mtext(side=1,line=1.5,at = 3.2,"Woody\nOpen",cex=1.1)

points(x = jitter(raw_x, factor = 1),y = TaxModel$Species_Rich, pch = 16, cex = 0.4, col = "black")


#Supporting plots----

#Elevation + Landscape Simpson
summary(Rich_E_LS)
head(second_richpred2)

BB <- second_richpred2$X500m.Simspson == Predictions_LS[10]
B_B <- second_richpred2$Elevation == Predictions_Elevation[10]

dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = TaxModel$Elevation,y = TaxModel$Species_Rich,xlab = expression("Elevation (m)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = 60,'a)',cex=1.1)

polygon(x = c(second_richpred2$Elevation[BB],rev(second_richpred2$Elevation[BB])), y = c(second_richpred2$lci[BB],rev(second_richpred2$uci[BB])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=second_richpred2$Elevation[BB],y = second_richpred2$fit[BB],lwd = 2,col = 'grey30')


plot(x = TaxModel$X500m.Simspson,y = TaxModel$Species_Rich,xlab = expression("Landscape Diversity within 500m"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = 0,'b)',cex=1.1)

polygon(x = c(second_richpred2$X500m.Simspson[B_B],rev(second_richpred2$X500m.Simspson[B_B])), y = c(second_richpred2$lci[B_B],rev(second_richpred2$uci[B_B])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=second_richpred2$X500m.Simspson[B_B],y = second_richpred2$fit[B_B],lwd = 2,col = 'grey30')


#Green GC + Grass Status
summary(Rich_GGC_GS)
head(Third_richpred2)

CC <- Third_richpred2$Grass_Status == "Native"
C_C <- Third_richpred2$Prop_Green_GC == Predictions_GGC[10]

raw_x1 <- ifelse(TaxModel$Grass_Status =="Native", 1, 
                ifelse(TaxModel$Grass_Status =="Introduced", 2, NA))


dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = TaxModel$Prop_Green_GC,y = TaxModel$Species_Rich,xlab = expression("Green Ground Cover (%)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = 0,'a)',cex=1.1)

polygon(x = c(Third_richpred2$Prop_Green_GC[CC],rev(Third_richpred2$Prop_Green_GC[CC])), y = c(Third_richpred2$lci[CC],rev(Third_richpred2$uci[CC])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Third_richpred2$Prop_Green_GC[CC],y = Third_richpred2$fit[CC],lwd = 2,col = 'grey30')


plot(x = 1:2,y = Third_richpred2$fit[C_C][c(1,3)],xlab = " ",ylab = 'Species Richness', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,20))
axis(side=1,at=1:2,labels=c('Introduced','Native'))
arrows(x0=1:2, y0=Third_richpred2$lci [C_C][c(1,3)],x1=1:2, y1=Third_richpred2$uci[C_C][c(1,3)],angle=90,length=0.1, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.2,'b)',cex=1.1)

points(x = jitter(raw_x1, factor = 1),y = TaxModel$Species_Rich, pch = 16, cex = 0.4, col = "black")


#Elevation + Green GC
summary(Rich_E_GGC)
head(Fourth_richpred2)

DD <- Fourth_richpred2$Prop_Green_GC == Predictions_GGC[10]
D_D <- Fourth_richpred2$Elevation == Predictions_Elevation[10]

dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = TaxModel$Elevation,y = TaxModel$Species_Rich,xlab = expression("Elevation (m)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = 60,'a)',cex=1.1)

polygon(x = c(Fourth_richpred2$Elevation[DD],rev(Fourth_richpred2$Elevation[DD])), y = c(Fourth_richpred2$lci[DD],rev(Fourth_richpred2$uci[DD])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Fourth_richpred2$Elevation[DD],y = Fourth_richpred2$fit[DD],lwd = 2,col = 'grey30')


plot(x = TaxModel$Prop_Green_GC,y = TaxModel$Species_Rich,xlab = expression("Green Ground Cover (%)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = 0,'b)',cex=1.1)

polygon(x = c(Fourth_richpred2$Prop_Green_GC[D_D],rev(Fourth_richpred2$Prop_Green_GC[D_D])), y = c(Fourth_richpred2$lci[D_D],rev(Fourth_richpred2$uci[D_D])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Fourth_richpred2$Prop_Green_GC[D_D],y = Fourth_richpred2$fit[D_D],lwd = 2,col = 'grey30')

#Height + Green GC
summary(Rich_H_GGC)
head(Fifth_richpred2)

EE <- Fifth_richpred2$Prop_Green_GC == Predictions_GGC[10]
E_E <- Fifth_richpred2$Plant_Height == Predictions_Height[10]

dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = TaxModel$Plant_Height,y = TaxModel$Species_Rich,xlab = expression("Grass Height (cm)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = -2,'a)',cex=1.1)

polygon(x = c(Fifth_richpred2$Plant_Height[EE],rev(Fifth_richpred2$Plant_Height[EE])), y = c(Fifth_richpred2$lci[EE],rev(Fifth_richpred2$uci[EE])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Fifth_richpred2$Plant_Height[EE],y = Fifth_richpred2$fit[EE],lwd = 2,col = 'grey30')


plot(x = TaxModel$Prop_Green_GC,y = TaxModel$Species_Rich,xlab = expression("Green Ground Cover (%)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = 0,'b)',cex=1.1)

polygon(x = c(Fifth_richpred2$Prop_Green_GC[E_E],rev(Fifth_richpred2$Prop_Green_GC[E_E])), y = c(Fifth_richpred2$lci[E_E],rev(Fifth_richpred2$uci[E_E])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Fifth_richpred2$Prop_Green_GC[E_E],y = Fifth_richpred2$fit[E_E],lwd = 2,col = 'grey30')

#Elevation X Ground Cover
summary(Rich_E_GC)
head(Sixth_richpred2)

FF <- Sixth_richpred2$Elevation == Predictions_Elevation[1]
F_F <- Sixth_richpred2$Elevation == Predictions_Elevation[20]

dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = TaxModel$Ground_Cover,y = TaxModel$Species_Rich,xlab = expression("Ground Cover (%)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)

polygon(x = c(Sixth_richpred2$Ground_Cover[FF],rev(Sixth_richpred2$Ground_Cover[FF])), y = c(Sixth_richpred2$lci[FF],rev(Sixth_richpred2$uci[FF])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Sixth_richpred2$Ground_Cover[FF],y = Sixth_richpred2$fit[FF],lwd = 2,col = 'grey30')


polygon(x = c(Sixth_richpred2$Ground_Cover[F_F],rev(Sixth_richpred2$Ground_Cover[F_F])), y = c(Sixth_richpred2$lci[F_F],rev(Sixth_richpred2$uci[F_F])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Sixth_richpred2$Ground_Cover[F_F],y = Sixth_richpred2$fit[F_F],lwd = 2,lty = 3,col = 'grey30')

legend('topleft',legend = c('Low Elevation', "High Elevation"), lty = c(1,2), col = 'grey30',pt.cex = 1)


#Green GC
summary(Rich_GGC)
head(Seventh_richpred2)

dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = TaxModel$Prop_Green_GC,y = TaxModel$Species_Rich,xlab = expression("Green Ground Cover (%)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = 1,'a)',cex=1.1)

polygon(x = c(Seventh_richpred2$Prop_Green_GC,rev(Seventh_richpred2$Prop_Green_GC)), y = c(Seventh_richpred2$lci,rev(Seventh_richpred2$uci)),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Seventh_richpred2$Prop_Green_GC,y = Seventh_richpred2$fit,lwd = 2,col = 'grey30')

#Supporting Figure----

##Elevation + Landscape Simpson
##Green Ground Cover + Grass Status
##Elevation + Green Ground Cover
##Height + Green Ground Cover
## Elevation X Ground Cover
##Green Ground Cover

#so needed in figure: Elevation - Landscape Simpson - GGC - Grass Status - Height - ElevationxGC 


dev.new(height=10,width=15,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(2,3),mgp=c(2.5,1,0),xpd = T)

plot(x = TaxModel$Elevation,y = TaxModel$Species_Rich,xlab = expression("Elevation (m)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.axis = 1.2, cex.lab=1.3)
mtext(side=3,line=0,at = 60,'a)',cex=0.8)

polygon(x = c(second_richpred2$Elevation[BB],rev(second_richpred2$Elevation[BB])), y = c(second_richpred2$lci[BB],rev(second_richpred2$uci[BB])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=second_richpred2$Elevation[BB],y = second_richpred2$fit[BB],lwd = 2,col = 'grey30')


plot(x = TaxModel$X500m.Simspson,y = TaxModel$Species_Rich,xlab = expression("Habitat Diversity within 500m"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.axis = 1.2, cex.lab=1.3)
mtext(side=3,line=0,at = 0,'b)',cex=0.8)

polygon(x = c(second_richpred2$X500m.Simspson[B_B],rev(second_richpred2$X500m.Simspson[B_B])), y = c(second_richpred2$lci[B_B],rev(second_richpred2$uci[B_B])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=second_richpred2$X500m.Simspson[B_B],y = second_richpred2$fit[B_B],lwd = 2,col = 'grey30')


plot(x = TaxModel$Prop_Green_GC,y = TaxModel$Species_Rich,xlab = expression("Green Ground Cover (%)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.axis = 1.2, cex.lab=1.3)
mtext(side=3,line=0,at = 0,'c)',cex=0.8)

polygon(x = c(Third_richpred2$Prop_Green_GC[CC],rev(Third_richpred2$Prop_Green_GC[CC])), y = c(Third_richpred2$lci[CC],rev(Third_richpred2$uci[CC])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Third_richpred2$Prop_Green_GC[CC],y = Third_richpred2$fit[CC],lwd = 2,col = 'grey30')


plot(x = 1:2,y = Third_richpred2$fit[C_C][c(1,3)],xlab = " ",ylab = 'Species Richness', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,20),cex.axis = 1.2, cex.lab=1.3)
axis(side=1,at=c(0.9,2.1),labels=c('Introduced','Native'),cex.axis=1.4)
arrows(x0=1:2, y0=Third_richpred2$lci [C_C][c(1,3)],x1=1:2, y1=Third_richpred2$uci[C_C][c(1,3)],angle=90,length=0.1, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.2,'d)',cex=0.8)

points(x = jitter(raw_x1, factor = 1),y = TaxModel$Species_Rich, pch = 16, cex = 0.5, col = "black")


plot(x = TaxModel$Plant_Height,y = TaxModel$Species_Rich,xlab = expression("Grass Height (cm)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.axis = 1.2, cex.lab=1.3)
mtext(side=3,line=0,at = -2,'e)',cex=0.8)

polygon(x = c(Fifth_richpred2$Plant_Height[EE],rev(Fifth_richpred2$Plant_Height[EE])), y = c(Fifth_richpred2$lci[EE],rev(Fifth_richpred2$uci[EE])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Fifth_richpred2$Plant_Height[EE],y = Fifth_richpred2$fit[EE],lwd = 2,col = 'grey30')


plot(x = TaxModel$Ground_Cover,y = TaxModel$Species_Rich,xlab = expression("Ground Cover (%)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.axis = 1.2, cex.lab=1.3)
mtext(side=3,line=0,at = 33,'f)',cex=0.8)

polygon(x = c(Sixth_richpred2$Ground_Cover[FF],rev(Sixth_richpred2$Ground_Cover[FF])), y = c(Sixth_richpred2$lci[FF],rev(Sixth_richpred2$uci[FF])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Sixth_richpred2$Ground_Cover[FF],y = Sixth_richpred2$fit[FF],lwd = 2,col = 'grey30')

polygon(x = c(Sixth_richpred2$Ground_Cover[F_F],rev(Sixth_richpred2$Ground_Cover[F_F])), y = c(Sixth_richpred2$lci[F_F],rev(Sixth_richpred2$uci[F_F])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Sixth_richpred2$Ground_Cover[F_F],y = Sixth_richpred2$fit[F_F],lwd = 2,lty = 3,col = 'grey30')

legend('topleft',legend = c('Low Elevation', "High Elevation"), lty = c(1,2), col = 'grey30',pt.cex = 1)



#ALL SPECIES DIVERSITY----

head(TaxModel)
str(TaxModel)

#update all 0 to be 0.000001 for model fitting
TaxModel$Diversity[TaxModel$Diversity==0] <- 0.000001


#Include Day?----

Div_null <- glmer(Diversity ~ 1 + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_Day <- glmer(Diversity ~ Day_Sampled + (1 | Property), family = Gamma(link = "log"), data = TaxModel)


aictab(list("Null" = Div_null,"Day" = Div_null))
#no need to include day in next step

#Environmental Models ----

##Single----

head(TaxModel)

Div_E <- glmer(Diversity ~ Elevation + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_H <- glmer(Diversity ~ Plant_Height + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_GC <- glmer(Diversity ~ Ground_Cover + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_GGC <- glmer(Diversity ~ Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_WE <- glmer(Diversity ~ Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_GS <- glmer(Diversity ~ Grass_Status + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_C <- glmer(Diversity ~ Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_LS <- glmer(Diversity ~ X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_LC <- glmer(Diversity ~ X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_modnames <- c("Div_null", "Div_E","Div_H", "Div_GC", "Div_GGC", 
                  "Div_WE", "Div_GS", "Div_C", "Div_LS","Div_LC")

Divmodlist <- mget(Div_modnames)
aictab(Divmodlist)
#Null

#Because the top is the null model additive models aren't  going to be any better so skipping straight to interactions



##Interactive----

head(TaxModel)
names(TaxModel)

TaxModel$Elevation_scaled <- scale(TaxModel$Elevation)
TaxModel$Height_scaled <- scale(TaxModel$Plant_Height)
TaxModel$GC_scaled <- scale(TaxModel$Ground_Cover)
TaxModel$Green_GC_scaled <- scale(TaxModel$Prop_Green_GC)


Div_ExH <- glmer(Diversity ~ Elevation_scaled * Plant_Height + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_ExGC <- glmer(Diversity ~ Elevation_scaled * Ground_Cover + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_ExGGC <- glmer(Diversity ~ Elevation_scaled * Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_ExWE <- glmer(Diversity ~ Elevation * Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_ExGS <- glmer(Diversity ~ Elevation * Grass_Status + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_ExC <- glmer(Diversity ~ Elevation_scaled * Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_ExLS <- glmer(Diversity ~ Elevation * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_ExLC <- glmer(Diversity ~ Elevation * X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_HxGC <- glmer(Diversity ~ Height_scaled * Ground_Cover + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_HxGGC <- glmer(Diversity ~ Plant_Height * Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_HxWE <- glmer(Diversity ~ Plant_Height * Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_HxGS <- glmer(Diversity ~ Plant_Height * Grass_Status + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_HxC <- glmer(Diversity ~ Plant_Height * Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_HxLS <- glmer(Diversity ~ Plant_Height * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_HxLC <- glmer(Diversity ~ Plant_Height * X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_GCxGGC <- glmer(Diversity ~ GC_scaled * Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GCxWE <- glmer(Diversity ~ Ground_Cover * Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GCxGS <- glmer(Diversity ~ Ground_Cover * Grass_Status + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GCxC <- glmer(Diversity ~ GC_scaled * Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GCxLS <- glmer(Diversity ~ Ground_Cover * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GCxLC <- glmer(Diversity ~ Ground_Cover * X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_GGCxWE <- glmer(Diversity ~ Prop_Green_GC * Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GGCxGS <- glmer(Diversity ~ Prop_Green_GC * Grass_Status + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GGCxC <- glmer(Diversity ~ Green_GC_scaled * Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GGCxLS <- glmer(Diversity ~ Prop_Green_GC * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GGCxLC <- glmer(Diversity ~ Prop_Green_GC * X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_WExC <- glmer(Diversity ~ Weed_Estimate * Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_WExLS <- glmer(Diversity ~ Weed_Estimate * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_GSxC <- glmer(Diversity ~ Grass_Status * Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GSxLS <- glmer(Diversity ~ Grass_Status * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_CxLS <- glmer(Diversity ~ Cropping_500m * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_CxLC <- glmer(Diversity ~ Cropping_500m * X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_LSxLC <- glmer(Diversity ~ X500m.Simspson * X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = TaxModel)


Divmodnames2 <- c("Div_null","Div_ExH","Div_ExGC",
               "Div_ExGGC","Div_ExWE","Div_ExGS",
               "Div_ExC","Div_ExLS","Div_ExLC","Div_HxGC",
               "Div_HxGGC","Div_HxWE","Div_HxGS",
               "Div_HxC","Div_HxLS","Div_HxLC",
               "Div_GCxGGC","Div_GCxWE","Div_GCxGS",
               "Div_GCxC","Div_GCxLS","Div_GCxLC",
               "Div_GGCxWE","Div_GGCxGS","Div_GGCxC",
               "Div_GGCxLS","Div_GGCxLC", "Div_WExC",
               "Div_WExLS", "Div_GSxC","Div_GSxLS", 
               "Div_CxLS","Div_CxLC","Div_LSxLC")
Divmodlist2 <- mget(Divmodnames2)

aictab(Divmodlist2)
#Still the null model at the top so that's as far as we get with that one



#END----