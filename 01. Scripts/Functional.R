
#Author: Rhiannon Bird
#Written under version R 4.5.1

#Used Methods for assessing functional responses to environmental gradients -- Kleyer et al. -- June 22, 2009

options(scipen = 999) #So R doesn't use scientific notation
write.xlsx(aictab(TG_F_modlist2), 'TEMPDOC.xlsx')

#Libraries----

library("AICcmodavg")
library("glmmTMB")
library('lme4')
library("openxlsx")

FDModel$Grass_Status[FDModel$Grass_Status=="Unknown"] <- NA


#Group A----

head(FDModel);dim(FDModel)

##Species Richness-----

###Modelling----


####Day

TG_A_Rich_null <- glmmTMB(A_Rich ~ 1 + (1 | Property), family = nbinom2, data = FDModel)
#issues with convergence - possibly don't need to do nbinom2

TG_A_Rich_null2 <- glmmTMB(A_Rich ~ 1 + (1 | Property), family = nbinom1, data = FDModel)
TG_A_Rich_null3 <- glmmTMB(A_Rich ~ 1 + (1 | Property), family = poisson, data = FDModel)
aictab(list("nbiom1" = TG_A_Rich_null2,"possion" = TG_A_Rich_null3))
#using possion now as it fits best with the data

TG_A_Rich_Day <- glmmTMB(A_Rich ~ Day_Sampled + (1 | Property), family = poisson, data = FDModel)


aictab(list("Null" = TG_A_Rich_null3,"Day" = TG_A_Rich_Day))
#no need to include day in next step

####Single

head(FDModel)

FDModel$Elevation_Scaled <- scale(FDModel$Elevation)


TG_A_Rich_E <- glmmTMB(A_Rich ~ Elevation_Scaled + (1 | Property), family = poisson, data = FDModel)

TG_A_Rich_H <- glmmTMB(A_Rich ~ Plant_Height + (1 | Property), family = poisson, data = FDModel)

TG_A_Rich_GC <- glmmTMB(A_Rich ~ Ground_Cover + (1 | Property), family = poisson, data = FDModel)

TG_A_Rich_GGC <- glmmTMB(A_Rich ~ Prop_Green_GC + (1 | Property), family = poisson, data = FDModel)

TG_A_Rich_WE <- glmmTMB(A_Rich ~ Weed_Estimate + (1 | Property), family = poisson, data = FDModel)

TG_A_Rich_GS <- glmmTMB(A_Rich ~ Grass_Status + (1 | Property), family = poisson, data = FDModel)

TG_A_Rich_C <- glmmTMB(A_Rich ~ Cropping_500m + (1 | Property), family = poisson, data = FDModel)

TG_A_Rich_LS <- glmmTMB(A_Rich ~ X500m.Simspson + (1 | Property), family = poisson, data = FDModel)

TG_A_Rich_LC <- glmmTMB(A_Rich ~ X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_A_richmodlist <- list("null" = TG_A_Rich_null3, 
                    "Elevation" = TG_A_Rich_E,
                    "Height"=TG_A_Rich_H, 
                    "GC" = TG_A_Rich_GC, 
                    "Green GC" = TG_A_Rich_GGC, 
                    "Weed Estimate" = TG_A_Rich_WE,
                    "Grass Status" = TG_A_Rich_GS, 
                    "Crops" = TG_A_Rich_C,
                    "Lanscape Simpson" = TG_A_Rich_LS, 
                    "Landscape Class" = TG_A_Rich_LC)

aictab(TG_A_richmodlist)
#Grass Status

####Additive

head(FDModel)
names(FDModel)


TG_A_Rich_E_H <- glmmTMB(A_Rich ~ Elevation_Scaled + Plant_Height + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_E_GC <- glmmTMB(A_Rich ~ Elevation_Scaled + Ground_Cover + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_E_GGC <- glmmTMB(A_Rich ~ Elevation_Scaled + Prop_Green_GC + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_E_WE <- glmmTMB(A_Rich ~ Elevation_Scaled + Weed_Estimate + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_E_GS <- glmmTMB(A_Rich ~ Elevation_Scaled + Grass_Status + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_E_C <- glmmTMB(A_Rich ~ Elevation_Scaled + Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_E_LS <- glmmTMB(A_Rich ~ Elevation_Scaled + X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_E_LC <- glmmTMB(A_Rich ~ Elevation_Scaled + X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)


TG_A_Rich_H_GC <- glmmTMB(A_Rich ~ Plant_Height + Ground_Cover + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_H_GGC <- glmmTMB(A_Rich ~ Plant_Height + Prop_Green_GC + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_H_WE <- glmmTMB(A_Rich ~ Plant_Height + Weed_Estimate + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_H_GS <- glmmTMB(A_Rich ~ Plant_Height + Grass_Status + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_H_C <- glmmTMB(A_Rich ~ Plant_Height + Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_H_LS <- glmmTMB(A_Rich ~ Plant_Height + X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_H_LC <- glmmTMB(A_Rich ~ Plant_Height + X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)


TG_A_Rich_GC_GGC <- glmmTMB(A_Rich ~ Ground_Cover + Prop_Green_GC + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GC_WE <- glmmTMB(A_Rich ~ Ground_Cover + Weed_Estimate + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GC_GS <- glmmTMB(A_Rich ~ Ground_Cover + Grass_Status + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GC_C <- glmmTMB(A_Rich ~ Ground_Cover + Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GC_LS <- glmmTMB(A_Rich ~ Ground_Cover + X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GC_LC <- glmmTMB(A_Rich ~ Ground_Cover + X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_A_Rich_GGC_WE <- glmmTMB(A_Rich ~ Prop_Green_GC + Weed_Estimate + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GGC_GS <- glmmTMB(A_Rich ~ Prop_Green_GC + Grass_Status + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GGC_C <- glmmTMB(A_Rich ~ Prop_Green_GC + Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GGC_LS <- glmmTMB(A_Rich ~ Prop_Green_GC + X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GGC_LC <- glmmTMB(A_Rich ~ Prop_Green_GC + X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_A_Rich_WE_GS <- glmmTMB(A_Rich ~ Weed_Estimate + Grass_Status + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_WE_C <- glmmTMB(A_Rich ~ Weed_Estimate + Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_WE_LS <- glmmTMB(A_Rich ~ Weed_Estimate + X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_WE_LC <- glmmTMB(A_Rich ~ Weed_Estimate + X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_A_Rich_GS_C <- glmmTMB(A_Rich ~ Grass_Status + Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GS_LS <- glmmTMB(A_Rich ~ Grass_Status + X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GS_LC <- glmmTMB(A_Rich ~ Grass_Status + X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_A_Rich_C_LS <- glmmTMB(A_Rich ~ Cropping_500m + X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_C_LC <- glmmTMB(A_Rich ~ Cropping_500m + X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_A_Rich_LS_LC <- glmmTMB(A_Rich ~ X500m.Simspson + X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)


TG_A_Modnames <- c("TG_A_Rich_null3","TG_A_Rich_E_H",
                   "TG_A_Rich_E_GC","TG_A_Rich_E_GGC",
                   "TG_A_Rich_E_WE","TG_A_Rich_E_GS",
                   "TG_A_Rich_E_C","TG_A_Rich_E_LS",
                   "TG_A_Rich_E_LC","TG_A_Rich_H_GC",
                   "TG_A_Rich_H_GGC","TG_A_Rich_H_WE",
                   "TG_A_Rich_H_GS","TG_A_Rich_H_C",
                   "TG_A_Rich_H_LS","TG_A_Rich_H_LC",
                   "TG_A_Rich_GC_GGC","TG_A_Rich_GC_WE",
                   "TG_A_Rich_GC_GS","TG_A_Rich_GC_C",
                   "TG_A_Rich_GC_LS","TG_A_Rich_GC_LC",
                   "TG_A_Rich_GGC_WE","TG_A_Rich_GGC_GS",
                   "TG_A_Rich_GGC_C","TG_A_Rich_GGC_LS",
                   "TG_A_Rich_GGC_LC","TG_A_Rich_WE_GS", 
                   "TG_A_Rich_WE_C","TG_A_Rich_WE_LS",
                   "TG_A_Rich_WE_LC","TG_A_Rich_GS_C",
                   "TG_A_Rich_GS_LS","TG_A_Rich_GS_LC",
                   "TG_A_Rich_C_LS","TG_A_Rich_C_LC",
                   "TG_A_Rich_LS_LC")
TG_A_richmodlist1 <- mget(TG_A_Modnames)

aictab(TG_A_richmodlist1)

#Ground Cover + Grass Status

####Interaction

head(FDModel)
names(FDModel)

FDModel$GC_Scaled <- scale(FDModel$Ground_Cover)
FDModel$Height_Scaled <- scale(FDModel$Plant_Height)
FDModel$Crops_Scaled <- scale(FDModel$Cropping_500m)

TG_A_Rich_ExH <- glmmTMB(A_Rich ~ Elevation_Scaled * Plant_Height + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_ExGC <- glmmTMB(A_Rich ~ Elevation_Scaled * GC_Scaled + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_ExGGC <- glmmTMB(A_Rich ~ Elevation_Scaled * Prop_Green_GC + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_ExWE <- glmmTMB(A_Rich ~ Elevation_Scaled * Weed_Estimate + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_ExGS <- glmmTMB(A_Rich ~ Elevation_Scaled * Grass_Status + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_ExC <- glmmTMB(A_Rich ~ Elevation_Scaled * Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_ExLS <- glmmTMB(A_Rich ~ Elevation_Scaled * X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_ExLC <- glmmTMB(A_Rich ~ Elevation_Scaled * X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_A_Rich_HxGC <- glmmTMB(A_Rich ~ Plant_Height * GC_Scaled + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_HxGGC <- glmmTMB(A_Rich ~ Height_Scaled * Prop_Green_GC + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_HxWE <- glmmTMB(A_Rich ~ Plant_Height * Weed_Estimate + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_HxGS <- glmmTMB(A_Rich ~ Plant_Height * Grass_Status + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_HxC <- glmmTMB(A_Rich ~ Height_Scaled * Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_HxLS <- glmmTMB(A_Rich ~ Plant_Height * X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_HxLC <- glmmTMB(A_Rich ~ Plant_Height * X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_A_Rich_GCxGGC <- glmmTMB(A_Rich ~ GC_Scaled * Prop_Green_GC + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GCxWE <- glmmTMB(A_Rich ~ GC_Scaled * Weed_Estimate + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GCxGS <- glmmTMB(A_Rich ~ GC_Scaled * Grass_Status + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GCxC <- glmmTMB(A_Rich ~ GC_Scaled * Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GCxLS <- glmmTMB(A_Rich ~ GC_Scaled * X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GCxLC <- glmmTMB(A_Rich ~ GC_Scaled * X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_A_Rich_GGCxWE <- glmmTMB(A_Rich ~ Prop_Green_GC * Weed_Estimate + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GGCxGS <- glmmTMB(A_Rich ~ Prop_Green_GC * Grass_Status + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GGCxC <- glmmTMB(A_Rich ~ Prop_Green_GC * Crops_Scaled + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GGCxLS <- glmmTMB(A_Rich ~ Prop_Green_GC * X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GGCxLC <- glmmTMB(A_Rich ~ Prop_Green_GC * X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_A_Rich_WExC <- glmmTMB(A_Rich ~ Weed_Estimate * Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_WExLS <- glmmTMB(A_Rich ~ Weed_Estimate * X500m.Simspson + (1 | Property), family = poisson, data = FDModel)

TG_A_Rich_GSxC <- glmmTMB(A_Rich ~ Grass_Status * Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_GSxLS <- glmmTMB(A_Rich ~ Grass_Status * X500m.Simspson + (1 | Property), family = poisson, data = FDModel)

TG_A_Rich_CxLS <- glmmTMB(A_Rich ~ Cropping_500m * X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_A_Rich_CxLC <- glmmTMB(A_Rich ~ Cropping_500m * X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_A_Rich_LSxLC <- glmmTMB(A_Rich ~ X500m.Simspson * X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)


TG_A_Modnames2 <- c("TG_A_Rich_null3","TG_A_Rich_ExH",
                    "TG_A_Rich_ExGC",
               "TG_A_Rich_ExGGC","TG_A_Rich_ExWE","TG_A_Rich_ExGS",
               "TG_A_Rich_ExC","TG_A_Rich_ExLS","TG_A_Rich_ExLC",
               "TG_A_Rich_HxGC",
               "TG_A_Rich_HxGGC","TG_A_Rich_HxWE","TG_A_Rich_HxGS",
               "TG_A_Rich_HxC","TG_A_Rich_HxLS","TG_A_Rich_HxLC",
               "TG_A_Rich_GCxGGC","TG_A_Rich_GCxGS",
               "TG_A_Rich_GCxWE",
               "TG_A_Rich_GCxC","TG_A_Rich_GCxLS",
               "TG_A_Rich_GCxLC",
               "TG_A_Rich_GGCxWE","TG_A_Rich_GGCxGS",
               "TG_A_Rich_GGCxC",
               "TG_A_Rich_GGCxLS","TG_A_Rich_GGCxLC", 
               "TG_A_Rich_WExC",
               "TG_A_Rich_WExLS", "TG_A_Rich_GSxC",
               "TG_A_Rich_GSxLS", 
               "TG_A_Rich_CxLS","TG_A_Rich_CxLC","TG_A_Rich_LSxLC")
TG_A_richmodlist2 <- mget(TG_A_Modnames2)

aictab(TG_A_richmodlist2)

#Height X Grass Status


####Final models----

TG_A_Modnames3<- c("TG_A_Rich_null3", "TG_A_Rich_GS", "TG_A_Rich_GC_GS","TG_A_Rich_HxGS")


TG_A_richmodlist_Final <- mget(TG_A_Modnames3)
aictab(TG_A_richmodlist_Final)

#Top model = Height x Grass Status

#Equivalent models (within 2 AICc):
##Ground Cover + Grass Status

###Predictions----

#Height x Grass Status
summary(TG_A_Rich_HxGS)

TG_Predictions_Height <- seq(min(FDModel$Plant_Height),max(FDModel$Plant_Height),length.out=20)

TG_A_toprichpred <- expand.grid("Plant_Height" = TG_Predictions_Height,Grass_Status=c("Native","Introduced"))
head(TG_A_toprichpred);dim(TG_A_toprichpred)

TG_A_toprichpred1 <- predict(object = TG_A_Rich_HxGS,newdata= TG_A_toprichpred,se.fit = T, type = "link",re.form = NA)

TG_A_toprichpred2<-data.frame(TG_A_toprichpred,fit.link=TG_A_toprichpred1$fit,se.link=TG_A_toprichpred1$se.fit)

TG_A_toprichpred2$lci.link<-TG_A_toprichpred2$fit.link-
  (1.96*TG_A_toprichpred2$se.link)
TG_A_toprichpred2$uci.link<-TG_A_toprichpred2$fit.link+
  (1.96*TG_A_toprichpred2$se.link)

TG_A_toprichpred2$fit<-exp(TG_A_toprichpred2$fit.link)
TG_A_toprichpred2$se<-exp(TG_A_toprichpred2$se.link)
TG_A_toprichpred2$lci<-exp(TG_A_toprichpred2$lci.link)
TG_A_toprichpred2$uci<-exp(TG_A_toprichpred2$uci.link)

head(TG_A_toprichpred2);dim(TG_A_toprichpred2)

#Ground Cover + Grass Status
summary(TG_A_Rich_GC_GS)

TG_Predictions_GC <- seq(min(FDModel$Ground_Cover),max(FDModel$Ground_Cover),length.out=20)


TG_A_toprichpred3 <- expand.grid(Ground_Cover = TG_Predictions_GC, Grass_Status = c("Native","Introduced"))
head(TG_A_toprichpred3);dim(TG_A_toprichpred3)

TG_A_toprichpred4 <- predict(object = TG_A_Rich_GC_GS,newdata= TG_A_toprichpred3,se.fit = T, type = "link",re.form = NA)

TG_A_toprichpred5<-data.frame(TG_A_toprichpred3,fit.link=TG_A_toprichpred4$fit,se.link=TG_A_toprichpred4$se.fit)

TG_A_toprichpred5$lci.link<-TG_A_toprichpred5$fit.link-
  (1.96*TG_A_toprichpred5$se.link)
TG_A_toprichpred5$uci.link<-TG_A_toprichpred5$fit.link+
  (1.96*TG_A_toprichpred5$se.link)

TG_A_toprichpred5$fit<-exp(TG_A_toprichpred5$fit.link)
TG_A_toprichpred5$se<-exp(TG_A_toprichpred5$se.link)
TG_A_toprichpred5$lci<-exp(TG_A_toprichpred5$lci.link)
TG_A_toprichpred5$uci<-exp(TG_A_toprichpred5$uci.link)

head(TG_A_toprichpred5);dim(TG_A_toprichpred5)


###Visualize----

#Height x Grass Status
summary(TG_A_Rich_GC)
summary(TG_A_toprichpred2)

EE <- TG_A_toprichpred2$Grass_Status == "Introduced"
E_E <- TG_A_toprichpred2$Grass_Status == "Native"

dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = FDModel$Plant_Height,y = FDModel$A_Rich,xlab = expression("Grass Height (cm)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,ylim=c(0,5.5))
mtext(side=3,line=0,at = -2,'a)',cex=1.1)

polygon(x = c(TG_A_toprichpred2$Plant_Height[EE],rev(TG_A_toprichpred2$Plant_Height[EE])), y = c(TG_A_toprichpred2$lci[EE],rev(TG_A_toprichpred2$uci[EE])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_A_toprichpred2$Plant_Height[EE],y = TG_A_toprichpred2$fit[EE],lwd = 2,lty=1,col = 'grey30')

polygon(x = c(TG_A_toprichpred2$Plant_Height[E_E],rev(TG_A_toprichpred2$Plant_Height[E_E])), y = c(TG_A_toprichpred2$lci[E_E],rev(TG_A_toprichpred2$uci[E_E])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_A_toprichpred2$Plant_Height[E_E],y = TG_A_toprichpred2$fit[E_E],lwd = 2,lty =2,col = 'grey30')

legend('topright',legend = c("Native", "Introduced"), lty = c(2,1), col = 'grey30',pt.cex = 1)

#Ground Cover + Grass Status
summary(TG_A_Rich_GC_GS)
sumamry(TG_A_toprichpred5)

FF <- TG_A_toprichpred5$Grass_Status == "Introduced"
F_F <- TG_A_toprichpred5$Ground_Cover == TG_Predictions_GC[10]

raw_x1 <- ifelse(FDModel$Grass_Status ==
                  "Native", 1, 
                ifelse(FDModel$Grass_Status ==
                         "Introduced", 2, NA))


dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = FDModel$Ground_Cover,y = FDModel$A_Rich,xlab = expression("Ground Cover (%)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,ylim = c(0,5))
mtext(side=3,line=0,at = -5,'a)',cex=1.1)

polygon(x = c(TG_A_toprichpred5$Ground_Cover[FF],rev(TG_A_toprichpred5$Ground_Cover[FF])), y = c(TG_A_toprichpred5$lci[FF],rev(TG_A_toprichpred5$uci[FF])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_A_toprichpred5$Ground_Cover[FF],y = TG_A_toprichpred5$fit[FF],lwd = 2,col = 'grey30')


plot(x = 1:2,y = TG_A_toprichpred5$fit [F_F],xlab = " ",ylab = 'Species Richness', type = 'p',pch = 16,cex =2.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,5))
axis(side=1,at=1:2,labels=c(' ',' '))
arrows(x0=1:2, y0=TG_A_toprichpred5$lci [F_F],x1=1:2, y1=TG_A_toprichpred5$uci[F_F],angle=90,length=0.2, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.3,'b)',cex=1.1)
mtext(side=1,line=1.5,at = 0.9,'Native\n Grass',cex=1.1)
mtext(side=1,line=1.5,at = 2.1,'Introduced\n Grass',cex=1.1)

points(x = jitter(raw_x1, factor = 1),y = FDModel$A_Rich, pch = 16, cex = 0.4, col = "black")

##Diversity----

###Modelling----

head(FDModel);dim(FDModel)
####Day

TG_A_Div_null <- glmer(A_Div ~ 1 + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_Day <- glmer(A_Div ~ Day_Sampled + (1 | Property), family = Gamma(link = "log"), data = FDModel)


aictab(list("Null" = TG_A_Div_null,"Day" = TG_A_Div_null))
#no need to include day in next step

####Single

head(FDModel)

TG_A_Div_E <- glmer(A_Div ~ Elevation + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_A_Div_H <- glmer(A_Div ~ Plant_Height + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_A_Div_GC <- glmer(A_Div ~ Ground_Cover + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_A_Div_GGC <- glmer(A_Div ~ Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_A_Div_WE <- glmer(A_Div ~ Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_A_Div_GS <- glmer(A_Div ~ Grass_Status + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_A_Div_C <- glmer(A_Div ~ Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_A_Div_LS <- glmer(A_Div ~ X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_A_Div_LC <- glmer(A_Div ~ X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_A_Div_modnames <- c("TG_A_Div_null", "TG_A_Div_E","TG_A_Div_H",
                  "TG_A_Div_GC", "TG_A_Div_GGC", "TG_A_Div_WE", 
                  "TG_A_Div_GS", "TG_A_Div_C", "TG_A_Div_LS",
                  "TG_A_Div_LC")

TG_A_Divmodlist <- mget(TG_A_Div_modnames)
aictab(TG_A_Divmodlist)
#Null

#Because the top is the null model additive models aren't  going to be any better so skipping straight to interactions

####Interaction

head(FDModel)
names(FDModel)


TG_A_Div_ExH <- glmer(A_Div ~ Elevation_Scaled * Plant_Height + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_ExGC <- glmer(A_Div ~ Elevation_Scaled * Ground_Cover + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_ExGGC <- glmer(A_Div ~ Elevation_Scaled * Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_ExWE <- glmer(A_Div ~ Elevation * Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_ExGS <- glmer(A_Div ~ Elevation * Grass_Status + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_ExC <- glmer(A_Div ~ Elevation_Scaled * Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_ExLS <- glmer(A_Div ~ Elevation * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_ExLC <- glmer(A_Div ~ Elevation * X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_A_Div_HxGC <- glmer(A_Div ~ Plant_Height * GC_Scaled + (1 | Property), family = Gamma(link = "log"), data = FDModel) 
TG_A_Div_HxGGC <- glmer(A_Div ~ Plant_Height * Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_HxWE <- glmer(A_Div ~ Plant_Height * Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_HxGS <- glmer(A_Div ~ Plant_Height * Grass_Status + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_HxC <- glmer(A_Div ~ Plant_Height * Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_HxLS <- glmer(A_Div ~ Plant_Height * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_HxLC <- glmer(A_Div ~ Plant_Height * X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_A_Div_GCxGGC <- glmer(A_Div ~ GC_Scaled * Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_GCxWE <- glmer(A_Div ~ Ground_Cover * Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_GCxGS <- glmer(A_Div ~ Ground_Cover * Grass_Status + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_GCxC <- glmer(A_Div ~ GC_Scaled * Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = FDModel) 
TG_A_Div_GCxLS <- glmer(A_Div ~ Ground_Cover * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_GCxLC <- glmer(A_Div ~ Ground_Cover * X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_A_Div_GGCxWE <- glmer(A_Div ~ Prop_Green_GC * Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_GGCxGS <- glmer(A_Div ~ Prop_Green_GC * Grass_Status + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_GGCxC <- glmer(A_Div ~ Prop_Green_GC * Crops_Scaled + (1 | Property), family = Gamma(link = "log"), data = FDModel) 
TG_A_Div_GGCxLS <- glmer(A_Div ~ Prop_Green_GC * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_GGCxLC <- glmer(A_Div ~ Prop_Green_GC * X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_A_Div_WExC <- glmer(A_Div ~ Weed_Estimate * Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_WExLS <- glmer(A_Div ~ Weed_Estimate * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_A_Div_GSxC <- glmer(A_Div ~ Grass_Status * Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_GSxLS <- glmer(A_Div ~ Grass_Status * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_A_Div_CxLS <- glmer(A_Div ~ Cropping_500m * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_A_Div_CxLC <- glmer(A_Div ~ Crops_Scaled * X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel) #Did not converge 

TG_A_Div_LSxLC <- glmer(A_Div ~ X500m.Simspson * X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)


TG_A_Divmodnames2 <- c("TG_A_Div_null","TG_A_Div_ExH",
                       "TG_A_Div_ExGC","TG_A_Div_ExGGC",
                       "TG_A_Div_ExWE","TG_A_Div_ExGS",
                       "TG_A_Div_ExC","TG_A_Div_ExLS",
                       "TG_A_Div_ExLC","TG_A_Div_HxGC",
                       "TG_A_Div_HxGGC","TG_A_Div_HxWE",
                       "TG_A_Div_HxGS","TG_A_Div_HxC",
                       "TG_A_Div_HxLS","TG_A_Div_HxLC",
                       "TG_A_Div_GCxGGC","TG_A_Div_GCxWE",
                       "TG_A_Div_GCxGS","TG_A_Div_GCxC",
                       "TG_A_Div_GCxLS","TG_A_Div_GCxLC",
                       "TG_A_Div_GGCxWE","TG_A_Div_GGCxGS",
                       "TG_A_Div_GGCxC","TG_A_Div_GGCxLS",
                       "TG_A_Div_GGCxLC", "TG_A_Div_WExC",
                       "TG_A_Div_WExLS", "TG_A_Div_GSxC",
                       "TG_A_Div_GSxLS","TG_A_Div_CxLS",
                       "TG_A_Div_LSxLC")
TG_A_Divmodlist2 <- mget(TG_A_Divmodnames2)

aictab(TG_A_Divmodlist2)
#Still the null model at the top so no more for group A diversity


#Group C----

##Species Richness-----

###Modelling----
head(FDModel)

####Day

TG_C_Rich_null <- glmmTMB(C_Rich ~ 1 + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_day <- glmmTMB(C_Rich ~ Day_Sampled + (1 | Property), family = poisson, data = FDModel)

aictab(list("Null" = TG_C_Rich_null,"Day" = TG_C_Rich_day))
#no need to include day in next step

####Single

head(FDModel)


TG_C_Rich_E <- glmmTMB(C_Rich ~ Elevation_Scaled + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_H <- glmmTMB(C_Rich ~ Plant_Height + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_GC <- glmmTMB(C_Rich ~ Ground_Cover + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_GGC <- glmmTMB(C_Rich ~ Prop_Green_GC + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_WE <- glmmTMB(C_Rich ~ Weed_Estimate + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_GS <- glmmTMB(C_Rich ~ Grass_Status + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_C <- glmmTMB(C_Rich ~ Cropping_500m + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_LS <- glmmTMB(C_Rich ~ X500m.Simspson + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_LC <- glmmTMB(C_Rich ~ X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_C_richmodlist <- list("null" = TG_C_Rich_null, 
                         "Elevation" = TG_C_Rich_E,
                         "Height"=TG_C_Rich_H, 
                         "GC" = TG_C_Rich_GC, 
                         "Green GC" = TG_C_Rich_GGC, 
                         "Weed Estimate" = TG_C_Rich_WE,
                         "Grass Status" = TG_C_Rich_GS, 
                         "Crops" = TG_C_Rich_C,
                         "Lanscape Simpson" = TG_C_Rich_LS, 
                         "Landscape Class" = TG_C_Rich_LC)

aictab(TG_C_richmodlist)
#Grass Status
####Additive

TG_C_Rich_E_H <- glmmTMB(C_Rich ~ Elevation_Scaled + Plant_Height + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_E_GC <- glmmTMB(C_Rich ~ Elevation_Scaled + Ground_Cover + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_E_GGC <- glmmTMB(C_Rich ~ Elevation_Scaled + Prop_Green_GC + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_E_WE <- glmmTMB(C_Rich ~ Elevation_Scaled + Weed_Estimate + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_E_GS <- glmmTMB(C_Rich ~ Elevation_Scaled + Grass_Status + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_E_C <- glmmTMB(C_Rich ~ Elevation_Scaled + Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_E_LS <- glmmTMB(C_Rich ~ Elevation_Scaled + X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_E_LC <- glmmTMB(C_Rich ~ Elevation_Scaled + X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)


TG_C_Rich_H_GC <- glmmTMB(C_Rich ~ Plant_Height + Ground_Cover + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_H_GGC <- glmmTMB(C_Rich ~ Plant_Height + Prop_Green_GC + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_H_WE <- glmmTMB(C_Rich ~ Plant_Height + Weed_Estimate + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_H_GS <- glmmTMB(C_Rich ~ Plant_Height + Grass_Status + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_H_C <- glmmTMB(C_Rich ~ Plant_Height + Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_H_LS <- glmmTMB(C_Rich ~ Plant_Height + X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_H_LC <- glmmTMB(C_Rich ~ Plant_Height + X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)


TG_C_Rich_GC_GGC <- glmmTMB(C_Rich ~ Ground_Cover + Prop_Green_GC + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GC_WE <- glmmTMB(C_Rich ~ Ground_Cover + Weed_Estimate + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GC_GS <- glmmTMB(C_Rich ~ Ground_Cover + Grass_Status + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GC_C <- glmmTMB(C_Rich ~ Ground_Cover + Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GC_LS <- glmmTMB(C_Rich ~ Ground_Cover + X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GC_LC <- glmmTMB(C_Rich ~ Ground_Cover + X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_GGC_WE <- glmmTMB(C_Rich ~ Prop_Green_GC + Weed_Estimate + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GGC_GS <- glmmTMB(C_Rich ~ Prop_Green_GC + Grass_Status + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GGC_C <- glmmTMB(C_Rich ~ Prop_Green_GC + Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GGC_LS <- glmmTMB(C_Rich ~ Prop_Green_GC + X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GGC_LC <- glmmTMB(C_Rich ~ Prop_Green_GC + X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_WE_GS <- glmmTMB(C_Rich ~ Weed_Estimate + Grass_Status + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_WE_C <- glmmTMB(C_Rich ~ Weed_Estimate + Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_WE_LS <- glmmTMB(C_Rich ~ Weed_Estimate + X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_WE_LC <- glmmTMB(C_Rich ~ Weed_Estimate + X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_GS_C <- glmmTMB(C_Rich ~ Grass_Status + Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GS_LS <- glmmTMB(C_Rich ~ Grass_Status + X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GS_LC <- glmmTMB(C_Rich ~ Grass_Status + X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_C_LS <- glmmTMB(C_Rich ~ Cropping_500m + X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_C_LC <- glmmTMB(C_Rich ~ Cropping_500m + X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_LS_LC <- glmmTMB(C_Rich ~ X500m.Simspson + X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)


TG_C_Modnames <- c("TG_C_Rich_null","TG_C_Rich_E_H",
                   "TG_C_Rich_E_GC","TG_C_Rich_E_GGC",
                   "TG_C_Rich_E_WE","TG_C_Rich_E_GS",
                   "TG_C_Rich_E_C","TG_C_Rich_E_LS",
                   "TG_C_Rich_E_LC","TG_C_Rich_H_GC",
                   "TG_C_Rich_H_GGC","TG_C_Rich_H_WE",
                   "TG_C_Rich_H_GS","TG_C_Rich_H_C",
                   "TG_C_Rich_H_LS","TG_C_Rich_H_LC",
                   "TG_C_Rich_GC_GGC","TG_C_Rich_GC_WE",
                   "TG_C_Rich_GC_GS","TG_C_Rich_GC_C",
                   "TG_C_Rich_GC_LS","TG_C_Rich_GC_LC",
                   "TG_C_Rich_GGC_WE","TG_C_Rich_GGC_GS",
                   "TG_C_Rich_GGC_C","TG_C_Rich_GGC_LS",
                   "TG_C_Rich_GGC_LC","TG_C_Rich_WE_GS", 
                   "TG_C_Rich_WE_C","TG_C_Rich_WE_LS",
                   "TG_C_Rich_WE_LC","TG_C_Rich_GS_C",
                   "TG_C_Rich_GS_LS","TG_C_Rich_GS_LC",
                   "TG_C_Rich_C_LS","TG_C_Rich_C_LC",
                   "TG_C_Rich_LS_LC")
TG_C_richmodlist1 <- mget(TG_C_Modnames)

aictab(TG_C_richmodlist1)


#Elevation + Grass Status


####Interaction

head(FDModel)
names(FDModel)

TG_C_Rich_ExH <- glmmTMB(C_Rich ~ Elevation_Scaled * Plant_Height + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_ExGC <- glmmTMB(C_Rich ~ Elevation_Scaled * Ground_Cover + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_ExGGC <- glmmTMB(C_Rich ~ Elevation_Scaled * Prop_Green_GC + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_ExWE <- glmmTMB(C_Rich ~ Elevation_Scaled * Weed_Estimate + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_ExGS <- glmmTMB(C_Rich ~ Elevation_Scaled * Grass_Status + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_ExC <- glmmTMB(C_Rich ~ Elevation_Scaled * Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_ExLS <- glmmTMB(C_Rich ~ Elevation_Scaled * X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_ExLC <- glmmTMB(C_Rich ~ Elevation_Scaled * X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_HxGC <- glmmTMB(C_Rich ~ Height_Scaled * Ground_Cover + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_HxGGC <- glmmTMB(C_Rich ~ Height_Scaled * Prop_Green_GC + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_HxWE <- glmmTMB(C_Rich ~ Plant_Height * Weed_Estimate + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_HxGS <- glmmTMB(C_Rich ~ Plant_Height * Grass_Status + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_HxC <- glmmTMB(C_Rich ~ Height_Scaled * Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_HxLS <- glmmTMB(C_Rich ~ Plant_Height * X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_HxLC <- glmmTMB(C_Rich ~ Plant_Height * X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_GCxGGC <- glmmTMB(C_Rich ~ GC_Scaled * Prop_Green_GC + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GCxWE <- glmmTMB(C_Rich ~ Ground_Cover * Weed_Estimate + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GCxGS <- glmmTMB(C_Rich ~ Ground_Cover * Grass_Status + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GCxC <- glmmTMB(C_Rich ~ GC_Scaled * Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GCxLS <- glmmTMB(C_Rich ~ Ground_Cover * X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GCxLC <- glmmTMB(C_Rich ~ Ground_Cover * X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_GGCxWE <- glmmTMB(C_Rich ~ Prop_Green_GC * Weed_Estimate + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GGCxGS <- glmmTMB(C_Rich ~ Prop_Green_GC * Grass_Status + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GGCxC <- glmmTMB(C_Rich ~ Prop_Green_GC * Crops_Scaled + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GGCxLS <- glmmTMB(C_Rich ~ Prop_Green_GC * X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GGCxLC <- glmmTMB(C_Rich ~ Prop_Green_GC * X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_WExC <- glmmTMB(C_Rich ~ Weed_Estimate * Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_WExLS <- glmmTMB(C_Rich ~ Weed_Estimate * X500m.Simspson + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_GSxC <- glmmTMB(C_Rich ~ Grass_Status * Cropping_500m + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_GSxLS <- glmmTMB(C_Rich ~ Grass_Status * X500m.Simspson + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_CxLS <- glmmTMB(C_Rich ~ Cropping_500m * X500m.Simspson + (1 | Property), family = poisson, data = FDModel)
TG_C_Rich_CxLC <- glmmTMB(C_Rich ~ Cropping_500m * X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)

TG_C_Rich_LSxLC <- glmmTMB(C_Rich ~ X500m.Simspson * X500m.Dominant.Landscape.Class + (1 | Property), family = poisson, data = FDModel)


TG_C_Modnames2 <- c("TG_C_Rich_null","TG_C_Rich_ExH",
                    "TG_C_Rich_ExGC","TG_C_Rich_ExGGC",
                    "TG_C_Rich_ExWE","TG_C_Rich_ExGS",
                    "TG_C_Rich_ExC","TG_C_Rich_ExLS",
                    "TG_C_Rich_ExLC","TG_C_Rich_HxGC",
                    "TG_C_Rich_HxGGC","TG_C_Rich_HxWE",
                    "TG_C_Rich_HxGS","TG_C_Rich_HxC",
                    "TG_C_Rich_HxLS","TG_C_Rich_HxLC",
                    "TG_C_Rich_GCxGGC","TG_C_Rich_GCxGS",
                    "TG_C_Rich_GCxWE","TG_C_Rich_GCxC",
                    "TG_C_Rich_GCxLS","TG_C_Rich_GCxLC",
                    "TG_C_Rich_GGCxWE","TG_C_Rich_GGCxGS",
                    "TG_C_Rich_GGCxC","TG_C_Rich_GGCxLS",
                    "TG_C_Rich_GGCxLC","TG_C_Rich_WExC",
                    "TG_C_Rich_WExLS", "TG_C_Rich_GSxC",
                    "TG_C_Rich_GSxLS", "TG_C_Rich_CxLS",
                    "TG_C_Rich_CxLC","TG_C_Rich_LSxLC")
TG_C_richmodlist2 <- mget(TG_C_Modnames2)

aictab(TG_C_richmodlist2)

#Elevation X Grass Status


####Final models----

TG_C_Modnames3<- c("TG_C_Rich_null", "TG_C_Rich_GS", "TG_C_Rich_E_GS","TG_C_Rich_ExGS")


TG_C_richmodlist_Final <- mget(TG_C_Modnames3)
aictab(TG_C_richmodlist_Final)




#Top model = Elevation + Grass Status

#Equivalent models (within 2 AICc):
##Elevation X Grass Status


###Predictions----

#Elevation + Grass Status
summary(TG_C_Rich_E_GS)

TG_Predictions_Elevation_Scaled <- seq(min(FDModel$Elevation_Scaled),max(FDModel$Elevation_Scaled),length.out=20)


TG_C_toprichpred <- expand.grid(Elevation_Scaled = TG_Predictions_Elevation_Scaled, Grass_Status = c("Native","Introduced"))
head(TG_C_toprichpred);dim(TG_C_toprichpred)

TG_C_toprichpred1 <- predict(object = TG_C_Rich_E_GS,newdata= TG_C_toprichpred,se.fit = T, type = "link",re.form = NA)

TG_C_toprichpred2<-data.frame(TG_C_toprichpred,fit.link=TG_C_toprichpred1$fit,se.link=TG_C_toprichpred1$se.fit)

TG_C_toprichpred2$lci.link<-TG_C_toprichpred2$fit.link-
  (1.96*TG_C_toprichpred2$se.link)
TG_C_toprichpred2$uci.link<-TG_C_toprichpred2$fit.link+
  (1.96*TG_C_toprichpred2$se.link)

TG_C_toprichpred2$fit<-exp(TG_C_toprichpred2$fit.link)
TG_C_toprichpred2$se<-exp(TG_C_toprichpred2$se.link)
TG_C_toprichpred2$lci<-exp(TG_C_toprichpred2$lci.link)
TG_C_toprichpred2$uci<-exp(TG_C_toprichpred2$uci.link)

head(TG_C_toprichpred2);dim(TG_C_toprichpred2)

##Elevation X Grass Status
summary(TG_C_Rich_ExGS)

TG_C_toprichpred3 <- expand.grid(Elevation_Scaled = TG_Predictions_Elevation_Scaled, Grass_Status = c("Native","Introduced"))
head(TG_C_toprichpred3);dim(TG_C_toprichpred3)

TG_C_toprichpred4 <- predict(object = TG_C_Rich_ExGS,newdata= TG_C_toprichpred3,se.fit = T, type = "link",re.form = NA)

TG_C_toprichpred5<-data.frame(TG_C_toprichpred3,fit.link=TG_C_toprichpred4$fit,se.link=TG_C_toprichpred4$se.fit)

TG_C_toprichpred5$lci.link<-TG_C_toprichpred5$fit.link-
  (1.96*TG_C_toprichpred5$se.link)
TG_C_toprichpred5$uci.link<-TG_C_toprichpred5$fit.link+
  (1.96*TG_C_toprichpred5$se.link)

TG_C_toprichpred5$fit<-exp(TG_C_toprichpred5$fit.link)
TG_C_toprichpred5$se<-exp(TG_C_toprichpred5$se.link)
TG_C_toprichpred5$lci<-exp(TG_C_toprichpred5$lci.link)
TG_C_toprichpred5$uci<-exp(TG_C_toprichpred5$uci.link)

head(TG_C_toprichpred5);dim(TG_C_toprichpred5)



###Visualize----

##Elevation + Grass Status
summary(TG_C_Rich_E_GS)
head(TG_C_toprichpred2)

GG <- TG_C_toprichpred2$Grass_Status == "Introduced"
G_G <- TG_C_toprichpred2$Elevation_Scaled == TG_Predictions_Elevation_Scaled[10]


dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = FDModel$Elevation_Scaled,y = FDModel$C_Rich,xlab = expression("Elevation (m)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,ylim = c(0,11),xaxt = 'n')
axis(side=1, at=seq(from=min(TG_C_toprichpred2$Elevation_Scaled),to=max(TG_C_toprichpred2$Elevation_Scaled),length.out=5),labels=round(seq(from=min(FDModel$Elevation),to=max(FDModel$Elevation),length.out=5),0),cex.axis=1)
mtext(side=3,line=0,at = -2.05,'a)',cex=1.1)

polygon(x = c(TG_C_toprichpred2$Elevation_Scaled[GG],rev(TG_C_toprichpred2$Elevation_Scaled[GG])), y = c(TG_C_toprichpred2$lci[GG],rev(TG_C_toprichpred2$uci[GG])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_C_toprichpred2$Elevation_Scaled[GG],y = TG_C_toprichpred2$fit[GG],lwd = 2,col = 'grey30')


plot(x = 1:2,y = TG_C_toprichpred2$fit [G_G],xlab = " ",ylab = 'Species Richness', type = 'p',pch = 16,cex =2.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,11))
axis(side=1,at=1:2,labels=c(' ',' '))
arrows(x0=1:2, y0=TG_C_toprichpred2$lci [G_G],x1=1:2, y1=TG_C_toprichpred2$uci[G_G],angle=90,length=0.2, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.3,'b)',cex=1.1)
mtext(side=1,line=1.5,at = 0.9,'Native\n Grass',cex=1.1)
mtext(side=1,line=1.5,at = 2.1,'Introduced\n Grass',cex=1.1)

points(x = jitter(raw_x1, factor = 1),y = FDModel$C_Rich, pch = 16, cex = 0.4, col = "black")

###Elevation X Grass Status
summary(TG_C_Rich_ExGS)
head(TG_C_toprichpred5)

HH <- TG_C_toprichpred5$Grass_Status == "Introduced"
H_H <- TG_C_toprichpred5$Grass_Status == "Native"


dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = FDModel$Elevation_Scaled,y = FDModel$C_Rich,xlab = expression("Elevation (m)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,ylim = c(0,13),xaxt = 'n')
axis(side=1, at=seq(from=min(TG_C_toprichpred5$Elevation_Scaled),to=max(TG_C_toprichpred5$Elevation_Scaled),length.out=5),labels=round(seq(from=min(FDModel$Elevation),to=max(FDModel$Elevation),length.out=5),0),cex.axis=1)
mtext(side=3,line=0,at = -2,'a)',cex=1)

polygon(x = c(TG_C_toprichpred5$Elevation_Scaled[HH],rev(TG_C_toprichpred5$Elevation_Scaled[HH])), y = c(TG_C_toprichpred5$lci[HH],rev(TG_C_toprichpred5$uci[HH])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_C_toprichpred5$Elevation_Scaled[HH],y = TG_C_toprichpred5$fit[HH],lwd = 2,lty=1,col = 'grey30')

polygon(x = c(TG_C_toprichpred5$Elevation_Scaled[H_H],rev(TG_C_toprichpred5$Elevation_Scaled[H_H])), y = c(TG_C_toprichpred5$lci[H_H],rev(TG_C_toprichpred5$uci[H_H])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_C_toprichpred5$Elevation_Scaled[H_H],y = TG_C_toprichpred5$fit[H_H],lwd = 2,lty=2,col = 'grey30')

legend('top',legend = c("Native", "Introduced"), lty = c(2,1), col = 'grey30',pt.cex = 1)

##Diversity----

###Modelling----
head(FDModel)

####Day

TG_C_Div_null <- glmer(C_Div ~ 1 + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_Day <- glmer(C_Div ~ Day_Sampled + (1 | Property), family = Gamma(link = "log"), data = FDModel)


aictab(list("Null" = TG_C_Div_null,"Day" = TG_C_Div_Day))
#no need to include day in next step

####Single

head(FDModel)

TG_C_Div_E <- glmer(C_Div ~ Elevation + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_C_Div_H <- glmer(C_Div ~ Plant_Height + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_C_Div_GC <- glmer(C_Div ~ Ground_Cover + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_C_Div_GGC <- glmer(C_Div ~ Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_C_Div_WE <- glmer(C_Div ~ Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_C_Div_GS <- glmer(C_Div ~ Grass_Status + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_C_Div_C <- glmer(C_Div ~ Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_C_Div_LS <- glmer(C_Div ~ X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_C_Div_LC <- glmer(C_Div ~ X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_C_Div_modnames <- c("TG_C_Div_null", "TG_C_Div_E","TG_C_Div_H",
                       "TG_C_Div_GC", "TG_C_Div_GGC", 
                       "TG_C_Div_WE", "TG_C_Div_GS", "TG_C_Div_C", 
                       "TG_C_Div_LS","TG_C_Div_LC")

TG_C_Divmodlist <- mget(TG_C_Div_modnames)
aictab(TG_C_Divmodlist)
#Grass Status

##Additive

head(FDModel)
names(FDModel)


TG_C_Div_E_H <- glmer(C_Div ~ Elevation + Plant_Height + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_E_GC <- glmer(C_Div ~ Elevation + Ground_Cover + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_E_GGC <- glmer(C_Div ~ Elevation + Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_E_WE <- glmer(C_Div ~ Elevation + Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_E_GS <- glmer(C_Div ~ Elevation + Grass_Status + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_E_C <- glmer(C_Div ~ Elevation + Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_E_LS <- glmer(C_Div ~ Elevation + X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_E_LC <- glmer(C_Div ~ Elevation + X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)


TG_C_Div_H_GC <- glmer(C_Div ~ Plant_Height + Ground_Cover + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_H_GGC <- glmer(C_Div ~ Plant_Height + Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_H_WE <- glmer(C_Div ~ Plant_Height + Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_H_GS <- glmer(C_Div ~ Plant_Height + Grass_Status + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_H_C <- glmer(C_Div ~ Plant_Height + Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_H_LS <- glmer(C_Div ~ Plant_Height + X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_H_LC <- glmer(C_Div ~ Plant_Height + X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)


TG_C_Div_GC_GGC <- glmer(C_Div ~ Ground_Cover + Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_GC_WE <- glmer(C_Div ~ Ground_Cover + Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_GC_GS <- glmer(C_Div ~ Ground_Cover + Grass_Status + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_GC_C <- glmer(C_Div ~ Ground_Cover + Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_GC_LS <- glmer(C_Div ~ Ground_Cover + X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_GC_LC <- glmer(C_Div ~ Ground_Cover + X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_C_Div_GGC_WE <- glmer(C_Div ~ Prop_Green_GC + Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_GGC_GS <- glmer(C_Div ~ Prop_Green_GC + Grass_Status + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_GGC_C <- glmer(C_Div ~ Prop_Green_GC + Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_GGC_LS <- glmer(C_Div ~ Prop_Green_GC + X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_GGC_LC <- glmer(C_Div ~ Prop_Green_GC + X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_C_Div_WE_GS <- glmer(C_Div ~ Weed_Estimate + Grass_Status + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_WE_C <- glmer(C_Div ~ Weed_Estimate + Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_WE_LS <- glmer(C_Div ~ Weed_Estimate + X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_WE_LC <- glmer(C_Div ~ Weed_Estimate + X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_C_Div_GS_C <- glmer(C_Div ~ Grass_Status + Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_GS_LS <- glmer(C_Div ~ Grass_Status + X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_GS_LC <- glmer(C_Div ~ Grass_Status + X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_C_Div_C_LS <- glmer(C_Div ~ Cropping_500m + X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_C_LC <- glmer(C_Div ~ Cropping_500m + X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_C_Div_LS_LC <- glmer(C_Div ~ X500m.Simspson + X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)


TG_C_DivModnames <- c("TG_C_Div_null","TG_C_Div_E_H",
                      "TG_C_Div_E_GC",
                 "TG_C_Div_E_GGC","TG_C_Div_E_WE","TG_C_Div_E_GS",
                 "TG_C_Div_E_C","TG_C_Div_E_LS","TG_C_Div_E_LC",
                 "TG_C_Div_H_GC",
                 "TG_C_Div_H_GGC","TG_C_Div_H_WE","TG_C_Div_H_GS",
                 "TG_C_Div_H_C","TG_C_Div_H_LS","TG_C_Div_H_LC",
                 "TG_C_Div_GC_GGC","TG_C_Div_GC_WE",
                 "TG_C_Div_GC_GS",
                 "TG_C_Div_GC_C","TG_C_Div_GC_LS","TG_C_Div_GC_LC",
                 "TG_C_Div_GGC_WE","TG_C_Div_GGC_GS",
                 "TG_C_Div_GGC_C",
                 "TG_C_Div_GGC_LS","TG_C_Div_GGC_LC",
                 "TG_C_Div_WE_GS", 
                 "TG_C_Div_WE_C","TG_C_Div_WE_LS",
                 "TG_C_Div_WE_LC", 
                 "TG_C_Div_GS_C","TG_C_Div_GS_LS",
                 "TG_C_Div_GS_LC", 
                 "TG_C_Div_C_LS","TG_C_Div_C_LC","TG_C_Div_LS_LC")
TG_C_divmodlist <- mget(TG_C_DivModnames)

aictab(TG_C_divmodlist)
#Elevation + Grass Status


####Interaction

head(FDModel)
names(FDModel)


TG_C_Div_ExH <- glmer(C_Div ~ Elevation_Scaled * Plant_Height + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_ExGC <- glmer(C_Div ~ Elevation_Scaled * Ground_Cover + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_ExGGC <- glmer(C_Div ~ Elevation_Scaled * Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_ExWE <- glmer(C_Div ~ Elevation * Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_ExGS <- glmer(C_Div ~ Elevation * Grass_Status + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_ExC <- glmer(C_Div ~ Elevation_Scaled * Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_ExLS <- glmer(C_Div ~ Elevation * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_ExLC <- glmer(C_Div ~ Elevation * X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_C_Div_HxGC <- glmer(C_Div ~ Plant_Height * GC_Scaled + (1 | Property), family = Gamma(link = "log"), data = FDModel) 
TG_C_Div_HxGGC <- glmer(C_Div ~ Plant_Height * Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = FDModel) 
TG_C_Div_HxWE <- glmer(C_Div ~ Height_Scaled * Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = FDModel) #no convergence
TG_C_Div_HxGS <- glmer(C_Div ~ Plant_Height * Grass_Status + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_HxC <- glmer(C_Div ~ Plant_Height * Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_HxLS <- glmer(C_Div ~ Plant_Height * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_HxLC <- glmer(C_Div ~ Plant_Height * X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_C_Div_GCxGGC <- glmer(C_Div ~ GC_Scaled * Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_GCxWE <- glmer(C_Div ~ Ground_Cover * Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_GCxGS <- glmer(C_Div ~ Ground_Cover * Grass_Status + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_GCxC <- glmer(C_Div ~ GC_Scaled * Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = FDModel) 
TG_C_Div_GCxLS <- glmer(C_Div ~ Ground_Cover * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_GCxLC <- glmer(C_Div ~ Ground_Cover * X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_C_Div_GGCxWE <- glmer(C_Div ~ Prop_Green_GC * Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = FDModel) #no convergence
TG_C_Div_GGCxGS <- glmer(C_Div ~ Prop_Green_GC * Grass_Status + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_GGCxC <- glmer(C_Div ~ Prop_Green_GC * Crops_Scaled + (1 | Property), family = Gamma(link = "log"), data = FDModel) 
TG_C_Div_GGCxLS <- glmer(C_Div ~ Prop_Green_GC * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_GGCxLC <- glmer(C_Div ~ Prop_Green_GC * X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_C_Div_WExC <- glmer(C_Div ~ Weed_Estimate * Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_WExLS <- glmer(C_Div ~ Weed_Estimate * Simpson + (1 | Property), family = Gamma(link = "log"), data = FDModel) #no convergence

TG_C_Div_GSxC <- glmer(C_Div ~ Grass_Status * Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_GSxLS <- glmer(C_Div ~ Grass_Status * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_C_Div_CxLS <- glmer(C_Div ~ Cropping_500m * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = FDModel)
TG_C_Div_CxLC <- glmer(C_Div ~ Cropping_500m * X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)

TG_C_Div_LSxLC <- glmer(C_Div ~ X500m.Simspson * X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = FDModel)


TG_C_Divmodnames2 <- c("TG_C_Div_null","TG_C_Div_ExH",
                       "TG_C_Div_ExGC","TG_C_Div_ExGGC",
                       "TG_C_Div_ExWE","TG_C_Div_ExGS",
                       "TG_C_Div_ExC","TG_C_Div_ExLS",
                       "TG_C_Div_ExLC","TG_C_Div_HxGC",
                       "TG_C_Div_HxGGC",
                       "TG_C_Div_HxGS","TG_C_Div_HxC",
                       "TG_C_Div_HxLS","TG_C_Div_HxLC",
                       "TG_C_Div_GCxGGC","TG_C_Div_GCxWE",
                       "TG_C_Div_GCxGS","TG_C_Div_GCxC",
                       "TG_C_Div_GCxLS","TG_C_Div_GCxLC",
                       "TG_C_Div_GGCxGS",
                       "TG_C_Div_GGCxC","TG_C_Div_GGCxLS",
                       "TG_C_Div_GGCxLC", "TG_C_Div_WExC",
                       "TG_C_Div_GSxC",
                       "TG_C_Div_GSxLS","TG_C_Div_CxLS",
                       "TG_C_Div_CxLC","TG_C_Div_LSxLC")
TG_C_Divmodlist2 <- mget(TG_C_Divmodnames2)

aictab(TG_C_Divmodlist2)
#Elevation x Grass Status



###Final AICC----

TG_C_DivModnames3<- c("TG_C_Div_null", "TG_C_Div_GS",
                      "TG_C_Div_E_GS","TG_C_Div_ExGS")
TG_C_divmodlist_Final <- mget(TG_C_DivModnames3)
aictab(TG_C_divmodlist_Final)



#Top model = Elevation + Grass Status

#Equivalent models:
##Grass Status

###Predictions----

#Elevation + Grass Status
summary(TG_C_Div_E_GS)

TG_Predictions_Elevation <- seq(min(FDModel$Elevation),max(FDModel$Elevation),length.out=20)

TG_C_topdivpred <- expand.grid(Elevation = TG_Predictions_Elevation, Grass_Status = c("Native","Introduced"))
head(TG_C_topdivpred);dim(TG_C_topdivpred)

TG_C_topdivpred1 <- predict(object = TG_C_Div_E_GS,newdata= TG_C_topdivpred,se.fit = T, type = "link",re.form = NA)

TG_C_topdivpred2<-data.frame(TG_C_topdivpred,fit.link=TG_C_topdivpred1$fit,se.link=TG_C_topdivpred1$se.fit)

TG_C_topdivpred2$lci.link<-TG_C_topdivpred2$fit.link-
  (1.96*TG_C_topdivpred2$se.link)
TG_C_topdivpred2$uci.link<-TG_C_topdivpred2$fit.link+
  (1.96*TG_C_topdivpred2$se.link)

TG_C_topdivpred2$fit<-exp(TG_C_topdivpred2$fit.link)
TG_C_topdivpred2$se<-exp(TG_C_topdivpred2$se.link)
TG_C_topdivpred2$lci<-exp(TG_C_topdivpred2$lci.link)
TG_C_topdivpred2$uci<-exp(TG_C_topdivpred2$uci.link)

head(TG_C_topdivpred2);dim(TG_C_topdivpred2)

##Grass Status
summary(TG_C_Div_GS)

TG_C_topdivpred3 <- data.frame(Grass_Status = c("Native","Introduced"))
head(TG_C_topdivpred3);dim(TG_C_topdivpred3)

TG_C_topdivpred4 <- predict(object = TG_C_Div_GS,newdata= TG_C_topdivpred3,se.fit = T, type = "link",re.form = NA)

TG_C_topdivpred5<-data.frame(TG_C_topdivpred3,fit.link=TG_C_topdivpred4$fit,se.link=TG_C_topdivpred4$se.fit)

TG_C_topdivpred5$lci.link<-TG_C_topdivpred5$fit.link-
  (1.96*TG_C_topdivpred5$se.link)
TG_C_topdivpred5$uci.link<-TG_C_topdivpred5$fit.link+
  (1.96*TG_C_topdivpred5$se.link)

TG_C_topdivpred5$fit<-exp(TG_C_topdivpred5$fit.link)
TG_C_topdivpred5$se<-exp(TG_C_topdivpred5$se.link)
TG_C_topdivpred5$lci<-exp(TG_C_topdivpred5$lci.link)
TG_C_topdivpred5$uci<-exp(TG_C_topdivpred5$uci.link)

head(TG_C_topdivpred5);dim(TG_C_topdivpred5)

###Visualize----

##Elevation + Grass Status
summary(TG_C_Div_E_GS)
head(TG_C_topdivpred2)

II <- TG_C_topdivpred2$Grass_Status == "Introduced"
I_I <- TG_C_topdivpred2$Elevation == TG_Predictions_Elevation[10]


dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = FDModel$Elevation,y = FDModel$C_Div,xlab = expression("Elevation (m)"),ylab = 'Diversity', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,ylim = c(0,6))
mtext(side=3,line=0,at = 60,'a)',cex=1.1)

polygon(x = c(TG_C_topdivpred2$Elevation[II],rev(TG_C_topdivpred2$Elevation[II])), y = c(TG_C_topdivpred2$lci[II],rev(TG_C_topdivpred2$uci[II])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_C_topdivpred2$Elevation[II],y = TG_C_topdivpred2$fit[II],lwd = 2,col = 'grey30')


plot(x = 1:2,y = TG_C_topdivpred2$fit[I_I],xlab = " ",ylab = 'Diversity', type = 'p',pch = 16,cex =2.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,6))
axis(side=1,at=1:2,labels=c(' ',' '))
arrows(x0=1:2, y0=TG_C_topdivpred2$lci[I_I],x1=1:2, y1=TG_C_topdivpred2$uci[I_I],angle=90,length=0.2, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.25,'b)',cex=1.1)
mtext(side=1,line=1.5,at = 0.9,'Native\n Grass',cex=1.1)
mtext(side=1,line=1.5,at = 2.1,'Introduced\n Grass',cex=1.1)

points(x = jitter(raw_x1, factor = 1),y = FDModel$C_Div, pch = 16, cex = 0.4, col = "black")

##Grass Status

summary(TG_C_Div_GS)
head(TG_C_topdivpred5)

dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = 1:2,y = TG_C_topdivpred5$fit,xlab = " ",ylab = 'Diversity', type = 'p',pch = 16,cex =2.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,6))
axis(side=1,at=1:2,labels=c(' ',' '))
arrows(x0=1:2, y0=TG_C_topdivpred5$lci,x1=1:2, y1=TG_C_topdivpred5$uci,angle=90,length=0.2, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.25,'a)',cex=1.1)
mtext(side=1,line=1.5,at = 0.9,'Native\n Grass',cex=1.1)
mtext(side=1,line=1.5,at = 2.1,'Introduced\n Grass',cex=1.1)

points(x = jitter(raw_x1, factor = 1),y = FDModel$C_Div, pch = 16, cex = 0.4, col = "black")



#Species Rich/Diversity Figures----

##Main----

dev.new(height=10,width=15,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,3,2),mfrow=c(2,3),mgp=c(2.5,1,0),xpd = T)

plot(x = FDModel$Plant_Height,y = FDModel$A_Rich,xlab = expression("Grass Height (cm)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,ylim=c(0,5.5),cex.axis=1.3,cex.lab=1.4)
mtext(side=3,line=0,at = -2,'a)',cex=0.9)

polygon(x = c(TG_A_toprichpred2$Plant_Height[EE],rev(TG_A_toprichpred2$Plant_Height[EE])), y = c(TG_A_toprichpred2$lci[EE],rev(TG_A_toprichpred2$uci[EE])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_A_toprichpred2$Plant_Height[EE],y = TG_A_toprichpred2$fit[EE],lwd = 2,lty=1,col = 'grey30')

polygon(x = c(TG_A_toprichpred2$Plant_Height[E_E],rev(TG_A_toprichpred2$Plant_Height[E_E])), y = c(TG_A_toprichpred2$lci[E_E],rev(TG_A_toprichpred2$uci[E_E])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_A_toprichpred2$Plant_Height[E_E],y = TG_A_toprichpred2$fit[E_E],lwd = 2,lty =2,col = 'grey30')

legend('topright',legend = c("Native", "Introduced"), lty = c(2,1), col = 'grey30',pt.cex = 1,cex = 1.2)

mtext(side=3,line=0.5,at = 43,'Flies, small herbivorous \nHemiptera and small Orthoptera',cex=0.7, font = 2)


plot(x = FDModel$Elevation_Scaled,y = FDModel$C_Rich,xlab = expression("Elevation (m)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,ylim = c(0,11),xaxt = 'n',cex.lab=1.4,cex.axis=1.3)
axis(side=1, at=seq(from=min(TG_C_toprichpred2$Elevation_Scaled),to=max(TG_C_toprichpred2$Elevation_Scaled),length.out=5),labels=round(seq(from=min(FDModel$Elevation),to=max(FDModel$Elevation),length.out=5),0),cex.axis=1.3)
mtext(side=3,line=0,at = -2.05,'b)',cex=0.9)

polygon(x = c(TG_C_toprichpred2$Elevation_Scaled[GG],rev(TG_C_toprichpred2$Elevation_Scaled[GG])), y = c(TG_C_toprichpred2$lci[GG],rev(TG_C_toprichpred2$uci[GG])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_C_toprichpred2$Elevation_Scaled[GG],y = TG_C_toprichpred2$fit[GG],lwd = 2,col = 'grey30')

mtext(side=3,line=1.2,at = 2,'Non-web building spiders, herbivorous Hemiptera and medium/large grasshoppers',cex=0.7, font = 2)


plot(x = 1:2,y = TG_C_toprichpred2$fit [G_G],xlab = " ",ylab = 'Species Richness', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,11),cex.axis=1.3,cex.lab=1.4)
axis(side=1,at=1:2,labels=c(' ',' '))
arrows(x0=1:2, y0=TG_C_toprichpred2$lci [G_G],x1=1:2, y1=TG_C_toprichpred2$uci[G_G],angle=90,length=0.1, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.3,'c)',cex=0.9)
mtext(side=1,line=2,at = 0.9,'Native\n Grass',cex=0.9)
mtext(side=1,line=2,at = 2.1,'Introduced\n Grass',cex=0.9)

points(x = jitter(raw_x1, factor = 1),y = FDModel$C_Rich, pch = 16, cex = 0.4, col = "black")


plot(x = FDModel$Elevation,y = FDModel$C_Div,xlab = expression("Elevation (m)"),ylab = 'Diversity', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,ylim = c(0,6),cex.lab=1.4,cex.axis=1.3,xaxt = 'n')
axis(side=1, at=seq(from=min(TG_C_topdivpred2$Elevation),to=max(TG_C_topdivpred2$Elevation),length.out=5),labels=round(seq(from=min(FDModel$Elevation),to=max(FDModel$Elevation),length.out=5),0),cex.axis=1.3)
mtext(side=3,line=0,at = 60,'d)',cex=0.9)

polygon(x = c(TG_C_topdivpred2$Elevation[II],rev(TG_C_topdivpred2$Elevation[II])), y = c(TG_C_topdivpred2$lci[II],rev(TG_C_topdivpred2$uci[II])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_C_topdivpred2$Elevation[II],y = TG_C_topdivpred2$fit[II],lwd = 2,col = 'grey30')

mtext(side=3,line=1.2,at = 740,'Non-web building spiders, herbivorous Hemiptera and medium/large grasshoppers',cex=0.7, font = 2)


plot(x = 1:2,y = TG_C_topdivpred2$fit[I_I],xlab = " ",ylab = 'Diversity', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,6),cex.lab=1.4,cex.axis=1.3)
axis(side=1,at=1:2,labels=c(' ',' '))
arrows(x0=1:2, y0=TG_C_topdivpred2$lci[I_I],x1=1:2, y1=TG_C_topdivpred2$uci[I_I],angle=90,length=0.1, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.25,'e)',cex=0.9)
mtext(side=1,line=2,at = 0.9,'Native\n Grass',cex=0.9)
mtext(side=1,line=2,at = 2.1,'Introduced\n Grass',cex=0.9)

points(x = jitter(raw_x1, factor = 1),y = FDModel$C_Div, pch = 16, cex = 0.4, col = "black")



##Supporting info----

dev.new(height=10,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,3,2),mfrow=c(2,2),mgp=c(2.5,1,0),xpd = T)

plot(x = FDModel$Ground_Cover,y = FDModel$A_Rich,xlab = expression("Ground Cover (%)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,ylim = c(0,5),xaxt = 'n')
axis(side=1, at=seq(from=min(TG_A_toprichpred5$Ground_Cover),to=max(TG_A_toprichpred5$Ground_Cover),length.out=5),labels=round(seq(from=min(FDModel$Ground_Cover),to=max(FDModel$Ground_Cover),length.out=5),0),cex.axis=1)
mtext(side=3,line=0,at = 33,'a)',cex=0.9)

polygon(x = c(TG_A_toprichpred5$Ground_Cover[FF],rev(TG_A_toprichpred5$Ground_Cover[FF])), y = c(TG_A_toprichpred5$lci[FF],rev(TG_A_toprichpred5$uci[FF])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_A_toprichpred5$Ground_Cover[FF],y = TG_A_toprichpred5$fit[FF],lwd = 2,col = 'grey30')

mtext(side=3,line=1.2,at = 110,'----------Flies, small herbivorous Hemiptera and small Orthoptera---------',cex=0.7, font = 2)


plot(x = 1:2,y = TG_A_toprichpred5$fit [F_F],xlab = " ",ylab = 'Species Richness', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,5))
axis(side=1,at=1:2,labels=c(' ',' '))
arrows(x0=1:2, y0=TG_A_toprichpred5$lci [F_F],x1=1:2, y1=TG_A_toprichpred5$uci[F_F],angle=90,length=0.1, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.3,'b)',cex=0.9)
mtext(side=1,line=1.5,at = 0.9,'Native\n Grass',cex=0.9)
mtext(side=1,line=1.5,at = 2.1,'Introduced\n Grass',cex=0.9)

points(x = jitter(raw_x1, factor = 1),y = FDModel$A_Rich, pch = 16, cex = 0.4, col = "black")



plot(x = FDModel$Elevation_Scaled,y = FDModel$C_Rich,xlab = expression("Elevation (m)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,ylim = c(0,13),xaxt = 'n')
axis(side=1, at=seq(from=min(TG_C_toprichpred5$Elevation_Scaled),to=max(TG_C_toprichpred5$Elevation_Scaled),length.out=5),labels=round(seq(from=min(FDModel$Elevation),to=max(FDModel$Elevation),length.out=5),0),cex.axis=1)
mtext(side=3,line=0,at = -2,'c)',cex=0.9)

polygon(x = c(TG_C_toprichpred5$Elevation_Scaled[HH],rev(TG_C_toprichpred5$Elevation_Scaled[HH])), y = c(TG_C_toprichpred5$lci[HH],rev(TG_C_toprichpred5$uci[HH])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_C_toprichpred5$Elevation_Scaled[HH],y = TG_C_toprichpred5$fit[HH],lwd = 2,lty=1,col = 'grey30')

polygon(x = c(TG_C_toprichpred5$Elevation_Scaled[H_H],rev(TG_C_toprichpred5$Elevation_Scaled[H_H])), y = c(TG_C_toprichpred5$lci[H_H],rev(TG_C_toprichpred5$uci[H_H])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_C_toprichpred5$Elevation_Scaled[H_H],y = TG_C_toprichpred5$fit[H_H],lwd = 2,lty=2,col = 'grey30')

legend('top',legend = c("Native", "Introduced"), lty = c(2,1), col = 'grey30',pt.cex = 1)



plot(x = 1:2,y = TG_C_topdivpred5$fit,xlab = " ",ylab = 'Diversity', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,6))
axis(side=1,at=1:2,labels=c(' ',' '))
arrows(x0=1:2, y0=TG_C_topdivpred5$lci,x1=1:2, y1=TG_C_topdivpred5$uci,angle=90,length=0.1, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.25,'d)',cex=0.9)
mtext(side=1,line=1.5,at = 0.9,'Native\n Grass',cex=0.9)
mtext(side=1,line=1.5,at = 2.1,'Introduced\n Grass',cex=0.9)

points(x = jitter(raw_x1, factor = 1),y = FDModel$C_Div, pch = 16, cex = 0.4, col = "black")

mtext(side=3,line=1.2,at = -1.25,'Non-web building spiders, herbivorous Hemiptera and medium/large grasshoppers',cex=0.7, font = 2)


#Group B----


###Modelling----

head(FDModel);dim(FDModel)

####Day

TG_B_null <- glmmTMB(TG_B ~ 1 + (1 | Property), family = binomial, data = FDModel)

TG_B_day <- glmmTMB(TG_B ~ Day_Sampled + (1 | Property), family = binomial, data = FDModel)

aictab(list("Null" = TG_B_null,"Day" = TG_B_day))
#no need to include day in next step

####Single

TG_B_E <- glmmTMB(TG_B ~ Elevation + (1 | Property), family = binomial, data = FDModel)

TG_B_H <- glmmTMB(TG_B ~ Plant_Height + (1 | Property), family = binomial, data = FDModel)

TG_B_GC <- glmmTMB(TG_B ~ Ground_Cover + (1 | Property), family = binomial, data = FDModel)

TG_B_GGC <- glmmTMB(TG_B ~ Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)

TG_B_WE <- glmmTMB(TG_B ~ Weed_Estimate + (1 | Property), family = binomial, data = FDModel)

TG_B_GS <- glmmTMB(TG_B ~ Grass_Status + (1 | Property), family = binomial, data = FDModel)

TG_B_C <- glmmTMB(TG_B ~ Cropping_500m + (1 | Property), family = binomial, data = FDModel)

TG_B_LS <- glmmTMB(TG_B ~ X500m.Simspson + (1 | Property), family = binomial, data = FDModel)

TG_B_LC <- glmmTMB(TG_B ~ X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_B_modlist <- list("null" = TG_B_null,"Elevation" = TG_B_E,
                     "Height"=TG_B_H, "GC" = TG_B_GC,
                     "Green GC" = TG_B_GGC, 
                     "Weed Estimate" = TG_B_WE,
                     "Grass Status" = TG_B_GS, "Crops" = TG_B_C,
                     "Lanscape Simpson" = TG_B_LS,
                     "Landscape Class" = TG_B_LC)

aictab(TG_B_modlist)
#Grass Status

####Additive

TG_B_E_H <- glmmTMB(TG_B ~ Elevation + Plant_Height + (1 | Property), family = binomial, data = FDModel)
TG_B_E_GC <- glmmTMB(TG_B ~ Elevation + Ground_Cover + (1 | Property), family = binomial, data = FDModel)
TG_B_E_GGC <- glmmTMB(TG_B ~ Elevation + Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_B_E_WE <- glmmTMB(TG_B ~ Elevation + Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_B_E_GS <- glmmTMB(TG_B ~ Elevation + Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_B_E_C <- glmmTMB(TG_B ~ Elevation + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_B_E_LS <- glmmTMB(TG_B ~ Elevation + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_B_E_LC <- glmmTMB(TG_B ~ Elevation + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)


TG_B_H_GC <- glmmTMB(TG_B ~ Plant_Height + Ground_Cover + (1 | Property), family = binomial, data = FDModel)
TG_B_H_GGC <- glmmTMB(TG_B ~ Plant_Height + Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_B_H_WE <- glmmTMB(TG_B ~ Plant_Height + Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_B_H_GS <- glmmTMB(TG_B ~ Plant_Height + Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_B_H_C <- glmmTMB(TG_B ~ Plant_Height + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_B_H_LS <- glmmTMB(TG_B ~ Plant_Height + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_B_H_LC <- glmmTMB(TG_B ~ Plant_Height + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)


TG_B_GC_GGC <- glmmTMB(TG_B ~ Ground_Cover + Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_B_GC_WE <- glmmTMB(TG_B ~ Ground_Cover + Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_B_GC_GS <- glmmTMB(TG_B ~ Ground_Cover + Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_B_GC_C <- glmmTMB(TG_B ~ Ground_Cover + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_B_GC_LS <- glmmTMB(TG_B ~ Ground_Cover + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_B_GC_LC <- glmmTMB(TG_B ~ Ground_Cover + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_B_GGC_WE <- glmmTMB(TG_B ~ Prop_Green_GC + Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_B_GGC_GS <- glmmTMB(TG_B ~ Prop_Green_GC + Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_B_GGC_C <- glmmTMB(TG_B ~ Prop_Green_GC + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_B_GGC_LS <- glmmTMB(TG_B ~ Prop_Green_GC + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_B_GGC_LC <- glmmTMB(TG_B ~ Prop_Green_GC + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_B_WE_GS <- glmmTMB(TG_B ~ Weed_Estimate + Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_B_WE_C <- glmmTMB(TG_B ~ Weed_Estimate + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_B_WE_LS <- glmmTMB(TG_B ~ Weed_Estimate + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_B_WE_LC <- glmmTMB(TG_B ~ Weed_Estimate + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_B_GS_C <- glmmTMB(TG_B ~ Grass_Status + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_B_GS_LS <- glmmTMB(TG_B ~ Grass_Status + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_B_GS_LC <- glmmTMB(TG_B ~ Grass_Status + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_B_C_LS <- glmmTMB(TG_B ~ Cropping_500m + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_B_C_LC <- glmmTMB(TG_B ~ Cropping_500m + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_B_LS_LC <- glmmTMB(TG_B ~ X500m.Simspson + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)


TG_B_Modnames <- c("TG_B_null","TG_B_E_H",
                   "TG_B_E_GC","TG_B_E_GGC",
                   "TG_B_E_WE","TG_B_E_GS",
                   "TG_B_E_C","TG_B_E_LS",
                   "TG_B_E_LC","TG_B_H_GC",
                   "TG_B_H_GGC","TG_B_H_WE",
                   "TG_B_H_GS","TG_B_H_C",
                   "TG_B_H_LS","TG_B_H_LC",
                   "TG_B_GC_GGC","TG_B_GC_WE",
                   "TG_B_GC_GS","TG_B_GC_C",
                   "TG_B_GC_LS","TG_B_GC_LC",
                   "TG_B_GGC_WE","TG_B_GGC_GS",
                   "TG_B_GGC_C","TG_B_GGC_LS",
                   "TG_B_GGC_LC","TG_B_WE_GS", 
                   "TG_B_WE_C","TG_B_WE_LS",
                   "TG_B_WE_LC","TG_B_GS_C",
                   "TG_B_GS_LS","TG_B_GS_LC",
                   "TG_B_C_LS","TG_B_C_LC",
                   "TG_B_LS_LC")
TG_B_modlist1 <- mget(TG_B_Modnames)

aictab(TG_B_modlist1)

#Ground Cover + Grass Status

####Interaction

TG_B_ExH <- glmmTMB(TG_B ~ Elevation * Plant_Height + (1 | Property), family = binomial, data = FDModel)
TG_B_ExGC <- glmmTMB(TG_B ~ Elevation * Ground_Cover + (1 | Property), family = binomial, data = FDModel)
TG_B_ExGGC <- glmmTMB(TG_B ~ Elevation * Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_B_ExWE <- glmmTMB(TG_B ~ Elevation * Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_B_ExGS <- glmmTMB(TG_B ~ Elevation * Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_B_ExC <- glmmTMB(TG_B ~ Elevation * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_B_ExLS <- glmmTMB(TG_B ~ Elevation * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_B_ExLC <- glmmTMB(TG_B ~ Elevation * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_B_HxGC <- glmmTMB(TG_B ~ Plant_Height * Ground_Cover + (1 | Property), family = binomial, data = FDModel)
TG_B_HxGGC <- glmmTMB(TG_B ~ Plant_Height * Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_B_HxWE <- glmmTMB(TG_B ~ Plant_Height * Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_B_HxGS <- glmmTMB(TG_B ~ Height_Scaled * Grass_Status + (1 | Property), family = binomial, data = FDModel) #model convergence issue
TG_B_HxC <- glmmTMB(TG_B ~ Plant_Height * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_B_HxLS <- glmmTMB(TG_B ~ Plant_Height * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_B_HxLC <- glmmTMB(TG_B ~ Plant_Height * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_B_GCxGGC <- glmmTMB(TG_B ~ Ground_Cover * Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_B_GCxWE <- glmmTMB(TG_B ~ GC_Scaled * Weed_Estimate + (1 | Property), family = binomial, data = FDModel) #Model Convergance Issues
TG_B_GCxGS <- glmmTMB(TG_B ~ Ground_Cover * Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_B_GCxC <- glmmTMB(TG_B ~ Ground_Cover * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_B_GCxLS <- glmmTMB(TG_B ~ Ground_Cover * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_B_GCxLC <- glmmTMB(TG_B ~ Ground_Cover * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_B_GGCxWE <- glmmTMB(TG_B ~ Prop_Green_GC * Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_B_GGCxGS <- glmmTMB(TG_B ~ Prop_Green_GC * Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_B_GGCxC <- glmmTMB(TG_B ~ Prop_Green_GC * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_B_GGCxLS <- glmmTMB(TG_B ~ Prop_Green_GC * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_B_GGCxLC <- glmmTMB(TG_B ~ Prop_Green_GC * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_B_WExC <- glmmTMB(TG_B ~ Weed_Estimate * Crops_Scaled + (1 | Property), family = binomial, data = FDModel) #Model convergence issue
TG_B_WExLS <- glmmTMB(TG_B ~ Weed_Estimate * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)

TG_B_GSxC <- glmmTMB(TG_B ~ Grass_Status * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_B_GSxLS <- glmmTMB(TG_B ~ Grass_Status * X500m.Simspson + (1 | Property), family = binomial, data = FDModel) #Model Convergence issues

TG_B_CxLS <- glmmTMB(TG_B ~ Cropping_500m * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_B_CxLC <- glmmTMB(TG_B ~ Cropping_500m * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_B_LSxLC <- glmmTMB(TG_B ~ X500m.Simspson * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)


TG_B_Modnames2 <- c("TG_B_null","TG_B_ExH",
                    "TG_B_ExGC","TG_B_ExGGC",
                    "TG_B_ExWE","TG_B_ExGS",
                    "TG_B_ExC","TG_B_ExLS",
                    "TG_B_ExLC","TG_B_HxGC",
                    "TG_B_HxGGC","TG_B_HxWE",
                    "TG_B_HxC",
                    "TG_B_HxLS","TG_B_HxLC",
                    "TG_B_GCxGGC","TG_B_GCxGS",
                    "TG_B_GCxC",
                    "TG_B_GCxLS","TG_B_GCxLC",
                    "TG_B_GGCxWE","TG_B_GGCxGS",
                    "TG_B_GGCxC","TG_B_GGCxLS",
                    "TG_B_GGCxLC",
                    "TG_B_WExLS", "TG_B_GSxC",
                    "TG_B_CxLS",
                    "TG_B_CxLC","TG_B_LSxLC")
TG_B_modlist2 <- mget(TG_B_Modnames2)

aictab(TG_B_modlist2)

#Ground Cover x Grass Status
#Elevation x Grass Status

####Final models----

TG_B_Modnames3<- c("TG_B_null", "TG_B_GC", "TG_B_GC_C", "TG_B_GCxLS")


TG_B_modlist_Final <- mget(TG_B_Modnames3)
aictab(TG_B_modlist_Final)


#Top model = Ground Cover x Landscape Simpson

#Equivalent models (within 2 AICc):
##Ground Cover + Crops

###Predictions----

#Ground Cover x Simpson
summary(TG_B_GCxLS)

TG_Predictions_GC <- seq(min(FDModel$Ground_Cover),max(FDModel$Ground_Cover),length.out=20)


TG_B_pred <- expand.grid(Ground_Cover = TG_Predictions_GC, X500m.Simspson = TG_Predictions_Simspon)
head(TG_B_pred);dim(TG_B_pred)

TG_B_pred1 <- predict(object = TG_B_GCxLS,newdata= TG_B_pred,se.fit = T, type = "link",re.form = ~0)

TG_B_pred2<-data.frame(TG_B_pred,fit.link=TG_B_pred1$fit,se.link=TG_B_pred1$se.fit)

TG_B_pred2$lci.link<-TG_B_pred2$fit.link-(1.96*TG_B_pred2$se.link)
TG_B_pred2$uci.link<-TG_B_pred2$fit.link+(1.96*TG_B_pred2$se.link)

TG_B_pred2$fit<-plogis(TG_B_pred2$fit.link)
TG_B_pred2$se<-plogis(TG_B_pred2$se.link)
TG_B_pred2$lci<-plogis(TG_B_pred2$lci.link)
TG_B_pred2$uci<-plogis(TG_B_pred2$uci.link)

head(TG_B_pred2);dim(TG_B_pred2)

#Ground Cover + Crops

summary(TG_B_GC_C)

TG_Predictions_Crops <- seq(min(FDModel$Cropping_500m),max(FDModel$Cropping_500m),length.out=20)

TG_B_pred3 <- expand.grid(Ground_Cover = TG_Predictions_GC, Cropping_500m = TG_Predictions_Crops)
head(TG_B_pred3);dim(TG_B_pred3)

TG_B_pred4 <- predict(object = TG_B_GC_C,newdata= TG_B_pred3,se.fit = T, type = "link",re.form = ~0)

TG_B_pred5<-data.frame(TG_B_pred3,fit.link=TG_B_pred4$fit,se.link=TG_B_pred4$se.fit)

TG_B_pred5$lci.link<-TG_B_pred5$fit.link-(1.96*TG_B_pred5$se.link)
TG_B_pred5$uci.link<-TG_B_pred5$fit.link+(1.96*TG_B_pred5$se.link)

TG_B_pred5$fit<-plogis(TG_B_pred5$fit.link)
TG_B_pred5$se<-plogis(TG_B_pred5$se.link)
TG_B_pred5$lci<-plogis(TG_B_pred5$lci.link)
TG_B_pred5$uci<-plogis(TG_B_pred5$uci.link)

head(TG_B_pred5);dim(TG_B_pred5)


###Visualize----

#Ground Cover x Simpson
summary(TG_B_GCxLS)
head(TG_B_pred2)

JJ <- TG_B_pred2$Ground_Cover == TG_Predictions_GC[3]
J_J <- TG_B_pred2$Ground_Cover == TG_Predictions_GC[17]

dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = FDModel$X500m.Simspson,y = FDModel$TG_B,xlab = expression("Habitat Diversity within 500m"),ylab = 'Probability of Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = 32,'a)',cex=1)

polygon(x = c(TG_B_pred2$X500m.Simspson[JJ],rev(TG_B_pred2$X500m.Simspson[JJ])), y = c(TG_B_pred2$lci[JJ],rev(TG_B_pred2$uci[JJ])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_B_pred2$X500m.Simspson[JJ],y = TG_B_pred2$fit[JJ],lwd = 2,col = 'grey30')

polygon(x = c(TG_B_pred2$X500m.Simspson[J_J],rev(TG_B_pred2$X500m.Simspson[J_J])), y = c(TG_B_pred2$lci[J_J],rev(TG_B_pred2$uci[J_J])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_B_pred2$X500m.Simspson[J_J],y = TG_B_pred2$fit[J_J],lwd = 2,col = 'grey30',lty=2)

legend('left',legend = c("40% GC", "80% GC"), lty = c(1,2), col = 'grey30',pt.cex = 1)

#Ground Cover + Crops
summary(TG_B_GC_C)
head(TG_B_pred5)

KK <- TG_B_pred5$Cropping_500m == TG_Predictions_Crops[10]
K_K <- TG_B_pred5$Ground_Cover == TG_Predictions_GC[10]

dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = FDModel$Ground_Cover,y = FDModel$TG_B,xlab = expression("Ground Cover (%)"),ylab = 'Probability of Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = 32,'a)',cex=1)

polygon(x = c(TG_B_pred5$Ground_Cover[KK],rev(TG_B_pred5$Ground_Cover[KK])), y = c(TG_B_pred5$lci[KK],rev(TG_B_pred5$uci[KK])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_B_pred5$Ground_Cover[KK],y = TG_B_pred5$fit[KK],lwd = 2,col = 'grey30')

plot(x = FDModel$Cropping_500m,y = FDModel$TG_B,xlab = expression("Crops within 1km (%)"),ylab = 'Probability of Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = -8,'b)',cex=1)

polygon(x = c(TG_B_pred5$Cropping_500m[K_K],rev(TG_B_pred5$Cropping_500m[K_K])), y = c(TG_B_pred5$lci[K_K],rev(TG_B_pred5$uci[K_K])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_B_pred5$Cropping_500m[K_K],y = TG_B_pred5$fit[K_K],lwd = 2,col = 'grey30')

#Group D----

###Modelling----

head(FDModel)

####Day

TG_D_null <- glmmTMB(TG_D ~ 1 + (1 | Property), family = binomial, data = FDModel)

TG_D_day <- glmmTMB(TG_D ~ Day_Sampled + (1 | Property), family = binomial, data = FDModel)

aictab(list("Null" = TG_D_null,"Day" = TG_D_day))
#no need to include day in next step

####Single

TG_D_E <- glmmTMB(TG_D ~ Elevation + (1 | Property), family = binomial, data = FDModel)

TG_D_H <- glmmTMB(TG_D ~ Plant_Height + (1 | Property), family = binomial, data = FDModel)

TG_D_GC <- glmmTMB(TG_D ~ Ground_Cover + (1 | Property), family = binomial, data = FDModel)

TG_D_GGC <- glmmTMB(TG_D ~ Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)

TG_D_WE <- glmmTMB(TG_D ~ Weed_Estimate + (1 | Property), family = binomial, data = FDModel)

TG_D_GS <- glmmTMB(TG_D ~ Grass_Status + (1 | Property), family = binomial, data = FDModel)

TG_D_C <- glmmTMB(TG_D ~ Cropping_500m + (1 | Property), family = binomial, data = FDModel)

TG_D_LS <- glmmTMB(TG_D ~ X500m.Simspson + (1 | Property), family = binomial, data = FDModel)

TG_D_LC <- glmmTMB(TG_D ~ X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_D_modlist <- list("null" = TG_D_null,"Elevation" = TG_D_E,
                     "Height"=TG_D_H, "GC" = TG_D_GC,
                     "Green GC" = TG_D_GGC, 
                     "Weed Estimate" = TG_D_WE,
                     "Grass Status" = TG_D_GS, "Crops" = TG_D_C,
                     "Landscape Simpson" = TG_D_LS,
                     "Landscape Class" = TG_D_LC)

aictab(TG_D_modlist)
#Grass Status

####Additive

TG_D_E_H <- glmmTMB(TG_D ~ Elevation + Plant_Height + (1 | Property), family = binomial, data = FDModel)
TG_D_E_GC <- glmmTMB(TG_D ~ Elevation + Ground_Cover + (1 | Property), family = binomial, data = FDModel)
TG_D_E_GGC <- glmmTMB(TG_D ~ Elevation + Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_D_E_WE <- glmmTMB(TG_D ~ Elevation + Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_D_E_GS <- glmmTMB(TG_D ~ Elevation + Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_D_E_C <- glmmTMB(TG_D ~ Elevation + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_D_E_LS <- glmmTMB(TG_D ~ Elevation + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_D_E_LC <- glmmTMB(TG_D ~ Elevation + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)


TG_D_H_GC <- glmmTMB(TG_D ~ Plant_Height + Ground_Cover + (1 | Property), family = binomial, data = FDModel)
TG_D_H_GGC <- glmmTMB(TG_D ~ Plant_Height + Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_D_H_WE <- glmmTMB(TG_D ~ Plant_Height + Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_D_H_GS <- glmmTMB(TG_D ~ Plant_Height + Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_D_H_C <- glmmTMB(TG_D ~ Plant_Height + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_D_H_LS <- glmmTMB(TG_D ~ Plant_Height + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_D_H_LC <- glmmTMB(TG_D ~ Plant_Height + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)


TG_D_GC_GGC <- glmmTMB(TG_D ~ Ground_Cover + Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_D_GC_WE <- glmmTMB(TG_D ~ Ground_Cover + Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_D_GC_GS <- glmmTMB(TG_D ~ Ground_Cover + Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_D_GC_C <- glmmTMB(TG_D ~ Ground_Cover + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_D_GC_LS <- glmmTMB(TG_D ~ Ground_Cover + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_D_GC_LC <- glmmTMB(TG_D ~ Ground_Cover + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_D_GGC_WE <- glmmTMB(TG_D ~ Prop_Green_GC + Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_D_GGC_GS <- glmmTMB(TG_D ~ Prop_Green_GC + Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_D_GGC_C <- glmmTMB(TG_D ~ Prop_Green_GC + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_D_GGC_LS <- glmmTMB(TG_D ~ Prop_Green_GC + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_D_GGC_LC <- glmmTMB(TG_D ~ Prop_Green_GC + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_D_WE_GS <- glmmTMB(TG_D ~ Weed_Estimate + Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_D_WE_C <- glmmTMB(TG_D ~ Weed_Estimate + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_D_WE_LS <- glmmTMB(TG_D ~ Weed_Estimate + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_D_WE_LC <- glmmTMB(TG_D ~ Weed_Estimate + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_D_GS_C <- glmmTMB(TG_D ~ Grass_Status + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_D_GS_LS <- glmmTMB(TG_D ~ Grass_Status + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_D_GS_LC <- glmmTMB(TG_D ~ Grass_Status + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_D_C_LS <- glmmTMB(TG_D ~ Cropping_500m + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_D_C_LC <- glmmTMB(TG_D ~ Cropping_500m + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_D_LS_LC <- glmmTMB(TG_D ~ X500m.Simspson + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)


TG_D_Modnames <- c("TG_D_null","TG_D_E_H",
                   "TG_D_E_GC","TG_D_E_GGC",
                   "TG_D_E_WE","TG_D_E_GS",
                   "TG_D_E_C","TG_D_E_LS",
                   "TG_D_E_LC","TG_D_H_GC",
                   "TG_D_H_GGC","TG_D_H_WE",
                   "TG_D_H_GS","TG_D_H_C",
                   "TG_D_H_LS","TG_D_H_LC",
                   "TG_D_GC_GGC","TG_D_GC_WE",
                   "TG_D_GC_GS","TG_D_GC_C",
                   "TG_D_GC_LS","TG_D_GC_LC",
                   "TG_D_GGC_WE","TG_D_GGC_GS",
                   "TG_D_GGC_C","TG_D_GGC_LS",
                   "TG_D_GGC_LC","TG_D_WE_GS", 
                   "TG_D_WE_C","TG_D_WE_LS",
                   "TG_D_WE_LC","TG_D_GS_C",
                   "TG_D_GS_LS","TG_D_GS_LC",
                   "TG_D_C_LS","TG_D_C_LC",
                   "TG_D_LS_LC")
TG_D_modlist1 <- mget(TG_D_Modnames)

aictab(TG_D_modlist1)

#Height + Grass Status
#Grass Status + Landscape Class
#Grass Status + Landscape Simpson

####Interaction

TG_D_ExH <- glmmTMB(TG_D ~ Elevation * Plant_Height + (1 | Property), family = binomial, data = FDModel)
TG_D_ExGC <- glmmTMB(TG_D ~ Elevation * Ground_Cover + (1 | Property), family = binomial, data = FDModel)
TG_D_ExGGC <- glmmTMB(TG_D ~ Elevation * Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_D_ExWE <- glmmTMB(TG_D ~ Elevation_Scaled * Weed_Estimate + (1 | Property), family = binomial, data = FDModel) #Model Convergence problems
TG_D_ExGS <- glmmTMB(TG_D ~ Elevation * Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_D_ExC <- glmmTMB(TG_D ~ Elevation * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_D_ExLS <- glmmTMB(TG_D ~ Elevation * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_D_ExLC <- glmmTMB(TG_D ~ Elevation * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_D_HxGC <- glmmTMB(TG_D ~ Plant_Height * Ground_Cover + (1 | Property), family = binomial, data = FDModel)
TG_D_HxGGC <- glmmTMB(TG_D ~ Plant_Height * Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_D_HxWE <- glmmTMB(TG_D ~ Plant_Height * Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_D_HxGS <- glmmTMB(TG_D ~ Plant_Height * Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_D_HxC <- glmmTMB(TG_D ~ Plant_Height * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_D_HxLS <- glmmTMB(TG_D ~ Plant_Height * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_D_HxLC <- glmmTMB(TG_D ~ Plant_Height * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_D_GCxGGC <- glmmTMB(TG_D ~ Ground_Cover * Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_D_GCxWE <- glmmTMB(TG_D ~ GC_Scaled * Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_D_GCxGS <- glmmTMB(TG_D ~ Ground_Cover * Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_D_GCxC <- glmmTMB(TG_D ~ Ground_Cover * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_D_GCxLS <- glmmTMB(TG_D ~ Ground_Cover * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_D_GCxLC <- glmmTMB(TG_D ~ Ground_Cover * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_D_GGCxWE <- glmmTMB(TG_D ~ Prop_Green_GC * Weed_Estimate + (1 | Property), family = binomial, data = FDModel) #convergence problems
TG_D_GGCxGS <- glmmTMB(TG_D ~ Prop_Green_GC * Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_D_GGCxC <- glmmTMB(TG_D ~ Prop_Green_GC * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_D_GGCxLS <- glmmTMB(TG_D ~ Prop_Green_GC * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_D_GGCxLC <- glmmTMB(TG_D ~ Prop_Green_GC * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_D_WExC <- glmmTMB(TG_D ~ Weed_Estimate * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_D_WExLS <- glmmTMB(TG_D ~ Weed_Estimate * X500m.Simspson + (1 | Property), family = binomial, data = FDModel) #convergence problems

TG_D_GSxC <- glmmTMB(TG_D ~ Grass_Status * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_D_GSxLS <- glmmTMB(TG_D ~ Grass_Status * X500m.Simspson + (1 | Property), family = binomial, data = FDModel) 

TG_D_CxLS <- glmmTMB(TG_D ~ Cropping_500m * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_D_CxLC <- glmmTMB(TG_D ~ Cropping_500m * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_D_LSxLC <- glmmTMB(TG_D ~ X500m.Simspson * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)


TG_D_Modnames2 <- c("TG_D_null","TG_D_ExH",
                    "TG_D_ExGC","TG_D_ExGGC",
                    "TG_D_ExGS",
                    "TG_D_ExC","TG_D_ExLS",
                    "TG_D_ExLC","TG_D_HxGC",
                    "TG_D_HxGGC","TG_D_HxWE",
                    "TG_D_HxC","TG_D_HxGS",
                    "TG_D_HxLS","TG_D_HxLC",
                    "TG_D_GCxGGC","TG_D_GCxGS",
                    "TG_D_GCxC", 
                    "TG_D_GCxLS","TG_D_GCxLC",
                    "TG_D_GGCxGS",
                    "TG_D_GGCxC","TG_D_GGCxLS",
                    "TG_D_GGCxLC","TG_D_WExC",
                    "TG_D_GSxC",
                    "TG_D_GSxLS","TG_D_CxLS",
                    "TG_D_CxLC","TG_D_LSxLC")
TG_D_modlist2 <- mget(TG_D_Modnames2)

aictab(TG_D_modlist2)
#Height x Grass Status
#Grass Status x Landscape Class


####Final models----

TG_D_Modnames3<- c("TG_D_null","TG_D_GS", "TG_D_H_GS", "TG_D_GS_LC", "TG_D_GS_LS", "TG_D_HxGS","TG_D_GSxLS" )


TG_D_modlist_Final <- mget(TG_D_Modnames3)
aictab(TG_D_modlist_Final)



#Top model = Height + Grass Status

#Equivalent models (within 2 AICc):
#Grass Status + Landscape Class
#Grass Status + Landscape Simpson

###Predictions----

#Height + Grass Status
summary(TG_D_H_GS)

TG_Predictions_Height <- seq(min(FDModel$Plant_Height),max(FDModel$Plant_Height),length.out=20)

TG_D_pred <- expand.grid(Plant_Height = TG_Predictions_Height, Grass_Status = c("Native","Introduced"))
head(TG_D_pred);dim(TG_D_pred)

TG_D_pred1 <- predict(object = TG_D_H_GS,newdata= TG_D_pred,se.fit = T, type = "link",re.form = ~0)

TG_D_pred2<-data.frame(TG_D_pred,fit.link=TG_D_pred1$fit,se.link=TG_D_pred1$se.fit)

TG_D_pred2$lci.link<-TG_D_pred2$fit.link-(1.96*TG_D_pred2$se.link)
TG_D_pred2$uci.link<-TG_D_pred2$fit.link+(1.96*TG_D_pred2$se.link)

TG_D_pred2$fit<-plogis(TG_D_pred2$fit.link)
TG_D_pred2$se<-plogis(TG_D_pred2$se.link)
TG_D_pred2$lci<-plogis(TG_D_pred2$lci.link)
TG_D_pred2$uci<-plogis(TG_D_pred2$uci.link)

head(TG_D_pred2);dim(TG_D_pred2)

#Grass Status + Landscape Class
summary(TG_D_GS_LC)


TG_D_pred3 <- expand.grid(Grass_Status = c("Native","Introduced"),X500m.Dominant.Landscape.Class = unique(FDModel$X500m.Dominant.Landscape.Class))
head(TG_D_pred3);dim(TG_D_pred3)

TG_D_pred4 <- predict(object = TG_D_GS_LC,newdata= TG_D_pred3,se.fit = T, type = "link",re.form = ~0)

TG_D_pred5<-data.frame(TG_D_pred3,fit.link=TG_D_pred4$fit,se.link=TG_D_pred4$se.fit)

TG_D_pred5$lci.link<-TG_D_pred5$fit.link-(1.96*TG_D_pred5$se.link)
TG_D_pred5$uci.link<-TG_D_pred5$fit.link+(1.96*TG_D_pred5$se.link)

TG_D_pred5$fit<-plogis(TG_D_pred5$fit.link)
TG_D_pred5$se<-plogis(TG_D_pred5$se.link)
TG_D_pred5$lci<-plogis(TG_D_pred5$lci.link)
TG_D_pred5$uci<-plogis(TG_D_pred5$uci.link)

head(TG_D_pred5);dim(TG_D_pred5)

#Grass Status + Landscape Simpson
summary(TG_D_GS_LS)

TG_D_pred6 <- expand.grid(Grass_Status = c("Native","Introduced"),X500m.Simspson = TG_Predictions_Simspon)
head(TG_D_pred6);dim(TG_D_pred6)

TG_D_pred7 <- predict(object = TG_D_GS_LS,newdata= TG_D_pred6,se.fit = T, type = "link",re.form = ~0)

TG_D_pred8<-data.frame(TG_D_pred6,fit.link=TG_D_pred7$fit,se.link=TG_D_pred7$se.fit)

TG_D_pred8$lci.link<-TG_D_pred8$fit.link-(1.96*TG_D_pred8$se.link)
TG_D_pred8$uci.link<-TG_D_pred8$fit.link+(1.96*TG_D_pred8$se.link)

TG_D_pred8$fit<-plogis(TG_D_pred8$fit.link)
TG_D_pred8$se<-plogis(TG_D_pred8$se.link)
TG_D_pred8$lci<-plogis(TG_D_pred8$lci.link)
TG_D_pred8$uci<-plogis(TG_D_pred8$uci.link)

head(TG_D_pred8);dim(TG_D_pred8)


###Visualize----

#Height + Grass Status
summary(TG_D_H_GS)
head(TG_D_pred2)

LL <- TG_D_pred2$Grass_Status == "Introduced"
L_L <- TG_D_pred2$Plant_Height == TG_Predictions_Height[10]

dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = FDModel$Plant_Height,y = FDModel$TG_D,xlab = expression("Grass Height (cm)"),ylab = 'Probability of Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = -2,'a)',cex=1)

polygon(x = c(TG_D_pred2$Plant_Height[LL],rev(TG_D_pred2$Plant_Height[LL])), y = c(TG_D_pred2$lci[LL],rev(TG_D_pred2$uci[LL])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_D_pred2$Plant_Height[LL],y = TG_D_pred2$fit[LL],lwd = 2,col = 'grey30')

plot(x = 1:2,y = TG_D_pred2$fit[L_L],xlab = " ",ylab = 'Probability of Occurrence', type = 'p',pch = 16,cex =2.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,1))
axis(side=1,at=1:2,labels=c(' ',' '))
arrows(x0=1:2, y0=TG_D_pred2$lci[L_L],x1=1:2, y1=TG_D_pred2$uci[L_L],angle=90,length=0.2, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.25,'b)',cex=1.1)
mtext(side=1,line=1.5,at = 0.9,'Native\n Grass',cex=1.1)
mtext(side=1,line=1.5,at = 2.1,'Introduced\n Grass',cex=1.1)

points(x = jitter(raw_x1, factor = 1),y = FDModel$TG_D, pch = 16, cex = 0.4, col = "black")

#Grass Status + Landscape Class
summary(TG_D_GS_LC)
head(TG_D_pred5)

MM <- TG_D_pred5$Grass_Status == "Introduced"
M_M <- TG_D_pred5$X500m.Dominant.Landscape.Class == "NTV_Herbaceous_Open"

raw_x2 <- ifelse(FDModel$X500m.Dominant.Landscape.Class ==
                   "NTV_Woody_Closed", 1, 
                 ifelse(FDModel$X500m.Dominant.Landscape.Class ==
                          "NTV_Woody_Open", 2, 
                        ifelse(FDModel$X500m.Dominant.Landscape.Class ==
                                 "NTV_Herbaceous_Open", 3, NA)))

dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = 1:3,y = TG_D_pred5$fit [MM],xlab = " ",ylab = 'Probability of Occurrence', type = 'p',pch = 16,cex =2.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,4),ylim = c(0,1))
axis(side=1,at=1:3,labels=c(' ',' ', " "))
arrows(x0=1:3, y0=TG_D_pred5$lci [MM],x1=1:3, y1=TG_D_pred5$uci[MM],angle=90,length=0.1, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.3,'a)',cex=1)
mtext(side=1,line=1.5,at = 0.9,'Woody\nClosed',cex=1)
mtext(side=1,line=1.5,at = 2,'Herbaceous\nOpen',cex=1)
mtext(side=1,line=1.5,at = 3.1,"Woody\nOpen",cex=1)

points(x = jitter(raw_x2, factor = 1),y = FDModel$TG_D, pch = 16, cex = 0.4, col = "black")


plot(x = 1:2,y = TG_D_pred5$fit[M_M],xlab = " ",ylab = 'Probability of Occurrence', type = 'p',pch = 16,cex =2.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,1))
axis(side=1,at=1:2,labels=c(' ',' '))
arrows(x0=1:2, y0=TG_D_pred5$lci[M_M],x1=1:2, y1=TG_D_pred5$uci[M_M],angle=90,length=0.2, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.25,'b)',cex=1.1)
mtext(side=1,line=1.5,at = 0.9,'Native\n Grass',cex=1.1)
mtext(side=1,line=1.5,at = 2.1,'Introduced\n Grass',cex=1.1)

points(x = jitter(raw_x1, factor = 1),y = FDModel$TG_D, pch = 16, cex = 0.4, col = "black")


#Grass Status + Simpson
summary(TG_D_GS_LS)
head(TG_D_pred8)

NN <- TG_D_pred8$Grass_Status == "Introduced"
N_N <- TG_D_pred8$X500m.Simspson == TG_Predictions_Simspon[10]

dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = FDModel$X500m.Simspson,y = FDModel$TG_D,xlab = expression("Habitat Diversity within 500m"),ylab = 'Probability of Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = 0,'a)',cex=1)

polygon(x = c(TG_D_pred8$X500m.Simspson[NN],rev(TG_D_pred8$X500m.Simspson[NN])), y = c(TG_D_pred8$lci[NN],rev(TG_D_pred8$uci[NN])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_D_pred8$X500m.Simspson[NN],y = TG_D_pred8$fit[NN],lwd = 2,col = 'grey30')

plot(x = 1:2,y = TG_D_pred8$fit[N_N],xlab = " ",ylab = 'Probability of Occurrence', type = 'p',pch = 16,cex =2.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,1))
axis(side=1,at=1:2,labels=c(' ',' '))
arrows(x0=1:2, y0=TG_D_pred8$lci[N_N],x1=1:2, y1=TG_D_pred8$uci[N_N],angle=90,length=0.2, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.25,'b)',cex=1.1)
mtext(side=1,line=1.5,at = 0.9,'Native\n Grass',cex=1.1)
mtext(side=1,line=1.5,at = 2.1,'Introduced\n Grass',cex=1.1)

points(x = jitter(raw_x1, factor = 1),y = FDModel$TG_D, pch = 16, cex = 0.4, col = "black")

#Group E----

###Modelling----

head(FDModel);dim(FDModel)

####Day

TG_E_null <- glmmTMB(TG_E ~ 1 + (1 | Property), family = binomial, data = FDModel)

TG_E_day <- glmmTMB(TG_E ~ Day_Sampled + (1 | Property), family = binomial, data = FDModel)

aictab(list("Null" = TG_E_null,"Day" = TG_E_day))
#Include day in next step

####Single

TG_E_E <- glmmTMB(TG_E ~ Day_Sampled + Elevation + (1 | Property), family = binomial, data = FDModel)

TG_E_H <- glmmTMB(TG_E ~ Day_Sampled + Plant_Height + (1 | Property), family = binomial, data = FDModel)

TG_E_GC <- glmmTMB(TG_E ~ Day_Sampled + Ground_Cover + (1 | Property), family = binomial, data = FDModel)

TG_E_GGC <- glmmTMB(TG_E ~ Day_Sampled + Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)

TG_E_WE <- glmmTMB(TG_E ~ Day_Sampled + Weed_Estimate + (1 | Property), family = binomial, data = FDModel)

TG_E_GS <- glmmTMB(TG_E ~ Day_Sampled + Grass_Status + (1 | Property), family = binomial, data = FDModel)

TG_E_C <- glmmTMB(TG_E ~ Day_Sampled + Cropping_500m + (1 | Property), family = binomial, data = FDModel)

TG_E_LS <- glmmTMB(TG_E ~ Day_Sampled + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)

TG_E_LC <- glmmTMB(TG_E ~ Day_Sampled + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_E_modlist <- list("null" = TG_E_null,"Elevation" = TG_E_E,
                     "Height"=TG_E_H, "GC" = TG_E_GC,
                     "Green GC" = TG_E_GGC, 
                     "Weed Estimate" = TG_E_WE,
                     "Grass Status" = TG_E_GS, "Crops" = TG_E_C,
                     "Landscape Simpson" = TG_E_LS,
                     "Landscape Class" = TG_E_LC)

aictab(TG_E_modlist)
#Grass Status

####Additive

TG_E_E_H <- glmmTMB(TG_E ~ Day_Sampled + Elevation + Plant_Height + (1 | Property), family = binomial, data = FDModel)
TG_E_E_GC <- glmmTMB(TG_E ~ Day_Sampled + Elevation + Ground_Cover + (1 | Property), family = binomial, data = FDModel)
TG_E_E_GGC <- glmmTMB(TG_E ~ Day_Sampled + Elevation + Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_E_E_WE <- glmmTMB(TG_E ~ Day_Sampled + Elevation + Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_E_E_GS <- glmmTMB(TG_E ~ Day_Sampled + Elevation + Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_E_E_C <- glmmTMB(TG_E ~ Day_Sampled + Elevation + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_E_E_LS <- glmmTMB(TG_E ~ Day_Sampled + Elevation + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_E_E_LC <- glmmTMB(TG_E ~ Day_Sampled + Elevation + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)


TG_E_H_GC <- glmmTMB(TG_E ~ Day_Sampled + Plant_Height + Ground_Cover + (1 | Property), family = binomial, data = FDModel)
TG_E_H_GGC <- glmmTMB(TG_E ~ Day_Sampled + Plant_Height + Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_E_H_WE <- glmmTMB(TG_E ~ Day_Sampled + Plant_Height + Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_E_H_GS <- glmmTMB(TG_E ~ Day_Sampled + Plant_Height + Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_E_H_C <- glmmTMB(TG_E ~ Day_Sampled + Plant_Height + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_E_H_LS <- glmmTMB(TG_E ~ Day_Sampled + Plant_Height + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_E_H_LC <- glmmTMB(TG_E ~ Day_Sampled + Plant_Height + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)


TG_E_GC_GGC <- glmmTMB(TG_E ~ Day_Sampled + Ground_Cover + Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_E_GC_WE <- glmmTMB(TG_E ~ Day_Sampled + Ground_Cover + Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_E_GC_GS <- glmmTMB(TG_E ~ Day_Sampled + Ground_Cover + Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_E_GC_C <- glmmTMB(TG_E ~ Day_Sampled + Ground_Cover + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_E_GC_LS <- glmmTMB(TG_E ~ Day_Sampled + Ground_Cover + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_E_GC_LC <- glmmTMB(TG_E ~ Day_Sampled + Ground_Cover + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_E_GGC_WE <- glmmTMB(TG_E ~ Day_Sampled + Prop_Green_GC + Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_E_GGC_GS <- glmmTMB(TG_E ~ Day_Sampled + Prop_Green_GC + Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_E_GGC_C <- glmmTMB(TG_E ~ Day_Sampled + Prop_Green_GC + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_E_GGC_LS <- glmmTMB(TG_E ~ Day_Sampled + Prop_Green_GC + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_E_GGC_LC <- glmmTMB(TG_E ~ Day_Sampled + Prop_Green_GC + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_E_WE_GS <- glmmTMB(TG_E ~ Day_Sampled + Weed_Estimate + Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_E_WE_C <- glmmTMB(TG_E ~ Day_Sampled + Weed_Estimate + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_E_WE_LS <- glmmTMB(TG_E ~ Day_Sampled + Weed_Estimate + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_E_WE_LC <- glmmTMB(TG_E ~ Day_Sampled + Weed_Estimate + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_E_GS_C <- glmmTMB(TG_E ~ Day_Sampled + Grass_Status + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_E_GS_LS <- glmmTMB(TG_E ~ Day_Sampled + Grass_Status + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_E_GS_LC <- glmmTMB(TG_E ~ Day_Sampled + Grass_Status + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_E_C_LS <- glmmTMB(TG_E ~ Day_Sampled + Cropping_500m + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_E_C_LC <- glmmTMB(TG_E ~ Day_Sampled + Cropping_500m + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_E_LS_LC <- glmmTMB(TG_E ~ Day_Sampled + X500m.Simspson + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)


TG_E_Modnames <- c("TG_E_null","TG_E_E_H",
                   "TG_E_E_GC","TG_E_E_GGC",
                   "TG_E_E_WE","TG_E_E_GS",
                   "TG_E_E_C","TG_E_E_LS",
                   "TG_E_E_LC","TG_E_H_GC",
                   "TG_E_H_GGC","TG_E_H_WE",
                   "TG_E_H_GS","TG_E_H_C",
                   "TG_E_H_LS","TG_E_H_LC",
                   "TG_E_GC_GGC","TG_E_GC_WE",
                   "TG_E_GC_GS","TG_E_GC_C",
                   "TG_E_GC_LS","TG_E_GC_LC",
                   "TG_E_GGC_WE","TG_E_GGC_GS",
                   "TG_E_GGC_C","TG_E_GGC_LS",
                   "TG_E_GGC_LC","TG_E_WE_GS", 
                   "TG_E_WE_C","TG_E_WE_LS",
                   "TG_E_WE_LC","TG_E_GS_C",
                   "TG_E_GS_LS","TG_E_GS_LC",
                   "TG_E_C_LS","TG_E_C_LC",
                   "TG_E_LS_LC")
TG_E_modlist1 <- mget(TG_E_Modnames)

aictab(TG_E_modlist1)

#Ground Cover + Grass Status
#Green Ground Cover + Grass Status
#Height + Grass Status
#Elevation + Grass Status


####Interaction

FDModel$Day_scaled <- scale(FDModel$Day_Sampled)

TG_E_ExH <- glmmTMB(TG_E ~ Day_Sampled + Elevation_Scaled * Plant_Height + (1 | Property), family = binomial, data = FDModel)
TG_E_ExGC <- glmmTMB(TG_E ~ Day_Sampled + Elevation_Scaled * Ground_Cover + (1 | Property), family = binomial, data = FDModel)
TG_E_ExGGC <- glmmTMB(TG_E ~ Day_Sampled + Elevation_Scaled * Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_E_ExWE <- glmmTMB(TG_E ~ Day_scaled + Elevation_Scaled * Weed_Estimate + (1 | Property), family = binomial, data = FDModel) #Model Convergence problems
TG_E_ExGS <- glmmTMB(TG_E ~ Day_Sampled + Elevation * Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_E_ExC <- glmmTMB(TG_E ~ Day_Sampled + Elevation_Scaled * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_E_ExLS <- glmmTMB(TG_E ~ Day_Sampled + Elevation * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_E_ExLC <- glmmTMB(TG_E ~ Day_Sampled + Elevation * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_E_HxGC <- glmmTMB(TG_E ~ Day_Sampled + Plant_Height * Ground_Cover + (1 | Property), family = binomial, data = FDModel)
TG_E_HxGGC <- glmmTMB(TG_E ~ Day_Sampled + Plant_Height * Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_E_HxWE <- glmmTMB(TG_E ~ Day_scaled + Height_Scaled * Weed_Estimate + (1 | Property), family = binomial, data = FDModel) #model converge problem
TG_E_HxGS <- glmmTMB(TG_E ~ Day_Sampled + Plant_Height * Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_E_HxC <- glmmTMB(TG_E ~ Day_Sampled + Plant_Height * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_E_HxLS <- glmmTMB(TG_E ~ Day_Sampled + Plant_Height * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_E_HxLC <- glmmTMB(TG_E ~ Day_Sampled + Plant_Height * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_E_GCxGGC <- glmmTMB(TG_E ~ Day_Sampled + GC_Scaled * Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_E_GCxWE <- glmmTMB(TG_E ~ Day_scaled + GC_Scaled * Weed_Estimate + (1 | Property), family = binomial, data = FDModel) #convergence issue
TG_E_GCxGS <- glmmTMB(TG_E ~ Day_Sampled + Ground_Cover * Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_E_GCxC <- glmmTMB(TG_E ~ Day_Sampled + GC_Scaled * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_E_GCxLS <- glmmTMB(TG_E ~ Day_Sampled + Ground_Cover * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_E_GCxLC <- glmmTMB(TG_E ~ Day_Sampled + Ground_Cover * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_E_GGCxWE <- glmmTMB(TG_E ~ Day_Sampled + Prop_Green_GC * Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_E_GGCxGS <- glmmTMB(TG_E ~ Day_Sampled + Prop_Green_GC * Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_E_GGCxC <- glmmTMB(TG_E ~ Day_Sampled + Prop_Green_GC * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_E_GGCxLS <- glmmTMB(TG_E ~ Day_Sampled + Prop_Green_GC * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_E_GGCxLC <- glmmTMB(TG_E ~ Day_Sampled + Prop_Green_GC * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_E_WExC <- glmmTMB(TG_E ~ Day_Sampled + Weed_Estimate * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_E_WExLS <- glmmTMB(TG_E ~ Day_Sampled + Weed_Estimate * X500m.Simspson + (1 | Property), family = binomial, data = FDModel) 

TG_E_GSxC <- glmmTMB(TG_E ~ Day_Sampled + Grass_Status * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_E_GSxLS <- glmmTMB(TG_E ~ Day_Sampled + Grass_Status * X500m.Simspson + (1 | Property), family = binomial, data = FDModel) 

TG_E_CxLS <- glmmTMB(TG_E ~ Day_Sampled + Cropping_500m * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_E_CxLC <- glmmTMB(TG_E ~ Day_Sampled + Cropping_500m * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_E_LSxLC <- glmmTMB(TG_E ~ Day_Sampled + X500m.Simspson * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)


TG_E_Modnames2 <- c("TG_E_null","TG_E_ExH",
                    "TG_E_ExGC","TG_E_ExGGC",
                    "TG_E_ExGS",
                    "TG_E_ExC","TG_E_ExLS",
                    "TG_E_ExLC","TG_E_HxGC",
                    "TG_E_HxGGC","TG_E_HxWE",
                    "TG_E_HxC","TG_E_HxGS",
                    "TG_E_HxLS","TG_E_HxLC",
                    "TG_E_GCxGGC","TG_E_GCxGS",
                    "TG_E_GCxC", 
                    "TG_E_GCxLS","TG_E_GCxLC",
                    "TG_E_GGCxGS", "TG_E_GGCxWE",
                    "TG_E_GGCxC","TG_E_GGCxLS",
                    "TG_E_GGCxLC","TG_E_WExC",
                    "TG_E_WExLS","TG_E_GSxC",
                    "TG_E_GSxLS","TG_E_CxLS",
                    "TG_E_CxLC","TG_E_LSxLC")
TG_E_modlist2 <- mget(TG_E_Modnames2)

aictab(TG_E_modlist2)

#Green GC x Grass Status

####Final models----

TG_E_Modnames3<- c("TG_E_GS", "TG_E_null","TG_E_GC_GS", 
                   "TG_E_GGC_GS", "TG_E_H_GS","TG_E_E_GS",
                   "TG_E_GGCxGS")


TG_E_modlist_Final <- mget(TG_E_Modnames3)
aictab(TG_E_modlist_Final)



#Top model = Green GC x Grass Status   

#Equivalent models (within 2 AICc):
##Grass Status
##Ground Cover + Grass Status

###Predictions----

#Green GC x Grass Status
summary(TG_E_GGCxGS)

TG_Predictions_Day <- seq(min(FDModel$Day_Sampled),max(FDModel$Day_Sampled),length.out=20)

TG_Predictions_GreenGC <- seq(min(FDModel$Prop_Green_GC),max(FDModel$Prop_Green_GC),length.out=20)

TG_E_pred <- expand.grid(Day_Sampled = TG_Predictions_Day, Prop_Green_GC = TG_Predictions_GreenGC, Grass_Status  = c("Native","Introduced") )
head(TG_E_pred);dim(TG_E_pred)

TG_E_pred1 <- predict(object = TG_E_GGCxGS,newdata= TG_E_pred,se.fit = T, type = "link",re.form = ~0)

TG_E_pred2<-data.frame(TG_E_pred,fit.link=TG_E_pred1$fit,se.link=TG_E_pred1$se.fit)

TG_E_pred2$lci.link<-TG_E_pred2$fit.link-(1.96*TG_E_pred2$se.link)
TG_E_pred2$uci.link<-TG_E_pred2$fit.link+(1.96*TG_E_pred2$se.link)

TG_E_pred2$fit<-plogis(TG_E_pred2$fit.link)
TG_E_pred2$se<-plogis(TG_E_pred2$se.link)
TG_E_pred2$lci<-plogis(TG_E_pred2$lci.link)
TG_E_pred2$uci<-plogis(TG_E_pred2$uci.link)

head(TG_E_pred2);dim(TG_E_pred2)


##Grass Status
summary(TG_E_GS)

TG_E_pred3 <- expand.grid(Day_Sampled = TG_Predictions_Day, Grass_Status = c("Native","Introduced"))
head(TG_E_pred3);dim(TG_E_pred3)

TG_E_pred4 <- predict(object = TG_E_GS,newdata= TG_E_pred3,se.fit = T, type = "link",re.form = ~0)

TG_E_pred5<-data.frame(TG_E_pred3,fit.link=TG_E_pred4$fit,se.link=TG_E_pred4$se.fit)

TG_E_pred5$lci.link<-TG_E_pred5$fit.link-(1.96*TG_E_pred5$se.link)
TG_E_pred5$uci.link<-TG_E_pred5$fit.link+(1.96*TG_E_pred5$se.link)

TG_E_pred5$fit<-plogis(TG_E_pred5$fit.link)
TG_E_pred5$se<-plogis(TG_E_pred5$se.link)
TG_E_pred5$lci<-plogis(TG_E_pred5$lci.link)
TG_E_pred5$uci<-plogis(TG_E_pred5$uci.link)

head(TG_E_pred5);dim(TG_E_pred5)

##Ground Cover + Grass Status
summary(TG_E_GC_GS)

TG_E_pred6 <- expand.grid(Day_Sampled = TG_Predictions_Day, Ground_Cover  = TG_Predictions_GC,Grass_Status = c("Native","Introduced"))
head(TG_E_pred6);dim(TG_E_pred6)

TG_E_pred7 <- predict(object = TG_E_GC_GS,newdata= TG_E_pred6,se.fit = T, type = "link",re.form = ~0)

TG_E_pred8<-data.frame(TG_E_pred6,fit.link=TG_E_pred7$fit,se.link=TG_E_pred7$se.fit)

TG_E_pred8$lci.link<-TG_E_pred8$fit.link-(1.96*TG_E_pred8$se.link)
TG_E_pred8$uci.link<-TG_E_pred8$fit.link+(1.96*TG_E_pred8$se.link)

TG_E_pred8$fit<-plogis(TG_E_pred8$fit.link)
TG_E_pred8$se<-plogis(TG_E_pred8$se.link)
TG_E_pred8$lci<-plogis(TG_E_pred8$lci.link)
TG_E_pred8$uci<-plogis(TG_E_pred8$uci.link)

head(TG_E_pred8);dim(TG_E_pred8)

###Visualize----

#Green GC x Grass Status + Day
summary(TG_E_GGCxGS)
head(TG_E_pred2)

OO <- TG_E_pred2$Grass_Status == "Native" & TG_E_pred2$Day_Sampled == TG_Predictions_Day[10]
O_O <- TG_E_pred2$Grass_Status == "Introduced" & TG_E_pred2$Day_Sampled == TG_Predictions_Day[10]
OOO <- TG_E_pred2$Grass_Status == "Introduced" & TG_E_pred2$Prop_Green_GC == TG_Predictions_GreenGC[10]

dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = FDModel$Prop_Green_GC,y = FDModel$TG_E,xlab = expression("Green Ground Cover (%)"),ylab = 'Probability of Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = 1.5,'a)',cex=1)

polygon(x = c(TG_E_pred2$Prop_Green_GC[OO],rev(TG_E_pred2$Prop_Green_GC[OO])), y = c(TG_E_pred2$lci[OO],rev(TG_E_pred2$uci[OO])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_E_pred2$Prop_Green_GC[OO],y = TG_E_pred2$fit[OO],lwd = 2,col = 'grey30')

polygon(x = c(TG_E_pred2$Prop_Green_GC[O_O],rev(TG_E_pred2$Prop_Green_GC[O_O])), y = c(TG_E_pred2$lci[O_O],rev(TG_E_pred2$uci[O_O])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_E_pred2$Prop_Green_GC[O_O],y = TG_E_pred2$fit[O_O],lwd = 2,col = 'grey30',lty = 2)

legend('bottomleft',legend = c("Native", "Introduced"), lty = c(1,2), col = 'grey30',pt.cex = 1)

plot(x = FDModel$Day_Sampled,y = FDModel$TG_E,xlab = expression("Day Sampled"),ylab = 'Probability of Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = -3,'b)',cex=1)

polygon(x = c(TG_E_pred2$Day_Sampled[OOO],rev(TG_E_pred2$Day_Sampled[OOO])), y = c(TG_E_pred2$lci[OOO],rev(TG_E_pred2$uci[OOO])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_E_pred2$Day_Sampled[OOO],y = TG_E_pred2$fit[OOO],lwd = 2,col = 'grey30')

#Grass Status + Day
summary(TG_E_GS)
head(TG_E_pred5)

PP <- TG_E_pred5$Day_Sampled == TG_Predictions_Day[10]
P_P <- TG_E_pred5$Grass_Status == "Introduced"

dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = 1:2,y = TG_E_pred5$fit[PP],xlab = " ",ylab = 'Probability of Occurrence', type = 'p',pch = 16,cex =2.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,1))
axis(side=1,at=1:2,labels=c(' ',' '))
arrows(x0=1:2, y0=TG_E_pred5$lci[PP],x1=1:2, y1=TG_E_pred5$uci[PP],angle=90,length=0.2, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.25,'a)',cex=1.1)
mtext(side=1,line=1.5,at = 0.9,'Native\n Grass',cex=1.1)
mtext(side=1,line=1.5,at = 2.1,'Introduced\n Grass',cex=1.1)

points(x = jitter(raw_x1, factor = 1),y = FDModel$TG_E, pch = 16, cex = 0.4, col = "black")


plot(x = FDModel$Day_Sampled,y = FDModel$TG_E,xlab = expression("Day Sampled"),ylab = 'Probability of Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = -3,'b)',cex=1)

polygon(x = c(TG_E_pred5$Day_Sampled[P_P],rev(TG_E_pred5$Day_Sampled[P_P])), y = c(TG_E_pred5$lci[P_P],rev(TG_E_pred5$uci[P_P])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_E_pred5$Day_Sampled[P_P],y = TG_E_pred5$fit[P_P],lwd = 2,col = 'grey30')

##Ground Cover + Grass Status + Day
summary(TG_E_GC_GS)
head(TG_E_pred8)

QQ <- TG_E_pred8$Grass_Status == "Introduced" & TG_E_pred8$Day_Sampled == TG_Predictions_Day[10]
Q_Q <- TG_E_pred8$Ground_Cover == TG_Predictions_GC[10] & TG_E_pred8$Day_Sampled == TG_Predictions_Day[10]
QQQ <- TG_E_pred8$Grass_Status == "Introduced" & TG_E_pred8$Ground_Cover == TG_Predictions_GC[14]

dev.new(height=10,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(2,2),mgp=c(2.5,1,0),xpd = T)

plot(x = FDModel$Ground_Cover,y = FDModel$TG_E,xlab = expression("Ground Cover (%)"),ylab = 'Probability of Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = 33,'a)',cex=0.9)

polygon(x = c(TG_E_pred8$Ground_Cover[QQ],rev(TG_E_pred8$Ground_Cover[QQ])), y = c(TG_E_pred8$lci[QQ],rev(TG_E_pred8$uci[QQ])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_E_pred8$Ground_Cover[QQ],y = TG_E_pred8$fit[QQ],lwd = 2,col = 'grey30')

plot(x = 1:2,y = TG_E_pred8$fit[Q_Q],xlab = " ",ylab = 'Probability of Occurrence', type = 'p',pch = 16,cex =2.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,1))
axis(side=1,at=1:2,labels=c(' ',' '))
arrows(x0=1:2, y0=TG_E_pred8$lci[Q_Q],x1=1:2, y1=TG_E_pred8$uci[Q_Q],angle=90,length=0.2, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.25,'b)',cex=0.9)
mtext(side=1,line=1.5,at = 0.9,'Native\n Grass',cex=0.9)
mtext(side=1,line=1.5,at = 2.1,'Introduced\n Grass',cex=0.9)

points(x = jitter(raw_x1, factor = 1),y = FDModel$TG_E, pch = 16, cex = 0.4, col = "black")

plot(x = FDModel$Day_Sampled,y = FDModel$TG_E,xlab = expression("Day Sampled"),ylab = 'Probability of Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = -3,'c)',cex=1)

polygon(x = c(TG_E_pred8$Day_Sampled[QQQ],rev(TG_E_pred8$Day_Sampled[QQQ])), y = c(TG_E_pred8$lci[QQQ],rev(TG_E_pred8$uci[QQQ])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_E_pred8$Day_Sampled[QQQ],y = TG_E_pred8$fit[QQQ],lwd = 2,col = 'grey30')

#Group F----

###Modelling----

head(FDModel);dim(FDModel)

####Day

TG_F_null <- glmmTMB(TG_F ~ 1 + (1 | Property), family = binomial, data = FDModel)

TG_F_day <- glmmTMB(TG_F ~ Day_Sampled + (1 | Property), family = binomial, data = FDModel)

aictab(list("Null" = TG_F_null,"Day" = TG_F_day))
#Include day in next step

####Single

TG_F_E <- glmmTMB(TG_F ~ Day_Sampled + Elevation + (1 | Property), family = binomial, data = FDModel)

TG_F_H <- glmmTMB(TG_F ~ Day_Sampled + Plant_Height + (1 | Property), family = binomial, data = FDModel)

TG_F_GC <- glmmTMB(TG_F ~ Day_Sampled + Ground_Cover + (1 | Property), family = binomial, data = FDModel)

TG_F_GGC <- glmmTMB(TG_F ~ Day_Sampled + Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)

TG_F_WE <- glmmTMB(TG_F ~ Day_Sampled + Weed_Estimate + (1 | Property), family = binomial, data = FDModel)

TG_F_GS <- glmmTMB(TG_F ~ Day_Sampled + Grass_Status + (1 | Property), family = binomial, data = FDModel)

TG_F_C <- glmmTMB(TG_F ~ Day_Sampled + Cropping_500m + (1 | Property), family = binomial, data = FDModel)

TG_F_LS <- glmmTMB(TG_F ~ Day_Sampled + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)

TG_F_LC <- glmmTMB(TG_F ~ Day_Sampled + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_F_modlist <- list("null" = TG_F_null,"Elevation" = TG_F_E,
                     "Height"= TG_F_H, "GC" = TG_F_GC,
                     "Green GC" = TG_F_GGC, 
                     "Weed Estimate" = TG_F_WE,
                     "Grass Status" = TG_F_GS, "Crops" = TG_F_C,
                     "Landscape Simpson" = TG_F_LS,
                     "Landscape Class" = TG_F_LC)

aictab(TG_F_modlist)
#Grass Status


####Additive

TG_F_E_H <- glmmTMB(TG_F ~ Day_Sampled + Elevation + Plant_Height + (1 | Property), family = binomial, data = FDModel)
TG_F_E_GC <- glmmTMB(TG_F ~ Day_Sampled + Elevation + Ground_Cover + (1 | Property), family = binomial, data = FDModel)
TG_F_E_GGC <- glmmTMB(TG_F ~ Day_Sampled + Elevation + Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_F_E_WE <- glmmTMB(TG_F ~ Day_Sampled + Elevation + Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_F_E_GS <- glmmTMB(TG_F ~ Day_Sampled + Elevation + Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_F_E_C <- glmmTMB(TG_F ~ Day_Sampled + Elevation + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_F_E_LS <- glmmTMB(TG_F ~ Day_Sampled + Elevation + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_F_E_LC <- glmmTMB(TG_F ~ Day_Sampled + Elevation + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)


TG_F_H_GC <- glmmTMB(TG_F ~ Day_Sampled + Plant_Height + Ground_Cover + (1 | Property), family = binomial, data = FDModel)
TG_F_H_GGC <- glmmTMB(TG_F ~ Day_Sampled + Plant_Height + Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_F_H_WE <- glmmTMB(TG_F ~ Day_Sampled + Plant_Height + Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_F_H_GS <- glmmTMB(TG_F ~ Day_Sampled + Plant_Height + Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_F_H_C <- glmmTMB(TG_F ~ Day_Sampled + Plant_Height + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_F_H_LS <- glmmTMB(TG_F ~ Day_Sampled + Plant_Height + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_F_H_LC <- glmmTMB(TG_F ~ Day_Sampled + Plant_Height + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)


TG_F_GC_GGC <- glmmTMB(TG_F ~ Day_Sampled + Ground_Cover + Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_F_GC_WE <- glmmTMB(TG_F ~ Day_Sampled + Ground_Cover + Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_F_GC_GS <- glmmTMB(TG_F ~ Day_Sampled + Ground_Cover + Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_F_GC_C <- glmmTMB(TG_F ~ Day_Sampled + Ground_Cover + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_F_GC_LS <- glmmTMB(TG_F ~ Day_Sampled + Ground_Cover + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_F_GC_LC <- glmmTMB(TG_F ~ Day_Sampled + Ground_Cover + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_F_GGC_WE <- glmmTMB(TG_F ~ Day_Sampled + Prop_Green_GC + Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_F_GGC_GS <- glmmTMB(TG_F ~ Day_Sampled + Prop_Green_GC + Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_F_GGC_C <- glmmTMB(TG_F ~ Day_Sampled + Prop_Green_GC + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_F_GGC_LS <- glmmTMB(TG_F ~ Day_Sampled + Prop_Green_GC + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_F_GGC_LC <- glmmTMB(TG_F ~ Day_Sampled + Prop_Green_GC + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_F_WE_GS <- glmmTMB(TG_F ~ Day_Sampled + Weed_Estimate + Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_F_WE_C <- glmmTMB(TG_F ~ Day_Sampled + Weed_Estimate + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_F_WE_LS <- glmmTMB(TG_F ~ Day_Sampled + Weed_Estimate + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_F_WE_LC <- glmmTMB(TG_F ~ Day_Sampled + Weed_Estimate + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_F_GS_C <- glmmTMB(TG_F ~ Day_Sampled + Grass_Status + Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_F_GS_LS <- glmmTMB(TG_F ~ Day_Sampled + Grass_Status + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_F_GS_LC <- glmmTMB(TG_F ~ Day_Sampled + Grass_Status + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_F_C_LS <- glmmTMB(TG_F ~ Day_Sampled + Cropping_500m + X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_F_C_LC <- glmmTMB(TG_F ~ Day_Sampled + Cropping_500m + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_F_LS_LC <- glmmTMB(TG_F ~ Day_Sampled + X500m.Simspson + X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)


TG_F_Modnames <- c("TG_F_null","TG_F_E_H",
                   "TG_F_E_GC","TG_F_E_GGC",
                   "TG_F_E_WE","TG_F_E_GS",
                   "TG_F_E_C","TG_F_E_LS",
                   "TG_F_E_LC","TG_F_H_GC",
                   "TG_F_H_GGC","TG_F_H_WE",
                   "TG_F_H_GS","TG_F_H_C",
                   "TG_F_H_LS","TG_F_H_LC",
                   "TG_F_GC_GGC","TG_F_GC_WE",
                   "TG_F_GC_GS","TG_F_GC_C",
                   "TG_F_GC_LS","TG_F_GC_LC",
                   "TG_F_GGC_WE","TG_F_GGC_GS",
                   "TG_F_GGC_C","TG_F_GGC_LS",
                   "TG_F_GGC_LC","TG_F_WE_GS", 
                   "TG_F_WE_C","TG_F_WE_LS",
                   "TG_F_WE_LC","TG_F_GS_C",
                   "TG_F_GS_LS","TG_F_GS_LC",
                   "TG_F_C_LS","TG_F_C_LC",
                   "TG_F_LS_LC")
TG_F_modlist1 <- mget(TG_F_Modnames)

aictab(TG_F_modlist1)

#Grass Status + Landscape Class
#Weed Estimate + Grass Status

####Interaction

FDModel$Green_GC_Scaled <- scale(FDModel$Prop_Green_GC)

TG_F_ExH <- glmmTMB(TG_F ~ Day_Sampled + Elevation_Scaled * Plant_Height + (1 | Property), family = binomial, data = FDModel)
TG_F_ExGC <- glmmTMB(TG_F ~ Day_Sampled + Elevation_Scaled * Ground_Cover + (1 | Property), family = binomial, data = FDModel)
TG_F_ExGGC <- glmmTMB(TG_F ~ Day_Sampled + Elevation_Scaled * Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_F_ExWE <- glmmTMB(TG_F ~ Day_Sampled + Elevation_Scaled * Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_F_ExGS <- glmmTMB(TG_F ~ Day_Sampled + Elevation * Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_F_ExC <- glmmTMB(TG_F ~ Day_Sampled + Elevation_Scaled * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_F_ExLS <- glmmTMB(TG_F ~ Day_Sampled + Elevation * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_F_ExLC <- glmmTMB(TG_F ~ Day_Sampled + Elevation * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_F_HxGC <- glmmTMB(TG_F ~ Day_Sampled + Plant_Height * Ground_Cover + (1 | Property), family = binomial, data = FDModel)
TG_F_HxGGC <- glmmTMB(TG_F ~ Day_Sampled + Plant_Height * Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_F_HxWE <- glmmTMB(TG_F ~ Day_Sampled + Height_Scaled * Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_F_HxGS <- glmmTMB(TG_F ~ Day_Sampled + Plant_Height * Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_F_HxC <- glmmTMB(TG_F ~ Day_Sampled + Plant_Height * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_F_HxLS <- glmmTMB(TG_F ~ Day_Sampled + Plant_Height * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_F_HxLC <- glmmTMB(TG_F ~ Day_Sampled + Plant_Height * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_F_GCxGGC <- glmmTMB(TG_F ~ Day_Sampled + GC_Scaled * Prop_Green_GC + (1 | Property), family = binomial, data = FDModel)
TG_F_GCxWE <- glmmTMB(TG_F ~ Day_Sampled + Ground_Cover * Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_F_GCxGS <- glmmTMB(TG_F ~ Day_Sampled + Ground_Cover * Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_F_GCxC <- glmmTMB(TG_F ~ Day_Sampled + GC_Scaled * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_F_GCxLS <- glmmTMB(TG_F ~ Day_Sampled + Ground_Cover * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_F_GCxLC <- glmmTMB(TG_F ~ Day_Sampled + Ground_Cover * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_F_GGCxWE <- glmmTMB(TG_F ~ Day_scaled + Green_GC_Scaled * Weed_Estimate + (1 | Property), family = binomial, data = FDModel)
TG_F_GGCxGS <- glmmTMB(TG_F ~ Day_Sampled + Prop_Green_GC * Grass_Status + (1 | Property), family = binomial, data = FDModel)
TG_F_GGCxC <- glmmTMB(TG_F ~ Day_Sampled + Prop_Green_GC * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_F_GGCxLS <- glmmTMB(TG_F ~ Day_Sampled + Prop_Green_GC * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_F_GGCxLC <- glmmTMB(TG_F ~ Day_Sampled + Prop_Green_GC * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_F_WExC <- glmmTMB(TG_F ~ Day_Sampled + Weed_Estimate * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_F_WExLS <- glmmTMB(TG_F ~ Day_Sampled + Weed_Estimate * X500m.Simspson + (1 | Property), family = binomial, data = FDModel) 

TG_F_GSxC <- glmmTMB(TG_F ~ Day_Sampled + Grass_Status * Cropping_500m + (1 | Property), family = binomial, data = FDModel)
TG_F_GSxLS <- glmmTMB(TG_F ~ Day_Sampled + Grass_Status * X500m.Simspson + (1 | Property), family = binomial, data = FDModel) 

TG_F_CxLS <- glmmTMB(TG_F ~ Day_Sampled + Cropping_500m * X500m.Simspson + (1 | Property), family = binomial, data = FDModel)
TG_F_CxLC <- glmmTMB(TG_F ~ Day_Sampled + Cropping_500m * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)

TG_F_LSxLC <- glmmTMB(TG_F ~ Day_Sampled + X500m.Simspson * X500m.Dominant.Landscape.Class + (1 | Property), family = binomial, data = FDModel)


TG_F_Modnames2 <- c("TG_F_null","TG_F_ExH",
                    "TG_F_ExGC","TG_F_ExGGC",
                    "TG_F_ExGS", "TG_F_ExWE",
                    "TG_F_ExC","TG_F_ExLS",
                    "TG_F_ExLC","TG_F_HxGC",
                    "TG_F_HxGGC","TG_F_HxWE",
                    "TG_F_HxC","TG_F_HxGS",
                    "TG_F_HxLS","TG_F_HxLC",
                    "TG_F_GCxGGC","TG_F_GCxGS",
                    "TG_F_GCxC", "TG_F_GCxWE",
                    "TG_F_GCxLS","TG_F_GCxLC",
                    "TG_F_GGCxGS", "TG_F_GGCxWE",
                    "TG_F_GGCxC","TG_F_GGCxLS",
                    "TG_F_GGCxLC","TG_F_WExC",
                    "TG_F_WExLS","TG_F_GSxC",
                    "TG_F_GSxLS","TG_F_CxLS",
                    "TG_F_CxLC","TG_F_LSxLC")
TG_F_modlist2 <- mget(TG_F_Modnames2)

aictab(TG_F_modlist2)

#Grass Status x Landscape Class
#Elevation x Grass Status
#Grass Status x Crops

####Final models----

TG_F_Modnames3<- c("TG_F_null", "TG_F_GS", "TG_F_GS_LC", 
                   "TG_F_WE_GS", "TG_F_GSxLS", "TG_F_ExGS", 
                   "TG_F_GSxC")

TG_F_modlist_Final <- mget(TG_F_Modnames3)
aictab(TG_F_modlist_Final)



#Top model = Grass Status + Landscape Class

#Equivalent models (within 2 AICc):
##Weed Estimate + Grass Status

###Predictions----


##Grass Status + Landscape Class
summary(TG_F_GS_LC)

TG_F_pred <- expand.grid(Day_Sampled = TG_Predictions_Day, Grass_Status = c("Native","Introduced"), X500m.Dominant.Landscape.Class = unique(FDModel$X500m.Dominant.Landscape.Class))
head(TG_F_pred);dim(TG_F_pred)

TG_F_pred1 <- predict(object = TG_F_GS_LC,newdata= TG_F_pred,se.fit = T, type = "link",re.form = ~0)

TG_F_pred2<-data.frame(TG_F_pred,fit.link=TG_F_pred1$fit,se.link=TG_F_pred1$se.fit)

TG_F_pred2$lci.link<-TG_F_pred2$fit.link-(1.96*TG_F_pred2$se.link)
TG_F_pred2$uci.link<-TG_F_pred2$fit.link+(1.96*TG_F_pred2$se.link)

TG_F_pred2$fit<-plogis(TG_F_pred2$fit.link)
TG_F_pred2$se<-plogis(TG_F_pred2$se.link)
TG_F_pred2$lci<-plogis(TG_F_pred2$lci.link)
TG_F_pred2$uci<-plogis(TG_F_pred2$uci.link)

head(TG_F_pred2);dim(TG_F_pred2)


##Weed Estimate + Grass Status
summary(TG_F_WE_GS)

TG_F_pred3 <- expand.grid(Day_Sampled = TG_Predictions_Day, Grass_Status = c("Native","Introduced"), Weed_Estimate = unique(FDModel$Weed_Estimate))
head(TG_F_pred3);dim(TG_F_pred3)

TG_F_pred4 <- predict(object = TG_F_WE_GS,newdata= TG_F_pred3,se.fit = T, type = "link",re.form = ~0)

TG_F_pred5<-data.frame(TG_F_pred3,fit.link=TG_F_pred4$fit,se.link=TG_F_pred4$se.fit)

TG_F_pred5$lci.link<-TG_F_pred5$fit.link-(1.96*TG_F_pred5$se.link)
TG_F_pred5$uci.link<-TG_F_pred5$fit.link+(1.96*TG_F_pred5$se.link)

TG_F_pred5$fit<-plogis(TG_F_pred5$fit.link)
TG_F_pred5$se<-plogis(TG_F_pred5$se.link)
TG_F_pred5$lci<-plogis(TG_F_pred5$lci.link)
TG_F_pred5$uci<-plogis(TG_F_pred5$uci.link)

head(TG_F_pred5);dim(TG_F_pred5)

###Visualize----

#Grass Status + Landscape Class + Day
summary(TG_F_GS_LC)
head(TG_F_pred2)

RR <- TG_F_pred2$X500m.Dominant.Landscape.Class == "NTV_Herbaceous_Open" & TG_F_pred2$Day_Sampled == TG_Predictions_Day[10]
R_R <- TG_F_pred2$Grass_Status == "Introduced" & TG_F_pred2$Day_Sampled == TG_Predictions_Day[10]
RRR <- TG_F_pred2$X500m.Dominant.Landscape.Class == "NTV_Woody_Open" & TG_F_pred2$Grass_Status == "Introduced"

dev.new(height=10,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(2,2),mgp=c(2.5,1,0),xpd = T)

plot(x = 1:2,y = TG_F_pred2$fit[RR],xlab = " ",ylab = 'Probability of Occurrence', type = 'p',pch = 16,cex =2.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,1))
axis(side=1,at=1:2,labels=c(' ',' '))
arrows(x0=1:2, y0=TG_F_pred2$lci[RR],x1=1:2, y1=TG_F_pred2$uci[RR],angle=90,length=0.1, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.25,'a)',cex=0.9)
mtext(side=1,line=1.5,at = 0.9,'Native\n Grass',cex=0.9)
mtext(side=1,line=1.5,at = 2.1,'Introduced\n Grass',cex=0.9)

points(x = jitter(raw_x1, factor = 1),y = FDModel$TG_F, pch = 16, cex = 0.4, col = "black")


plot(x = 1:3,y = TG_F_pred2$fit [R_R],xlab = " ",ylab = 'Probability of Occurrence', type = 'p',pch = 16,cex =2.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,4),ylim = c(0,1))
axis(side=1,at=1:3,labels=c(' ',' ', " "))
arrows(x0=1:3, y0=TG_F_pred2$lci [R_R],x1=1:3, y1=TG_F_pred2$uci[R_R],angle=90,length=0.1, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.3,'b)',cex=1)
mtext(side=1,line=1.5,at = 0.7,'Woody\nClosed',cex=0.7)
mtext(side=1,line=1.5,at = 2,'Herbaceous\nOpen',cex=0.7)
mtext(side=1,line=1.5,at = 3.3,"Woody\nOpen",cex=0.7)

points(x = jitter(raw_x2, factor = 1),y = FDModel$TG_F, pch = 16, cex = 0.4, col = "black")

plot(x = FDModel$Day_Sampled,y = FDModel$TG_F,xlab = expression("Day Sampled"),ylab = 'Probability of Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = -5,'a)',cex=1)

polygon(x = c(TG_F_pred2$Day_Sampled[RRR],rev(TG_F_pred2$Day_Sampled[RRR])), y = c(TG_F_pred2$lci[RRR],rev(TG_F_pred2$uci[RRR])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_F_pred2$Day_Sampled[RRR],y = TG_F_pred2$fit[RRR],lwd = 2,col = 'grey30')

##Grass Status + Weed Estimate + Day
summary(TG_F_WE_GS)
head(TG_F_pred5)

SS <- TG_F_pred5$Weed_Estimate == "20-40%" & TG_F_pred5$Day_Sampled == TG_Predictions_Day[15]
S_S <- TG_F_pred5$Grass_Status == "Introduced" & TG_F_pred5$Day_Sampled == TG_Predictions_Day[10]
SSS <- TG_F_pred5$Grass_Status == "Introduced" & TG_F_pred5$Weed_Estimate == "20-40%"

raw_x3 <- ifelse(FDModel$Weed_Estimate == "0-20%", 1, 
                 ifelse(FDModel$Weed_Estimate =="20-40%", 2,
                        ifelse(FDModel$Weed_Estimate ==
                            "40-60%", 3, 
                            ifelse(FDModel$Weed_Estimate == 
                                     "60-80%", 4,
                                   ifelse(FDModel$Weed_Estimate ==
                                          "80-100%", 5, NA)))))


dev.new(height=10,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(2,2),mgp=c(2.5,1,0),xpd = T)

plot(x = 1:2,y = TG_F_pred5$fit[SS],xlab = " ",ylab = 'Probability of Occurrence', type = 'p',pch = 16,cex =2.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,1))
axis(side=1,at=1:2,labels=c(' ',' '))
arrows(x0=1:2, y0=TG_F_pred5$lci[SS],x1=1:2, y1=TG_F_pred5$uci[SS],angle=90,length=0.1, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.25,'a)',cex=0.9)
mtext(side=1,line=1.5,at = 0.9,'Native\n Grass',cex=0.9)
mtext(side=1,line=1.5,at = 2.1,'Introduced\n Grass',cex=0.9)

points(x = jitter(raw_x1, factor = 1),y = FDModel$TG_F, pch = 16, cex = 0.4, col = "black")
plot(x = 1:5,y = TG_F_pred5$fit [S_S],xlab = " ",ylab = 'Probability of Occurrence', type = 'p',pch = 16,cex =2.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,6),ylim = c(-0.1,1.1))
axis(side=1,at=1:5,labels=c(' ',' ', " ","",""))
arrows(x0=1:5, y0=TG_F_pred5$lci [S_S],x1=1:5, y1=TG_F_pred5$uci[S_S],angle=90,length=0.1, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.5,'b)',cex=1)
mtext(side=1,line=2,at = 3,"Weed Estimate (%)",cex=0.9)
mtext(side=1,line=0.5,at = 0.6,'0-20',cex=0.7)
mtext(side=1,line=0.5,at = 1.8,'20-40',cex=0.7)
mtext(side=1,line=0.5,at = 3,"40-60",cex=0.7)
mtext(side=1,line=0.5,at = 4.2,"60-80",cex=0.7)
mtext(side=1,line=0.5,at = 5.6,"80-100",cex=0.7)

points(x = jitter(raw_x3, factor = 1),y = FDModel$TG_F, pch = 16, cex = 0.4, col = "black")


plot(x = FDModel$Day_Sampled,y = FDModel$TG_F,xlab = expression("Day Sampled"),ylab = 'Probability of Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = -5,'c)',cex=1)

polygon(x = c(TG_F_pred5$Day_Sampled[SSS],rev(TG_F_pred5$Day_Sampled[SSS])), y = c(TG_F_pred5$lci[SSS],rev(TG_F_pred5$uci[SSS])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_F_pred5$Day_Sampled[SSS],y = TG_F_pred5$fit[SSS],lwd = 2,col = 'grey30')

#Probability of Occurrence Figures----

##Main----

dev.new(height=15,width=15,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(3,3),mgp=c(2.5,1,0),xpd = T)

plot(x = 1:2,y = TG_F_pred2$fit[RR],xlab = " ",ylab = 'Occurrence', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,1))
axis(side=1,at=1:2,labels=c(' ',' '))
arrows(x0=1:2, y0=TG_F_pred2$lci[RR],x1=1:2, y1=TG_F_pred2$uci[RR],angle=90,length=0.1, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.25,'a)',cex=0.8)
mtext(side=1,line=1.5,at = 0.8,'Native\n Grass',cex=0.7)
mtext(side=1,line=1.5,at = 2.3,'Introduced\n Grass',cex=0.7)

points(x = jitter(raw_x1, factor = 1),y = FDModel$TG_F, pch = 16, cex = 0.4, col = "black")


plot(x = 1:3,y = TG_F_pred2$fit [R_R],xlab = " ",ylab = 'Occurrence', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,4),ylim = c(0,1))
axis(side=1,at=1:3,labels=c(' ',' ', " "))
arrows(x0=1:3, y0=TG_F_pred2$lci [R_R],x1=1:3, y1=TG_F_pred2$uci[R_R],angle=90,length=0.05, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.3,'b)',cex=0.8)
mtext(side=1,line=1,at = 1,'WC',cex=0.7)
mtext(side=1,line=1,at = 2,'HO',cex=0.7)
mtext(side=1,line=1,at = 3,"WO",cex=0.7)

points(x = jitter(raw_x2, factor = 1),y = FDModel$TG_F, pch = 16, cex = 0.4, col = "black")

mtext(side=3,line=1,at = 2,'Small herbivorous beetles, detrivores, moths, ticks and Hymenoptera',cex=0.7, font = 2)


plot(x = FDModel$Day_Sampled,y = FDModel$TG_F,xlab = expression("Day Sampled"),ylab = 'Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,xaxt = 'n')
axis(side=1, at=seq(from=min(FDModel$Day_Sampled),to=max(FDModel$Day_Sampled),length.out=5),labels=round(seq(from=min(FDModel$Day_Sampled),to=max(FDModel$Day_Sampled),length.out=5),0),cex.axis=1)
mtext(side=3,line=0,at = -5,'c)',cex=0.8)

polygon(x = c(TG_F_pred2$Day_Sampled[RRR],rev(TG_F_pred2$Day_Sampled[RRR])), y = c(TG_F_pred2$lci[RRR],rev(TG_F_pred2$uci[RRR])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_F_pred2$Day_Sampled[RRR],y = TG_F_pred2$fit[RRR],lwd = 2,col = 'grey30')

plot(x = FDModel$Plant_Height,y = FDModel$TG_D,xlab = expression("Grass Height (cm)"),ylab = 'Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = -6,'d)',cex=0.8)

polygon(x = c(TG_D_pred2$Plant_Height[LL],rev(TG_D_pred2$Plant_Height[LL])), y = c(TG_D_pred2$lci[LL],rev(TG_D_pred2$uci[LL])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_D_pred2$Plant_Height[LL],y = TG_D_pred2$fit[LL],lwd = 2,col = 'grey30')

mtext(side=3,line=0.9,at = 100,'Web building spiders, small Hemiptera and\n active hunting lacewings',cex=0.7, font = 2)

plot(x = 1:2,y = TG_D_pred2$fit[L_L],xlab = " ",ylab = 'Occurrence', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,1))
axis(side=1,at=1:2,labels=c(' ',' '))
arrows(x0=1:2, y0=TG_D_pred2$lci[L_L],x1=1:2, y1=TG_D_pred2$uci[L_L],angle=90,length=0.1, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.25,'e)',cex=0.8)
mtext(side=1,line=1.5,at = 0.8,'Native\n Grass',cex=0.6)
mtext(side=1,line=1.5,at = 2.2,'Introduced\n Grass',cex=0.6)

points(x = jitter(raw_x1, factor = 1),y = FDModel$TG_D, pch = 16, cex = 0.4, col = "black")

plot(x = FDModel$X500m.Simspson,y = FDModel$TG_B,xlab = expression("Habitat Diversity within 500m"),ylab = 'Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,xaxt = 'n')
axis(side=1, at=seq(from=min(FDModel$X500m.Simspson),to=max(FDModel$X500m.Simspson),length.out=4),labels=round(seq(from=min(FDModel$X500m.Simspson),to=max(FDModel$X500m.Simspson),length.out=4),1),cex.axis=1)
mtext(side=3,line=0,at = -0.03,'f)',cex=0.8)

polygon(x = c(TG_B_pred2$X500m.Simspson[JJ],rev(TG_B_pred2$X500m.Simspson[JJ])), y = c(TG_B_pred2$lci[JJ],rev(TG_B_pred2$uci[JJ])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_B_pred2$X500m.Simspson[JJ],y = TG_B_pred2$fit[JJ],lwd = 2,col = 'grey30')

polygon(x = c(TG_B_pred2$X500m.Simspson[J_J],rev(TG_B_pred2$X500m.Simspson[J_J])), y = c(TG_B_pred2$lci[J_J],rev(TG_B_pred2$uci[J_J])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_B_pred2$X500m.Simspson[J_J],y = TG_B_pred2$fit[J_J],lwd = 2,col = 'grey30',lty=2)

legend(x=0.02,y=0.5,legend = c("40% GC", "80% GC"), lty = c(1,2), col = 'grey30',pt.cex = 1,cex = 0.9)

mtext(side=3,line=0.9,at = 0.4,'Small flies',cex=0.7, font = 2)


plot(x = FDModel$Prop_Green_GC,y = FDModel$TG_E,xlab = expression("Green Ground Cover (%)"),ylab = 'Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,xaxt = 'n')
axis(side=1, at=seq(from=min(FDModel$Prop_Green_GC),to=max(FDModel$Prop_Green_GC),length.out=5),labels=round(seq(from=min(FDModel$Prop_Green_GC),to=max(FDModel$Prop_Green_GC),length.out=5),0),cex.axis=1)
mtext(side=3,line=0,at = 1.5,'g)',cex=0.8)

polygon(x = c(TG_E_pred2$Prop_Green_GC[OO],rev(TG_E_pred2$Prop_Green_GC[OO])), y = c(TG_E_pred2$lci[OO],rev(TG_E_pred2$uci[OO])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_E_pred2$Prop_Green_GC[OO],y = TG_E_pred2$fit[OO],lwd = 2,col = 'grey30')

polygon(x = c(TG_E_pred2$Prop_Green_GC[O_O],rev(TG_E_pred2$Prop_Green_GC[O_O])), y = c(TG_E_pred2$lci[O_O],rev(TG_E_pred2$uci[O_O])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_E_pred2$Prop_Green_GC[O_O],y = TG_E_pred2$fit[O_O],lwd = 2,col = 'grey30',lty = 2)

legend('bottomleft',legend = c("Native", "Introduced"), lty = c(1,2), col = 'grey30',pt.cex = 1,cex=0.9)

mtext(side=3,line=1,at = 110,'Hymenoptera, moths, fungivore and predatory beetles',cex=0.7, font = 2)

plot(x = FDModel$Day_Sampled,y = FDModel$TG_E,xlab = expression("Day Sampled"),ylab = 'Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,xaxt = 'n')
axis(side=1, at=seq(from=min(FDModel$Day_Sampled),to=max(FDModel$Day_Sampled),length.out=5),labels=round(seq(from=min(FDModel$Day_Sampled),to=max(FDModel$Day_Sampled),length.out=5),0),cex.axis=1)
mtext(side=3,line=0,at = -5,'h)',cex=0.8)

polygon(x = c(TG_E_pred2$Day_Sampled[OOO],rev(TG_E_pred2$Day_Sampled[OOO])), y = c(TG_E_pred2$lci[OOO],rev(TG_E_pred2$uci[OOO])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_E_pred2$Day_Sampled[OOO],y = TG_E_pred2$fit[OOO],lwd = 2,col = 'grey30')


##Supporting info----

dev.new(height=15,width=25,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(3,5),mgp=c(2.7,1,0),xpd = T)

plot(x = 1:2,y = TG_E_pred5$fit[PP],xlab = " ",ylab = 'Occurrence', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,1))
axis(side=1,at=1:2,labels=c(' ',' '))
arrows(x0=1:2, y0=TG_E_pred5$lci[PP],x1=1:2, y1=TG_E_pred5$uci[PP],angle=90,length=0.1, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.25,'a)',cex=0.7)
mtext(side=1,line=1.5,at = 0.8,'Native\n Grass',cex=0.7)
mtext(side=1,line=1.5,at = 2.3,'Introduced\n Grass',cex=0.7)

points(x = jitter(raw_x1, factor = 1),y = FDModel$TG_E, pch = 16, cex = 0.4, col = "black")


plot(x = FDModel$Day_Sampled,y = FDModel$TG_E,xlab = expression("Day Sampled"),ylab = 'Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,xaxt = 'n')
axis(side=1, at=seq(from=min(FDModel$Day_Sampled),to=max(FDModel$Day_Sampled),length.out=4),labels=round(seq(from=min(FDModel$Day_Sampled),to=max(FDModel$Day_Sampled),length.out=4),0),cex.axis=1)
mtext(side=3,line=0,at = -3,'b)',cex=0.7)

polygon(x = c(TG_E_pred5$Day_Sampled[P_P],rev(TG_E_pred5$Day_Sampled[P_P])), y = c(TG_E_pred5$lci[P_P],rev(TG_E_pred5$uci[P_P])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_E_pred5$Day_Sampled[P_P],y = TG_E_pred5$fit[P_P],lwd = 2,col = 'grey30')

mtext(side=3,line=1,at = 25,'----------Hymenoptera, moths, fungivore and predatory beetles----------',cex=0.7, font = 2)

plot(x = FDModel$Ground_Cover,y = FDModel$TG_E,xlab = expression("Ground Cover (%)"),ylab = 'Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,xaxt = 'n')
axis(side=1, at=seq(from=min(FDModel$Ground_Cover),to=max(FDModel$Ground_Cover),length.out=4),labels=round(seq(from=min(FDModel$Ground_Cover),to=max(FDModel$Ground_Cover),length.out=4),0),cex.axis=1)
mtext(side=3,line=0,at = 33,'c)',cex=0.7)

polygon(x = c(TG_E_pred8$Ground_Cover[QQ],rev(TG_E_pred8$Ground_Cover[QQ])), y = c(TG_E_pred8$lci[QQ],rev(TG_E_pred8$uci[QQ])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_E_pred8$Ground_Cover[QQ],y = TG_E_pred8$fit[QQ],lwd = 2,col = 'grey30')


plot(x = FDModel$Ground_Cover,y = FDModel$TG_B,xlab = expression("Ground Cover (%)"),ylab = 'Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,xaxt = 'n')
axis(side=1, at=seq(from=min(FDModel$Ground_Cover),to=max(FDModel$Ground_Cover),length.out=4),labels=round(seq(from=min(FDModel$Ground_Cover),to=max(FDModel$Ground_Cover),length.out=4),0),cex.axis=1)
mtext(side=3,line=0,at = 32,'d)',cex=0.7)

polygon(x = c(TG_B_pred5$Ground_Cover[KK],rev(TG_B_pred5$Ground_Cover[KK])), y = c(TG_B_pred5$lci[KK],rev(TG_B_pred5$uci[KK])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_B_pred5$Ground_Cover[KK],y = TG_B_pred5$fit[KK],lwd = 2,col = 'grey30')

mtext(side=3,line=0.9,at = 110,'---------------Small flies---------------',cex=0.7, font = 2)

plot(x = FDModel$Cropping_500m,y = FDModel$TG_B,xlab = expression("Crops within 1km (%)"),ylab = 'Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,xaxt = 'n')
axis(side=1, at=seq(from=min(FDModel$Cropping_500m),to=max(FDModel$Cropping_500m),length.out=4),labels=round(seq(from=min(FDModel$Cropping_500m),to=max(FDModel$Cropping_500m),length.out=4),0),cex.axis=1)
mtext(side=3,line=0,at = -8,'e)',cex=0.7)

polygon(x = c(TG_B_pred5$Cropping_500m[K_K],rev(TG_B_pred5$Cropping_500m[K_K])), y = c(TG_B_pred5$lci[K_K],rev(TG_B_pred5$uci[K_K])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_B_pred5$Cropping_500m[K_K],y = TG_B_pred5$fit[K_K],lwd = 2,col = 'grey30')



plot(x = 1:3,y = TG_D_pred5$fit [MM],xlab = " ",ylab = 'Occurrence', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,4),ylim = c(0,1))
axis(side=1,at=1:3,labels=c(' ',' ', " "))
arrows(x0=1:3, y0=TG_D_pred5$lci [MM],x1=1:3, y1=TG_D_pred5$uci[MM],angle=90,length=0.05, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.3,'f)',cex=0.7)
mtext(side=1,line=1,at = 0.9,'WC',cex=0.7)
mtext(side=1,line=1,at = 2,'HO',cex=0.7)
mtext(side=1,line=1,at = 3.1,"WO",cex=0.7)

points(x = jitter(raw_x2, factor = 1),y = FDModel$TG_D, pch = 16, cex = 0.4, col = "black")


plot(x = 1:2,y = TG_D_pred5$fit[M_M],xlab = " ",ylab = 'Occurrence', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,1))
axis(side=1,at=1:2,labels=c(' ',' '))
arrows(x0=1:2, y0=TG_D_pred5$lci[M_M],x1=1:2, y1=TG_D_pred5$uci[M_M],angle=90,length=0.1, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.25,'g)',cex=0.7)
mtext(side=1,line=1.5,at = 0.8,'Native\n Grass',cex=0.7)
mtext(side=1,line=1.5,at = 2.3,'Introduced\n Grass',cex=0.7)

points(x = jitter(raw_x1, factor = 1),y = FDModel$TG_D, pch = 16, cex = 0.4, col = "black")

mtext(side=3,line=0.9,at = 2,'Web building spiders, small Hemiptera and active hunting lacewings',cex=0.7, font = 2)


plot(x = FDModel$X500m.Simspson,y = FDModel$TG_D,xlab = expression("Habitat Diversity within 500m"),ylab = 'Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,xaxt = 'n')
axis(side=1, at=seq(from=min(FDModel$X500m.Simspson),to=max(FDModel$X500m.Simspson),length.out=4),labels=round(seq(from=min(FDModel$X500m.Simspson),to=max(FDModel$X500m.Simspson),length.out=4),1),cex.axis=1)
mtext(side=3,line=0,at = 0,'h)',cex=0.7)

polygon(x = c(TG_D_pred8$X500m.Simspson[NN],rev(TG_D_pred8$X500m.Simspson[NN])), y = c(TG_D_pred8$lci[NN],rev(TG_D_pred8$uci[NN])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_D_pred8$X500m.Simspson[NN],y = TG_D_pred8$fit[NN],lwd = 2,col = 'grey30')


plot(x = 1:2,y = TG_F_pred5$fit[SS],xlab = " ",ylab = 'Occurrence', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,1))
axis(side=1,at=1:2,labels=c(' ',' '))
arrows(x0=1:2, y0=TG_F_pred5$lci[SS],x1=1:2, y1=TG_F_pred5$uci[SS],angle=90,length=0.1, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.25,'i)',cex=0.7)
mtext(side=1,line=1.5,at = 0.8,'Native\n Grass',cex=0.7)
mtext(side=1,line=1.5,at = 2.3,'Introduced\n Grass',cex=0.7)

points(x = jitter(raw_x1, factor = 1),y = FDModel$TG_F, pch = 16, cex = 0.4, col = "black")

plot(x = 1:5,y = TG_F_pred5$fit [S_S],xlab = " ",ylab = 'Occurrence', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,6),ylim = c(-0.1,1.1))
axis(side=1,at=1:5,labels=c(' ',' ', " ","",""))
arrows(x0=1:5, y0=TG_F_pred5$lci [S_S],x1=1:5, y1=TG_F_pred5$uci[S_S],angle=90,length=0.05, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.5,'j)',cex=0.7)
mtext(side=1,line=2,at = 3,"Weed Estimate (%)",cex=0.7)
mtext(side=1,line=0.4,at = 1,'0-20',cex=0.5)
mtext(side=1,line=1,at = 2,'20-40',cex=0.5)
mtext(side=1,line=0.4,at = 3,"40-60",cex=0.5)
mtext(side=1,line=1,at = 4,"60-80",cex=0.5)
mtext(side=1,line=0.4,at = 5.2,"80-100",cex=0.5)

points(x = jitter(raw_x3, factor = 1),y = FDModel$TG_F, pch = 16, cex = 0.4, col = "black")

mtext(side=3,line=0.3,at = -4,'Small herbivorous beetles, detrivores, moths, ticks and\n Hymenoptera',cex=0.7, font = 2)


plot(x = FDModel$Day_Sampled,y = FDModel$TG_F,xlab = expression("Day Sampled"),ylab = 'Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,xaxt = 'n')
axis(side=1, at=seq(from=min(FDModel$Day_Sampled),to=max(FDModel$Day_Sampled),length.out=4),labels=round(seq(from=min(FDModel$Day_Sampled),to=max(FDModel$Day_Sampled),length.out=4),0),cex.axis=1)
mtext(side=3,line=0,at = -5,'k)',cex=0.7)

polygon(x = c(TG_F_pred5$Day_Sampled[SSS],rev(TG_F_pred5$Day_Sampled[SSS])), y = c(TG_F_pred5$lci[SSS],rev(TG_F_pred5$uci[SSS])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_F_pred5$Day_Sampled[SSS],y = TG_F_pred5$fit[SSS],lwd = 2,col = 'grey30')

mtext(side=3,line=1,at = 30,'Small herbivorous beetles, detrivores,\n moths, ticks and Hymenoptera',cex=0.7, font = 2)







#OLD----
dev.new(height=10,width=15,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,3,2),mfrow=c(2,3),mgp=c(2.7,1,0),xpd = T)

plot(x = FDModel$Ground_Cover,y = FDModel$TG_E,xlab = expression("Ground Cover (%)"),ylab = 'Probability of Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.axis=1.2,cex.lab=1.2)
mtext(side=3,line=0,at = 32,'a)',cex=0.9)

mtext(side=3,line=1.3,at = 110,'Hymenoptera, moths, fungivore and predatory beetles',cex=0.7, font = 2)

polygon(x = c(TG_E_pred8$Ground_Cover[TT],rev(TG_E_pred8$Ground_Cover[TT])), y = c(TG_E_pred8$lci[TT],rev(TG_E_pred8$uci[TT])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_E_pred8$Ground_Cover[TT],y = TG_E_pred8$fit[TT],lwd = 2,col = 'grey30')

plot(x = FDModel$Day_Sampled,y = FDModel$TG_E,xlab = expression("Day Sampled"),ylab = 'Probability of Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.axis=1.2,cex.lab=1.2)
mtext(side=3,line=0,at = -4,'b)',cex=0.9)

polygon(x = c(TG_E_pred8$Day_Sampled[T_T],rev(TG_E_pred8$Day_Sampled[T_T])), y = c(TG_E_pred8$lci[T_T],rev(TG_E_pred8$uci[T_T])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_E_pred8$Day_Sampled[T_T],y = TG_E_pred8$fit[T_T],lwd = 2,col = 'grey30')


plot(x = FDModel$X500m.Simspson,y = FDModel$TG_D,xlab = expression("Habitat Diversity within 500m"),ylab = 'Probability of Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.axis=1.2,cex.lab=1.2,xaxt = 'n')
axis(side=1, at=seq(from=min(FDModel$X500m.Simspson),to=max(FDModel$X500m.Simspson),length.out=5),labels=round(seq(from=min(FDModel$X500m.Simspson),to=max(FDModel$X500m.Simspson),length.out=5),1),cex.axis=1.2)
mtext(side=3,line=0,at = -0.01,'c)',cex=0.9)

mtext(side=3,line=0.9,at = 0.4,'Web building spiders, small Hemiptera \nand active hunting lacewings',cex=0.7, font = 2)

polygon(x = c(TG_D_pred4$X500m.Simspson[QQ],rev(TG_D_pred4$X500m.Simspson[QQ])), y = c(TG_D_pred4$lci[QQ],rev(TG_D_pred4$uci[QQ])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_D_pred4$X500m.Simspson[QQ],y = TG_D_pred4$fit[QQ],lwd = 2,col = 'grey30')

polygon(x = c(TG_D_pred4$X500m.Simspson[Q_Q],rev(TG_D_pred4$X500m.Simspson[Q_Q])), y = c(TG_D_pred4$lci[Q_Q],rev(TG_D_pred4$uci[Q_Q])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_D_pred4$X500m.Simspson[Q_Q],y = TG_D_pred4$fit[Q_Q],lwd = 2,col = 'grey30',lty = 2)

legend('topleft',legend = c("Low Elevation", "High Elevation"), lty = c(1,2), col = 'grey30',pt.cex = 1)



plot(x = FDModel$Ground_Cover,y = FDModel$TG_B,xlab = expression("Ground Cover (%)"),ylab = 'Probability of Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.axis=1.2,cex.lab=1.2)
mtext(side=3,line=0,at = 32,'d)',cex=0.9)

polygon(x = c(TG_B_pred5$Ground_Cover[OO],rev(TG_B_pred5$Ground_Cover[OO])), y = c(TG_B_pred5$lci[OO],rev(TG_B_pred5$uci[OO])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_B_pred5$Ground_Cover[OO],y = TG_B_pred5$fit[OO],lwd = 2,col = 'grey30')

mtext(side=3,line=1.2,at = 110,'----------------------Small Flies----------------------',cex=0.7, font = 2)


plot(x = FDModel$Cropping_500m,y = FDModel$TG_B,xlab = expression("Crops within 1km (%)"),ylab = 'Probability of Occurrence', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.axis=1.2,cex.lab=1.2)
mtext(side=3,line=0,at = -8,'e)',cex=0.9)

polygon(x = c(TG_B_pred5$Cropping_500m[O_O],rev(TG_B_pred5$Cropping_500m[O_O])), y = c(TG_B_pred5$lci[O_O],rev(TG_B_pred5$uci[O_O])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=TG_B_pred5$Cropping_500m[O_O],y = TG_B_pred5$fit[O_O],lwd = 2,col = 'grey30')