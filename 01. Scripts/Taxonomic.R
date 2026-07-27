
#Author: Rhiannon Bird
#Written under version R 4.5.1


options(scipen = 999) #So R doesn't use scientific notation

#Libraries----

library("AICcmodavg")
library("glmmTMB")
library('lme4')
library("openxlsx")

#ALL SPECIES RICHNESS----

head(TaxModel)
str(TaxModel)

TaxModel$Grass_Status[TaxModel$Grass_Status=="Unknown"] <- NA

##Modelling----

#Include Day?

Rich_null <- glmmTMB(Species_Rich ~ 1 + (1 | Property), family = nbinom2, data = TaxModel)
Rich_Day <- glmmTMB(Species_Rich ~ Day_Sampled + (1 | Property), family = nbinom2, data = TaxModel)


aictab(list("Null" = Rich_null,"Day" = Rich_null))
#no need to include day in next step


##Single

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
#Grass Status



##Additive

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
#Elevation + Grass Status
#Green Ground Cover + Grass Status
#Ground Cover + Grass Status




##Interactive

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
richmodlist3 <- mget(Modnames2)

aictab(richmodlist3)
#Elevation X Grass Status
#Green GC X Grass Status
#Grass Status X Landscape Simpson


##Final AICC----

Modnames3<- c("Rich_null", "Rich_GS", "Rich_E_GS",
              "Rich_GGC_GS","Rich_GC_GS", 
              "Rich_ExGS","Rich_GGCxGS","Rich_GSxLS")
richmodlist_Final <- mget(Modnames3)
aictab(richmodlist_Final)


#Top model = Elevation + Grass Status

#Equivalent models:
##Green GC + Grass Status

##Predictions----

##Elevation + Grass Status
summary(Rich_E_GS)

Predictions_Elevation <- seq(min(TaxModel$Elevation),max(TaxModel$Elevation),length.out=20)

toprichpred <- expand.grid(Elevation = Predictions_Elevation, Grass_Status = c("Native","Introduced"))
head(toprichpred);dim(toprichpred)

toprichpred1 <- predict(object = Rich_E_GS,newdata= toprichpred,se.fit = T, type = "link",re.form = NA)

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


##Green GC + Grass Status
summary(Rich_GGCxGS)

Predictions_GGC <- seq(min(TaxModel$Prop_Green_GC),max(TaxModel$Prop_Green_GC),length.out=20)

second_richpred <- expand.grid(Prop_Green_GC = Predictions_GGC, Grass_Status = c("Native","Introduced"))
head(second_richpred);dim(second_richpred)

second_richpred1 <- predict(object = Rich_GGCxGS,newdata= second_richpred,se.fit = T, type = "link",re.form = NA)

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

##Visual----

#Elevation + Grass Status
summary(Rich_E_GS)
head(toprichpred2);dim(toprichpred2)

AA <- toprichpred2$Grass_Status == "Introduced"
A_A <- toprichpred2$Elevation == Predictions_Elevation[10]

raw_x <- ifelse(TaxModel$Grass_Status ==
                  "Native", 1, 
                ifelse(TaxModel$Grass_Status ==
                         "Introduced", 2, NA))


dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = TaxModel$Elevation,y = TaxModel$Species_Rich,xlab = expression("Elevation (m)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = 60,'a)',cex=1.1)

polygon(x = c(toprichpred2$Elevation[AA],rev(toprichpred2$Elevation[AA])), y = c(toprichpred2$lci[AA],rev(toprichpred2$uci[AA])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=toprichpred2$Elevation[AA],y = toprichpred2$fit[AA],lwd = 2,col = 'grey30')


plot(x = 1:2,y = toprichpred2$fit [A_A],xlab = " ",ylab = 'Species Richness', type = 'p',pch = 16,cex =2.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,16))
axis(side=1,at=1:2,labels=c('Native','Introduced'))
arrows(x0=1:2, y0=toprichpred2$lci [A_A],x1=1:2, y1=toprichpred2$uci[A_A],angle=90,length=0.2, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.2,'b)',cex=1.1)

points(x = jitter(raw_x, factor = 1),y = TaxModel$Species_Rich, pch = 16, cex = 0.4, col = "black")

#GGC + Grass Status

summary(Rich_GGC_GS)
head(second_richpred2)

BB <- second_richpred2$Grass_Status == "Introduced"
B_B <- second_richpred2$Prop_Green_GC == Predictions_GGC[10]


dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = TaxModel$Prop_Green_GC,y = TaxModel$Species_Rich,xlab = expression("Green Ground Cover (%)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = 0,'a)',cex=1.1)

polygon(x = c(second_richpred2$Prop_Green_GC[BB],rev(second_richpred2$Prop_Green_GC[BB])), y = c(second_richpred2$lci[BB],rev(second_richpred2$uci[BB])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=second_richpred2$Prop_Green_GC[BB],y = second_richpred2$fit[BB],lwd = 2,col = 'grey30')


plot(x = 1:2,y = second_richpred2$fit[B_B],xlab = " ",ylab = 'Species Richness', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,15))
axis(side=1,at=1:2,labels=c('Native','Introduced'))
arrows(x0=1:2, y0=second_richpred2$lci [B_B],x1=1:2, y1=second_richpred2$uci[B_B],angle=90,length=0.1, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.2,'b)',cex=1.1)

points(x = jitter(raw_x, factor = 1),y = TaxModel$Species_Rich, pch = 16, cex = 0.4, col = "black")


#ALL SPECIES DIVERSITY----



head(TaxModel)
str(TaxModel)

#update all 0 to be 0.000001 for model fitting
TaxModel$Diversity[TaxModel$Diversity==0] <- 0.000001

##Modelling----

#Include Day?

Div_null <- glmer(Diversity ~ 1 + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_Day <- glmer(Diversity ~ Day_Sampled + (1 | Property), family = Gamma(link = "log"), data = TaxModel)


aictab(list("Null" = Div_null,"Day" = Div_null))
#no need to include day in next step


##Single

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
#Grass Status


##Additive

head(TaxModel)
names(TaxModel)


Div_E_H <- glmer(Diversity ~ Elevation + Plant_Height + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_E_GC <- glmer(Diversity ~ Elevation + Ground_Cover + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_E_GGC <- glmer(Diversity ~ Elevation + Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_E_WE <- glmer(Diversity ~ Elevation + Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_E_GS <- glmer(Diversity ~ Elevation + Grass_Status + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_E_C <- glmer(Diversity ~ Elevation + Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_E_LS <- glmer(Diversity ~ Elevation + X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_E_LC <- glmer(Diversity ~ Elevation + X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = TaxModel)


Div_H_GC <- glmer(Diversity ~ Plant_Height + Ground_Cover + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_H_GGC <- glmer(Diversity ~ Plant_Height + Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_H_WE <- glmer(Diversity ~ Plant_Height + Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_H_GS <- glmer(Diversity ~ Plant_Height + Grass_Status + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_H_C <- glmer(Diversity ~ Plant_Height + Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_H_LS <- glmer(Diversity ~ Plant_Height + X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_H_LC <- glmer(Diversity ~ Plant_Height + X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = TaxModel)


Div_GC_GGC <- glmer(Diversity ~ Ground_Cover + Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GC_WE <- glmer(Diversity ~ Ground_Cover + Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GC_GS <- glmer(Diversity ~ Ground_Cover + Grass_Status + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GC_C <- glmer(Diversity ~ Ground_Cover + Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GC_LS <- glmer(Diversity ~ Ground_Cover + X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GC_LC <- glmer(Diversity ~ Ground_Cover + X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_GGC_WE <- glmer(Diversity ~ Prop_Green_GC + Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GGC_GS <- glmer(Diversity ~ Prop_Green_GC + Grass_Status + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GGC_C <- glmer(Diversity ~ Prop_Green_GC + Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GGC_LS <- glmer(Diversity ~ Prop_Green_GC + X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GGC_LC <- glmer(Diversity ~ Prop_Green_GC + X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_WE_GS <- glmer(Diversity ~ Weed_Estimate + Grass_Status + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_WE_C <- glmer(Diversity ~ Weed_Estimate + Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_WE_LS <- glmer(Diversity ~ Weed_Estimate + X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_WE_LC <- glmer(Diversity ~ Weed_Estimate + X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_GS_C <- glmer(Diversity ~ Grass_Status + Cropping_500m + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GS_LS <- glmer(Diversity ~ Grass_Status + X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_GS_LC <- glmer(Diversity ~ Grass_Status + X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_C_LS <- glmer(Diversity ~ Cropping_500m + X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = TaxModel)
Div_C_LC <- glmer(Diversity ~ Cropping_500m + X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = TaxModel)

Div_LS_LC <- glmer(Diversity ~ X500m.Simspson + X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = TaxModel)


DivModnames <- c("Div_null","Div_E_H","Div_E_GC",
              "Div_E_GGC","Div_E_WE","Div_E_GS",
              "Div_E_C","Div_E_LS","Div_E_LC","Div_H_GC",
              "Div_H_GGC","Div_H_WE","Div_H_GS",
              "Div_H_C","Div_H_LS","Div_H_LC",
              "Div_GC_GGC","Div_GC_WE","Div_GC_GS",
              "Div_GC_C","Div_GC_LS","Div_GC_LC",
              "Div_GGC_WE","Div_GGC_GS","Div_GGC_C",
              "Div_GGC_LS","Div_GGC_LC","Div_WE_GS", 
              "Div_WE_C","Div_WE_LS","Div_WE_LC", 
              "Div_GS_C","Div_GS_LS","Div_GS_LC", 
              "Div_C_LS","Div_C_LC","Div_LS_LC")
divmodlist <- mget(DivModnames)

aictab(divmodlist)
#Elevation + Grass Status
#Green Ground Cover + Grass Status
#Ground Cover + Grass Status


##Interactive

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
#Elevation x Grass Status
#Green GC x Grass Status
#Grasst Status x Landscape simpson
#GC x Grass Status

##Final AICC----

DivModnames3<- c("Div_null", "Div_GS", "Div_E_GS",
              "Div_GGC_GS","Div_GC_GS", "Div_ExGS",
              "Div_GGCxGS","Div_GSxLS","Div_GCxGS")
divmodlist_Final <- mget(DivModnames3)
aictab(divmodlist_Final)





#Top model = Grass Status

#Equivalent models:
##Elevation + Grass Status
##Green GC + Grass Status


##Predictions----

##Grass Status
summary(Div_GS)

topdivpred <- data.frame(Grass_Status = c("Native","Introduced"))
head(topdivpred);dim(topdivpred)

topdivpred1 <- predict(object = Div_GS,newdata= topdivpred,se.fit = T, type = "link",re.form = NA)

topdivpred2<-data.frame(topdivpred,fit.link=topdivpred1$fit,se.link=topdivpred1$se.fit)

topdivpred2$lci.link<-topdivpred2$fit.link-
  (1.96*topdivpred2$se.link)
topdivpred2$uci.link<-topdivpred2$fit.link+ 
  (1.96*topdivpred2$se.link)

topdivpred2$fit<-exp(topdivpred2$fit.link)
topdivpred2$se<-exp(topdivpred2$se.link)
topdivpred2$lci<-exp(topdivpred2$lci.link)
topdivpred2$uci<-exp(topdivpred2$uci.link)

head(topdivpred2);dim(topdivpred2)



##Elevation + Grass Status
summary(Div_E_GS)

seconddivpred <- expand.grid(Elevation = Predictions_Elevation, Grass_Status = c("Native","Introduced"))
head(seconddivpred);dim(seconddivpred)

seconddivpred1 <- predict(object = Div_E_GS,newdata= seconddivpred,se.fit = T, type = "link",re.form = NA)

seconddivpred2<-data.frame(seconddivpred,fit.link=seconddivpred1$fit,se.link=seconddivpred1$se.fit)

seconddivpred2$lci.link<-seconddivpred2$fit.link-
  (1.96*seconddivpred2$se.link)
seconddivpred2$uci.link<-seconddivpred2$fit.link+
  (1.96*seconddivpred2$se.link)

seconddivpred2$fit<-exp(seconddivpred2$fit.link)
seconddivpred2$se<-exp(seconddivpred2$se.link)
seconddivpred2$lci<-exp(seconddivpred2$lci.link)
seconddivpred2$uci<-exp(seconddivpred2$uci.link)

head(seconddivpred2);dim(seconddivpred2)

##Green GC + Grass Status
summary(Div_GGC_GS)

thirddivpred <- expand.grid(Prop_Green_GC = Predictions_GGC, Grass_Status = c("Native","Introduced"))
head(thirddivpred);dim(thirddivpred)

thirddivpred1 <- predict(object = Div_GGC_GS,newdata= thirddivpred,se.fit = T, type = "link",re.form = NA)

thirddivpred2<-data.frame(thirddivpred,fit.link=thirddivpred1$fit,se.link=thirddivpred1$se.fit)

thirddivpred2$lci.link<-thirddivpred2$fit.link-
  (1.96*thirddivpred2$se.link)
thirddivpred2$uci.link<-thirddivpred2$fit.link+
  (1.96*thirddivpred2$se.link)

thirddivpred2$fit<-exp(thirddivpred2$fit.link)
thirddivpred2$se<-exp(thirddivpred2$se.link)
thirddivpred2$lci<-exp(thirddivpred2$lci.link)
thirddivpred2$uci<-exp(thirddivpred2$uci.link)

head(thirddivpred2);dim(thirddivpred2)


##Visual----

#Grass Status
summary(Div_GS)
head(topdivpred2);dim(topdivpred2)

dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = 1:2,y = topdivpred2$fit,xlab = " ",ylab = 'Diversity', type = 'p',pch = 16,cex =2.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,14))
axis(side=1,at=1:2,labels=c('Native','Introduced'))
arrows(x0=1:2, y0=topdivpred2$lci,x1=1:2, y1=topdivpred2$uci,angle=90,length=0.2, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.2,'b)',cex=1.1)

points(x = jitter(raw_x, factor = 1),y = TaxModel$Diversity, pch = 16, cex = 0.4, col = "black")

#Elevation + Grass Status
summary(Div_E_GS)
head(seconddivpred2);dim(seconddivpred2)

CC <- seconddivpred2$Grass_Status == "Introduced"
C_C <- seconddivpred2$Elevation == Predictions_Elevation[10]

dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = TaxModel$Elevation,y = TaxModel$Diversity,xlab = expression("Elevation (m)"),ylab = 'Diversity', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = 60,'a)',cex=1.1)

polygon(x = c(seconddivpred2$Elevation[CC],rev(seconddivpred2$Elevation[CC])), y = c(seconddivpred2$lci[CC],rev(seconddivpred2$uci[CC])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=seconddivpred2$Elevation[CC],y = seconddivpred2$fit[CC],lwd = 2,col = 'grey30')


plot(x = 1:2,y = seconddivpred2$fit [C_C],xlab = " ",ylab = 'Diversity', type = 'p',pch = 16,cex =2.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,14))
axis(side=1,at=1:2,labels=c('Native','Introduced'))
arrows(x0=1:2, y0=seconddivpred2$lci [C_C],x1=1:2, y1=seconddivpred2$uci[C_C],angle=90,length=0.2, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.2,'b)',cex=1.1)

points(x = jitter(raw_x, factor = 1),y = TaxModel$Diversity, pch = 16, cex = 0.4, col = "black")

#GGC + Grass Status

summary(Rich_GGC_GS)
head(thirddivpred2)

DD <- thirddivpred2$Grass_Status == "Introduced"
D_D <- thirddivpred2$Prop_Green_GC == Predictions_GGC[10]


dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = TaxModel$Prop_Green_GC,y = TaxModel$Diversity,xlab = expression("Green Ground Cover (%)"),ylab = 'Diversity', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)
mtext(side=3,line=0,at = 0,'a)',cex=1.1)

polygon(x = c(thirddivpred2$Prop_Green_GC[DD],rev(thirddivpred2$Prop_Green_GC[DD])), y = c(thirddivpred2$lci[DD],rev(thirddivpred2$uci[DD])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=thirddivpred2$Prop_Green_GC[DD],y = thirddivpred2$fit[DD],lwd = 2,col = 'grey30')


plot(x = 1:2,y = thirddivpred2$fit[D_D],xlab = " ",ylab = 'Diversity', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,14))
axis(side=1,at=1:2,labels=c('Native','Introduced'))
arrows(x0=1:2, y0=thirddivpred2$lci [D_D],x1=1:2, y1=thirddivpred2$uci[D_D],angle=90,length=0.1, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.2,'b)',cex=1.1)

points(x = jitter(raw_x, factor = 1),y = TaxModel$Diversity, pch = 16, cex = 0.4, col = "black")



#Main Figure----


dev.new(height=10,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(2,2),mgp=c(2.5,1,0),xpd = T)

plot(x = TaxModel$Elevation,y = TaxModel$Species_Rich,xlab = expression("Elevation (m)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,xaxt = "n")
mtext(side=3,line=0,at = 60,'a)',cex=0.9)
axis(side=1, at=seq(from=min(toprichpred2$Elevation),to=max(toprichpred2$Elevation),length.out=4),labels=round(seq(from=min(TaxModel$Elevation),to=max(TaxModel$Elevation),length.out=4),0),cex.axis=1)

polygon(x = c(toprichpred2$Elevation[AA],rev(toprichpred2$Elevation[AA])), y = c(toprichpred2$lci[AA],rev(toprichpred2$uci[AA])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=toprichpred2$Elevation[AA],y = toprichpred2$fit[AA],lwd = 2,col = 'grey30')


plot(x = 1:2,y = toprichpred2$fit [A_A],xlab = " ",ylab = 'Species Richness', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,16))
axis(side=1,at=1:2,labels=c('',''))
mtext(side=1,line=1.5,at = 0.8,'Native\n Grass',cex=0.9)
mtext(side=1,line=1.5,at = 2.2,'Introduced\n Grass',cex=0.9)
arrows(x0=1:2, y0=toprichpred2$lci [A_A],x1=1:2, y1=toprichpred2$uci[A_A],angle=90,length=0.1, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.2,'b)',cex=0.9)

points(x = jitter(raw_x, factor = 1),y = TaxModel$Species_Rich, pch = 16, cex = 0.4, col = "black")



plot(x = 1:2,y = topdivpred2$fit,xlab = " ",ylab = 'Diversity', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,14))
axis(side=1,at=1:2,labels=c('',''))
mtext(side=1,line=1.5,at = 0.8,'Native\n Grass',cex=0.9)
mtext(side=1,line=1.5,at = 2.2,'Introduced\n Grass',cex=0.9)
arrows(x0=1:2, y0=topdivpred2$lci,x1=1:2, y1=topdivpred2$uci,angle=90,length=0.1, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.2,'c)',cex=0.9)

points(x = jitter(raw_x, factor = 1),y = TaxModel$Diversity, pch = 16, cex = 0.4, col = "black")


#Supporting Figure----

dev.new(height=15,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(3,2),mgp=c(2.5,1,0),xpd = T)

plot(x = TaxModel$Prop_Green_GC,y = TaxModel$Species_Rich,xlab = expression("Green Ground Cover (%)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,xaxt = "n")
axis(side=1, at=seq(from=min(second_richpred2$Prop_Green_GC),to=max(second_richpred2$Prop_Green_GC),length.out=4),labels=round(seq(from=min(TaxModel$Prop_Green_GC),to=max(TaxModel$Prop_Green_GC),length.out=4),0),cex.axis=1)
mtext(side=3,line=0,at = 0,'a)',cex=0.8)

polygon(x = c(second_richpred2$Prop_Green_GC[BB],rev(second_richpred2$Prop_Green_GC[BB])), y = c(second_richpred2$lci[BB],rev(second_richpred2$uci[BB])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=second_richpred2$Prop_Green_GC[BB],y = second_richpred2$fit[BB],lwd = 2,col = 'grey30')


plot(x = 1:2,y = second_richpred2$fit[B_B],xlab = " ",ylab = 'Species Richness', type = 'p',pch = 16,cex =1.7,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,15))
axis(side=1,at=1:2,labels=c('',''))
mtext(side=1,line=1.5,at = 0.8,'Native\n Grass',cex=0.7)
mtext(side=1,line=1.5,at = 2.3,'Introduced\n Grass',cex=0.7)
arrows(x0=1:2, y0=second_richpred2$lci [B_B],x1=1:2, y1=second_richpred2$uci[B_B],angle=90,length=0.05, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.2,'b)',cex=0.8)

points(x = jitter(raw_x, factor = 1),y = TaxModel$Species_Rich, pch = 16, cex = 0.3, col = "black")



plot(x = TaxModel$Elevation,y = TaxModel$Diversity,xlab = expression("Elevation (m)"),ylab = 'Diversity', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,xaxt = "n")
axis(side=1, at=seq(from=min(seconddivpred2$Elevation),to=max(seconddivpred2$Elevation),length.out=4),labels=round(seq(from=min(TaxModel$Elevation),to=max(TaxModel$Elevation),length.out=4),0),cex.axis=1)
mtext(side=3,line=0,at = 60,'c)',cex=0.8)

polygon(x = c(seconddivpred2$Elevation[CC],rev(seconddivpred2$Elevation[CC])), y = c(seconddivpred2$lci[CC],rev(seconddivpred2$uci[CC])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=seconddivpred2$Elevation[CC],y = seconddivpred2$fit[CC],lwd = 2,col = 'grey30')


plot(x = 1:2,y = seconddivpred2$fit [C_C],xlab = " ",ylab = 'Diversity', type = 'p',pch = 16,cex =1.7,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,14))
axis(side=1,at=1:2,labels=c('',''))
mtext(side=1,line=1.5,at = 0.8,'Native\n Grass',cex=0.7)
mtext(side=1,line=1.5,at = 2.3,'Introduced\n Grass',cex=0.7)
arrows(x0=1:2, y0=seconddivpred2$lci [C_C],x1=1:2, y1=seconddivpred2$uci[C_C],angle=90,length=0.05, code=3, lwd=2,col = "black")
mtext(side=3,line=0,at = -0.2,'d)',cex=0.8)

points(x = jitter(raw_x, factor = 1),y = TaxModel$Diversity, pch = 16, cex = 0.4, col = "black")



plot(x = TaxModel$Prop_Green_GC,y = TaxModel$Diversity,xlab = expression("Green Ground Cover (%)"),ylab = 'Diversity', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,xaxt = "n")
axis(side=1, at=seq(from=min(thirddivpred2$Prop_Green_GC),to=max(thirddivpred2$Prop_Green_GC),length.out=4),labels=round(seq(from=min(TaxModel$Prop_Green_GC),to=max(TaxModel$Prop_Green_GC),length.out=4),0),cex.axis=1)
mtext(side=3,line=0,at = 0,'e)',cex=0.8)

polygon(x = c(thirddivpred2$Prop_Green_GC[DD],rev(thirddivpred2$Prop_Green_GC[DD])), y = c(thirddivpred2$lci[DD],rev(thirddivpred2$uci[DD])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=thirddivpred2$Prop_Green_GC[DD],y = thirddivpred2$fit[DD],lwd = 2,col = 'grey30')




#END----