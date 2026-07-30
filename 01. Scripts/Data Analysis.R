options(scipen = 999) #prevents r from automatically displaying large numbers with scientific notation

#Author: Rhiannon Bird
#Written under version R 4.5.1

#This script contains the data modelling and analysis

library("glmmTMB")
library("stats")
library("AICcmodavg")


#Site Backwards Step Model Selection----
head(ComVar);dim(ComVar)

#scale all contionus variables to limit converege issue due to variables on different scales
str(ComVar)

ComVar$Height <- scale(ComVar$Plant_Height)
ComVar$GC <- scale(ComVar$Ground_Cover)
ComVar$GGC <- scale(ComVar$Prop_Green_GC)

##Species Richness----



Site_SR_Full <- glmmTMB(Species_Rich ~ Height + GC + GGC + Weed_Estimate + Grass_Status + ResDay + Position + (1 | Property), family = nbinom2, data = ComVar)
#removed dom weed and grass, high levels and were messing with model

drop1(Site_SR_Full, test = "Chisq") 
#so none is if the model has no terms removed
#Others are the model AICc if that term is dropped 
#lowest AICc is the term to remove (as long as it's lower than none line)
#therefore remove weed estimate

Site_SR_Back1 <- glmmTMB(Species_Rich ~ Height + GC + GGC + Grass_Status + ResDay + Position + (1 | Property), family = nbinom2, data = ComVar)
#sanity check
aictab(list("back" = Site_SR_Back1,"full" = Site_SR_Full)) #yes back is better than full
drop1(Site_SR_Back1, test = "Chisq") #drop resday

Site_SR_Back2 <- glmmTMB(Species_Rich ~ Height + GC + GGC + Grass_Status + Position + (1 | Property), family = nbinom2, data = ComVar)
drop1(Site_SR_Back2, test = "Chisq") #drop height

Site_SR_Back3 <- glmmTMB(Species_Rich ~ GC + GGC + Grass_Status + Position + (1 | Property), family = nbinom2, data = ComVar)
drop1(Site_SR_Back3, test = "Chisq") #drop none

#check against a null model to make sure it makes sense
SR_null <-glmmTMB(Species_Rich ~ 1 + (1 | Property), family = nbinom2, data = ComVar)
aictab(list("null"=SR_null, "final"=Site_SR_Back3))
#it's good

###preds----
summary(Site_SR_Back3)

Predictions_GC <- seq(min(ComVar$GC),max(ComVar$GC),length.out=20)
Predictions_GGC <-seq(min(ComVar$GGC),max(ComVar$GGC),length.out=20)

Site_Rich <- expand.grid(GC = Predictions_GC, GGC = Predictions_GGC, Grass_Status = c("Native","Introduced","Unknown"),Position = c("Escarpment","Valley"))
head(Site_Rich);dim(Site_Rich)

Site_Rich1 <- predict(object = Site_SR_Back3,newdata= Site_Rich,se.fit = T, type = "link",re.form = NA)

Site_Rich2<-data.frame(Site_Rich,fit.link=Site_Rich1$fit,se.link=Site_Rich1$se.fit)

Site_Rich2$lci.link<-Site_Rich2$fit.link-
  (1.96*Site_Rich2$se.link)
Site_Rich2$uci.link<-Site_Rich2$fit.link+
  (1.96*Site_Rich2$se.link)

Site_Rich2$fit<-exp(Site_Rich2$fit.link)
Site_Rich2$se<-exp(Site_Rich2$se.link)
Site_Rich2$lci<-exp(Site_Rich2$lci.link)
Site_Rich2$uci<-exp(Site_Rich2$uci.link)

head(Site_Rich2);dim(Site_Rich2)

###Figure----
#basic fig no a b c and need to replace the x axis but gives a visual of model

head(Site_Rich2)
AA <- Site_Rich2$Grass_Status == "Introduced" & Site_Rich2$GGC == Predictions_GGC[10] & Site_Rich2$Position == "Escarpment" 
AAA <- Site_Rich2$Grass_Status == "Introduced" & Site_Rich2$GC == Predictions_GC[10] & Site_Rich2$Position == "Escarpment" 
A_A <- Site_Rich2$GGC == Predictions_GGC[10] & Site_Rich2$Position == "Escarpment" & Site_Rich2$GC == Predictions_GC[10]
A_A_A <- Site_Rich2$GGC == Predictions_GGC[10] & Site_Rich2$Grass_Status == "Introduced" & Site_Rich2$GC == Predictions_GC[10]

raw_x <- ifelse(ComVar$Grass_Status ==
                  "Native", 1, 
                ifelse(ComVar$Grass_Status ==
                         "Introduced", 2, NA))

raw_x1 <- ifelse(ComVar$Position ==
                  "Escarpment", 1, 
                ifelse(ComVar$Position ==
                         "Valley", 2, NA))

dev.new(height=10,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(2,2),mgp=c(2.5,1,0),xpd = T)

plot(x = ComVar$GC,y = ComVar$Species_Rich,xlab = expression("Ground Cover (%)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)

polygon(x = c(Site_Rich2$GC[AA],rev(Site_Rich2$GC[AA])), y = c(Site_Rich2$lci[AA],rev(Site_Rich2$uci[AA])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Rich2$GC[AA],y = Site_Rich2$fit[AA],lwd = 2,col = 'grey30')


plot(x = ComVar$GGC,y = ComVar$Species_Rich,xlab = expression("Green Ground Cover (%)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)

polygon(x = c(Site_Rich2$GGC[AAA],rev(Site_Rich2$GGC[AAA])), y = c(Site_Rich2$lci[AAA],rev(Site_Rich2$uci[AAA])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Rich2$GGC[AAA],y = Site_Rich2$fit[AAA],lwd = 2,col = 'grey30')


plot(x = 1:2,y = Site_Rich2$fit [A_A][1:2],xlab = " ",ylab = 'Species Richness', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,16))
axis(side=1,at=c(0.8,2.2),labels=c('Native','Introduced'))
arrows(x0=1:2, y0=Site_Rich2$lci [A_A][1:2],x1=1:2, y1=Site_Rich2$uci[A_A][1:2],angle=90,length=0.1, code=3, lwd=2,col = "black")
points(x = jitter(raw_x[-which(is.na(raw_x))], factor = 1),y = ComVar$Species_Rich[-which(is.na(raw_x))], pch = 16, cex = 0.4, col = "black")


plot(x = 1:2,y = Site_Rich2$fit [A_A_A],xlab = " ",ylab = 'Species Richness', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,16))
axis(side=1,at=c(0.8,2.2),labels=c('Escarpment','Valley'))
arrows(x0=1:2, y0=Site_Rich2$lci [A_A_A],x1=1:2, y1=Site_Rich2$uci[A_A_A],angle=90,length=0.1, code=3, lwd=2,col = "black")
points(x = jitter(raw_x1, factor = 1),y = ComVar$Species_Rich, pch = 16, cex = 0.4, col = "black")


#Does grazing impact veg----


grazetype_mod <- glmmTMB(Species_Rich ~ GC + GGC + Grass_Status + Position + Grazing_Type + (1 | Property), family = nbinom2, data = ComVar)



#overall fit
aictab(list("veg only"=Site_SR_Back3,"with grazing"=grazetype_mod))
#equivalent

#look at mod estimates
summary(Site_SR_Back3)
summary(grazetype_mod)

#Veg related signifigance stays the same but p-values shift, higher with grazing adding -- could indicate they are intertwined in some way?


#Landscape Backwards Step Model Selection----

#scale all continuous variables to limit converge issue due to variables on different scales
str(ComVar)

ComVar$Graze <- scale(ComVar$Natual_Grazing_1km)
ComVar$Hab_Div <- scale(ComVar$X500m.Simspson)


##Species Richness----

Land_SR_Full <- glmmTMB(Species_Rich ~ X500m.Dominant.Landscape.Class + Graze + Hab_Div + ResDay + Position + (1 | Property), family = nbinom2, data = ComVar)
drop1(Land_SR_Full, test = "Chisq") #drop graze

Land_SR_Back1 <- glmmTMB(Species_Rich ~ X500m.Dominant.Landscape.Class + Hab_Div + ResDay + Position + (1 | Property), family = nbinom2, data = ComVar)
drop1(Land_SR_Back1, test = "Chisq") #drop resday

Land_SR_Back2 <- glmmTMB(Species_Rich ~ X500m.Dominant.Landscape.Class + Hab_Div + Position + (1 | Property), family = nbinom2, data = ComVar)
drop1(Land_SR_Back2, test = "Chisq") #drop none

#check against a null model to make sure it makes sense
aictab(list("null"=SR_null, "final"=Land_SR_Back2))
#it's good


#END----