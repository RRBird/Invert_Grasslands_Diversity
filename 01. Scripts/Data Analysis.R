options(scipen = 999) #prevents r from automatically displaying large numbers with scientific notation

#Author: Rhiannon Bird
#Written under version R 4.5.1

#This script contains the data modelling and analysis

#Libraries----
library("glmmTMB")
library("stats")
library("AICcmodavg")
library("lme4")
library("partR2")
library("glmmTMB")
library("MuMIn")
library("vegan")

head(ComVar);dim(ComVar)

str(ComVar)


ComVar$Count[which(is.na(ComVar$Count))] <- 0

#Scale continuous variables

ComVar$Height <- scale(ComVar$Plant_Height)
ComVar$GC <- scale(ComVar$Ground_Cover)
ComVar$GGC <- scale(ComVar$Prop_Green_GC)
ComVar$Graze <- scale(ComVar$Natual_Grazing_1km)
ComVar$HabDiv <- scale(ComVar$X500m.Simspson)
ComVar$WeedScale <- scale(ComVar$Weed)
head(ComVar)

FunComVar$Height <- scale(FunComVar$Plant_Height)
FunComVar$GC <- scale(FunComVar$Ground_Cover)
FunComVar$GGC <- scale(FunComVar$Prop_Green_GC)
FunComVar$Graze <- scale(FunComVar$Natual_Grazing_1km)
FunComVar$HabDiv <- scale(FunComVar$X500m.Simspson)
FunComVar$WeedScale <- scale(FunComVar$Weed)
head(FunComVar)


#Q1 SITE----


##Species Richness---- 
head(ComVar)


Rich_Full <- glmmTMB(Species_Rich ~ ResDay + (Position + Height + GC + GGC + WeedScale)^2 + (1 | Property), family = nbinom2, data = ComVar,na.action = "na.fail",control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
summary(Rich_Full)

MuMIn::getAllTerms(Rich_Full)
#Wrapped in cond() so need to have that around fixed term to make it run properly

Rich_Dredge <- dredge(Rich_Full, fixed = c("cond(ResDay)","cond(Position)"),m.lim = c(NA, 5),trace = TRUE)

Rich_Models <- get.models(Rich_Dredge, subset = delta < 2)

length(names(Rich_Models))
names(Rich_Models)

#Check if the null is within 2 AICc 
Rich_Null <- glmmTMB(Species_Rich ~ 1 + (1 | Property), family = nbinom2, data = ComVar,na.action = "na.fail")
RichList <- list("null" = Rich_Null,"Top"=Rich_Models[1]$`66`)
aictab(RichList)
#it's good

#Top model:
Rich_Models[1]$`66`


RichList_All <- list("P*GC+D" = Rich_Models[1]$`66`,
                  "P+GC+D" = Rich_Models[2]$`2`,
                  "P+GC+GGC+D" = Rich_Models[3]$`4`,
                  "P+GGC+D" = Rich_Models[4]$`3`,
                  "P*GC+H+D" = Rich_Models[5]$`70`,
                  "P*GC+GGC+D" =Rich_Models[6]$`68`,
                  "P+H+GC+GGC+D" = Rich_Models[7]$`8`,
                  "P+GC+H+D" = Rich_Models[8]$`6`,
                  "P+GGC+H+D" =Rich_Models[9]$`7`,
                  "null" = Rich_Null)

aictab(RichList_All)

#Predictions 

Rich_Top <- glmmTMB(Species_Rich ~ ResDay + GC * Position + (1 | Property), family = nbinom2, data = ComVar,na.action = "na.fail")
summary(Rich_Top)

Predictions_GC <- seq(min(ComVar$GC),max(ComVar$GC),length.out=20)
Predictions_Day <- seq(min(ComVar$ResDay),max(ComVar$ResDay),length.out=20)


Site_Rich <- expand.grid(GC = Predictions_GC, Position = c("Escarpment","Valley"),ResDay = Predictions_Day)
head(Site_Rich);dim(Site_Rich)

Site_Rich1 <- predict(object = Rich_Top,newdata= Site_Rich,se.fit = T, type = "link",re.form = NA)

Site_Rich2<-data.frame(Site_Rich,fit.link=Site_Rich1$fit,se.link=Site_Rich1$se.fit)

Site_Rich2$lci.link<-Site_Rich2$fit.link-(1.96*Site_Rich2$se.link)
Site_Rich2$uci.link<-Site_Rich2$fit.link+(1.96*Site_Rich2$se.link)

Site_Rich2$fit<-exp(Site_Rich2$fit.link)
Site_Rich2$se<-exp(Site_Rich2$se.link)
Site_Rich2$lci<-exp(Site_Rich2$lci.link)
Site_Rich2$uci<-exp(Site_Rich2$uci.link)

head(Site_Rich2);dim(Site_Rich2)

##Diversity----

Div_Full <- glmer(Diversity ~ ResDay + (Position + Height + GC + GGC + WeedScale)^2 + (1 | Property), family = Gamma(link = "log"), data = ComVar,na.action = "na.fail")
summary(Div_Full)

MuMIn::getAllTerms(Div_Full) #Not wrapped this time

Div_Dredge <- dredge(Div_Full, fixed = c("ResDay","Position"),m.lim = c(NA, 5),trace = TRUE)

Div_Models <- get.models(Div_Dredge, subset = delta < 2)

length(names(Div_Models))
names(Div_Models)

#Check if the null is within 2 AICc 
Div_Null <- glmer(Diversity ~ 1 + (1 | Property), family = Gamma(link = "log"), data = ComVar,na.action = "na.fail")
DivList <- list("null" = Div_Null,"Top"=Div_Models[1]$`1`)
aictab(DivList)
#Null is better so don't continue with model
Div_Models[1]$`1`


##Abundance----

Abun_Full <- glmmTMB(Count ~ ResDay + (Position + Height + GC + GGC + WeedScale)^2 + (1 | Property), family = nbinom2, data = ComVar,na.action = "na.fail")
summary(Abun_Full)

MuMIn::getAllTerms(Abun_Full)
#Wrapped in cond() so need to have that around fixed term to make it run properly

Abun_Dredge <- dredge(Abun_Full, fixed = c("cond(ResDay)","cond(Position)"),m.lim = c(NA, 5),trace = TRUE)

Abun_Models <- get.models(Abun_Dredge, subset = delta < 2)

length(names(Abun_Models))
names(Abun_Models)

#Check if the null is within 2 AICc 
Abun_Null <- glmmTMB(Count ~ 1 + (1 | Property), family = nbinom2, data = ComVar,na.action = "na.fail")
AbunList <- list("null" = Abun_Null,"Top"=Abun_Models[1]$`8`)
aictab(AbunList)
#it's good

#Top model:
Abun_Models[1]$`8`

AbunList_All <- list("GC+GGC+H+P+D" = Abun_Models[1]$`8`,
                  "GC+P+D" = Abun_Models[2]$`2`,
                  "GC+H+W+P+D" = Abun_Models[3]$`14`,
                  "GC+GGC+P+D" = Abun_Models[4]$`4`,
                  "GC+W+P+D" = Abun_Models[5]$`10`,
                  "GC+H+P+D" =Abun_Models[6]$`6`,
                  "GC*GGC+P+D" = Abun_Models[7]$`20`,
                  "GC*P+H+D" = Abun_Models[8]$`70`,
                  "GC+GGC+W+P+D" =Abun_Models[9]$`12`,
                  "GC*P+D" =Abun_Models[10]$`66`,
                  "GC+H*P+D" =Abun_Models[11]$`2054`,
                  "GC*W+P+D" =Abun_Models[12]$`138`,
                  "GC*P+W+D" =Abun_Models[13]$`74`,
                  "null" = Abun_Null)

aictab(AbunList_All)

#Predictions

Abun_Top <- glmmTMB(Count ~ GC + GGC + Height + Position + ResDay + (1 | Property), family = nbinom2, data = ComVar,na.action = "na.fail")

summary(Abun_Top)

Predictions_GGC <-seq(min(ComVar$GGC),max(ComVar$GGC),length.out=20)
Predictions_Height <-seq(min(ComVar$Height),max(ComVar$Height),length.out=20)

Site_Abun <- expand.grid(GC = Predictions_GC, Position = c("Escarpment","Valley"),GGC = Predictions_GGC,ResDay = Predictions_Day, Height = Predictions_Height)
head(Site_Abun);dim(Site_Abun)

Site_Abun1 <- predict(object = Abun_Top,newdata= Site_Abun,se.fit = T, type = "link",re.form = NA)

Site_Abun2<-data.frame(Site_Abun,fit.link=Site_Abun1$fit,se.link=Site_Abun1$se.fit)

Site_Abun2$lci.link<-Site_Abun2$fit.link-(1.96*Site_Abun2$se.link)
Site_Abun2$uci.link<-Site_Abun2$fit.link+(1.96*Site_Abun2$se.link)

Site_Abun2$fit<-exp(Site_Abun2$fit.link)
Site_Abun2$se<-exp(Site_Abun2$se.link)
Site_Abun2$lci<-exp(Site_Abun2$lci.link)
Site_Abun2$uci<-exp(Site_Abun2$uci.link)

head(Site_Abun2);dim(Site_Abun2)

##Functional Richness---- 
head(FunComVar)

Fun_Rich_Full <- glmmTMB(FRic ~ ResDay + (Position + Height + GC + GGC + WeedScale)^2 + (1 | Property), family = nbinom2, data = ComVar,na.action = "na.fail",control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
summary(Fun_Rich_Full)

MuMIn::getAllTerms(Fun_Rich_Full) #Wrapped

Fun_Rich_Dredge <- dredge(Fun_Rich_Full, fixed = c("cond(ResDay)","cond(Position)"),m.lim = c(NA, 5),trace = TRUE)

Fun_Rich_Models <- get.models(Fun_Rich_Dredge, subset = delta < 2)

length(names(Fun_Rich_Models))
names(Fun_Rich_Models)

#Check if the null is within 2 AICc 
Fun_Rich_Null <- glmmTMB(FRic ~ 1 + (1 | Property), family = nbinom2, data = ComVar,na.action = "na.fail",control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
Fun_RichList <- list("null" = Fun_Rich_Null,"Top"=Fun_Rich_Models[1]$`3`)
aictab(Fun_RichList)
#Null model is better don't continue further 


##Functional Evenness----
head(FunComVar)
Eve_mod_data <- ComVar[-which(is.na(ComVar$FEve)),]

Fun_Eve_Full <- glmer(FEve ~ ResDay + (Position + Height + GC + GGC + WeedScale)^2 + (1 | Property), family = Gamma(link = "log"), data = Eve_mod_data,na.action = "na.fail")
summary(Fun_Eve_Full)

MuMIn::getAllTerms(Fun_Eve_Full) #Not wrapped

Fun_Eve_Dredge <- dredge(Fun_Eve_Full, fixed = c("ResDay","Position"),m.lim = c(NA, 5),trace = TRUE)

Fun_Eve_Models <- get.models(Fun_Eve_Dredge, subset = delta < 2)

length(names(Fun_Eve_Models))
names(Fun_Eve_Models)

#Check if the null is within 2 AICc 
Fun_Eve_Null <- glmer(FEve ~ 1 + (1 | Property), family = Gamma(link = "log"), data = Eve_mod_data,na.action = "na.fail")
Fun_EveList <- list("null" = Fun_Eve_Null,"Top"=Fun_Eve_Models[1]$`1`)
aictab(Fun_EveList)
#Null is better so don't continue with model
Fun_Eve_Models[1]$`1`

##Functional Dis----

Dis_Mod_Data <- ComVar[-which(is.na(ComVar$FDis)),]
#one site had no inverts so needed to omit them in model for dredge
Dis_Full <- glmmTMB(FDis ~ ResDay + (Position + Height + GC + GGC + WeedScale)^2 + (1 | Property), family = gaussian(), data = Dis_Mod_Data,na.action = "na.fail")
summary(Dis_Full)

MuMIn::getAllTerms(Dis_Full) #they are wrapped

Dis_Dredge <- dredge(Dis_Full, fixed = c("cond(ResDay)","cond(Position)"),m.lim = c(NA, 5),trace = TRUE)

Dis_Models <- get.models(Dis_Dredge, subset = delta < 2)

length(names(Dis_Models))
names(Dis_Models)

#Check if the null is within 2 AICc 
Dis_Null <- glmmTMB(FDis ~ 1 + (1 | Property), family = gaussian(), data = Dis_Mod_Data,na.action = "na.fail")
DisList <- list("null" = Dis_Null,"Top"=Dis_Models[1]$`1`)
aictab(DisList)
#Null is top

##Community Composition----

ComVar_Subsected <- ComVar[-which(is.na(ComVar$ComComp)),]
#one site had no inverts so needed to omit them in model for dredge
Comp_Full <- glmmTMB(ComComp ~ ResDay + (Position + Height + GC + GGC + WeedScale)^2 + (1 | Property), family = gaussian(), data = ComVar_Subsected,na.action = "na.fail")
summary(Comp_Full)

MuMIn::getAllTerms(Comp_Full) #they are wrapped

Comp_Dredge <- dredge(Comp_Full, fixed = c("cond(ResDay)","cond(Position)"),m.lim = c(NA, 5),trace = TRUE)

Comp_Models <- get.models(Comp_Dredge, subset = delta < 2)

length(names(Comp_Models))
names(Comp_Models)

#Check if the null is within 2 AICc 
Comp_Null <- glmmTMB(ComComp ~ 1 + (1 | Property), family = gaussian(), data = ComVar_Subsected,na.action = "na.fail")
CompList <- list("null" = Comp_Null,"Top"=Comp_Models[1]$`68`)
aictab(CompList)
#it's good

#Top model:
Comp_Models[1]$`68`

CompList_All <- list("GC*P+GGC+D" = Comp_Models[1]$`68`,
                     "GC+P+D" = Comp_Models[2]$`2`,
                     "GC+H+P+D" = Comp_Models[3]$`6`,
                     "GC*P+H+D" = Comp_Models[4]$`70`,
                     "GC*P+D" = Comp_Models[5]$`66`,
                     "H+P+D" =Comp_Models[6]$`5`,
                     "GC+GGC+H+P+D" = Comp_Models[7]$`8`,
                     "GC+GGC+P+D" = Comp_Models[8]$`4`,
                     "P+D" =Comp_Models[9]$`1`,
                     "GC+H+P+D" =Comp_Models[10]$`2054`,
                     "GC+H+P+D" =Comp_Models[11]$`7`,
                     "null" = Comp_Null)

aictab(CompList_All)

#Predictions

Comp_Top <- glmmTMB(ComComp ~ GC* Position + GGC +ResDay + (1 | Property), family = gaussian(), data = ComVar_Subsected,na.action = "na.fail")
summary(Comp_Top)

Site_Comp<- expand.grid(GC = Predictions_GC, Position = c("Escarpment","Valley"), ResDay = Predictions_Day, GGC=Predictions_GGC)
head(Site_Comp);dim(Site_Comp)

Site_Comp1 <- predict(object = Comp_Top,newdata= Site_Comp,se.fit = T, type = "link",re.form = NA)

Site_Comp2<-data.frame(Site_Comp,fit.link=Site_Comp1$fit,se.link=Site_Comp1$se.fit)

Site_Comp2$lci.link<-Site_Comp2$fit.link-(1.96*Site_Comp2$se.link)
Site_Comp2$uci.link<-Site_Comp2$fit.link+(1.96*Site_Comp2$se.link)

Site_Comp2$fit<-exp(Site_Comp2$fit.link)
Site_Comp2$se<-exp(Site_Comp2$se.link)
Site_Comp2$lci<-exp(Site_Comp2$lci.link)
Site_Comp2$uci<-exp(Site_Comp2$uci.link)

head(Site_Comp2);dim(Site_Comp2)

#Q2 LANDSCAPE----

##Species Richness---- 
head(ComVar)


Rich_Full_2 <- glmmTMB(Species_Rich ~ ResDay + (Position + HabDiv + Graze + X500m.Dominant.Landscape.Class)^2 + (1 | Property), family = nbinom2, data = ComVar,na.action = "na.fail")
summary(Rich_Full_2)

MuMIn::getAllTerms(Rich_Full_2) #Wrapped

Rich_Dredge_2 <- dredge(Rich_Full_2, fixed = c("cond(ResDay)","cond(Position)"),m.lim = c(NA, 5),trace = TRUE)

Rich_Models_2 <- get.models(Rich_Dredge_2, subset = delta < 2)

length(names(Rich_Models_2))
names(Rich_Models_2)

#Check if the null is within 2 AICc 
RichList_2 <- list("null" = Rich_Null,"Top"=Rich_Models_2[1]$`3`)
aictab(RichList_2)
#it's good

#Top model:
Rich_Models_2[1]$`3`

RichList_All_2 <- list("Div+P+D" = Rich_Models_2[1]$`3`,
                  "Div+Dom+P+D" = Rich_Models_2[2]$`7`,
                  "null" = Rich_Null)

aictab(RichList_All_2)

#Predictions

Rich_Top_2 <- glmmTMB(Species_Rich ~ ResDay + Position + HabDiv + (1 | Property), family = nbinom2, data = ComVar,na.action = "na.fail")
summary(Rich_Top_2)

Predictions_Hab_Div <-seq(min(ComVar$HabDiv),max(ComVar$HabDiv),length.out=20)


Land_Rich <- expand.grid(HabDiv = Predictions_Hab_Div,Position = c("Escarpment","Valley"),ResDay = Predictions_Day)
head(Land_Rich);dim(Land_Rich)

Land_Rich1 <- predict(object = Rich_Top_2,newdata= Land_Rich,se.fit = T, type = "link",re.form = NA)

Land_Rich2<-data.frame(Land_Rich,fit.link=Land_Rich1$fit,se.link=Land_Rich1$se.fit)

Land_Rich2$lci.link<-Land_Rich2$fit.link-(1.96*Land_Rich2$se.link)
Land_Rich2$uci.link<-Land_Rich2$fit.link+(1.96*Land_Rich2$se.link)

Land_Rich2$fit<-exp(Land_Rich2$fit.link)
Land_Rich2$se<-exp(Land_Rich2$se.link)
Land_Rich2$lci<-exp(Land_Rich2$lci.link)
Land_Rich2$uci<-exp(Land_Rich2$uci.link)

head(Land_Rich2);dim(Land_Rich2)

##Diversity----

Div_Full_2 <- glmer(Diversity ~ ResDay + + (Position + HabDiv + Graze + X500m.Dominant.Landscape.Class)^2 + (1 | Property), family = Gamma(link = "log"), data = ComVar,na.action = "na.fail")
summary(Div_Full_2)

MuMIn::getAllTerms(Div_Full_2) #Not wrapped

Div_Dredge_2 <- dredge(Div_Full_2, fixed = c("ResDay","Position"),m.lim = c(NA, 5),trace = TRUE)

Div_Models_2 <- get.models(Div_Dredge_2, subset = delta < 2)

length(names(Div_Models_2))
names(Div_Models_2)

#Check if the null is within 2 AICc 
DivList_2 <- list("null" = Div_Null,"Top"=Div_Models_2[1]$`1`)
aictab(DivList_2)
#Null is better so don't continue with model
Div_Models_2[1]$`1`


##Abundance----

Abun_Full_2 <- glmmTMB(Count ~ ResDay + (Position + HabDiv + Graze + X500m.Dominant.Landscape.Class)^2 + (1 | Property), family = nbinom2, data = ComVar,na.action = "na.fail")
summary(Abun_Full_2)

MuMIn::getAllTerms(Abun_Full_2) #Wrapped

Abun_Dredge_2 <- dredge(Abun_Full_2, fixed = c("cond(ResDay)","cond(Position)"),m.lim = c(NA, 5),trace = TRUE)

Abun_Models_2 <- get.models(Abun_Dredge_2, subset = delta < 2)

length(names(Abun_Models_2))
names(Abun_Models_2)

#Check if the null is within 2 AICc 
AbunList_2 <- list("null" = Abun_Null,"Top"=Abun_Models_2[1]$`3`)
aictab(AbunList_2)
#it's good

#Top model:
Abun_Models_2[1]$`3`

AbunList_All_2 <- list("Div+P+D" = Abun_Models_2[1]$`3`,
                     "G+Div+P+D" = Abun_Models_2[2]$`4`,
                     "null" = Abun_Null)

aictab(AbunList_All_2)

#Predictions
Abun_Top_2 <- glmmTMB(Count ~ ResDay + Position + HabDiv + (1 | Property), family = nbinom2, data = ComVar,na.action = "na.fail")

summary(Abun_Top_2)

Land_Abun <- expand.grid(HabDiv = Predictions_Hab_Div,Position = c("Escarpment","Valley"),ResDay = Predictions_Day)
head(Land_Abun);dim(Land_Abun)

Land_Abun1 <- predict(object = Abun_Top_2,newdata= Land_Abun,se.fit = T, type = "link",re.form = NA)

Land_Abun2<-data.frame(Land_Rich,fit.link=Land_Abun1$fit,se.link=Land_Abun1$se.fit)

Land_Abun2$lci.link<-Land_Abun2$fit.link-(1.96*Land_Abun2$se.link)
Land_Abun2$uci.link<-Land_Abun2$fit.link+(1.96*Land_Abun2$se.link)

Land_Abun2$fit<-exp(Land_Abun2$fit.link)
Land_Abun2$se<-exp(Land_Abun2$se.link)
Land_Abun2$lci<-exp(Land_Abun2$lci.link)
Land_Abun2$uci<-exp(Land_Abun2$uci.link)

head(Land_Abun2);dim(Land_Abun2)

##Functional Richness---- 
head(FunComVar)

Fun_Rich_Full_2 <- glmmTMB(FRic ~ ResDay + (Position + HabDiv + Graze + X500m.Dominant.Landscape.Class)^2 + (1 | Property), family = nbinom2, data = ComVar,na.action = "na.fail")
summary(Fun_Rich_Full_2)

MuMIn::getAllTerms(Fun_Rich_Full_2) #Wrapped

Fun_Rich_Dredge_2 <- dredge(Fun_Rich_Full_2, fixed = c("cond(ResDay)","cond(Position)"),m.lim = c(NA, 5),trace = TRUE)

Fun_Rich_Models_2 <- get.models(Fun_Rich_Dredge_2, subset = delta < 2)

length(names(Fun_Rich_Models_2))
names(Fun_Rich_Models_2)
#there's 20 models but 15 convergence warnings
#check null first then if better then null check all models and remove convergence issue models

#Check if the null is within 2 AICc 
Fun_RichList_2 <- list("null" = Fun_Rich_Null,"Top"=Fun_Rich_Models_2[1]$`67`)
aictab(Fun_RichList_2)
#Null is within 2 AICc

##Functional Evenness----

Fun_Eve_Full_2 <- glmer(FEve ~ ResDay + (Position + HabDiv + Graze + X500m.Dominant.Landscape.Class)^2 + (1 | Property), family = Gamma(link = "log"), data = Eve_mod_data,na.action = "na.fail")
summary(Fun_Eve_Full_2)

MuMIn::getAllTerms(Fun_Eve_Full_2) #Not wrapped

Fun_Eve_Dredge_2 <- dredge(Fun_Eve_Full_2, fixed = c("ResDay","Position"),m.lim = c(NA, 5),trace = TRUE)

Fun_Eve_Models_2 <- get.models(Fun_Eve_Dredge_2, subset = delta < 2)

length(names(Fun_Eve_Models_2))
names(Fun_Eve_Models_2)

#Check if the null is within 2 AICc 
Fun_EveList_2 <- list("null" = Fun_Eve_Null,"Top"=Fun_Eve_Models_2[1]$`1`)
aictab(Fun_EveList_2)
#Null is better so don't continue with model

##Functional Dis----

Dis_Full2 <- glmmTMB(FDis ~ ResDay + (Position + HabDiv + Graze + X500m.Dominant.Landscape.Class)^2 + (1 | Property), family = gaussian(), data = Dis_Mod_Data,na.action = "na.fail")
summary(Dis_Full)

MuMIn::getAllTerms(Dis_Full2) #they are wrapped

Dis_Dredge2 <- dredge(Dis_Full2, fixed = c("cond(ResDay)","cond(Position)"),m.lim = c(NA, 5),trace = TRUE)

Dis_Models2 <- get.models(Dis_Dredge2, subset = delta < 2)

length(names(Dis_Models2))
names(Dis_Models2)

#Check if the null is within 2 AICc 
DisList2 <- list("null" = Dis_Null,"Top"=Dis_Models2[1]$`3`)
aictab(DisList2)
#Null is top

##Community Composition----

Comp_Full_2 <- glmmTMB(ComComp ~ ResDay + (Position + HabDiv + Graze + X500m.Dominant.Landscape.Class)^2 + (1 | Property), family = gaussian(), data = ComVar_Subsected,na.action = "na.fail")
summary(Comp_Full_2)

MuMIn::getAllTerms(Comp_Full_2) #wrapped

Comp_Dredge_2 <- dredge(Comp_Full_2, fixed = c("cond(ResDay)","cond(Position)"),m.lim = c(NA, 5),trace = TRUE)

Comp_Models_2 <- get.models(Comp_Dredge_2, subset = delta < 2)

length(names(Comp_Models_2))
names(Comp_Models_2)

#Check if the null is within 2 AICc 
CompList_2 <- list("null" = Comp_Null,"Top"=Comp_Models_2[1]$`22`)
aictab(CompList_2)
#it's good

#Top model:
Comp_Models_2[1]$`22` #G*P+Dom+D

#Predictions
Predictions_Graze <-seq(min(ComVar$Graze),max(ComVar$Graze),length.out=20)
Predictions_DomHab <- unique(ComVar$X500m.Dominant.Landscape.Class)

Comp_Top_2 <- glmmTMB(ComComp ~ Graze * Position + X500m.Dominant.Landscape.Class + ResDay + (1 | Property), family = gaussian(), data = ComVar_Subsected,na.action = "na.fail")
summary(Comp_Top)

Land_Comp<- expand.grid(Graze = Predictions_Graze, Position = c("Escarpment","Valley"), ResDay = Predictions_Day, X500m.Dominant.Landscape.Class = Predictions_DomHab)
head(Land_Comp);dim(Land_Comp)

Land_Comp1 <- predict(object = Comp_Top_2,newdata= Land_Comp,se.fit = T, type = "link",re.form = NA)

Land_Comp2<-data.frame(Land_Comp,fit.link=Land_Comp1$fit,se.link=Land_Comp1$se.fit)

Land_Comp2$lci.link<-Land_Comp2$fit.link-(1.96*Land_Comp2$se.link)
Land_Comp2$uci.link<-Land_Comp2$fit.link+(1.96*Land_Comp2$se.link)

Land_Comp2$fit<-exp(Land_Comp2$fit.link)
Land_Comp2$se<-exp(Land_Comp2$se.link)
Land_Comp2$lci<-exp(Land_Comp2$lci.link)
Land_Comp2$uci<-exp(Land_Comp2$uci.link)

head(Land_Comp2);dim(Land_Comp2)


#Q3 SITE VS LANDSCAPE----


##Species Richness----
#Model with all variables from Q1 and Q3
Rich_Multi_Model <- glmmTMB(Species_Rich ~ GC*Position + HabDiv + ResDay + (1 | Property), family = nbinom2, data = ComVar)

summary(Rich_Multi_Model)

r2_SR_full <- partR2(Rich_Multi_Model, partvars = c("GC", "HabDiv", "Position","ResDay"),R2_type = "marginal", nboot = 1000, data = ComVar)
#Doesn't work with glmmTMB
#Need to do it manually

#create groups of site, landscape and design variables
Rich_site_vars <- c("GC")
Rich_landscape_vars <- c("HabDiv")
Rich_design_vars <- c("Position","ResDay")

#part_r2 function written by Rhiannon with help of Claude AI
part_r2 <- function(model, group_vars) {
  r2_full <- r.squaredGLMM(model)[1, "R2m"]
  all_terms <- attr(terms(model), "term.labels")
  terms_to_drop <- all_terms[sapply(all_terms, function(t) {
    term_parts <- strsplit(t, ":")[[1]]
    any(term_parts %in% group_vars)
  })]
  
  drop_formula <- as.formula(paste(". ~ . -", paste(terms_to_drop, collapse = " - ")))
  reduced_formula <- update(formula(model), drop_formula)
  reduced_model <- update(model, formula = reduced_formula)
  
  r2_reduced <- r.squaredGLMM(reduced_model)[1, "R2m"]
  r2_full - r2_reduced
}

Rich_site_r2 <- part_r2(Rich_Multi_Model, Rich_site_vars)
Rich_landscape_r2 <- part_r2(Rich_Multi_Model, Rich_landscape_vars)
Rich_design_r2 <- part_r2(Rich_Multi_Model, Rich_design_vars)

Rich_site_r2
Rich_landscape_r2
Rich_design_r2


##Abundance----
Abun_Multi_Model <- glmmTMB(Count ~ GC + GGC + Height + HabDiv + Position + ResDay+ (1 | Property), family = nbinom2, data = ComVar)

summary(Abun_Multi_Model)

r2_Abun_full <- r.squaredGLMM(Abun_Full_Model)[1, "R2m"]

Abun_site_vars <- c("GC","GGC","Height")
Abun_landscape_vars <- c("HabDiv")
Abun_design_vars <- c("Position","ResDay")

Abun_site_r2 <- part_r2(Abun_Multi_Model, Abun_site_vars)
Abun_landscape_r2 <- part_r2(Abun_Multi_Model, Abun_landscape_vars)
Abun_design_r2 <- part_r2(Abun_Multi_Model, Abun_design_vars)

Abun_site_r2
Abun_landscape_r2
Abun_design_r2

##Community Composition----

Comp_Multi_Model <- glmmTMB(ComComp ~ GC * Position + GGC + Graze + X500m.Dominant.Landscape.Class + ResDay + (1 | Property), family = gaussian(), data = ComVar_Subsected,na.action = "na.fail")

summary(Comp_Multi_Model)

r2_Comp_full <- r.squaredGLMM(Comp_Multi_Model)[1, "R2m"]

Comp_site_vars <- c("GC","GGC")
Comp_landscape_vars <- c("Graze","X500m.Dominant.Landscape.Class")
Comp_design_vars <- c("Position","ResDay")

Comp_site_r2 <- part_r2(Comp_Multi_Model, Comp_site_vars)
Comp_landscape_r2 <- part_r2(Comp_Multi_Model, Comp_landscape_vars)
Comp_design_r2 <- part_r2(Comp_Multi_Model, Comp_design_vars)

Comp_site_r2
Comp_landscape_r2
Comp_design_r2

#SEM WITH MANAGEMENT----

#END----