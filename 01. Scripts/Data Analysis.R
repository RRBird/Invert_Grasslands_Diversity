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
library("piecewiseSEM")
library("reformulas")
library("openxlsx")

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

Rich_Models <- get.models(Rich_Dredge, subset = delta < 10)

Rich_Dredge
class(Rich_Dredge)
Rich_Dredge[[1]]
rownames(Rich_Dredge)
names(attributes(Rich_Dredge))

attr(Rich_Dredge, "model.calls")
write.xlsx(Rich_Dredge.Excel, 'TEMPDOC.xlsx')

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

attr(Div_Dredge, "model.calls")
write.xlsx(Div_Dredge, 'TEMPDOC.xlsx')

Div_Models <- get.models(Div_Dredge, subset = delta < 2)

length(names(Div_Models))
names(Div_Models)

#Check if the null is within 2 AICc 
Div_Null <- glmer(Diversity ~ 1 + (1 | Property), family = Gamma(link = "log"), data = ComVar,na.action = "na.fail")
DivList <- list("null" = Div_Null,"Top"=Div_Models[1]$`1`)
aictab(DivList)
#Null is better so don't continue with model
Div_Models[1]$`1`

Div_Dredge[[1]]

##Abundance----

Abun_Full <- glmmTMB(Count ~ ResDay + (Position + Height + GC + GGC + WeedScale)^2 + (1 | Property), family = nbinom2, data = ComVar,na.action = "na.fail")
summary(Abun_Full)

MuMIn::getAllTerms(Abun_Full)
#Wrapped in cond() so need to have that around fixed term to make it run properly

Abun_Dredge <- dredge(Abun_Full, fixed = c("cond(ResDay)","cond(Position)"),m.lim = c(NA, 5),trace = TRUE)
Abun_Dredge[[1]]

attr(Abun_Dredge, "model.calls")
write.xlsx(Abun_Dredge, 'TEMPDOC.xlsx')

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

##Community Composition----

ComVar_Subsected <- ComVar[-which(is.na(ComVar$ComComp)),]
#one site had no inverts so needed to omit them in model for dredge
Comp_Full <- glmmTMB(ComComp ~ ResDay + (Position + Height + GC + GGC + WeedScale)^2 + (1 | Property), family = gaussian(), data = ComVar_Subsected,na.action = "na.fail")
summary(Comp_Full)

MuMIn::getAllTerms(Comp_Full) #they are wrapped

Comp_Dredge <- dredge(Comp_Full, fixed = c("cond(ResDay)","cond(Position)"),m.lim = c(NA, 5),trace = TRUE)

attr(Comp_Dredge, "model.calls")
write.xlsx(Comp_Dredge, 'TEMPDOC.xlsx')

Comp_Dredge[[1]]

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

Predictions_CompGC <- seq(min(ComVar_Subsected$GC),max(ComVar_Subsected$GC),length.out=20)
Predictions_CompGGC <- seq(min(ComVar_Subsected$GGC),max(ComVar_Subsected$GGC),length.out=20)
Predictions_CompDay <- seq(min(ComVar_Subsected$ResDay),max(ComVar_Subsected$ResDay),length.out=20)

Site_Comp<- expand.grid(GC = Predictions_CompGC, Position = c("Escarpment","Valley"), ResDay = Predictions_CompDay, GGC=Predictions_CompGGC)
head(Site_Comp);dim(Site_Comp)

Site_Comp1 <- predict(object = Comp_Top,newdata= Site_Comp,se.fit = T, type = "link",re.form = NA)

Site_Comp2<-data.frame(Site_Comp,fit.link=Site_Comp1$fit,se.link=Site_Comp1$se.fit)

Site_Comp2$lci.link<-Site_Comp2$fit.link-(1.96*Site_Comp2$se.link)
Site_Comp2$uci.link<-Site_Comp2$fit.link+(1.96*Site_Comp2$se.link)

head(Site_Comp2);dim(Site_Comp2)

##Functional Richness---- 
head(FunComVar)

Fun_Rich_Full <- glmmTMB(FRic ~ ResDay + (Position + Height + GC + GGC + WeedScale)^2 + (1 | Property), family = nbinom2, data = ComVar,na.action = "na.fail",control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
summary(Fun_Rich_Full)


MuMIn::getAllTerms(Fun_Rich_Full) #Wrapped

packageVersion("MuMIn")
options(warn = 1)

Fun_Rich_Dredge <- dredge(Fun_Rich_Full, fixed = c("cond(ResDay)","cond(Position)"),m.lim = c(NA, 5),trace = TRUE)

Fun_Rich_Dredge[[1]]
Fun_Rich_Dredge_No_Error <- Fun_Rich_Dredge[1:26, ]

attr(Fun_Rich_Dredge_No_Error, "model.calls")
write.xlsx(Fun_Rich_Dredge_No_Error, 'TEMPDOC.xlsx')

Fun_Rich_Models <- get.models(Fun_Rich_Dredge, subset = delta < 2 & !(row.names(Fun_Rich_Dredge) %in% bad_model_indices))

length(names(Fun_Rich_Models))
names(Fun_Rich_Models)

#Check if the null is within 2 AICc 
Fun_Rich_Null <- glmmTMB(FRic ~ 1 + (1 | Property), family = nbinom2, data = ComVar,na.action = "na.fail",control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
Fun_RichList <- list("null" = Fun_Rich_Null,"Top"=Fun_Rich_Models[1]$`8`)
aictab(Fun_RichList)
#Top is better

#need to check if top has warnings or no?
Fun_Rich_Top <- glmmTMB(FRic ~ GC + GGC + Height + Position + ResDay + (1 | Property), family = nbinom2, data = ComVar,na.action = "na.fail",control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS"))) #no warnings that's good can continue


#Predictions 

summary(Fun_Rich_Top)


Site_Fun_Rich <- expand.grid(GC = Predictions_GC, GGC = Predictions_GGC, Height = Predictions_Height, Position = c("Escarpment","Valley"),ResDay = Predictions_Day)
head(Site_Fun_Rich);dim(Site_Fun_Rich)

Site_Fun_Rich1 <- predict(object = Fun_Rich_Top,newdata= Site_Fun_Rich,se.fit = T, type = "link",re.form = NA)

Site_Fun_Rich2<-data.frame(Site_Fun_Rich,fit.link=Site_Fun_Rich1$fit,se.link=Site_Fun_Rich1$se.fit)

Site_Fun_Rich2$lci.link<-Site_Fun_Rich2$fit.link-(1.96*Site_Fun_Rich2$se.link)
Site_Fun_Rich2$uci.link<-Site_Fun_Rich2$fit.link+(1.96*Site_Fun_Rich2$se.link)

Site_Fun_Rich2$fit<-exp(Site_Fun_Rich2$fit.link)
Site_Fun_Rich2$se<-exp(Site_Fun_Rich2$se.link)
Site_Fun_Rich2$lci<-exp(Site_Fun_Rich2$lci.link)
Site_Fun_Rich2$uci<-exp(Site_Fun_Rich2$uci.link)

head(Site_Fun_Rich2);dim(Site_Fun_Rich2)

##Functional Evenness----
head(ComVar)
ComVar_Fun_Sub <- ComVar[-which(is.na(ComVar$FEve)),]


Fun_Eve_Full <- glmer(FEve ~ ResDay + (Position + Height + GC + GGC + WeedScale)^2 + (1 | Property), family = Gamma(link = "log"), data = ComVar_Fun_Sub,na.action = "na.fail")
summary(Fun_Eve_Full)

MuMIn::getAllTerms(Fun_Eve_Full) #Not wrapped

Fun_Eve_Dredge <- dredge(Fun_Eve_Full, fixed = c("ResDay","Position"),m.lim = c(NA, 5),trace = TRUE)

attr(Fun_Eve_Dredge, "model.calls")
write.xlsx(Fun_Eve_Dredge, 'TEMPDOC.xlsx')

Fun_Eve_Dredge[[1]]

Fun_Eve_Models <- get.models(Fun_Eve_Dredge, subset = delta < 2)

length(names(Fun_Eve_Models))
names(Fun_Eve_Models)

#Check if the null is within 2 AICc 
Fun_Eve_Null <- glmer(FEve ~ 1 + (1 | Property), family = Gamma(link = "log"), data = ComVar_Fun_Sub,na.action = "na.fail")
Fun_EveList <- list("null" = Fun_Eve_Null,"Top"=Fun_Eve_Models[1]$`1`)
aictab(Fun_EveList)
#Null is better so don't continue with model
Fun_Eve_Models[1]$`1`

##Functional Dispersion----

Dis_Full <- glmmTMB(FDis ~ ResDay + (Position + Height + GC + GGC + WeedScale)^2 + (1 | Property), family = gaussian(), data = ComVar_Fun_Sub,na.action = "na.fail")
summary(Dis_Full)

MuMIn::getAllTerms(Dis_Full) #they are wrapped

Dis_Dredge <- dredge(Dis_Full, fixed = c("cond(ResDay)","cond(Position)"),m.lim = c(NA, 5),trace = TRUE)

Dis_Dredge[[1]]

attr(Dis_Dredge, "model.calls")
write.xlsx(Dis_Dredge, 'TEMPDOC.xlsx')

Dis_Models <- get.models(Dis_Dredge, subset = delta < 2)

length(names(Dis_Models))
names(Dis_Models)

#Check if the null is within 2 AICc 
Dis_Null <- glmmTMB(FDis ~ 1 + (1 | Property), family = gaussian(), data = ComVar_Fun_Sub,na.action = "na.fail")
DisList <- list("null" = Dis_Null,"Top"=Dis_Models[1]$`1`)
aictab(DisList)
#Top is better than null
#however only has fixed effects? 
#Present in supporting as it doesn't answer question abound site
Dis_Models[1]$`1`

DisList_All <- list("P+D" = Dis_Models[1]$`1`,
                    "GC+P+D" = Dis_Models[2]$`2`, 
                    "Weed+Dom+P+D" = Dis_Models[3]$`9`,
                    "null" = Dis_Null)

aictab(DisList_All)

#Top model:
Dis_Models[1]$`1`

#Predictions

Dis_Top <- glmmTMB(FDis  ~ Position + ResDay + (1 | Property), family = gaussian(), data = ComVar_Fun_Sub,na.action = "na.fail")
summary(Dis_Top)

Predictions_FunResday <- seq(min(ComVar_Fun_Sub$ResDay),max(ComVar_Fun_Sub$ResDay),length.out=20)

Site_Dis<- expand.grid(Position = c("Escarpment","Valley"), ResDay = Predictions_FunResday)
head(Site_Dis);dim(Site_Dis)

Site_Dis1 <- predict(object = Dis_Top,newdata= Site_Dis,se.fit = T, type = "link",re.form = NA)

Site_Dis2<-data.frame(Site_Dis,fit.link=Site_Dis1$fit,se.link=Site_Dis1$se.fit)

Site_Dis2$lci.link<-Site_Dis2$fit.link-(1.96*Site_Dis2$se.link)
Site_Dis2$uci.link<-Site_Dis2$fit.link+(1.96*Site_Dis2$se.link)

head(Site_Dis2);dim(Site_Dis2)

#Q2 LANDSCAPE----

##Species Richness---- 
head(ComVar)


Rich_Full_2 <- glmmTMB(Species_Rich ~ ResDay + (Position + HabDiv + Graze + X500m.Dominant.Landscape.Class)^2 + (1 | Property), family = nbinom2, data = ComVar,na.action = "na.fail")
summary(Rich_Full_2)

MuMIn::getAllTerms(Rich_Full_2) #Wrapped

Rich_Dredge_2 <- dredge(Rich_Full_2, fixed = c("cond(ResDay)","cond(Position)"),m.lim = c(NA, 5),trace = TRUE)

Rich_Dredge_2[[1]]

attr(Rich_Dredge_2, "model.calls")
write.xlsx(Rich_Dredge_2, 'TEMPDOC.xlsx')

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

attr(Div_Dredge_2, "model.calls")
write.xlsx(Div_Dredge_2, 'TEMPDOC.xlsx')

Div_Dredge_2[[1]]

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

Abun_Dredge_2[[1]]

attr(Abun_Dredge_2, "model.calls")
write.xlsx(Abun_Dredge_2, 'TEMPDOC.xlsx')

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

##Community Composition----

Comp_Full_2 <- glmmTMB(ComComp ~ ResDay + (Position + HabDiv + Graze + X500m.Dominant.Landscape.Class)^2 + (1 | Property), family = gaussian(), data = ComVar_Subsected,na.action = "na.fail")
summary(Comp_Full_2)

MuMIn::getAllTerms(Comp_Full_2) #wrapped

Comp_Dredge_2 <- dredge(Comp_Full_2, fixed = c("cond(ResDay)","cond(Position)"),m.lim = c(NA, 5),trace = TRUE)

Comp_Dredge_2[[1]]

attr(Comp_Dredge_2, "model.calls")
write.xlsx(Comp_Dredge_2, 'TEMPDOC.xlsx')

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
Predictions_CompGraze <-seq(min(ComVar_Subsected$Graze),max(ComVar_Subsected$Graze),length.out=20)
Predictions_CompDomHab <- unique(ComVar_Subsected$X500m.Dominant.Landscape.Class)

Comp_Top_2 <- glmmTMB(ComComp ~ Graze * Position + X500m.Dominant.Landscape.Class + ResDay + (1 | Property), family = gaussian(), data = ComVar_Subsected,na.action = "na.fail")
summary(Comp_Top)

Land_Comp<- expand.grid(Graze = Predictions_CompGraze, Position = c("Escarpment","Valley"), ResDay = Predictions_CompDay, X500m.Dominant.Landscape.Class = Predictions_CompDomHab)
head(Land_Comp);dim(Land_Comp)

Land_Comp1 <- predict(object = Comp_Top_2,newdata= Land_Comp,se.fit = T, type = "link",re.form = NA)

Land_Comp2<-data.frame(Land_Comp,fit.link=Land_Comp1$fit,se.link=Land_Comp1$se.fit)

Land_Comp2$lci.link<-Land_Comp2$fit.link-(1.96*Land_Comp2$se.link)
Land_Comp2$uci.link<-Land_Comp2$fit.link+(1.96*Land_Comp2$se.link)

head(Land_Comp2);dim(Land_Comp2)

##Functional Richness---- 
head(ComVar)

Fun_Rich_Full_2 <- glmmTMB(FRic ~ ResDay + (Position + HabDiv + Graze + X500m.Dominant.Landscape.Class)^2 + (1 | Property), family = nbinom2, data = ComVar,na.action = "na.fail")
summary(Fun_Rich_Full_2)

MuMIn::getAllTerms(Fun_Rich_Full_2) #Wrapped

Fun_Rich_Dredge_2 <- dredge(Fun_Rich_Full_2, fixed = c("cond(ResDay)","cond(Position)"),m.lim = c(NA, 5),trace = TRUE)

Fun_Rich_Dredge_2[[1]]

Fun_Rich_Dredge_2_No_Error <- Fun_Rich_Dredge_2[1:12, ]

attr(Fun_Rich_Dredge_2_No_Error, "model.calls")
write.xlsx(Fun_Rich_Dredge_2_No_Error, 'TEMPDOC.xlsx')

Fun_Rich_Models_2 <- get.models(Fun_Rich_Dredge_2, subset = delta < 2)

length(names(Fun_Rich_Models_2))
names(Fun_Rich_Models_2)
#there's 20 models but 11 convergence warnings
#check null first then if better then null check all models and remove convergence issue models

#Check if the null is within 2 AICc 
Fun_RichList_2 <- list("null" = Fun_Rich_Null,"Top"=Fun_Rich_Models_2[1]$`1`)
aictab(Fun_RichList_2)
#Null is within 2 AICc don't continue

##Functional Evenness----

Fun_Eve_Full_2 <- glmer(FEve ~ ResDay + (Position + HabDiv + Graze + X500m.Dominant.Landscape.Class)^2 + (1 | Property), family = Gamma(link = "log"), data = ComVar_Fun_Sub,na.action = "na.fail")
summary(Fun_Eve_Full_2)

MuMIn::getAllTerms(Fun_Eve_Full_2) #Not wrapped

Fun_Eve_Dredge_2 <- dredge(Fun_Eve_Full_2, fixed = c("ResDay","Position"),m.lim = c(NA, 5),trace = TRUE)

Fun_Eve_Dredge_2[[1]]

attr(Fun_Eve_Dredge_2, "model.calls")
write.xlsx(Fun_Eve_Dredge_2, 'TEMPDOC.xlsx')

Fun_Eve_Models_2 <- get.models(Fun_Eve_Dredge_2, subset = delta < 2)

length(names(Fun_Eve_Models_2))
names(Fun_Eve_Models_2)

#Check if the null is within 2 AICc 
Fun_EveList_2 <- list("null" = Fun_Eve_Null,"Top"=Fun_Eve_Models_2[1]$`1`)
aictab(Fun_EveList_2)
#Null is better so don't continue with model

##Functional Dispersion----

Dis_Full2 <- glmmTMB(FDis ~ ResDay + (Position + HabDiv + Graze + X500m.Dominant.Landscape.Class)^2 + (1 | Property), family = gaussian(), data = ComVar_Fun_Sub,na.action = "na.fail")
summary(Dis_Full)

MuMIn::getAllTerms(Dis_Full2) #they are wrapped

Dis_Dredge2 <- dredge(Dis_Full2, fixed = c("cond(ResDay)","cond(Position)"),m.lim = c(NA, 5),trace = TRUE)

Dis_Dredge2[[1]]

attr(Dis_Dredge2, "model.calls")
write.xlsx(Dis_Dredge2, 'TEMPDOC.xlsx')

Dis_Models2 <- get.models(Dis_Dredge2, subset = delta < 2)

length(names(Dis_Models2))
names(Dis_Models2)

#Check if the null is within 2 AICc 
DisList2 <- list("null" = Dis_Null,"Top"=Dis_Models2[1]$`1`)
aictab(DisList2)
#Top is better but it's only the fixed effects
#Same model as site dis model


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

##Functional Richness----

summary(Fun_Rich_Top)

r2_Comp_full <- r.squaredGLMM(Fun_Rich_Top)[1, "R2m"]

FRic_site_vars <- c("GC","GGC","Height")
FRic_design_vars <- c("Position","ResDay")

FRic_site_r2 <- part_r2(Fun_Rich_Top, FRic_site_vars)
FRic_design_r2 <- part_r2(Fun_Rich_Top, FRic_design_vars)

FRic_site_r2
FRic_design_r2

#SEM WITH MANAGEMENT----

#Model for each relationship in concept diagram
head(ComVar);dim(ComVar)
head(property);dim(property)
ComVar$Height <- as.numeric(ComVar$Height)
ComVar$GC <- as.numeric(ComVar$GC)
ComVar$GGC <- as.numeric(ComVar$GGC)
ComVar$Graze <- as.numeric(ComVar$Graze)
ComVar$HabDiv <- as.numeric(ComVar$HabDiv)



mod_Rich <- glmmTMB(Species_Rich ~ management + Height + GC + GGC + Graze + HabDiv + (1 | Property), family = nbinom2, data = ComVar)

mod_H <- glmmTMB(Height ~ management + (1 | Property), family = gaussian, data = ComVar)

mod_GC <- glmmTMB(GC ~ management + (1 | Property), family = gaussian, data = ComVar)

mod_GGC <- glmmTMB(GGC ~ management + (1 | Property), family = gaussian, data = ComVar)

psem_Rich_mods <- psem(mod_Rich,mod_H,mod_GC,mod_GGC, data=ComVar)
summary(psem_Rich_mods)




mod_Abun <- glmmTMB(Count ~ management + Height + GC + GGC + Graze + HabDiv + (1 | Property), family = nbinom2, data = ComVar)

psem_Abun_mods <- psem(mod_Abun,mod_H,mod_GC,mod_GGC, data=ComVar)
summary(psem_Abun_mods)



ComVar_Subsected[,23:27] <- sapply(ComVar_Subsected[,23:27],as.numeric)
class(ComVar_Subsected$Height)

mod_H2 <- glmmTMB(Height ~ management + (1 | Property), family = gaussian, data = ComVar_Subsected)

mod_GC2 <- glmmTMB(GC ~ management + (1 | Property), family = gaussian, data = ComVar_Subsected)

mod_GGC2 <- glmmTMB(GGC ~ management + (1 | Property), family = gaussian, data = ComVar_Subsected)

psem_Comp_mods <- psem(mod_Comp,mod_H2,mod_GC2,mod_GGC2, data=ComVar_Subsected)
summary(psem_Comp_mods)

mod_Comp <- glmmTMB(ComComp ~ management + Height + GC + GGC + Graze + HabDiv + (1 | Property), family = gaussian(), data = ComVar_Subsected)


mod_Div <- glmer(Diversity ~ management + Height + GC + GGC + Graze + HabDiv + (1 | Property), family = Gamma(link = "log"), data = ComVar_Subsected)

psem_Div_mods <- psem(mod_Div,mod_H2,mod_GC2,mod_GGC2, data=ComVar_Subsected)
summary(psem_Div_mods)


#Functional 

mod_FRic <- glmmTMB(FRic ~ management + Height + GC + GGC + Graze + HabDiv +(1 | Property), family = nbinom2, data = ComVar)
mod_FEve <- glmer(FEve ~ management + Height + GC + GGC + Graze + HabDiv+ (1 | Property), family = Gamma(link = "log"), data = ComVar_Fun_Sub)
mod_FDis <- glmmTMB(FDis ~ management + Height + GC + GGC + Graze + HabDiv+ (1 | Property), family = gaussian(), data = ComVar_Fun_Sub)

psem_FRic_mods <- psem(mod_FRic,mod_H,mod_GC,mod_GGC, data=ComVar)
summary(psem_FRic_mods)
#Error - can't compute

mod_H3 <- glmmTMB(Height ~ management + (1 | Property), family = gaussian, data = ComVar_Fun_Sub)

mod_GC3 <- glmmTMB(GC ~ management + (1 | Property), family = gaussian, data = ComVar_Fun_Sub)

mod_GGC3 <- glmmTMB(GGC ~ management + (1 | Property), family = gaussian, data = ComVar_Fun_Sub)

psem_FEve_mods <- psem(mod_FEve,mod_H3,mod_GC3,mod_GGC3, data=ComVar_Fun_Sub)
summary(psem_FRic_mods)
#Error - can't compute

psem_FDis_mods <- psem(mod_FDis,mod_H3,mod_GC3,mod_GGC3, data=ComVar_Fun_Sub)
summary(psem_FRic_mods)
#Error - can't compute

#Figures----

##Species Richness----
head(Site_Rich2);dim(Site_Rich2)
summary(Rich_Top)

head(Land_Rich2);dim(Land_Rich2)
summary(Rich_Top_2)

AA <- Site_Rich2$ResDay == Predictions_Day & Site_Rich2$Position == "Escarpment"
A_A <- Site_Rich2$ResDay == Predictions_Day & Site_Rich2$Position == "Valley"
AAA <- Site_Rich2$GC == Predictions_GC[10] & Site_Rich2$Position == "Escarpment"

#par(mfg = c(2, 1, 2, 2))
#to skip panel and this above tells R where to put the next graph c(row, column, total row, total column)

BB <-Land_Rich2$Position == "Escarpment" & Land_Rich2$ResDay == Predictions_Day[10]
B_B <- Land_Rich2$HabDiv == Predictions_Hab_Div[10] & Land_Rich2$ResDay == Predictions_Day[10]
BBB <- Land_Rich2$HabDiv == Predictions_Hab_Div[10] & Land_Rich2$Position == "Escarpment"

raw_x1 <- ifelse(ComVar$Position ==
                   "Escarpment", 1, 
                 ifelse(ComVar$Position ==
                          "Valley", 2, NA))


dev.new(height=10,width=15,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,3,2),mfrow=c(2,3),mgp=c(2.5,1,0),xpd = T)

plot(x = ComVar$GC,y = ComVar$Species_Rich,xlab = "Ground Cover (%)",ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.lab=1.4,cex.axis=1.4,xaxt = 'n')
axis(side=1, at=seq(from=min(Site_Rich2$GC),to=max(Site_Rich2$GC),length.out=5),labels=round(seq(from=min(ComVar$Ground_Cover),to=max(ComVar$Ground_Cover),length.out=5),0),cex.axis=1.4)
mtext(side=3,line=0,at = -3.2,'a)',cex=0.8)

polygon(x = c(Site_Rich2$GC[AA],rev(Site_Rich2$GC[AA])), y = c(Site_Rich2$lci[AA],rev(Site_Rich2$uci[AA])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Rich2$GC[AA],y = Site_Rich2$fit[AA],lwd = 2,col = 'grey30')

polygon(x = c(Site_Rich2$GC[A_A],rev(Site_Rich2$GC[A_A])), y = c(Site_Rich2$lci[A_A],rev(Site_Rich2$uci[A_A])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Rich2$GC[A_A],y = Site_Rich2$fit[A_A],lwd = 2,col = 'grey30',lty=2)

legend("topleft",legend = c("Escaprment", "Valley"), lty = c(1,2), col = 'grey30',pt.cex = 1,cex = 1.4)


plot(x = ComVar$ResDay,y = ComVar$Species_Rich,xlab = "Day (position-adjusted)",ylab='Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd =2,cex.lab=1.4,cex.axis=1.4,xaxt = 'n')
axis(side=1, at=seq(from=min(Site_Rich2$ResDay),to=max(Site_Rich2$ResDay),length.out=5),labels=round(seq(from=min(ComVar$ResDay),to=max(ComVar$ResDay),length.out=5),0),cex.axis=1.4)
mtext(side=3,line=0,at = -15,'b)',cex=0.8)

polygon(x = c(Site_Rich2$ResDay[AAA],rev(Site_Rich2$ResDay[AAA])), y = c(Site_Rich2$lci[AAA],rev(Site_Rich2$uci[AAA])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Rich2$ResDay[AAA],y = Site_Rich2$fit[AAA],lwd = 2,col = 'grey30')

mtext(side=3,line=1,at = -25,'-------------------Site-------------------',cex=0.9, font = 2)


mtext(bquote(R^2 == 0.061), side=1,line=-10,at = 50,cex=1.1)
mtext("Site", side=1,line=-10,at = 40,cex=1.1)
mtext(bquote(R^2 == 0.061), side=1,line=-8,at = 50,cex=1.1)
mtext("Landscape", side=1,line=-8,at = 35,cex=1.1)
mtext(bquote(R^2 == 0.224), side=1,line=-6,at = 50,cex=1.1)
mtext("Fixed", side=1,line=-6,at = 39,cex=1.1)


par(mfg = c(2, 1, 2, 3))
plot(x = ComVar$HabDiv,y = ComVar$Species_Rich,xlab = "Habitat Diveristy within 500m",ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.lab=1.4,cex.axis=1.4,xaxt = 'n')
axis(side=1, at=seq(from=min(Land_Rich2$HabDiv),to=max(Land_Rich2$HabDiv),length.out=5),labels=round(seq(from=min(ComVar$X500m.Simspson),to=max(ComVar$X500m.Simspson),length.out=5),1),cex.axis=1.4)
mtext(side=3,line=0,at = -3.1,'c)',cex=0.8)

polygon(x = c(Land_Rich2$HabDiv[BB],rev(Land_Rich2$HabDiv[BB])), y = c(Land_Rich2$lci[BB],rev(Land_Rich2$uci[BB])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Land_Rich2$HabDiv[BB],y = Land_Rich2$fit[BB],lwd = 2,col = 'grey30')


plot(x = 1:2,y = Land_Rich2$fit[B_B],xlab = " ",ylab = 'Species Richness', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,20),cex.lab =1.4,cex.axis = 1.4)
axis(side=1,at=c(0.8,2.2),labels=c('Escarpment','Valley'),cex.axis=1.4)
mtext(side=3,line=0,at = -0.25,'d)',cex=0.8)
arrows(x0=1:2, y0=Land_Rich2$lci [B_B],x1=1:2, y1=Land_Rich2$uci[B_B],angle=90,length=0.1, code=3, lwd=2,col = "black")
points(x = jitter(raw_x1, factor = 1),y = ComVar$Species_Rich, pch = 16, cex = 0.4, col = "black")

mtext(side=3,line=1,at = 1.5,'--------------------------------Landscape--------------------------------',cex=0.9, font = 2)


plot(x = ComVar$ResDay,y = ComVar$Species_Rich,xlab = "Day (position-adjusted)",ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd= 2,cex.lab=1.4,cex.axis=1.4,xaxt = 'n')
axis(side=1, at=seq(from=min(Land_Rich2$ResDay),to=max(Land_Rich2$ResDay),length.out=5),labels=round(seq(from=min(ComVar$ResDay),to=max(ComVar$ResDay),length.out=5),0),cex.axis=1.4)
mtext(side=3,line=0,at = -15,'e)',cex=0.8)

polygon(x = c(Land_Rich2$ResDay[BBB],rev(Land_Rich2$ResDay[BBB])), y = c(Land_Rich2$lci[BBB],rev(Land_Rich2$uci[BBB])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Land_Rich2$ResDay[BBB],y = Land_Rich2$fit[BBB],lwd = 2,col = 'grey30')



##Abundance----
head(Site_Abun2);dim(Site_Abun2)
summary(Abun_Top)

head(Land_Abun2);dim(Land_Abun2)
summary(Abun_Top_2)


CC <- Site_Abun2$GGC == Predictions_GGC[10] & Site_Abun2$Height == Predictions_Height[10] & Site_Abun2$Position == "Escarpment" & Site_Abun2$ResDay == Predictions_Day[10]
C_C <- Site_Abun2$GC == Predictions_GC[10] & Site_Abun2$Height == Predictions_Height[10] & Site_Abun2$Position == "Escarpment" & Site_Abun2$ResDay == Predictions_Day[10]
CCC <- Site_Abun2$GGC == Predictions_GGC[10] & Site_Abun2$Position == "Escarpment" & Site_Abun2$GC == Predictions_GC[10] & Site_Abun2$ResDay == Predictions_Day[10]
C_C_C <- Site_Abun2$GGC == Predictions_GGC[10] & Site_Abun2$Height == Predictions_Height[10] & Site_Abun2$GC == Predictions_GC[10] & Site_Abun2$ResDay == Predictions_Day[10]
CCCC <- Site_Abun2$GGC == Predictions_GGC[10] & Site_Abun2$Height == Predictions_Height[10] & Site_Abun2$GC == Predictions_GC[10] &  Site_Abun2$Position == "Escarpment"


DD <- Land_Abun2$Position == "Escarpment" & Land_Abun2$ResDay == Predictions_Day[10]
D_D <- Land_Abun2$HabDiv == Predictions_Hab_Div[10] & Land_Abun2$ResDay == Predictions_Day[10]
DDD <- Land_Abun2$Position == "Escarpment" & Land_Abun2$HabDiv == Predictions_Hab_Div[10]



dev.new(height=15,width=15,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,3,2),mfrow=c(3,3),mgp=c(2.5,1,0),xpd = T)

plot(x = ComVar$GC,y = ComVar$Count,xlab = "Ground Cover (%)",ylab = 'Abundance', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.lab=1.2,cex.axis=1.2,xaxt = 'n')
axis(side=1, at=seq(from=min(Site_Abun2$GC),to=max(Site_Abun2$GC),length.out=4),labels=round(seq(from=min(ComVar$Ground_Cover),to=max(ComVar$Ground_Cover),length.out=4),0),cex.axis=1.2)
mtext(side=3,line=0,at = -3.3,'a)',cex=0.8)

polygon(x = c(Site_Abun2$GC[CC],rev(Site_Abun2$GC[CC])), y = c(Site_Abun2$lci[CC],rev(Site_Abun2$uci[CC])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Abun2$GC[CC],y = Site_Abun2$fit[CC],lwd = 2,col = 'grey30')


plot(x = ComVar$GGC,y = ComVar$Count,xlab = "Green Ground Cover (%)",ylab = 'Abundance', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.lab=1.2,cex.axis=1.2,xaxt = 'n')
axis(side=1, at=seq(from=min(Site_Abun2$GGC),to=max(Site_Abun2$GGC),length.out=4),labels=round(seq(from=min(ComVar$Prop_Green_GC),to=max(ComVar$Prop_Green_GC),length.out=4),0),cex.axis=1.2)
mtext(side=3,line=0,at = -2.1,'b)',cex=0.8)

polygon(x = c(Site_Abun2$GGC[C_C],rev(Site_Abun2$GGC[C_C])), y = c(Site_Abun2$lci[C_C],rev(Site_Abun2$uci[C_C])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Abun2$GGC[C_C],y = Site_Abun2$fit[C_C],lwd = 2,col = 'grey30')

plot(x = ComVar$Height,y = ComVar$Count,xlab = "Grass Height (cm)",ylab = 'Abundance', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.lab=1.2,cex.axis=1.2,xaxt = 'n')
axis(side=1, at=seq(from=min(Site_Abun2$Height),to=max(Site_Abun2$Height),length.out=4),labels=round(seq(from=min(ComVar$Plant_Height),to=max(ComVar$Plant_Height),length.out=4),0),cex.axis=1.2)
mtext(side=3,line=0,at = -1.6,'c)',cex=0.8)

polygon(x = c(Site_Abun2$Height[CCC],rev(Site_Abun2$Height[CCC])), y = c(Site_Abun2$lci[CCC],rev(Site_Abun2$uci[CCC])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Abun2$Height[CCC],y = Site_Abun2$fit[CCC],lwd = 2,col = 'grey30')

mtext(side=3,line=1,at = -7,'------------------------Site------------------------',cex=0.8, font = 2)

plot(x = 1:2,y = Site_Abun2$fit[C_C_C],xlab = " ",ylab = 'Abundance', type = 'p',pch = 16,cex =1.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,40),cex.lab =1.2,cex.axis = 1.2)
axis(side=1,at=c(0.7,2.3),labels=c('Escarpment','Valley'),cex.axis=0.9)
mtext(side=3,line=0,at = -0.25,'d)',cex=0.8)
arrows(x0=1:2, y0=Site_Abun2$lci [C_C_C],x1=1:2, y1=Site_Abun2$uci[C_C_C],angle=90,length=0.05, code=3, lwd=2,col = "black")
points(x = jitter(raw_x1, factor = 1),y = ComVar$Count, pch = 16, cex = 0.4, col = "grey30")

mtext(side=3,line=1,at = 4,'----------Site----------',cex=0.8, font = 2)


plot(x = ComVar$ResDay,y = ComVar$Count,xlab = "Day (position-adjusted)",ylab = 'Abundance', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.lab=1.2,cex.axis=1.2,xaxt = 'n')
axis(side=1, at=seq(from=min(Site_Abun2$ResDay),to=max(Site_Abun2$ResDay),length.out=4),labels=round(seq(from=min(ComVar$ResDay),to=max(ComVar$ResDay),length.out=4),0),cex.axis=1.2)
mtext(side=3,line=0,at = -15,'e)',cex=0.8)

polygon(x = c(Site_Abun2$ResDay[CCCC],rev(Site_Abun2$ResDay[CCCC])), y = c(Site_Abun2$lci[CCCC],rev(Site_Abun2$uci[CCCC])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Abun2$ResDay[CCCC],y = Site_Abun2$fit[CCCC],lwd = 2,col = 'grey30')

mtext(bquote(R^2 == 0.131), side=1,line=-6,at = 60,cex=0.8)
mtext("Site", side=1,line=-6,at = 46,cex=0.8)
mtext(bquote(R^2 == 0.094), side=1,line=-4,at = 60,cex=0.8)
mtext("Landscape", side=1,line=-4,at = 40,cex=0.8)
mtext(bquote(R^2 == 0.183), side=1,line=-2,at = 60,cex=0.8)
mtext("Fixed", side=1,line=-2,at = 45,cex=0.8)


par(mfg = c(3, 1, 3, 3))
plot(x = ComVar$HabDiv,y = ComVar$Count,xlab = "Habitat Diveristy in 500m",ylab = 'Abundance', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.lab=1.2,cex.axis=1.2,xaxt = 'n')
axis(side=1, at=seq(from=min(Land_Abun2$HabDiv),to=max(Land_Abun2$HabDiv),length.out=4),labels=round(seq(from=min(ComVar$X500m.Simspson),to=max(ComVar$X500m.Simspson),length.out=4),1),cex.axis=1.2)
mtext(side=3,line=0,at = -3.1,'f)',cex=0.8)

polygon(x = c(Land_Abun2$HabDiv[DD],rev(Land_Abun2$HabDiv[DD])), y = c(Land_Abun2$lci[DD],rev(Land_Abun2$uci[DD])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Land_Abun2$HabDiv[DD],y = Land_Abun2$fit[DD],lwd = 2,col = 'grey30')


plot(x = 1:2,y = Land_Abun2$fit[D_D],xlab = " ",ylab = 'Abundance', type = 'p',pch = 16,cex =1.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,40),cex.lab =1.2,cex.axis = 1.2)
axis(side=1,at=c(0.7,2.3),labels=c('Escarpment','Valley'),cex.axis=0.9)
mtext(side=3,line=0,at = -0.25,'g)',cex=0.8)
arrows(x0=1:2, y0=Land_Abun2$lci [D_D],x1=1:2, y1=Land_Abun2$uci[D_D],angle=90,length=0.05, code=3, lwd=2,col = "black")
points(x = jitter(raw_x1, factor = 1),y = ComVar$Count, pch = 16, cex = 0.4, col = "grey30")

mtext(side=3,line=1,at = 1.5,'----------------------Landscape----------------------',cex=0.8, font = 2)


plot(x = ComVar$ResDay,y = ComVar$Count,xlab = "Day (position-adjusted)",ylab = 'Abundance', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.lab=1.2,cex.axis=1.2,xaxt = 'n')
axis(side=1, at=seq(from=min(Land_Abun2$ResDay),to=max(Land_Abun2$ResDay),length.out=4),labels=round(seq(from=min(ComVar$ResDay),to=max(ComVar$ResDay),length.out=4),0),cex.axis=1.2)
mtext(side=3,line=0,at = -15,'h)',cex=0.8)

polygon(x = c(Land_Abun2$ResDay[DDD],rev(Land_Abun2$ResDay[DDD])), y = c(Land_Abun2$lci[DDD],rev(Land_Abun2$uci[DDD])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Land_Abun2$ResDay[DDD],y = Land_Abun2$fit[DDD],lwd = 2,col = 'grey30')


##Composition----
head(ComVar_Subsected);dim(ComVar_Subsected)

head(Site_Comp2);dim(Site_Comp2)
summary(Comp_Top)

head(Land_Comp2);dim(Land_Comp2)
summary(Comp_Top_2)


EE <- Site_Comp2$GC == Predictions_CompGC[10] & Site_Comp2$Position == "Escarpment" & Site_Comp2$ResDay == Predictions_CompDay[10]
E_E <- Site_Comp2$GGC == Predictions_CompGGC[10] & Site_Comp2$Position == "Escarpment" & Site_Comp2$ResDay == Predictions_CompDay[10]
EEE <- Site_Comp2$GGC == Predictions_CompGGC[10] & Site_Comp2$Position == "Valley" & Site_Comp2$ResDay == Predictions_CompDay[10]
E_E_E <- Site_Comp2$GGC == Predictions_CompGGC[10] & Site_Comp2$Position == "Escarpment" & Site_Comp2$GC == Predictions_CompGC[10]

FF <- Land_Comp2$X500m.Dominant.Landscape.Class == "NTV_Woody_Closed" & Land_Comp2$Position == "Escarpment" & Land_Comp2$ResDay == Predictions_CompDay[10]
F_F <- Land_Comp2$X500m.Dominant.Landscape.Class == "NTV_Woody_Closed" & Land_Comp2$ Position == "Valley" & Land_Comp2$ResDay == Predictions_CompDay[10]
FFF <- Land_Comp2$Graze == Predictions_CompGraze[10] & Land_Comp2$Position == "Escarpment" & Land_Comp2$ResDay == Predictions_CompDay[10]
F_F_F <- Land_Comp2$Graze == Predictions_CompGraze[10] & Land_Comp2$Position == "Escarpment" & Land_Comp2$X500m.Dominant.Landscape.Class == "NTV_Woody_Closed"

raw_x2 <- ifelse(ComVar_Subsected$X500m.Dominant.Landscape.Class ==
                   "NTV_Woody_Closed", 1, 
                 ifelse(ComVar_Subsected$X500m.Dominant.Landscape.Class ==
                          "NTV_Herbaceous_Open", 2, 3))

dev.new(height=15,width=30,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,3,2),mfrow=c(2,4),mgp=c(2.5,0.7,0),xpd = T)

plot(x = ComVar_Subsected$GGC,y = ComVar_Subsected$ComComp,xlab = "Green Ground Cover (%)",ylab = 'Community Dissimiliaty Index', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.lab=1.2,cex.axis=1.2,xaxt = 'n')
axis(side=1, at=seq(from=min(Site_Comp2$GGC),to=max(Site_Comp2$GGC),length.out=5),labels=round(seq(from=min(ComVar_Subsected$Prop_Green_GC),to=max(ComVar_Subsected$Prop_Green_GC),length.out=5),0),cex.axis=1.2)
mtext(side=3,line=0,at = -2.1,'a)',cex=0.8)

polygon(x = c(Site_Comp2$GGC[EE],rev(Site_Comp2$GGC[EE])), y = c(Site_Comp2$lci[EE],rev(Site_Comp2$uci[EE])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Comp2$GGC[EE],y = Site_Comp2$fit[EE],lwd = 2,col = 'grey30')

plot(x = ComVar_Subsected$GC,y = ComVar_Subsected$ComComp,xlab = "Ground Cover (%)",ylab = 'Community Dissimiliaty Index', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.lab=1.2,cex.axis=1.2,xaxt = 'n')
axis(side=1, at=seq(from=min(Site_Comp2$GC),to=max(Site_Comp2$GC),length.out=5),labels=round(seq(from=min(ComVar_Subsected$Ground_Cover),to=max(ComVar_Subsected$Ground_Cover),length.out=5),0),cex.axis=1.2)
mtext(side=3,line=0,at = -3.2,'b)',cex=0.8)

polygon(x = c(Site_Comp2$GC[E_E],rev(Site_Comp2$GC[E_E])), y = c(Site_Comp2$lci[E_E],rev(Site_Comp2$uci[E_E])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Comp2$GC[E_E],y = Site_Comp2$fit[E_E],lwd = 2,col = 'grey30')

polygon(x = c(Site_Comp2$GC[EEE],rev(Site_Comp2$GC[EEE])), y = c(Site_Comp2$lci[EEE],rev(Site_Comp2$uci[EEE])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Comp2$GC[EEE],y = Site_Comp2$fit[EEE],lwd = 2,col = 'grey30',lty=2)

legend("bottomleft",legend = c("Escaprment", "Valley"), lty = c(1,2), col = 'grey30',pt.cex = 1,cex = 1.2)



plot(x = ComVar_Subsected$ResDay,y = ComVar_Subsected$ComComp,xlab = "Day (position-adjusted) ",ylab = 'Community Dissimiliaty Index', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.lab=1.2,cex.axis=1.2,xaxt = 'n')
axis(side=1, at=seq(from=min(Site_Comp2$ResDay),to=max(Site_Comp2$ResDay),length.out=5),labels=round(seq(from=min(ComVar_Subsected$ResDay),to=max(ComVar_Subsected$ResDay),length.out=5),0),cex.axis=1.2)
mtext(side=3,line=0,at = -15.5,'c)',cex=0.8)

polygon(x = c(Site_Comp2$ResDay[E_E_E],rev(Site_Comp2$ResDay[E_E_E])), y = c(Site_Comp2$lci[E_E_E],rev(Site_Comp2$uci[E_E_E])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Comp2$ResDay[E_E_E],y = Site_Comp2$fit[E_E_E],lwd = 2,col = 'grey30')


mtext(side=3,line=1,at = -45,'-----------------------Site-----------------------',cex=0.9, font = 2)

mtext(bquote(R^2 == 0.019), side=1,line=-8,at = 40,cex=0.9)
mtext("Site", side=1,line=-8,at = 30,cex=0.9)
mtext(bquote(R^2 == 0.201), side=1,line=-6,at = 40,cex=0.9)
mtext("Landscape", side=1,line=-6,at = 25,cex=0.9)
mtext(bquote(R^2 == 0.226), side=1,line=-4,at = 40,cex=0.9)
mtext("Fixed", side=1,line=-4,at = 29,cex=0.9)

par(mfg = c(2, 1, 2, 4))
plot(x = ComVar_Subsected$Graze,y = ComVar_Subsected$ComComp,xlab = "Grazing Land in 1km (%)",ylab = 'Community Dissimiliaty Index', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.lab=1.2,cex.axis=1.2,xaxt = 'n')
axis(side=1, at=seq(from=min(Land_Comp2$Graze),to=max(Land_Comp2$Graze),length.out=5),labels=round(seq(from=min(ComVar_Subsected$Natual_Grazing_1km),to=max(ComVar_Subsected$Natual_Grazing_1km),length.out=5),0),cex.axis=1.2)
mtext(side=3,line=0,at = -1.9,'d)',cex=0.8)

polygon(x = c(Land_Comp2$Graze[FF],rev(Land_Comp2$Graze[FF])), y = c(Land_Comp2$lci[FF],rev(Land_Comp2$uci[FF])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Land_Comp2$Graze[FF],y = Land_Comp2$fit[FF],lwd = 2,col = 'grey30')

polygon(x = c(Land_Comp2$Graze[F_F],rev(Land_Comp2$Graze[F_F])), y = c(Land_Comp2$lci[F_F],rev(Land_Comp2$uci[F_F])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Land_Comp2$Graze[F_F],y = Land_Comp2$fit[F_F],lwd = 2,col = 'grey30',lty=2)

legend("bottomleft",legend = c("Escaprment", "Valley"), lty = c(1,2), col = 'grey30',pt.cex = 1,cex = 1.2)


plot(x = 1:3,y = Land_Comp2$fit[FFF],xlab = " ",ylab = 'Community Dissimiliaty Index', type = 'p',pch = 16,cex =1.5,col = 'black', las = 1,xaxt = "n",xlim = c(0,4),ylim = c(-0.7,0.4),cex.lab =1.2,cex.axis = 1.2)
axis(side=1,at=c(1,2,3),labels=c("","",""),cex.axis=0.9)
mtext(side=1,line =1.5, at=0.8,'Woody\nClosed',cex=0.6)
mtext(side=1,line =1.5, at=2,'Herbaceous\nOpen',cex=0.6)
mtext(side=1,line =1.5, at=3.2,'Woody\nOpen',cex=0.6)
mtext(side=1,line =2.5,"Dominant Surrounding Habitat",cex = 0.8)
mtext(side=3,line=0,at = -0.26,'e)',cex=0.8)
arrows(x0=1:3, y0=Land_Comp2$lci[FFF],x1=1:3, y1=Land_Comp2$uci[FFF],angle=90,length=0.05, code=3, lwd=2,col = "black")
points(x = jitter(raw_x2, factor = 1),y = ComVar_Subsected$ComComp, pch = 16, cex = 0.4, col = "grey30")


plot(x = ComVar_Subsected$ResDay,y = ComVar_Subsected$ComComp,xlab = "Day (position-adjusted) ",ylab = 'Community Dissimiliaty Index', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.lab=1.2,cex.axis=1.2,xaxt = 'n')
axis(side=1, at=seq(from=min(Land_Comp2$ResDay),to=max(Land_Comp2$ResDay),length.out=5),labels=round(seq(from=min(ComVar_Subsected$ResDay),to=max(ComVar_Subsected$ResDay),length.out=5),0),cex.axis=1.2)
mtext(side=3,line=0,at = -15.5,'f)',cex=0.8)

polygon(x = c(Land_Comp2$ResDay[F_F_F],rev(Land_Comp2$ResDay[F_F_F])), y = c(Land_Comp2$lci[F_F_F],rev(Land_Comp2$uci[F_F_F])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Land_Comp2$ResDay[F_F_F],y = Land_Comp2$fit[F_F_F],lwd = 2,col = 'grey30')


mtext(side=3,line=1,at = -45,'------------------Landscape------------------',cex=0.9, font = 2)


##Functional Richness----
summary(Fun_Rich_Top)
head(Site_Fun_Rich2)


GG <- Site_Fun_Rich2$GGC == Predictions_GGC[10] & Site_Fun_Rich2$Height == Predictions_Height[10] & Site_Fun_Rich2$Position == "Escarpment" & Site_Fun_Rich2$ResDay == Predictions_Day[10]
G_G <- Site_Fun_Rich2$GC == Predictions_GC[10] & Site_Fun_Rich2$Height == Predictions_Height[10] & Site_Fun_Rich2$Position == "Escarpment" & Site_Fun_Rich2$ResDay == Predictions_Day[10]
GGG <- Site_Fun_Rich2$GGC == Predictions_GGC[10] & Site_Fun_Rich2$Position == "Escarpment" & Site_Fun_Rich2$GC == Predictions_GC[10] & Site_Fun_Rich2$ResDay == Predictions_Day[10]
G_G_G <- Site_Fun_Rich2$GGC == Predictions_GGC[10] & Site_Fun_Rich2$Height == Predictions_Height[10] & Site_Fun_Rich2$GC == Predictions_GC[10] & Site_Fun_Rich2$ResDay == Predictions_Day[10]
GGGG <- Site_Fun_Rich2$GGC == Predictions_GGC[10] & Site_Fun_Rich2$Height == Predictions_Height[10] & Site_Fun_Rich2$GC == Predictions_GC[10] &  Site_Fun_Rich2$Position == "Escarpment"


dev.new(height=10,width=15,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,3,2),mfrow=c(2,3),mgp=c(2.5,1,0),xpd = T)

plot(x = ComVar$GC,y = ComVar$FRic,xlab = "Ground Cover (%)",ylab = 'Functional Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.lab=1.2,cex.axis=1.2,xaxt = 'n')
axis(side=1, at=seq(from=min(Site_Fun_Rich2$GC),to=max(Site_Fun_Rich2$GC),length.out=5),labels=round(seq(from=min(ComVar$Ground_Cover),to=max(ComVar$Ground_Cover),length.out=5),0),cex.axis=1.2)
mtext(side=3,line=0,at = -3.3,'a)',cex=0.8)

polygon(x = c(Site_Fun_Rich2$GC[GG],rev(Site_Fun_Rich2$GC[GG])), y = c(Site_Fun_Rich2$lci[GG],rev(Site_Fun_Rich2$uci[GG])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Fun_Rich2$GC[GG],y = Site_Fun_Rich2$fit[GG],lwd = 2,col = 'grey30')


plot(x = ComVar$GGC,y = ComVar$FRic,xlab = "Green Ground Cover (%)",ylab = 'Functional Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.lab=1.2,cex.axis=1.2,xaxt = 'n')
axis(side=1, at=seq(from=min(Site_Fun_Rich2$GGC),to=max(Site_Fun_Rich2$GGC),length.out=5),labels=round(seq(from=min(ComVar$Prop_Green_GC),to=max(ComVar$Prop_Green_GC),length.out=5),0),cex.axis=1.2)
mtext(side=3,line=0,at = -2,'b)',cex=0.8)

polygon(x = c(Site_Fun_Rich2$GGC[G_G],rev(Site_Fun_Rich2$GGC[G_G])), y = c(Site_Fun_Rich2$lci[G_G],rev(Site_Fun_Rich2$uci[G_G])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Fun_Rich2$GGC[G_G],y = Site_Fun_Rich2$fit[G_G],lwd = 2,col = 'grey30')

plot(x = ComVar$Height,y = ComVar$FRic,xlab = "Grass Height (cm)",ylab = 'Functional Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.lab=1.2,cex.axis=1.2,xaxt = 'n')
axis(side=1, at=seq(from=min(Site_Fun_Rich2$Height),to=max(Site_Fun_Rich2$Height),length.out=5),labels=round(seq(from=min(ComVar$Plant_Height),to=max(ComVar$Plant_Height),length.out=5),0),cex.axis=1.2)
mtext(side=3,line=0,at = -1.5,'c)',cex=0.8)

polygon(x = c(Site_Fun_Rich2$Height[GGG],rev(Site_Fun_Rich2$Height[GGG])), y = c(Site_Fun_Rich2$lci[GGG],rev(Site_Fun_Rich2$uci[GGG])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Fun_Rich2$Height[GGG],y = Site_Fun_Rich2$fit[GGG],lwd = 2,col = 'grey30')


plot(x = 1:2,y = Site_Fun_Rich2$fit[G_G_G],xlab = " ",ylab = 'Functional Richness', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,14),cex.lab =1.2,cex.axis = 1.2)
axis(side=1,at=c(0.8,2.2),labels=c('Escarpment','Valley'),cex.axis=1.2)
mtext(side=3,line=0,at = -0.25,'d)',cex=0.8)
arrows(x0=1:2, y0=Site_Fun_Rich2$lci [G_G_G],x1=1:2, y1=Site_Fun_Rich2$uci[G_G_G],angle=90,length=0.1, code=3, lwd=2,col = "black")
points(x = jitter(raw_x1, factor = 1),y = ComVar$FRic, pch = 16, cex = 0.4, col = "grey30")


plot(x = ComVar$ResDay,y = ComVar$FRic,xlab = "Day (position-adjusted)",ylab = 'Functional Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.lab=1.2,cex.axis=1.2,xaxt = 'n')
axis(side=1, at=seq(from=min(Site_Fun_Rich2$ResDay),to=max(Site_Fun_Rich2$ResDay),length.out=5),labels=round(seq(from=min(ComVar$ResDay),to=max(ComVar$ResDay),length.out=5),0),cex.axis=1.2)
mtext(side=3,line=0,at = -15,'e)',cex=0.8)

polygon(x = c(Site_Fun_Rich2$ResDay[GGGG],rev(Site_Fun_Rich2$ResDay[GGGG])), y = c(Site_Fun_Rich2$lci[GGGG],rev(Site_Fun_Rich2$uci[GGGG])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Fun_Rich2$ResDay[GGGG],y = Site_Fun_Rich2$fit[GGGG],lwd = 2,col = 'grey30')

mtext(bquote(R^2 == 0.117), side=1,line=-10,at = 50,cex=0.9)
mtext("Site", side=1,line=-10,at = 41,cex=0.9)
mtext(bquote(R^2 == 0.031), side=1,line=-8,at = 50,cex=0.9)
mtext("Fixed", side=1,line=-8,at = 40.5,cex=0.9)



#Supporting Figure----

head(Site_Dis2)
summary(Top_Dis)

head(ComVar_Fun_Sub);dim(ComVar_Fun_Sub)


XX <- Site_Dis2$ResDay == Predictions_FunResday[10]
X_X <- Site_Dis2$Position == "Escarpment"


raw_x3 <- ifelse(ComVar_Fun_Sub$Position ==
                   "Escarpment", 1,2)

dev.new(height=10,width=20,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,3,2),mfrow=c(1,2),mgp=c(2.5,0.7,0),xpd = T)

plot(x = 1:2,y = Site_Dis2$fit[XX],xlab = " ",ylab = 'Functional Dispersion', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0.1,0.6),cex.lab =1,cex.axis = 1)
axis(side=1,at=c(0.8,2.2),labels=c('Escarpment','Valley'),cex.axis=1)
mtext(side=3,line=0,at = -0.15,'a)',cex=1)
arrows(x0=1:2, y0=Site_Dis2$lci [XX],x1=1:2, y1=Site_Dis2$uci[XX],angle=90,length=0.1, code=3, lwd=2,col = "black")
points(x = jitter(raw_x3, factor = 1),y = ComVar_Fun_Sub$FDis, pch = 16, cex = 0.4, col = "grey30")


plot(x = ComVar_Fun_Sub$ResDay,y = ComVar_Fun_Sub$FDis,xlab = "Day (position-adjusted) ",ylab = 'Functional Dispersion', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.lab=1,cex.axis=1)
mtext(side=3,line=0,at = -14.6,'b)',cex=1)

polygon(x = c(Site_Dis2$ResDay[X_X],rev(Site_Dis2$ResDay[X_X])), y = c(Site_Dis2$lci[X_X],rev(Site_Dis2$uci[X_X])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Dis2$ResDay[X_X],y = Site_Dis2$fit[X_X],lwd = 2,col = 'grey30')





#END----