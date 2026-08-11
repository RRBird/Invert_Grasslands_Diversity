

#Archive


FunGroups<- expand.grid(Trophic = unique(morpho$Trophic), Hunting_Style = unique(morpho$Hunting_Style), Size =unique(morpho$Size))
head(FunGroups);dim(FunGroups)
#Produces 200 combinations

FunGroups$GroupNum <- 1:200

morpho1 <- morpho %>%
  left_join(FunGroups, by = c("Trophic", "Hunting_Style", "Size")) 

head(morpho1);dim(morpho1)

length(unique(morpho1$GroupNum))
#Total of 36 functional group combinations

invert2 <- data.frame(Point = obs$Point,Morphospecies = obs$Morphospecies)
head(invert2)

invert2 <- merge(invert2,morpho1,by = "Morphospecies")
head(invert2);dim(invert2)


FunCommunity <- data.frame(Point = variables$Point)
head(FunCommunity);dim(FunCommunity)

##Richness----

Funrichness <- aggregate(GroupNum ~ Point, data = invert2, FUN = function(x) length(unique(x)))
dim(Funrichness) 

FunCommunity <- merge(FunCommunity,Funrichness,by = "Point",,all.x = T)
head(FunCommunity);dim(FunCommunity)

colnames(FunCommunity)[2] <- "Fun_Rich"
head(FunCommunity);dim(FunCommunity)

min(FunCommunity$Fun_Rich,na.rm=T)
max(FunCommunity$Fun_Rich,na.rm=T)

FunCommunity$Fun_Rich[is.na(FunCommunity$Fun_Rich)] <- 0

##Diversity----

Fundiversity <- aggregate(GroupNum ~ Point, data = invert2, FUN = function(x) diversity(table(x), index = "invsimpson"))
dim(Fundiversity)

FunCommunity <- merge(FunCommunity,Fundiversity,by = "Point",all.x = T)
head(FunCommunity);dim(FunCommunity)
colnames(FunCommunity)[3] <- "FunDiversity"
head(FunCommunity);dim(FunCommunity)

min(FunCommunity$FunDiversity,na.rm=T)
max(FunCommunity$FunDiversity,na.rm=T)

FunCommunity$FunDiversity[is.na(FunCommunity$FunDiversity)] <- 0.000001

#Functional Data ----

head(FunCommunity);dim(FunCommunity)
head(variables);dim(variables)

FunComVar <- merge(FunCommunity,variables, by = "Point")
head(FunComVar);dim(FunComVar)

FunComVar<- FunComVar %>% select(-Elevation)

FunComVar$ResDay <- resid(lm(Day_Sampled ~ Position, FunComVar))

head(FunComVar);dim(FunComVar)

#Turn weed estimate into numeric
#Turn into middle of each bracket

FunComVar$Weed <- ifelse(FunComVar$Weed_Estimate =="0-20%",10,
                         ifelse(FunComVar$Weed_Estimate == "20-40%",30,
                                ifelse(FunComVar$Weed_Estimate == "40-60%",50,
                                       ifelse(FunComVar$Weed_Estimate=="60-80%",70,90))))
head(FunComVar);dim(FunComVar)


FunComVar <- FunComVar %>%
  left_join(property %>% select(Property, management), by = "Property")
head(FunComVar)

##SPECIES RICHNESS----

###Model Selection----

head(ComVar)
ComVar$Height_Scale <- scale(ComVar$Plant_Height)

#Design variables

SR_Null <- glmmTMB(Species_Rich ~ 1 + (1 | Property), family = nbinom2, data = ComVar)

SR_Day <- glmmTMB(Species_Rich ~ ResDay + (1 | Property), family = nbinom2, data = ComVar)
SR_Pos <- glmmTMB(Species_Rich ~ Position + (1 | Property), family = nbinom2, data = ComVar)

SR_PD <- glmmTMB(Species_Rich ~ Position + ResDay + (1 | Property), family = nbinom2, data = ComVar)
SR_PxD <- glmmTMB(Species_Rich ~ Position * ResDay + (1 | Property), family = nbinom2, data = ComVar)

aictab(list("Null" = SR_Null,"Day" = SR_Day,"Pos" = SR_Pos,
            "P+D" = SR_PD,"PxD" = SR_PxD))

#Position as fixed in all next models

#Single models
head(ComVar)

SR_Site_H <- glmmTMB(Species_Rich ~ Position + Plant_Height + (1 | Property), family = nbinom2, data = ComVar)
SR_Site_GC <- glmmTMB(Species_Rich ~ Position + Ground_Cover + (1 | Property), family = nbinom2, data = ComVar)
SR_Site_GGC <- glmmTMB(Species_Rich ~ Position + Prop_Green_GC + (1 | Property), family = nbinom2, data = ComVar)
SR_Site_WE <- glmmTMB(Species_Rich ~ Position + Weed_Estimate + (1 | Property), family = nbinom2, data = ComVar)
SR_Site_GS <- glmmTMB(Species_Rich ~ Position + Grass_Status + (1 | Property), family = nbinom2, data = ComVar)

#Addative models
SR_Site_H_GC <- glmmTMB(Species_Rich ~ Position + Plant_Height + Ground_Cover + (1 | Property), family = nbinom2, data = ComVar)
SR_Site_H_GGC <- glmmTMB(Species_Rich ~ Position + Plant_Height + Prop_Green_GC + (1 | Property), family = nbinom2, data = ComVar)
SR_Site_H_WE <- glmmTMB(Species_Rich ~ Position + Plant_Height + Weed_Estimate + (1 | Property), family = nbinom2, data = ComVar)
SR_Site_H_GS <- glmmTMB(Species_Rich ~ Position + Plant_Height + Grass_Status + (1 | Property), family = nbinom2, data = ComVar)

SR_Site_GC_GGC <- glmmTMB(Species_Rich ~ Position + Ground_Cover + Prop_Green_GC + (1 | Property), family = nbinom2, data = ComVar)
SR_Site_GC_WE <- glmmTMB(Species_Rich ~ Position + Ground_Cover + Weed_Estimate + (1 | Property), family = nbinom2, data = ComVar)
SR_Site_GC_GS <- glmmTMB(Species_Rich ~ Position + Ground_Cover + Grass_Status + (1 | Property), family = nbinom2, data = ComVar)

SR_Site_GGC_WE <- glmmTMB(Species_Rich ~ Position + Prop_Green_GC + Weed_Estimate + (1 | Property), family = nbinom2, data = ComVar)
SR_Site_GGC_GS <- glmmTMB(Species_Rich ~ Position + Prop_Green_GC + Grass_Status + (1 | Property), family = nbinom2, data = ComVar)

SR_Site_WE_GS <- glmmTMB(Species_Rich ~ Position + Weed_Estimate + Grass_Status + (1 | Property), family = nbinom2, data = ComVar)

#interactive

SR_Site_HxGC <- glmmTMB(Species_Rich ~ Position + Plant_Height * Ground_Cover + (1 | Property), family = nbinom2, data = ComVar)
SR_Site_HxGGC <- glmmTMB(Species_Rich ~ Position + Plant_Height * Prop_Green_GC + (1 | Property), family = nbinom2, data = ComVar)
SR_Site_HxWE <- glmmTMB(Species_Rich ~ Position + Height_Scale * Weed_Estimate + (1 | Property), family = nbinom2, data = ComVar)
SR_Site_HxGS <- glmmTMB(Species_Rich ~ Position + Plant_Height * Grass_Status + (1 | Property), family = nbinom2, data = ComVar)

SR_Site_GCxGGC <- glmmTMB(Species_Rich ~ Position + Ground_Cover * Prop_Green_GC + (1 | Property), family = nbinom2, data = ComVar)
SR_Site_GCxWE <- glmmTMB(Species_Rich ~ Position + Ground_Cover * Weed_Estimate + (1 | Property), family = nbinom2, data = ComVar)
SR_Site_GCxGS <- glmmTMB(Species_Rich ~ Position + Ground_Cover * Grass_Status + (1 | Property), family = nbinom2, data = ComVar)

SR_Site_GGCxWE <- glmmTMB(Species_Rich ~ Position + Prop_Green_GC * Weed_Estimate + (1 | Property), family = nbinom2, data = ComVar)
SR_Site_GGCxGS <- glmmTMB(Species_Rich ~ Position + Prop_Green_GC * Grass_Status + (1 | Property), family = nbinom2, data = ComVar)


#Compare models

Modnames1 <- c("SR_Null","SR_Pos","SR_Site_H", "SR_Site_GC", 
               "SR_Site_GGC", "SR_Site_WE", "SR_Site_GS",
               "SR_Site_H_GC","SR_Site_H_GGC", "SR_Site_H_WE",
               "SR_Site_H_GS", "SR_Site_GC_GGC", "SR_Site_GC_WE",
               "SR_Site_GC_GS", "SR_Site_GGC_WE", "SR_Site_GGC_GS",
               "SR_Site_WE_GS", "SR_Site_HxGC", "SR_Site_HxGGC",
               "SR_Site_HxWE", "SR_Site_HxGS", "SR_Site_GCxGGC",
               "SR_Site_GCxWE", "SR_Site_GCxGS", "SR_Site_GGCxWE",
               "SR_Site_GGCxGS")

SR_Site_Modnames <- mget(Modnames1)


aictab(SR_Site_Modnames)
#Position and Ground Cover

#Eight models within 2 AICc

###Predictions----
summary(SR_Site_GC)

Predictions_GC <- seq(min(ComVar$Ground_Cover),max(ComVar$Ground_Cover),length.out=20)

Site_Rich <- expand.grid(Ground_Cover = Predictions_GC, Position = c("Escarpment","Valley"))
head(Site_Rich);dim(Site_Rich)

Site_Rich1 <- predict(object = SR_Site_GC,newdata= Site_Rich,se.fit = T, type = "link",re.form = NA)

Site_Rich2<-data.frame(Site_Rich,fit.link=Site_Rich1$fit,se.link=Site_Rich1$se.fit)

Site_Rich2$lci.link<-Site_Rich2$fit.link-(1.96*Site_Rich2$se.link)
Site_Rich2$uci.link<-Site_Rich2$fit.link+(1.96*Site_Rich2$se.link)

Site_Rich2$fit<-exp(Site_Rich2$fit.link)
Site_Rich2$se<-exp(Site_Rich2$se.link)
Site_Rich2$lci<-exp(Site_Rich2$lci.link)
Site_Rich2$uci<-exp(Site_Rich2$uci.link)

head(Site_Rich2);dim(Site_Rich2)

#TO DO - supporting preds----

#eight within 2 aicc

###Visual ----
#basic fig no a b c and need to replace the x axis but gives a visual of model

head(Site_Rich2)
AA <- Site_Rich2$Position == "Escarpment" 
A_A <- Site_Rich2$Ground_Cover == Predictions_GC[10]


raw_x1 <- ifelse(ComVar$Position ==
                   "Escarpment", 1, 
                 ifelse(ComVar$Position ==
                          "Valley", 2, NA))

dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = ComVar$Ground_Cover,y = ComVar$Species_Rich,xlab = expression("Ground Cover (%)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)

polygon(x = c(Site_Rich2$Ground_Cover[AA],rev(Site_Rich2$Ground_Cover[AA])), y = c(Site_Rich2$lci[AA],rev(Site_Rich2$uci[AA])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Rich2$Ground_Cover[AA],y = Site_Rich2$fit[AA],lwd = 2,col = 'grey30')


plot(x = 1:2,y = Site_Rich2$fit[A_A],xlab = " ",ylab = 'Species Richness', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,16))
axis(side=1,at=c(1,2),labels=c('Escarpment','Valley'))
arrows(x0=1:2, y0=Site_Rich2$lci [A_A],x1=1:2, y1=Site_Rich2$uci[A_A],angle=90,length=0.1, code=3, lwd=2,col = "black")
points(x = jitter(raw_x1, factor = 1),y = ComVar$Species_Rich, pch = 16, cex = 0.4, col = "black")

#TO  DO visual supporting ----

##Eight models within 2 aicc i.e. B-I

##DIVERSITY----
head(ComVar)

###Model Selection----

head(ComVar)
ComVar$GC <- scale(ComVar$Ground_Cover)

#Design variables

Div_Null <- glmer(Diversity ~ 1 + (1 | Property), family = Gamma(link = "log"), data = ComVar)

Div_Day <- glmer(Diversity ~ ResDay + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_Pos <- glmer(Diversity ~ Position + (1 | Property), family = Gamma(link = "log"), data = ComVar)

Div_PD <- glmer(Diversity ~ Position + ResDay + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_PxD <- glmer(Diversity ~ Position * ResDay + (1 | Property), family = Gamma(link = "log"), data = ComVar)

aictab(list("Null" = Div_Null,"Day" = Div_Day,"Pos" = Div_Pos,
            "P+D" = Div_PD,"PxD" = Div_PxD))

#Null top not needed in the next models

#Single models
head(ComVar)

Div_Site_H <- glmer(Diversity ~ Plant_Height + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_Site_GC <- glmer(Diversity ~ Ground_Cover + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_Site_GGC <- glmer(Diversity ~ Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_Site_WE <- glmer(Diversity ~ Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_Site_GS <- glmer(Diversity ~ Grass_Status + (1 | Property), family = Gamma(link = "log"), data = ComVar)

#Addative models
Div_Site_H_GC <- glmer(Diversity ~ Plant_Height + Ground_Cover + (1 | Property), family =  Gamma(link = "log"), data = ComVar)
Div_Site_H_GGC <- glmer(Diversity ~ Plant_Height + Prop_Green_GC + (1 | Property), family =  Gamma(link = "log"), data = ComVar)
Div_Site_H_WE <- glmer(Diversity ~ Plant_Height + Weed_Estimate + (1 | Property), family =  Gamma(link = "log"), data = ComVar)
Div_Site_H_GS <- glmer(Diversity ~ Plant_Height + Grass_Status + (1 | Property), family =  Gamma(link = "log"), data = ComVar)

Div_Site_GC_GGC <- glmer(Diversity ~ Ground_Cover + Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_Site_GC_WE <- glmer(Diversity ~ Ground_Cover + Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_Site_GC_GS <- glmer(Diversity ~ Ground_Cover + Grass_Status + (1 | Property), family = Gamma(link = "log"), data = ComVar)

Div_Site_GGC_WE <- glmer(Diversity ~ Prop_Green_GC + Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_Site_GGC_GS <- glmer(Diversity ~ Prop_Green_GC + Grass_Status + (1 | Property), family = Gamma(link = "log"), data = ComVar)

Div_Site_WE_GS <- glmer(Diversity ~ Weed_Estimate + Grass_Status + (1 | Property), family = Gamma(link = "log"), data = ComVar)

#interactive

Div_Site_HxGC <- glmer(Diversity ~ Height_Scale * Ground_Cover + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_Site_HxGGC <- glmer(Diversity ~ Plant_Height * Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_Site_HxWE <- glmer(Diversity ~ Plant_Height * Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_Site_HxGS <- glmer(Diversity ~ Plant_Height * Grass_Status + (1 | Property), family = Gamma(link = "log"), data = ComVar)

Div_Site_GCxGGC <- glmer(Diversity ~ GC * Prop_Green_GC + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_Site_GCxWE <- glmer(Diversity ~ Ground_Cover * Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_Site_GCxGS <- glmer(Diversity ~ Ground_Cover * Grass_Status + (1 | Property), family = Gamma(link = "log"), data = ComVar)

Div_Site_GGCxWE <- glmer(Diversity ~ Prop_Green_GC * Weed_Estimate + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_Site_GGCxGS <- glmer(Diversity ~ Prop_Green_GC * Grass_Status + (1 | Property), family = Gamma(link = "log"), data = ComVar)


#Compare models

DivSite_Modnames <- c("Div_Null","Div_Site_H", "Div_Site_GC", 
                      "Div_Site_GGC", "Div_Site_WE", "Div_Site_GS",
                      "Div_Site_H_GC","Div_Site_H_GGC", "Div_Site_H_WE",
                      "Div_Site_H_GS", "Div_Site_GC_GGC", "Div_Site_GC_WE",
                      "Div_Site_GC_GS","Div_Site_GGC_WE","Div_Site_GGC_GS",
                      "Div_Site_WE_GS", "Div_Site_HxGC", "Div_Site_HxGGC",
                      "Div_Site_HxWE", "Div_Site_HxGS", "Div_Site_GCxGGC",
                      "Div_Site_GCxWE", "Div_Site_GCxGS","Div_Site_GGCxWE",
                      "Div_Site_GGCxGS")

Div_Site_Modnames1 <- mget(DivSite_Modnames)
aictab(Div_Site_Modnames1)
#Null is top model

##ABUNDANCE----

###Model Selection----

#Design variables
head(ComVar)

Abun_Null <- glmmTMB(Count ~ 1 + (1 | Property), family = nbinom2, data = ComVar)

Abun_Day <- glmmTMB(Count ~ ResDay + (1 | Property), family = nbinom2, data = ComVar)
Abun_Pos <- glmmTMB(Count ~ Position + (1 | Property), family = nbinom2, data = ComVar)

Abun_PD <- glmmTMB(Count ~ Position + ResDay + (1 | Property), family = nbinom2, data = ComVar)
Abun_PxD <- glmmTMB(Count ~ Position * ResDay + (1 | Property), family = nbinom2, data = ComVar)

aictab(list("Null" = Abun_Null,"Day" = Abun_Day,"Pos" = Abun_Pos,
            "P+D" = Abun_PD,"PxD" = Abun_PxD))

#Position as fixed in all next models

#Single models
head(ComVar)

Abun_Site_H <- glmmTMB(Count ~ Position + Plant_Height + (1 | Property), family = nbinom2, data = ComVar)
Abun_Site_GC <- glmmTMB(Count ~ Position + Ground_Cover + (1 | Property), family = nbinom2, data = ComVar)
Abun_Site_GGC <- glmmTMB(Count ~ Position + Prop_Green_GC + (1 | Property), family = nbinom2, data = ComVar)
Abun_Site_WE <- glmmTMB(Count ~ Position + Weed_Estimate + (1 | Property), family = nbinom2, data = ComVar)
Abun_Site_GS <- glmmTMB(Count ~ Position + Grass_Status + (1 | Property), family = nbinom2, data = ComVar)

#Addative models
Abun_Site_H_GC <- glmmTMB(Count ~ Position + Plant_Height + Ground_Cover + (1 | Property), family = nbinom2, data = ComVar)
Abun_Site_H_GGC <- glmmTMB(Count ~ Position + Plant_Height + Prop_Green_GC + (1 | Property), family = nbinom2, data = ComVar)
Abun_Site_H_WE <- glmmTMB(Count ~ Position + Plant_Height + Weed_Estimate + (1 | Property), family = nbinom2, data = ComVar)
Abun_Site_H_GS <- glmmTMB(Count ~ Position + Plant_Height + Grass_Status + (1 | Property), family = nbinom2, data = ComVar)

Abun_Site_GC_GGC <- glmmTMB(Count ~ Position + Ground_Cover + Prop_Green_GC + (1 | Property), family = nbinom2, data = ComVar)
Abun_Site_GC_WE <- glmmTMB(Count ~ Position + Ground_Cover + Weed_Estimate + (1 | Property), family = nbinom2, data = ComVar)
Abun_Site_GC_GS <- glmmTMB(Count ~ Position + Ground_Cover + Grass_Status + (1 | Property), family = nbinom2, data = ComVar)

Abun_Site_GGC_WE <- glmmTMB(Count ~ Position + Prop_Green_GC + Weed_Estimate + (1 | Property), family = nbinom2, data = ComVar)
Abun_Site_GGC_GS <- glmmTMB(Count ~ Position + Prop_Green_GC + Grass_Status + (1 | Property), family = nbinom2, data = ComVar)

Abun_Site_WE_GS <- glmmTMB(Count ~ Position + Weed_Estimate + Grass_Status + (1 | Property), family = nbinom2, data = ComVar)

#interactive

Abun_Site_HxGC <- glmmTMB(Count ~ Position + Plant_Height * Ground_Cover + (1 | Property), family = nbinom2, data = ComVar)
Abun_Site_HxGGC <- glmmTMB(Count ~ Position + Plant_Height * Prop_Green_GC + (1 | Property), family = nbinom2, data = ComVar)
Abun_Site_HxWE <- glmmTMB(Count ~ Position + Height_Scale * Weed_Estimate + (1 | Property), family = nbinom2, data = ComVar)
Abun_Site_HxGS <- glmmTMB(Count ~ Position + Plant_Height * Grass_Status + (1 | Property), family = nbinom2, data = ComVar)

Abun_Site_GCxGGC <- glmmTMB(Count ~ Position + Ground_Cover * Prop_Green_GC + (1 | Property), family = nbinom2, data = ComVar)
Abun_Site_GCxWE <- glmmTMB(Count ~ Position + Ground_Cover * Weed_Estimate + (1 | Property), family = nbinom2, data = ComVar)
Abun_Site_GCxGS <- glmmTMB(Count ~ Position + Ground_Cover * Grass_Status + (1 | Property), family = nbinom2, data = ComVar)

Abun_Site_GGCxWE <- glmmTMB(Count ~ Position + Prop_Green_GC * Weed_Estimate + (1 | Property), family = nbinom2, data = ComVar)
Abun_Site_GGCxGS <- glmmTMB(Count ~ Position + Prop_Green_GC * Grass_Status + (1 | Property), family = nbinom2, data = ComVar)


#Compare models

Abun_Modnames1 <- c("Abun_Null","Abun_Pos","Abun_Site_H",
                    "Abun_Site_GC", "Abun_Site_GGC", "Abun_Site_WE",
                    "Abun_Site_GS","Abun_Site_H_GC", 
                    "Abun_Site_H_GGC", "Abun_Site_H_WE",
                    "Abun_Site_H_GS", "Abun_Site_GC_GGC", 
                    "Abun_Site_GC_WE","Abun_Site_GC_GS", 
                    "Abun_Site_GGC_WE", "Abun_Site_GGC_GS",
                    "Abun_Site_WE_GS", "Abun_Site_HxGC", 
                    "Abun_Site_HxGGC","Abun_Site_HxWE", 
                    "Abun_Site_HxGS", "Abun_Site_GCxGGC", 
                    "Abun_Site_GCxWE", "Abun_Site_GCxGS", 
                    "Abun_Site_GGCxWE", "Abun_Site_GGCxGS")

Abun_Site_Modnames <- mget(Abun_Modnames1)


aictab(Abun_Site_Modnames)
#Position, Ground Cover and Weed Cover

#No models within 2 AICc

###Predictions----
summary(Abun_Site_GC_WE)

Predictions_Weed <- levels(ComVar$Weed_Estimate)

Site_Abun <- expand.grid(Ground_Cover = Predictions_GC, Position = c("Escarpment","Valley"),Weed_Estimate = Predictions_Weed)
head(Site_Abun);dim(Site_Abun)

Site_Abun1 <- predict(object = Abun_Site_GC_WE,newdata= Site_Abun,se.fit = T, type = "link",re.form = NA)

Site_Abun2<-data.frame(Site_Abun,fit.link=Site_Abun1$fit,se.link=Site_Abun1$se.fit)

Site_Abun2$lci.link<-Site_Abun2$fit.link-(1.96*Site_Abun2$se.link)
Site_Abun2$uci.link<-Site_Abun2$fit.link+(1.96*Site_Abun2$se.link)

Site_Abun2$fit<-exp(Site_Abun2$fit.link)
Site_Abun2$se<-exp(Site_Abun2$se.link)
Site_Abun2$lci<-exp(Site_Abun2$lci.link)
Site_Abun2$uci<-exp(Site_Abun2$uci.link)

head(Site_Abun2);dim(Site_Abun2)

###Visual ----
#basic fig no a b c and need to replace the x axis but gives a visual of model

head(Site_Abun2)
JJ <- Site_Abun2$Position == "Escarpment" & Site_Abun2$Weed_Estimate == "20-40%"
JJJ <- Site_Abun2$Position == "Escarpment" & Site_Abun2$Ground_Cover == Predictions_GC[10]
J_J <- Site_Abun2$Weed_Estimate == "20-40%" & Site_Abun2$Ground_Cover == Predictions_GC[10]


raw_x2 <- ifelse(ComVar$Weed_Estimate ==
                   "0-20%", 1, 
                 ifelse(ComVar$Weed_Estimate ==
                          "20-40%", 2, 
                        ifelse(ComVar$Weed_Estimate ==
                                 "40-60%", 3, 
                               ifelse(ComVar$Weed_Estimate ==
                                        "60-80%", 4, 5))))

dev.new(height=10,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(2,2),mgp=c(2.5,1,0),xpd = T)

plot(x = ComVar$Ground_Cover,y = ComVar$Count,xlab = expression("Ground Cover (%)"),ylab = 'Abundance', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)

polygon(x = c(Site_Abun2$Ground_Cover[JJ],rev(Site_Abun2$Ground_Cover[JJ])), y = c(Site_Abun2$lci[JJ],rev(Site_Abun2$uci[JJ])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Abun2$Ground_Cover[JJ],y = Site_Abun2$fit[JJ],lwd = 2,col = 'grey30')


plot(x = 1:5,y = Site_Abun2$fit[JJJ],xlab = " ",ylab = 'Abundance', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,6),ylim = c(0,40))
axis(side=1,at=c(1:5),labels=c(
  "0-20%","20-40%","40-60%","60-80%","80-100%"))
arrows(x0=1:5, y0=Site_Abun2$lci [JJJ],x1=1:5, y1= Site_Abun2$uci[JJJ], angle=90,length=0.1, code=3, lwd=2,col = "black")
points(x = jitter(raw_x2, factor = 1),y = ComVar$Count, pch = 16, cex = 0.4, col = "black")



plot(x = 1:2,y = Site_Abun2$fit[J_J],xlab = " ",ylab = 'Abundance', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,40))
axis(side=1,at=c(1,2),labels=c('Escarpment','Valley'))
arrows(x0=1:2, y0=Site_Abun2$lci [J_J],x1=1:2, y1=Site_Abun2$uci[J_J],angle=90,length=0.1, code=3, lwd=2,col = "black")
points(x = jitter(raw_x1, factor = 1),y = ComVar$Count, pch = 16, cex = 0.4, col = "black")

##Main Site Figure----



dev.new(height=10,width=15,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(2,3),mgp=c(2.5,1,0),xpd = T)

plot(x = ComVar$Ground_Cover,y = ComVar$Count,xlab = expression("Ground Cover (%)"),ylab = 'Abundance', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.lab=1.4,cex.axis=1.4)
mtext(side=3,line=0,at = 33,'a)',cex=0.9)

polygon(x = c(Site_Abun2$Ground_Cover[JJ],rev(Site_Abun2$Ground_Cover[JJ])), y = c(Site_Abun2$lci[JJ],rev(Site_Abun2$uci[JJ])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Abun2$Ground_Cover[JJ],y = Site_Abun2$fit[JJ],lwd = 2,col = 'grey30')


plot(x = 1:5,y = Site_Abun2$fit[JJJ],xlab = " ",ylab = 'Abundance', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,6),ylim = c(0,40),cex.lab=1.4,cex.axis=1.4)
axis(side=1,at=c(1:5),labels = NA)
mtext(side=1,line=2,at = c(1:5),c(
  "0-\n20%","20-\n40%","40-\n60%","60-\n80%","80-\n100%"),cex=0.7)
arrows(x0=1:5, y0=Site_Abun2$lci [JJJ],x1=1:5, y1= Site_Abun2$uci[JJJ], angle=90,length=0.1, code=3, lwd=2,col = "black")
points(x = jitter(raw_x2, factor = 1),y = ComVar$Count, pch = 16, cex = 0.4, col = "black")
mtext(side=3,line=0,at = -0.4,'b)',cex=0.9)



plot(x = 1:2,y = Site_Abun2$fit[J_J],xlab = " ",ylab = 'Abundance', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,40),cex.axis=1.4,cex.lab=1.4)
axis(side=1,at=c(0.8,2.2),labels=c('Escarpment','Valley'),cex.axis=1.4)
arrows(x0=1:2, y0=Site_Abun2$lci [J_J],x1=1:2, y1=Site_Abun2$uci[J_J],angle=90,length=0.1, code=3, lwd=2,col = "black")
points(x = jitter(raw_x1, factor = 1),y = ComVar$Count, pch = 16, cex = 0.4, col = "black")
mtext(side=3,line=0,at = -0.3,'c)',cex=0.9)

plot(x = ComVar$Ground_Cover,y = ComVar$Species_Rich,xlab = expression("Ground Cover (%)"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2,cex.axis=1.4,cex.lab=1.4)
mtext(side=3,line=0,at = 32,'d)',cex=0.9)

polygon(x = c(Site_Rich2$Ground_Cover[AA],rev(Site_Rich2$Ground_Cover[AA])), y = c(Site_Rich2$lci[AA],rev(Site_Rich2$uci[AA])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Site_Rich2$Ground_Cover[AA],y = Site_Rich2$fit[AA],lwd = 2,col = 'grey30')


plot(x = 1:2,y = Site_Rich2$fit[A_A],xlab = " ",ylab = 'Species Richness', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,20),cex.axis=1.4,cex.lab=1.4)
axis(side=1,at=c(0.8,2.2),labels=c('Escarpment','Valley'),cex.axis=1.4,)
arrows(x0=1:2, y0=Site_Rich2$lci [A_A],x1=1:2, y1=Site_Rich2$uci[A_A],angle=90,length=0.1, code=3, lwd=2,col = "black")
points(x = jitter(raw_x1, factor = 1),y = ComVar$Species_Rich, pch = 16, cex = 0.4, col = "black")
mtext(side=3,line=0,at = -0.2,'e)',cex=0.9)



#Q2 Landscape Variables----


##SPECIES RICHNESS----

###Model Selection----
#include position in models (as found at start of Q1)

#Single models
head(ComVar)

SR_Land_G <- glmmTMB(Species_Rich ~ Position + Natual_Grazing_1km + (1 | Property), family = nbinom2, data = ComVar)
SR_Land_Div <- glmmTMB(Species_Rich ~ Position + X500m.Simspson + (1 | Property), family = nbinom2, data = ComVar)
SR_Land_Hab <- glmmTMB(Species_Rich ~ Position + X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = ComVar)

#Addative models
SR_Land_G_Div <- glmmTMB(Species_Rich ~ Position + Natual_Grazing_1km + X500m.Simspson + (1 | Property), family = nbinom2, data = ComVar)
SR_Land_G_Hab <- glmmTMB(Species_Rich ~ Position + Natual_Grazing_1km + X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = ComVar)
SR_Land_Div_Hab <- glmmTMB(Species_Rich ~ Position + X500m.Simspson + X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = ComVar)

#interactive

SR_Land_GxDiv <- glmmTMB(Species_Rich ~ Position + Natual_Grazing_1km * X500m.Simspson + (1 | Property), family = nbinom2, data = ComVar)
SR_Land_GxHab <- glmmTMB(Species_Rich ~ Position + Natual_Grazing_1km * X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = ComVar)
SR_Land_DivxHab <- glmmTMB(Species_Rich ~ Position + X500m.Simspson * X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = ComVar)


#Compare models

Modnames2 <- c("SR_Null", "SR_Pos", "SR_Land_G", "SR_Land_Div",
               "SR_Land_Hab", "SR_Land_G_Div", "SR_Land_G_Hab",
               "SR_Land_Div_Hab", "SR_Land_GxDiv", "SR_Land_GxHab",
               "SR_Land_DivxHab")

SR_Land_Modnames <- mget(Modnames2)

aictab(SR_Land_Modnames)
#Habitat Diversity

#Three within 2 AICc


###preds----
summary(SR_Land_Div)

Predictions_Hab_Div <- seq(min(ComVar$X500m.Simspson),max(ComVar$X500m.Simspson),length.out=20)

Land_Rich <- expand.grid(X500m.Simspson = Predictions_Hab_Div,Position = c("Escarpment","Valley"))
head(Land_Rich);dim(Land_Rich)

Land_Rich1 <- predict(object = SR_Land_Div,newdata= Land_Rich,se.fit = T, type = "link",re.form = NA)

Land_Rich2<-data.frame(Land_Rich,fit.link=Land_Rich1$fit,se.link=Land_Rich1$se.fit)

Land_Rich2$lci.link<-Land_Rich2$fit.link-(1.96*Land_Rich2$se.link)
Land_Rich2$uci.link<-Land_Rich2$fit.link+(1.96*Land_Rich2$se.link)

Land_Rich2$fit<-exp(Land_Rich2$fit.link)
Land_Rich2$se<-exp(Land_Rich2$se.link)
Land_Rich2$lci<-exp(Land_Rich2$lci.link)
Land_Rich2$uci<-exp(Land_Rich2$uci.link)

head(Land_Rich2);dim(Land_Rich2)

#TO DO - Supporting preds----
#three within 2 AICc

###Visual----

head(Land_Rich2)
KK <- Land_Rich2$Position == "Escarpment" 
K_K <- Land_Rich2$X500m.Simspson == Predictions_Hab_Div[10] 


dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = ComVar$X500m.Simspson,y = ComVar$Species_Rich,xlab = expression("Habitat Diversity within 500m"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)

polygon(x = c(Land_Rich2$X500m.Simspson[KK],rev(Land_Rich2$X500m.Simspson[KK])), y = c(Land_Rich2$lci[KK],rev(Land_Rich2$uci[KK])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Land_Rich2$X500m.Simspson[KK],y = Land_Rich2$fit[KK],lwd = 2,col = 'grey30')


plot(x = 1:2,y = Land_Rich2$fit [K_K],xlab = " ",ylab = 'Species Richness', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,16))
axis(side=1,at=c(1,2),labels=c('Escarpment','Valley'))
arrows(x0=1:2, y0=Land_Rich2$lci [K_K],x1=1:2, y1=Land_Rich2$uci[K_K],angle=90,length=0.1, code=3, lwd=2,col = "black")
points(x = jitter(raw_x1, factor = 1),y = ComVar$Species_Rich, pch = 16, cex = 0.4, col = "black")

#TO DO - Supporting visual----
#three within 2 AICc

##DIVERSITY----

###Model Selection----
#include no design variable in models (as found at start of Q1)

#Single models
head(ComVar)

Div_Land_G <- glmer(Diversity ~ Position + Natual_Grazing_1km + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_Land_Div <- glmer(Diversity ~ Position + X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_Land_Hab <- glmer(Diversity ~ Position + X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = ComVar)

#Addative models
Div_Land_G_Div <- glmer(Diversity ~ Position + Natual_Grazing_1km + X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_Land_G_Hab <- glmer(Diversity ~ Position + Natual_Grazing_1km + X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_Land_Div_Hab <- glmer(Diversity ~ Position + X500m.Simspson + X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = ComVar)

#interactive

Div_Land_GxDiv <- glmer(Diversity ~ Position + Natual_Grazing_1km * X500m.Simspson + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_Land_GxHab <- glmer(Diversity ~ Position + Natual_Grazing_1km * X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = ComVar)
Div_Land_DivxHab <- glmer(Diversity ~ Position + X500m.Simspson * X500m.Dominant.Landscape.Class + (1 | Property), family = Gamma(link = "log"), data = ComVar)


#Compare models

Div_Modnames2 <- c("Div_Null", "Div_Pos", "Div_Land_G", 
                   "Div_Land_Div", "Div_Land_Hab","Div_Land_G_Div", 
                   "Div_Land_G_Hab","Div_Land_Div_Hab",
                   "Div_Land_GxDiv", "Div_Land_GxHab",
                   "Div_Land_DivxHab")

Div_Land_Modnames <- mget(Div_Modnames2)

aictab(Div_Land_Modnames)
#Null

##ABUNDANCE----

###Model Selection----
#include position in models (as found at start of Q1)

#Single models
head(ComVar)

Abun_Land_G <- glmmTMB(Count ~ Position + Natual_Grazing_1km + (1 | Property), family = nbinom2, data = ComVar)
Abun_Land_Div <- glmmTMB(Count ~ Position + X500m.Simspson + (1 | Property), family = nbinom2, data = ComVar)
Abun_Land_Hab <- glmmTMB(Count ~ Position + X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = ComVar)

#Addative models
Abun_Land_G_Div <- glmmTMB(Count ~ Position + Natual_Grazing_1km + X500m.Simspson + (1 | Property), family = nbinom2, data = ComVar)
Abun_Land_G_Hab <- glmmTMB(Count ~ Position + Natual_Grazing_1km + X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = ComVar)
Abun_Land_Div_Hab <- glmmTMB(Count ~ Position + X500m.Simspson + X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = ComVar)

#interactive

Abun_Land_GxDiv <- glmmTMB(Count ~ Position + Natual_Grazing_1km * X500m.Simspson + (1 | Property), family = nbinom2, data = ComVar)
Abun_Land_GxHab <- glmmTMB(Count ~ Position + Natual_Grazing_1km * X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = ComVar)
Abun_Land_DivxHab <- glmmTMB(Count ~ Position + X500m.Simspson * X500m.Dominant.Landscape.Class + (1 | Property), family = nbinom2, data = ComVar)


#Compare models

Abun_Modnames2 <- c("Abun_Null", "Abun_Pos", "Abun_Land_G",
                    "Abun_Land_Div","Abun_Land_Hab",
                    "Abun_Land_G_Div", "Abun_Land_G_Hab",
                    "Abun_Land_Div_Hab", "Abun_Land_GxDiv",
                    "Abun_Land_GxHab","Abun_Land_DivxHab")

Abun_Land_Modnames <- mget(Abun_Modnames2)

aictab(Abun_Land_Modnames)
#Habitat Diversity

#Five within 2 AICc

###preds----
summary(Abun_Land_Div)

Land_Abun <- expand.grid(X500m.Simspson = Predictions_Hab_Div,Position = c("Escarpment","Valley"))
head(Land_Abun);dim(Land_Abun)

Land_Abun1 <- predict(object = Abun_Land_Div,newdata= Land_Abun,se.fit = T, type = "link",re.form = NA)

Land_Abun2<-data.frame(Land_Rich,fit.link=Land_Abun1$fit,se.link=Land_Abun1$se.fit)

Land_Abun2$lci.link<-Land_Abun2$fit.link-(1.96*Land_Abun2$se.link)
Land_Abun2$uci.link<-Land_Abun2$fit.link+(1.96*Land_Abun2$se.link)

Land_Abun2$fit<-exp(Land_Abun2$fit.link)
Land_Abun2$se<-exp(Land_Abun2$se.link)
Land_Abun2$lci<-exp(Land_Abun2$lci.link)
Land_Abun2$uci<-exp(Land_Abun2$uci.link)

head(Land_Abun2);dim(Land_Abun2)

#TO DO - Supporting preds----
#five within 2 AICc

###Visual----

head(Land_Abun2)
LL <- Land_Abun2$Position == "Escarpment" 
L_L <- Land_Abun2$X500m.Simspson == Predictions_Hab_Div[10] 


dev.new(height=5,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(1,2),mgp=c(2.5,1,0),xpd = T)

plot(x = ComVar$X500m.Simspson,y = ComVar$Count,xlab = expression("Habitat Diversity within 500m"),ylab = 'Abundance', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2)

polygon(x = c(Land_Abun2$X500m.Simspson[LL],rev(Land_Abun2$X500m.Simspson[LL])), y = c(Land_Abun2$lci[LL],rev(Land_Abun2$uci[LL])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Land_Abun2$X500m.Simspson[LL],y = Land_Abun2$fit[LL],lwd = 2,col = 'grey30')


plot(x = 1:2,y = Land_Abun2$fit [L_L],xlab = " ",ylab = 'Abundance', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,40))
axis(side=1,at=c(1,2),labels=c('Escarpment','Valley'))
arrows(x0=1:2, y0=Land_Abun2$lci [L_L],x1=1:2, y1=Land_Abun2$uci[L_L],angle=90,length=0.1, code=3, lwd=2,col = "black")
points(x = jitter(raw_x1, factor = 1),y = ComVar$Count, pch = 16, cex = 0.4, col = "black")

#TO DO - Supporting visual----
#five within 2 AICc

##Main Landscape Figure----


dev.new(height=10,width=10,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(2,2),mgp=c(2.5,1,0),xpd = T)

plot(x = ComVar$X500m.Simspson,y = ComVar$Count,xlab = expression("Habitat Diversity within 500m"),ylab = 'Abundance', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2, xaxt = 'n')
axis(side=1, at=seq(from=min(ComVar$X500m.Simspson),to=max(ComVar$X500m.Simspson),length.out=5),labels=round(seq(from=min(ComVar$X500m.Simspson),to=max(ComVar$X500m.Simspson),length.out=5),1),cex.axis=1)

polygon(x = c(Land_Abun2$X500m.Simspson[LL],rev(Land_Abun2$X500m.Simspson[LL])), y = c(Land_Abun2$lci[LL],rev(Land_Abun2$uci[LL])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Land_Abun2$X500m.Simspson[LL],y = Land_Abun2$fit[LL],lwd = 2,col = 'grey30')
mtext(side=3,line=0,at = -0.01,'a)',cex=0.9)

plot(x = 1:2,y = Land_Abun2$fit [L_L],xlab = " ",ylab = 'Abundance', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,40))
axis(side=1,at=c(0.8,2.2),labels=c('Escarpment','Valley'))
arrows(x0=1:2, y0=Land_Abun2$lci [L_L],x1=1:2, y1=Land_Abun2$uci[L_L],angle=90,length=0.1, code=3, lwd=2,col = "black")
points(x = jitter(raw_x1, factor = 1),y = ComVar$Count, pch = 16, cex = 0.4, col = "black")
mtext(side=3,line=0,at = -0.25,'b)',cex=0.9)


plot(x = ComVar$X500m.Simspson,y = ComVar$Species_Rich,xlab = expression("Habitat Diversity within 500m"),ylab = 'Species Richness', type = 'p', pch = 16,cex =0.2,col = 'black', las = 1, lwd = 2, xaxt = 'n')
axis(side=1, at=seq(from=min(ComVar$X500m.Simspson),to=max(ComVar$X500m.Simspson),length.out=5),labels=round(seq(from=min(ComVar$X500m.Simspson),to=max(ComVar$X500m.Simspson),length.out=5),1),cex.axis=1)
mtext(side=3,line=0,at = -0.03,'c)',cex=0.9)


polygon(x = c(Land_Rich2$X500m.Simspson[KK],rev(Land_Rich2$X500m.Simspson[KK])), y = c(Land_Rich2$lci[KK],rev(Land_Rich2$uci[KK])),col = rgb(0.5, 0.5, 0.5, 0.5),border=NA)
lines(x=Land_Rich2$X500m.Simspson[KK],y = Land_Rich2$fit[KK],lwd = 2,col = 'grey30')


plot(x = 1:2,y = Land_Rich2$fit [K_K],xlab = " ",ylab = 'Species Richness', type = 'p',pch = 16,cex =2,col = 'black', las = 1,xaxt = "n",xlim = c(0,3),ylim = c(0,20))
axis(side=1,at=c(0.8,2.2),labels=c('Escarpment','Valley'))
arrows(x0=1:2, y0=Land_Rich2$lci [K_K],x1=1:2, y1=Land_Rich2$uci[K_K],angle=90,length=0.1, code=3, lwd=2,col = "black")
points(x = jitter(raw_x1, factor = 1),y = ComVar$Species_Rich, pch = 16, cex = 0.4, col = "black")
mtext(side=3,line=0,at = -0.2,'d)',cex=0.9)

#Q3 Site vs Landscape----

##Overall Species Richness----
#Model with all variables from Q1 and Q3

SR_Full_Model <- glmmTMB(Species_Rich ~ Ground_Cover + X500m.Simspson + Position + (1 | Property), family = nbinom2, data = ComVar)

summary(SR_Full_Model)

r2_SR_full <- partR2(SR_Full_Model, partvars = c("Ground_Cover", "X500m.Simspson", "Position"),R2_type = "marginal", nboot = 1000, data = ComVar)
#Doesn't work with glmmTMB
#Need to do it manually

#create groups of site, landscape and deisgn variables
site_vars <- c("Ground_Cover")
landscape_vars <- c("X500m.Simspson")
design_vars <- c("Position")

#part_r2 function written by Rhiannon with help of Claude AI
part_r2 <- function(model, group_vars) {
  r2_full <- r.squaredGLMM(model)[1, "R2m"]
  drop_formula <- as.formula(paste(". ~ . -", paste(group_vars, collapse = " - ")))
  reduced_formula <- update(formula(model), drop_formula)
  reduced_model <- update(model, formula = reduced_formula)
  r2_reduced <- r.squaredGLMM(reduced_model)[1, "R2m"]
  r2_full - r2_reduced
}

site_r2 <- part_r2(SR_Full_Model, site_vars)
landscape_r2 <- part_r2(SR_Full_Model, landscape_vars)
design_r2 <- part_r2(SR_Full_Model, design_vars)

site_r2
landscape_r2
design_r2

##Overall Abundance----
Abun_Full_Model <- glmmTMB(Count ~ Ground_Cover + Weed_Estimate + X500m.Simspson + Position + (1 | Property), family = nbinom2, data = ComVar)

summary(Abun_Full_Model)

r2_Abun_full <- r.squaredGLMM(Abun_Full_Model)[1, "R2m"]

#create groups of site, landscape and deisgn variables
Abun_site_vars <- c("Ground_Cover","Weed_Estimate")
Abun_landscape_vars <- c("X500m.Simspson")
Abun_design_vars <- c("Position")

Abun_site_r2 <- part_r2(Abun_Full_Model, site_vars)
Abun_landscape_r2 <- part_r2(Abun_Full_Model, landscape_vars)
Abun_design_r2 <- part_r2(Abun_Full_Model, design_vars)

Abun_site_r2
Abun_landscape_r2
Abun_design_r2


#Q4 Structural Equation Model----

#Model for each relationship in concept diagram
head(ComVar);dim(ComVar)
head(property);dim(property)

library("piecewiseSEM")

mod_Rich <- glmmTMB(Species_Rich ~ Land_Use + Plant_Height + Ground_Cover + Prop_Green_GC + (1 | Property), family = nbinom2, data = ComVar)

mod_H <- glmmTMB(Plant_Height ~ Land_Use + (1 | Property), family = gaussian, data = ComVar)

mod_GC <- glmmTMB(Ground_Cover ~ Land_Use + (1 | Property), family = gaussian, data = ComVar)

mod_GGC <- glmmTMB(Prop_Green_GC ~ Land_Use + (1 | Property), family = gaussian, data = ComVar)



psem_mods <- psem(mod_Rich,mod_H,mod_GC,mod_GGC, data=ComVar)
summary(psem_mods)


mod_Abun <- glmmTMB(Count ~ management + Plant_Height + Ground_Cover + Prop_Green_GC + (1 | Property), family = nbinom2, data = ComVar)

psem_mods2 <- psem(mod_Abun,mod_H,mod_GC,mod_GGC, data=ComVar)
summary(psem_mods2)

#Q5 Community comp environment----

#first need to do dissimilarity 

head(count);dim(count)
head(ComVar);dim(ComVar)

#had issues when I first ran it due to grass status lining up too well with specific levels within dom grass (not the variable as a whole) so removed from this one 

vars <- ComVar %>% select(-Species_Rich,-Diversity,-Count,-Property, -Day_Sampled,-GC,-GGC,-Graze,-Hab_Div,-Height,-Grass_Status)
head(vars);dim(vars)

dissim_mat <- vegdist(Count, method = "bray")

#Need to remove P57_P3 from vars since no obs at that site and dis-similarity struggles with 0 obs at a site
dim(vars)
vars <- vars[-c(vars$Point=="P57_P3"),]
dim(vars)

#PERMANOVA

permanova_res <- adonis2(dissim_mat ~ Plant_Height + Ground_Cover + Prop_Green_GC + Weed_Estimate + Dominant_Herb_Weed + Dominat_Grass + Natual_Grazing_1km + X500m.Simspson + X500m.Dominant.Landscape.Class + Position + ResDay, data = vars, permutations = 999, by = "margin")

permanova_res

