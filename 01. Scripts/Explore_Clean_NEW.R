

options(scipen = 999) #prevents r from automatically displaying large numbers with scientific notation

#Author: Rhiannon Bird
#Written under version R 4.5.1

#This script contains the data prepping, exploration and manipulation before moving into actual analysis


library("dplyr")
library('vegan')
library("corrplot")
library("tidyr")
library("tibble")
library("ggcorrplot")
library("FactoMineR")
library("factoextra")
library("scales")
library("fields")


#Data----
point <- read.csv("00. Data/Point.csv")
head(point);dim(point)

property <- read.csv("00. Data/Property.csv")
head(property);dim(property)

morpho <- read.csv("00. Data/Morpho.csv")
head(morpho);dim(morpho)

obs <- read.csv("00. Data/Obs.csv")
head(obs);dim(obs)

#Data cleaning----

##Remove unneeded columns----

summary(point)
head(point);dim(point)
point <- point[,-which(names(point) =="ImageJ_Min")]
point <- point[,-which(names(point) =="ImageJ_Max")]
point <- point[,-which(names(point) =="ImageJ_Min.1")]
point <- point[,-which(names(point) =="ImageJ_Max.1")]
point <- point[-79,]
colnames(point) [3] <- "Elevation"


summary(property)
head(property);dim(property)
property <- property[,-c(12:16)]


summary(morpho)
head(morpho);dim(morpho)
morpho <- morpho[,-which(names(morpho) =="Duplicates")]
colnames(morpho) [13] <- "Hunting_Style"


summary(obs)
head(obs);dim(obs)
obs <- obs[,-which(names(obs) =="Notes")]


##Checking enviro variables----

head(point);dim(point)
hist(point$Elevation) 
table(point$Elevation) #Elevation over 400 considered escarpment and under 300 considered valley 

point$Position <- ifelse(point$Elevation>400, "Escarpment","Valley")


hist(point$Plant_Height)
hist(point$Ground_Cover)
hist(point$Prop_Green_GC)

hist(point$Natual_Grazing_1km)
dim(table(point$Natual_Grazing_1km))
hist(point$Naural_Grazing_500m)
dim(table(point$Naural_Grazing_500m))

hist(point$Cropping_1km)
dim(table(point$Cropping_1km)) #histogram makes it look like not much variation with lots of 0 but there are a lot of values here so I think there's still enough variation
hist(point$Cropping_500m)
dim(table(point$Cropping_500m)) #whereas this one is almost half 0's, we'll leave it if its correlated with 1km crops then we'll pick that one because it has more variation
hist(point$X500m.Simspson)
table(point$X500m.Simspson) #lots of variation

table(point$Weed_Estimate) #skewed to the left
table(point$Dominant_Herb_Weed)
table(point$Dominat_Grass)
table(point$Grass_Status) #possibly confounded
table(point$Position,point$Grass_Status)

table(point$X500m.Dominant.Landscape.Class)


head(property)
table(property$Day_Sampled)
table(property$Land_Use)
table(property$If_Graze_Animal.)

colnames(property) [6] <- "Graze_Animal"

property$Grazing_Type <- ifelse(is.na(property$Graze_Animal),"Native_Grazing","Ungulate_Grazing")


##Checking functional traits----

head(morpho);dim(morpho)

table(morpho$Order)
table(morpho$ID_Level)
table(morpho$Trophic)

table(morpho$Hunting_Style)
morpho$Hunting_Style[is.na(morpho$Hunting_Style)] <- "Non-Predator"

table(morpho$Size)
morpho$Size[is.na(morpho$Size)] <- "No_Size"

#Correlation----
head(point)
head(property)

cordata <- point

cordata <- merge(cordata,property, by = "Property")

head(cordata);dim(cordata)

cordata <- cordata %>% dplyr::select(Elevation, Plant_Height, Ground_Cover,Prop_Green_GC,Weed_Estimate,Grass_Status, Natual_Grazing_1km,Cropping_1km,Naural_Grazing_500m,Cropping_500m,X500m.Simspson,X500m.Dominant.Landscape.Class,Day_Sampled,Dominant_Herb_Weed,Dominat_Grass,Position,Grazing_Type)


str(cordata)

#Character into numeric

cordata$Weed_Estimate <- as.factor(cordata$Weed_Estimate)
levels(cordata$Weed_Estimate) #already in the right order
cordata$Weed_Estimate <- as.numeric(cordata$Weed_Estimate)

cordata$X500m.Dominant.Landscape.Class <- as.factor(
  cordata$X500m.Dominant.Landscape.Class)
cordata$X500m.Dominant.Landscape.Class <- as.numeric(
  cordata$X500m.Dominant.Landscape.Class)

cordata$Grass_Status <- as.factor(cordata$Grass_Status)
cordata$Grass_Status <- as.numeric(cordata$Grass_Status)

cordata$Dominant_Herb_Weed <- as.factor(cordata$Dominant_Herb_Weed)
cordata$Dominant_Herb_Weed <- as.numeric(cordata$Dominant_Herb_Weed)

cordata$Dominat_Grass <- as.factor(cordata$Dominat_Grass)
cordata$Dominat_Grass <- as.numeric(cordata$Dominat_Grass)

cordata$Position <- as.factor(cordata$Position)
cordata$Position <- as.numeric(cordata$Position)

cordata$Grazing_Type <- as.factor(cordata$Grazing_Type)
cordata$Grazing_Type <- as.numeric(cordata$Grazing_Type)

str(cordata) #confirmed no character columns left

cor <- cor(cordata,method = "spearman")

colnames(cor) <- c("Elevation", "Height", "Ground Cover","Green Ground Cover","Weed Cover","Grass Status","Grazing 1km","Crops 1km","Grazing 500m","Crops 500m","Habitat Diversity","Habitat Structure", "Day Sampled","Dom Weed","Dom Grass","Position","Grazing Type")
rownames(cor) <- c("Elevation", "Height", "Ground Cover","Green Ground Cover","Weed Cover","Grass Status","Grazing 1km","Crops 1km","Grazing 500m","Crops 500m","Habitat Diversity","Habitat Structure", "Day Sampled","Dom Weed","Dom Grass","Position","Grazing Type")

dev.new(height=8,width=8,dpi=80,pointsize=14,noRStudioGD = T)
corrplot::corrplot(cor,method="color",  
                   type="upper",addCoef.col = 'black',number.cex = 0.6)

head(cor)

#remove grazing 500m, crops 1km, and crops 500m
#Chose Grazing 1km as this had the most variation among these four correlated variables

#Remove elevation or position
#need to work out how to deal with confounding factors of day and elevation/position since those are design factors in the survey

#Merging data bases----

head(point)
head(property)

variables <- point

variables <- merge(variables,property, by = "Property")

head(variables);dim(variables)

variables <- variables %>% dplyr::select(-Cropping_1km,-Naural_Grazing_500m,-Cropping_500m, -Sample_Date,-Julian_date,-Land_Use,-Graze_Animal,-Land_for_Wildlife,-Chemical.,-Slash.,-Burning.,-Rain_Week_Before_mm)
head(variables);dim(variables)

variables$Weed_Estimate <- as.factor(variables$Weed_Estimate)
levels(variables$Weed_Estimate)

table(variables$Grass_Status,variables$Day_Sampled)

head(obs)
#total observations = 1127

invert <- data.frame(Point = obs$Point,Morphospecies = obs$Morphospecies)
head(invert)

invert <- merge(invert,morpho,by = "Morphospecies")
head(invert);dim(invert)


#Community Measures----


Community <- data.frame(Point = variables$Point)
head(Community);dim(Community)

##Species Richness----

richness <- aggregate(Morphospecies ~ Point, data = invert, FUN = function(x) length(unique(x)))
dim(richness) 

Community <- merge(Community,richness,by = "Point",,all.x = T)
head(Community);dim(Community)

colnames(Community)[2] <- "Species_Rich"
head(Community);dim(Community)

min(Community$Species_Rich,na.rm=T)
max(Community$Species_Rich,na.rm=T)

Community$Species_Rich[is.na(Community$Species_Rich)] <- 0

sum(Community$Species_Rich==0)/length(Community$Species_Rich)

##Diversity (Inverse Simpson's diversity index)----

#diversity(table(x), index = "invsimpson")
#table (x) makes a table of species counts which is used to calculate diversity

diversity <- aggregate(Morphospecies ~ Point, data = invert, FUN = function(x) diversity(table(x), index = "invsimpson"))
dim(diversity)

Community <- merge(Community,diversity,by = "Point",all.x = T)
head(Community);dim(Community)
colnames(Community)[3] <- "Diversity"
head(Community);dim(Community)

min(Community$Diversity,na.rm=T)
max(Community$Diversity,na.rm=T)

Community$Diversity[is.na(Community$Diversity)] <- 0

#total count

count <- aggregate(Morphospecies ~ Point, data = invert, FUN = function(x) length(x))
dim(count) 

Community <- merge(Community,count,by = "Point",,all.x = T)
head(Community);dim(Community)

colnames(Community)[4] <- "Count"
head(Community);dim(Community)

min(Community$Species_Rich,na.rm=T)
max(Community$Species_Rich,na.rm=T)


#PCA----

#create a site by species matrix

count <- invert %>%
  count(Point, Morphospecies) %>% #obs per site
  pivot_wider(names_from = Morphospecies,
              values_from = n,
              values_fill = 0) %>% #fill with 0
  column_to_rownames("Point") 

pcadata <- decostand(count, method = "hellinger") #apparently makes results more ecologically meaningful, we'll see I guess
head(pcadata);dim(pcadata)

pca_result <- prcomp(pcadata, center = TRUE, scale. = FALSE)
#scale false becuase already transformed above with hellinger
summary(pca_result)
biplot(pca_result)

PoV2<-summary(pca_result)$importance[2,]

pcadata$pca.comp1<-pca_result$x[,1]
pcadata$pca.comp2<-pca_result$x[,2]

#Components
dev.new(height=10,width=10,dpi=80,pointsize=14,noRStudioGD = T)
plot(x=1:length(PoV2),y=PoV2,ylab="Propotion Varience Explained",xlab="Components",type="p",las=1)
lines(x=1:length(PoV2),y=PoV2)


#pull the property for each
pcadata$Point <- rownames(pcadata)
pcadata$property <- sub("_.*", "", pcadata$Point)
unique(pcadata$property)

length(unique(pcadata$property)) #need 13 colours

col.0<-c("black","red","orange","yellow","green4","blue2","purple",
         "pink","grey","lightgreen","lightblue","brown","grey40")
col.1<-col.0[as.factor(pcadata$property)]
##By property----
#plot component 1 and 2, colour with property and also added circles of clustering based on spread and standard deviation of points on that property
dev.new(height=10,width=10,dpi=80,pointsize=14,noRStudioGD = T)
plot(pcadata$pca.comp1,pcadata$pca.comp2,pch=19, 
     xlab="PC 1",ylab="PC 2",cex=2,las=1,col=alpha(col.1,1))
ordiellipse(cbind(pcadata$pca.comp1, pcadata$pca.comp2),
            groups = pcadata$property,col = col.0,lwd = 2,
            kind = "sd")

#lets try colour cordinating with different groups
head(pcadata);dim(pcadata)
head(variables)

pcadata2 <- merge(pcadata,variables,by = "Point")
head(pcadata2);dim(pcadata2)

##by grazing type----

length(unique(pcadata2$Grazing_Type)) #need 2 colours
col.2<-c("red","blue2")
col_graze<-col.2[as.factor(pcadata2$Grazing_Type)]

dev.new(height=10,width=10,dpi=80,pointsize=14,noRStudioGD = T)
plot(pcadata2$pca.comp1,pcadata2$pca.comp2,pch=19, 
     xlab="PC 1",ylab="PC 2",cex=2,las=1,col=alpha(col_graze,1))
ordiellipse(cbind(pcadata2$pca.comp1, pcadata2$pca.comp2),
            groups = pcadata2$Grazing_Type,col = col.2,lwd = 2,
            kind = "sd")
legend("bottomleft",legend = c("Native", "Ungulate"), pch = 19, col = col.2,pt.cex = 1,cex = 0.9)


##by weed estimate----

length(unique(pcadata2$Weed_Estimate)) #need 5 colours
col.3<-c("black","red","yellow","green4","blue2")
col_weed<-col.3[as.factor(pcadata2$Weed_Estimate)]

dev.new(height=10,width=10,dpi=80,pointsize=14,noRStudioGD = T)
plot(pcadata2$pca.comp1,pcadata2$pca.comp2,pch=19, 
     xlab="PC 1",ylab="PC 2",cex=2,las=1,col=alpha(col_weed,1))
ordiellipse(cbind(pcadata2$pca.comp1, pcadata2$pca.comp2),
            groups = pcadata2$Weed_Estimate,col = col.3,lwd = 2,
            kind = "sd")
legend("bottomleft",legend = c(
  "0-20%", "20-40%", "40-60%", "60-80%", "80-100%"), 
  pch = 19, col = col.3,pt.cex = 1,cex = 0.9)

##by grass status----

length(unique(pcadata2$Grass_Status)) #need 3 colours
col.4<-c("black","red","blue2")
col_status<-col.4[as.factor(pcadata2$Grass_Status)]

dev.new(height=10,width=10,dpi=80,pointsize=14,noRStudioGD = T)
plot(pcadata2$pca.comp1,pcadata2$pca.comp2,pch=19, 
     xlab="PC 1",ylab="PC 2",cex=2,las=1,col=alpha(col_status,1))
ordiellipse(cbind(pcadata2$pca.comp1, pcadata2$pca.comp2),
            groups = pcadata2$Grass_Status,col = col.4,lwd = 2,
            kind = "sd")
legend("bottomleft",legend = c("Introduced", "Unknown", "Native"), 
  pch = 19, col = col.4,pt.cex = 1,cex = 0.9)


##by dom surrounding habitat----

length(unique(pcadata2$X500m.Dominant.Landscape.Class)) #need 3 colours
col.5<-c("black","red","blue2")
col_domhab<-col.5[as.factor(pcadata2$X500m.Dominant.Landscape.Class)]

dev.new(height=10,width=10,dpi=80,pointsize=14,noRStudioGD = T)
plot(pcadata2$pca.comp1,pcadata2$pca.comp2,pch=19, 
     xlab="PC 1",ylab="PC 2",cex=2,las=1,col=alpha(col_domhab,1))
ordiellipse(cbind(pcadata2$pca.comp1, pcadata2$pca.comp2),
            groups = pcadata2$X500m.Dominant.Landscape.Class,
            col = col.5,lwd = 2, kind = "sd")
legend("bottomleft",legend = c("Herbaceous Open", "Woody Closed", "Woody Open"), pch = 19, col = col.5,pt.cex = 1,cex = 0.9)


##by position----

length(unique(pcadata2$Position)) #need 2 colours
col.6<-c("red","blue2")
col_pos<-col.6[as.factor(pcadata2$Position)]

dev.new(height=10,width=10,dpi=80,pointsize=14,noRStudioGD = T)
plot(pcadata2$pca.comp1,pcadata2$pca.comp2,pch=19, 
     xlab="PC 1",ylab="PC 2",cex=2,las=1,col=alpha(col_pos,1))
ordiellipse(cbind(pcadata2$pca.comp1, pcadata2$pca.comp2),
            groups = pcadata2$Position,
            col = col.6,lwd = 2, kind = "sd")
legend("bottomleft",legend = c("Escarpment", "Valley"), pch = 19, col = col.6,pt.cex = 1,cex = 0.9)


##by habitat diversity----

pcadata2$X500m.Simspson

col_ramp <-colorRampPalette(c("blue2", "yellow", "red"))
gradient_cols <- col_ramp(100)
grad_habdiv <- cut(pcadata2$X500m.Simspson, breaks = 100)
col_habdiv <- gradient_cols[as.numeric(grad_habdiv)]

dev.new(height=10, width=10, dpi=80, pointsize=14, noRStudioGD=T)
plot(pcadata2$pca.comp1, pcadata2$pca.comp2, pch=19,
     xlab="PC 1", ylab="PC 2", cex=2, las=1,
     col=alpha(col_habdiv,1))
image.plot(legend.only=TRUE, 
           zlim=range(pcadata2$X500m.Simspson,na.rm=TRUE),
           col=gradient_cols, legend.lab="Habitat Diversity",
           legend.line =2.3,
           smallplot=c(0.17, 0.20, 0.20, 0.45))


##by grazing----

pcadata2$Natual_Grazing_1km

grad_graze <- cut(pcadata2$Natual_Grazing_1km, breaks = 100)
col_graze1km <- gradient_cols[as.numeric(grad_graze)]

dev.new(height=10, width=10, dpi=80, pointsize=14, noRStudioGD=T)
plot(pcadata2$pca.comp1, pcadata2$pca.comp2, pch=19,
     xlab="PC 1", ylab="PC 2", cex=2, las=1,
     col=alpha(col_graze1km,1))
image.plot(legend.only=TRUE, 
           zlim=range(pcadata2$Natual_Grazing_1km,na.rm=TRUE),
           col=gradient_cols, legend.lab="Grazing within 1km",
           legend.line =2.3,
           smallplot=c(0.17, 0.20, 0.20, 0.45))


##by ground cover----

pcadata2$Ground_Cover

grad_GC <- cut(pcadata2$Ground_Cover, breaks = 100)
col_GC <- gradient_cols[as.numeric(grad_GC)]

dev.new(height=10, width=10, dpi=80, pointsize=14, noRStudioGD=T)
plot(pcadata2$pca.comp1, pcadata2$pca.comp2, pch=19,
     xlab="PC 1", ylab="PC 2", cex=2, las=1,
     col=alpha(col_GC,1))
image.plot(legend.only=TRUE, 
           zlim=range(pcadata2$Ground_Cover,na.rm=TRUE),
           col=gradient_cols, legend.lab="Ground Cover",
           legend.line =2.3,
           smallplot=c(0.17, 0.20, 0.20, 0.45))


##by green ground cover----

pcadata2$Prop_Green_GC

grad_GGC <- cut(pcadata2$Prop_Green_GC, breaks = 100)
col_GGC <- gradient_cols[as.numeric(grad_GGC)]

dev.new(height=10, width=10, dpi=80, pointsize=14, noRStudioGD=T)
plot(pcadata2$pca.comp1, pcadata2$pca.comp2, pch=19,
     xlab="PC 1", ylab="PC 2", cex=2, las=1,
     col=alpha(col_GGC,1))
image.plot(legend.only=TRUE, 
           zlim=range(pcadata2$Prop_Green_GC,na.rm=TRUE),
           col=gradient_cols, legend.lab="Green Ground Cover",
           legend.line =2.3,
           smallplot=c(0.17, 0.20, 0.20, 0.45))

##by height----

pcadata2$Plant_Height

grad_height <- cut(pcadata2$Plant_Height, breaks = 100)
col_height <- gradient_cols[as.numeric(grad_height)]

dev.new(height=10, width=10, dpi=80, pointsize=14, noRStudioGD=T)
plot(pcadata2$pca.comp1, pcadata2$pca.comp2, pch=19,
     xlab="PC 1", ylab="PC 2", cex=2, las=1,
     col=alpha(col_height,1))
image.plot(legend.only=TRUE, 
           zlim=range(pcadata2$Plant_Height,na.rm=TRUE),
           col=gradient_cols, legend.lab="Grass Height",
           legend.line =2.3,
           smallplot=c(0.17, 0.20, 0.20, 0.45))


#Fix correlated design variables----

head(Community);dim(Community)
head(variables);dim(variables)

ComVar <- merge(Community,variables, by = "Point")
head(ComVar);dim(ComVar)

ComVar<- ComVar %>% select(-Elevation)

ComVar$ResDay <- resid(lm(Day_Sampled ~ Position, ComVar))



#END----