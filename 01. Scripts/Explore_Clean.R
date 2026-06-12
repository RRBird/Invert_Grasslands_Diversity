
options(scipen = 999) #prevents r from automatically displaying large numbers with scientific notation

#Author: Rhiannon Bird
#Written under version R 4.5.1

#This script contains the data prepping, exploration and manipulation before moving into actual analysis

library("dplyr")
library('vegan')
library("corrplot")
library("tidyr")
library("tibble")


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
point <- point[,-c(21:23)]
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
hist(point$Plant_Height)
hist(point$Ground_Cover)
hist(point$Prop_Green_GC)
hist(point$Natual_Grazing_1km)
hist(point$Naural_Grazing_500m)

hist(point$Cropping_1km)
table(point$Cropping_1km) #histogram makes it look like not much variation with lots of 0 but there are a lot of values here so I think there's still enough variation
hist(point$Cropping_500m)
table(point$Cropping_500m) #whereas this one is almost half 0's, we'll leave it if its correlated with 1km crops then we'll pick that one becuase it has more variation
hist(point$X500m.Simspson)
table(point$X500m.Simspson) #lots of variation

table(point$Weed_Estimate)
table(point$Dominant_Herb_Weed)
table(point$Dominat_Grass)
table(point$Grass_Status)
table(point$X500m.Dominant.Landscape.Class)


head(property)
table(property$Day_Sampled)
table(property$Land_Use)
table(property$If_Graze_Animal.)

colnames(property) [6] <- "Graze_Animal"


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

cordata <- cordata %>% dplyr::select(Elevation, Plant_Height, Ground_Cover,Prop_Green_GC,Weed_Estimate,Dominant_Herb_Weed,Dominat_Grass,Natual_Grazing_1km,Cropping_1km,Naural_Grazing_500m,Cropping_500m,X500m.Simspson,X500m.Dominant.Landscape.Class,Day_Sampled)

str(cordata)

cordata$Weed_Estimate <- as.factor(cordata$Weed_Estimate)
levels(cordata$Weed_Estimate) #already in the right order
cordata$Weed_Estimate <- as.numeric(cordata$Weed_Estimate)

cordata$Dominant_Herb_Weed <- as.factor(cordata$Dominant_Herb_Weed)
cordata$Dominant_Herb_Weed <- as.numeric(cordata$Dominant_Herb_Weed)

cordata$Dominat_Grass <- as.factor(cordata$Dominat_Grass)
cordata$Dominat_Grass <- as.numeric(cordata$Dominat_Grass)


cordata$X500m.Dominant.Landscape.Class <- as.factor(
  cordata$X500m.Dominant.Landscape.Class)
cordata$X500m.Dominant.Landscape.Class <- as.numeric(
  cordata$X500m.Dominant.Landscape.Class)


str(cordata) #confirmed no character columns left

cor <- cor(cordata,method = "spearman")

colnames(cor) <- c("Elevation", "Height", "Ground Cover","Green Ground Cover","Weed Cover","Dom Herb Weed","Dom Grass","Grazing 1km","Crops 1km","Grazing 500m","Crops 500m","Simpson","Dom Landscape Class", "Day Sampled")
rownames(cor) <- c("Elevation", "Height", "Ground Cover","Green Ground Cover","Weed Cover","Dom Herb Weed","Dom Grass","Grazing 1km","Crops 1km","Grazing 500m","Crops 500m","Simpson","Dom Landscape Class", "Day Sampled")

dev.new(height=8,width=8,dpi=80,pointsize=14,noRStudioGD = T)
corrplot::corrplot(cor,method="color",  
                   type="upper",addCoef.col = 'black',number.cex = 0.6)

head(cor)

#removed due to correlation Simpson landscape, grazing 500m and crops 1km

#Merging data bases----

head(point)
head(property)

variables <- point

variables <- merge(variables,property, by = "Property")

head(variables);dim(variables)

variables <- variables %>% dplyr::select(-Grass_Status,-Cropping_1km,-Naural_Grazing_500m,-X500m.Simspson,-Sample_Date,-Julian_date,-Land_Use,-Graze_Animal,-Land_for_Wildlife,-Chemical.,-Slash.,-Burning.,-Rain_Week_Before_mm)
head(variables);dim(variables)

variables$Weed_Estimate <- as.factor(variables$Weed_Estimate)
levels(variables$Weed_Estimate)

head(obs)
#total observations = 1127

invert <- data.frame(Point = obs$Point,Morphospecies = obs$Morphospecies)
head(invert)

invert <- merge(invert,morpho,by = "Morphospecies")
head(invert);dim(invert)


#Prep for taxonomic Modelling----


TaxModel <- data.frame(Point = variables$Point)
head(TaxModel);dim(TaxModel)

##Species Richness----

richness <- aggregate(Morphospecies ~ Point, data = invert, FUN = function(x) length(unique(x)))
dim(richness) #only 1 point with 0 species 

TaxModel <- merge(TaxModel,richness,by = "Point",,all.x = T)
head(TaxModel);dim(TaxModel)

colnames(TaxModel)[2] <- "Species_Rich"
head(TaxModel);dim(TaxModel)

min(TaxModel$Species_Rich,na.rm=T)
max(TaxModel$Species_Rich,na.rm=T)

TaxModel$Species_Rich[is.na(TaxModel$Species_Rich)] <- 0

##Diversity (Inverse Simpson's diversity index)----

#diversity(table(x), index = "invsimpson")
#table (x) makes a table of species counts which is used to calculate diversity

diversity <- aggregate(Morphospecies ~ Point, data = invert, FUN = function(x) diversity(table(x), index = "invsimpson"))
dim(diversity)

TaxModel <- merge(TaxModel,diversity,by = "Point",all.x = T)
head(TaxModel);dim(TaxModel)
colnames(TaxModel)[3] <- "Diversity"
head(TaxModel);dim(TaxModel)

min(TaxModel$Diversity,na.rm=T)
max(TaxModel$Diversity,na.rm=T)

TaxModel$Diversity[is.na(TaxModel$Diversity)] <- 0



#add variables

TaxModel <- merge(TaxModel,variables,by = "Point",all.x = T)
head(TaxModel);dim(TaxModel)


#Setting up for RLQ Analysis----



#filter out morphospecies which are not at least ID'd to Order

length(which(is.na(invert$Order)))

invert_filtered <- invert[-which(is.na(invert$Order)), ]

head(invert_filtered);dim(invert_filtered) 
dim(invert);dim(invert_filtered)

##L Data (Site x Species)----

head(invert_filtered);dim(invert_filtered)


#Count occurrences of each morphospecies at each site
L_table <- invert_filtered %>%
  group_by(Point, Morphospecies) %>%
  summarise(abundance = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Morphospecies, 
              values_from = abundance, 
              values_fill = 0)
head(L_table);dim(L_table)
L_table[1,1]

#Convert to data frame with points as row names
L_matrix <- L_table %>%
  column_to_rownames("Point") %>%
  as.data.frame()
head(L_matrix);dim(L_matrix)

##Q Data (Species x Trait)----

head(invert_filtered)

#Get unique morphospecies with traits
Q_table <- invert_filtered %>%
  select(Morphospecies, Order, Size, Hunting_Style, Trophic) %>%
  distinct()
head(Q_table);dim(Q_table)

Q_matrix <- Q_table %>%
  column_to_rownames("Morphospecies") %>%
  as.data.frame()
head(Q_matrix);dim(Q_matrix)


##R Data (Site x Environmental)----

head(variables);dim(variables)

R_table <- variables %>%
  select(-Property)
head(R_table);dim(R_table)

#to make the next part transform correctly
R_table <- as.data.frame(R_table)
rownames(R_table) <- NULL

R_matrix <- R_table %>%
  column_to_rownames("Point") %>%
  as.data.frame()
##Checking that it worked----

#first up is points

points_L <- rownames(L_matrix)
points_R <- rownames(R_matrix)

if (!all(points_L %in% points_R)) {
  warning("Some points in L table are not in R table")
  print(setdiff(points_L, points_R))
} #Needs fixing

if (!all(points_R %in% points_L)) {
  warning("Some points in R table are not in L table")
  print(setdiff(points_R, points_L))
} #Needs fixing

#RQL can't handle missing data so can't have sites with no species included in the analysis
common_sites <- intersect(points_L, points_R)
L_matrix <- L_matrix[common_sites, ]
R_matrix <- R_matrix[common_sites, ]

points_L.2 <- rownames(L_matrix)
points_R.2 <- rownames(R_matrix)

if (!all(points_R.2 %in% points_L.2)) {
  warning("Some points in R table are not in L table")
  print(setdiff(points_R.2, points_L.2))
} #fixed they match

#Now checking morphospecies 

species_L <- colnames(L_matrix)
species_Q <- rownames(Q_matrix)

if (!all(species_L %in% species_Q)) {
  warning("Some morphospecies in L table are not in Q table")
  print(setdiff(species_L, species_Q))
} #looks good

if (!all(species_Q %in% species_L)) {
  warning("Some morphospecies in Q table are not in L table")
  print(setdiff(species_Q, species_L))
} #looks good

#match Q order to L
Q_matrix <- Q_matrix[species_L, ]

##Fixing missing values----

any(is.na(L_matrix)) #No NA's
any(is.na(R_matrix)) #No NA's
any(is.na(Q_matrix)) #NA's

any(is.na(Q_matrix$Order)) #No NA's
any(is.na(Q_matrix$Size)) #No NA's
any(is.na(Q_matrix$Hunting_Style)) #No NA's
any(is.na(Q_matrix$Trophic)) #NA's

Q_matrix$Trophic[which(is.na(Q_matrix$Trophic))] <- "Unknown"
any(is.na(Q_matrix)) #No NA's


head(L_matrix[,1:10]);dim(L_matrix)
head(R_matrix[,1:10]);dim(R_matrix)
head(Q_matrix);dim(Q_matrix)

#Traits need to be factors

Q_matrix[] <- lapply(Q_matrix, as.factor)
str(Q_matrix)

levels(Q_matrix$Size) #not in the right order
Q_matrix$Size <- factor(Q_matrix$Size, 
                        levels = c("0-2.5mm", "2.5-5mm", 
                                   "5-10mm", ">10mm", "No_Size",
                                   "Unknown"),
                        ordered = TRUE)
levels(Q_matrix$Size)


#END----