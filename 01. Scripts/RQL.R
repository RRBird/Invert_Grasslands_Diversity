
#Author: Rhiannon Bird
#Written under version R 4.5.1

options(scipen = 999) #So R doesn't use scientific notation


#Libraries----

library("ade4")
library("dplyr")
library("tidyverse")
library("vegan")
library('fpc')
library("tidyr")
library("stats")

str(Q_matrix)
str(L_matrix)
str(R_matrix)

#need to update some of the r matrix up to factors

R_matrix$Dominant_Herb_Weed <- factor(R_matrix$Dominant_Herb_Weed,
                                      ordered = FALSE)

R_matrix$Dominat_Grass <- factor(R_matrix$Dominat_Grass,
                                      ordered = FALSE)

R_matrix$X500m.Dominant.Landscape.Class <- factor(
  R_matrix$X500m.Dominant.Landscape.Class,ordered = FALSE)

str(R_matrix) #Good

#Species responses to environmental gradients----

coa1 <- dudi.coa(L_matrix, scannf = F)
cca1 <- pcaiv(coa1, R_matrix, scannf = F)

#percentage of variation in species composition explained by enviro variables
100 * sum(cca1$eig) / sum(coa1$eig) 


dev.new(height=10,width=10,dpi=80,pointsize=14,noRStudioGD = T)
s.label(cca1$c1, clabel = 0)
par(mar = c(0.1, 0.1, 0.1, 0.1))
s.arrow(cca1$cor[-1,], add.plot=TRUE)


#RLQ Analysis---- 

pca.traits <- dudi.hillsmith(Q_matrix, row.w = coa1$cw, scannf = FALSE)
pca.env <- dudi.hillsmith(R_matrix, row.w = coa1$lw, scannf = FALSE)

rlq1 <- rlq(pca.env, coa1, pca.traits, scannf = FALSE)
summary(rlq1)

dev.new(height=10,width=15,dpi=80,pointsize=14,noRStudioGD = T)
plot(rlq1)


##Percentage of co-Inertia for each axis
100*rlq1$eig/sum(rlq1$eig)

#To interpret the results, correlations can be computed:
## weighted correlations axes / env.
t(pca.env$tab)%*%(diag(pca.env$lw))%*%as.matrix(rlq1$mR)

##weighted correlations axes / traits.
t(pca.traits$tab)%*%(diag(pca.traits$lw))%*%as.matrix(rlq1$mQ)

##correlations traits / env.
rlq1$tab


#here's a prelim plot
dev.new(height=10,width=10,dpi=80,pointsize=14,noRStudioGD = T)
s.arrow(rlq1$c1, xlim=c(-1,1), boxes = FALSE)
s.label(rlq1$li, add.plot=T, clab=1.5)


#Classifying scores to obtain functional groups----
hc2 <- hclust(dist(rlq1$lQ), method = "ward.D")
dev.new(height=20,width=40,dpi=80,pointsize=14,noRStudioGD = T)
plot(hc2) #uninterpretable


#Calinsky-Harabasz criteria to find best partition 
#calinski didn't work so dchanged it to calinhara 

ntest <- 8
res <- rep(0,ntest - 1)

for (i in 2:ntest){
  fac <- cutree(hc2, k = i)
  res[i-1] <- as.numeric(calinhara(rlq1$lQ, fac))
}

#Trait group numbers----
dev.new(height=5,width=7,dpi=80,pointsize=14,noRStudioGD = T)
par(mfrow=c(1,2))
plot(2:ntest, res, type='b', pch=20, xlab="Number of groups", ylab = "C-H index")
mtext(side=3,line=0,at = 1.2,'a)',cex=1.1)
plot(3:ntest, diff(res), type='b', pch=20, xlab="Number of groups", ylab = "Diff in C-H index")
mtext(side=3,line=0,at = 2.2,'b)',cex=1.1)

#Nine functional groups


spe.group2 <- as.factor(cutree(hc2, k = which.max(res) +1))
summary(spe.group2)
levels(spe.group2) <- c("A","B","C","D","E","F","G","H","I")


#biplot (trait groups + Traits and also with trait groups + enviro)----
dev.new(height=10,width=10,dpi=80,pointsize=14,noRStudioGD = T)
ade4::s.class(rlq1$lQ, spe.group2, col= 1:nlevels(spe.group2))
s.arrow(rlq1$c1, add.plot = T,clab=0.8)

#here it is without the traits laid over the top
dev.new(height=10,width=10,dpi=80,pointsize=14,noRStudioGD = T)
ade4::s.class(rlq1$lQ, spe.group2, col= 1:nlevels(spe.group2))


#here it is with the enviro groups
dev.new(height=10,width=10,dpi=80,pointsize=14,noRStudioGD = T)
ade4::s.class(rlq1$lQ, spe.group2, col= 1:nlevels(spe.group2))
s.arrow(rlq1$l1, add.plot = T, clab = 0.6,boxes = FALSE)

#Trait loadings on Axis 1
print(sort(rlq1$c1[,1], decreasing = TRUE))

#Environmental loadings on Axis 1
print(sort(rlq1$l1[,1], decreasing = TRUE))

#group centroids
group_centers <- aggregate(rlq1$lQ, by = list(spe.group2), FUN = mean)


#Looking closer at the groups----
for(trait in names(Q_matrix)) {
  cat("\n", trait, ":\n")
  print(table(spe.group2, Q_matrix[, trait]))
  cat("\n")
}

#as proportions (easier to compare)
for(trait in names(Q_matrix)) {
  cat("\n", trait, " (proportions):\n")
  print(round(prop.table(table(spe.group2, Q_matrix[, trait]), margin = 1), 2))
  cat("\n")
}

#Find most common value for each group
group_profiles <- data.frame(Group = levels(spe.group2))

for(trait in names(Q_matrix)) {
  modal_values <- tapply(Q_matrix[, trait], spe.group2, function(x) {
    names(sort(table(x), decreasing = TRUE))[1]  # Most common value
  })
  group_profiles[, trait] <- modal_values
}

group_profiles


#Create a heatmap of group-trait associations----

#Calculate proportions for each trait-group combination
heatmap_data <- data.frame()
for(trait in names(Q_matrix)) {
  prop_table <- prop.table(table(spe.group2, Q_matrix[, trait]), margin = 1)
  temp_df <- as.data.frame(prop_table)
  names(temp_df) <- c("Group", "Trait_Value", "Proportion")
  temp_df$Trait <- trait
  heatmap_data <- rbind(heatmap_data, temp_df)
}

str(heatmap_data)

str(heatmap_data)
levels(heatmap_data$Trait_Value)


heatmap_data$Trait_Value <- gsub("No_Size", "No Size", heatmap_data$Trait_Value)

colnames(heatmap_data)[2] <- "Traits"
colnames(heatmap_data)
str(heatmap_data)
colnames(heatmap_data)[4] <- "Functional_Group"

#Trying to get a better order for each functional group traits

heatmap_data <- heatmap_data %>%
  group_by(Functional_Group) %>%
  mutate(Traits = if(unique(Functional_Group) == "Size") {
    factor(Traits, levels = c(
      "Unknown","No Size","0-2.5mm","2.5-5mm","5-10mm", ">10mm"))
  } else {
    factor(Traits, levels = sort(unique(Traits)))
  }) %>%
  ungroup()

dev.new(height=10,width=15,dpi=80,pointsize=14,noRStudioGD = T)
ggplot(heatmap_data, aes(x = Group, y = Traits, fill = Proportion)) +
  geom_tile() +
  facet_wrap(~Functional_Group, scales = "free_y") +
  scale_fill_gradient(low = "white", high = "darkblue") + 
  theme_minimal(base_size = 16)


heatmap_labs <- c("Hunting_Style" = "a)",
                  "Order" = "b)",
                  "Size" = "c)",
                  'Trophic' = "d)")

heatmap_data$Traits <- gsub("Active_Hunting", "Active", heatmap_data$Traits)
heatmap_data$Traits <- gsub("Ambush_Hunters", "Ambush", heatmap_data$Traits)
heatmap_data$Traits <- gsub("Web_Building", "Web", heatmap_data$Traits)


dev.new(height=20,width=17,dpi=80,pointsize=14,noRStudioGD = T)
ggplot(heatmap_data, aes(x = Group, y = Traits, fill = Proportion)) +
  geom_tile() +
  facet_grid(Functional_Group ~ ., scales = "free_y", space = "free_y",
             labeller = as_labeller(heatmap_labs),
             switch = "y") +
  scale_fill_gradient(low = "white", high = "black") +
  theme_minimal(base_size = 16) +
  theme(strip.text.y.left = element_text(hjust = 0, vjust = 1, angle = 0),
        strip.placement = "outside")


#Extracting Trait Groups----

Trait_Group <- data.frame(Morphospecies = names(spe.group2), trait_group = spe.group2)
head(Trait_Group);dim(Trait_Group)

invert_trait_group <- merge(Trait_Group,invert_filtered, by = "Morphospecies")
head(invert_trait_group);dim(invert_trait_group)

#Creating Modelling data and calculating functional Diversity 

FDModel <- data.frame(Point = variables$Point)
head(FDModel);dim(FDModel)

##Richness----

#Trait Group A
TG_A <- invert_trait_group[invert_trait_group$trait_group=="A",]
head(TG_A);dim(TG_A)

FUNrichness_A <- aggregate(Morphospecies ~ Point, data = TG_A, FUN = function(x) length(unique(x)))
FDModel <- merge(FDModel,FUNrichness_A,by = "Point",all.x = T)
head(FDModel);dim(FDModel)
colnames(FDModel)[2] <- "A_Rich"
head(FDModel);dim(FDModel)

FDModel$A_Rich[is.na(FDModel$A_Rich)] <- 0

#Trait Group B
TG_B <- invert_trait_group[invert_trait_group$trait_group=="B",]
head(TG_B);dim(TG_B)

FUNrichness_B <- aggregate(Morphospecies ~ Point, data = TG_B, FUN = function(x) length(unique(x)))
FDModel <- merge(FDModel,FUNrichness_B,by = "Point",all.x = T)
head(FDModel);dim(FDModel)
colnames(FDModel)[3] <- "B_Rich"
head(FDModel);dim(FDModel)

FDModel$B_Rich[is.na(FDModel$B_Rich)] <- 0

#Trait Group C
TG_C <- invert_trait_group[invert_trait_group$trait_group=="C",]
head(TG_C);dim(TG_C)

FUNrichness_C <- aggregate(Morphospecies ~ Point, data = TG_C, FUN = function(x) length(unique(x)))
FDModel <- merge(FDModel,FUNrichness_C,by = "Point",all.x = T)
head(FDModel);dim(FDModel)
colnames(FDModel)[4] <- "C_Rich"

FDModel$C_Rich[is.na(FDModel$C_Rich)] <- 0
head(FDModel);dim(FDModel)

#Trait Group D
TG_D <- invert_trait_group[invert_trait_group$trait_group=="D",]
head(TG_D);dim(TG_D)

FUNrichness_D <- aggregate(Morphospecies ~ Point, data = TG_D, FUN = function(x) length(unique(x)))
FDModel <- merge(FDModel,FUNrichness_D,by = "Point",all.x = T)
head(FDModel);dim(FDModel)
colnames(FDModel)[5] <- "D_Rich"

FDModel$D_Rich[is.na(FDModel$D_Rich)] <- 0
head(FDModel);dim(FDModel)

#Trait Group E
TG_E <- invert_trait_group[invert_trait_group$trait_group=="E",]
head(TG_E);dim(TG_E)

FUNrichness_E <- aggregate(Morphospecies ~ Point, data = TG_E, FUN = function(x) length(unique(x)))
FDModel <- merge(FDModel,FUNrichness_E,by = "Point",all.x = T)
head(FDModel);dim(FDModel)
colnames(FDModel)[6] <- "E_Rich"

FDModel$E_Rich[is.na(FDModel$E_Rich)] <- 0
head(FDModel);dim(FDModel)

#Trait Group F
TG_F <- invert_trait_group[invert_trait_group$trait_group=="F",]
head(TG_F);dim(TG_F)

FUNrichness_F <- aggregate(Morphospecies ~ Point, data = TG_F, FUN = function(x) length(unique(x)))
FDModel <- merge(FDModel,FUNrichness_F,by = "Point",all.x = T)
head(FDModel);dim(FDModel)
colnames(FDModel)[7] <- "F_Rich"

FDModel$F_Rich[is.na(FDModel$F_Rich)] <- 0
head(FDModel);dim(FDModel)

#Trait Group G
TG_G <- invert_trait_group[invert_trait_group$trait_group=="G",]
head(TG_G);dim(TG_G)

FUNrichness_G <- aggregate(Morphospecies ~ Point, data = TG_G, FUN = function(x) length(unique(x)))
FDModel <- merge(FDModel,FUNrichness_G,by = "Point",all.x = T)
head(FDModel);dim(FDModel)
colnames(FDModel)[8] <- "G_Rich"

FDModel$G_Rich[is.na(FDModel$G_Rich)] <- 0
head(FDModel);dim(FDModel)

#Trait Group H
TG_H <- invert_trait_group[invert_trait_group$trait_group=="H",]
head(TG_H);dim(TG_H)

FUNrichness_H <- aggregate(Morphospecies ~ Point, data = TG_H, FUN = function(x) length(unique(x)))
FDModel <- merge(FDModel,FUNrichness_H,by = "Point",all.x = T)
head(FDModel);dim(FDModel)
colnames(FDModel)[9] <- "H_Rich"

FDModel$H_Rich[is.na(FDModel$H_Rich)] <- 0
head(FDModel);dim(FDModel)

#Trait Group I
TG_I <- invert_trait_group[invert_trait_group$trait_group=="I",]
head(TG_I);dim(TG_I)

FUNrichness_I <- aggregate(Morphospecies ~ Point, data = TG_I, FUN = function(x) length(unique(x)))
FDModel <- merge(FDModel,FUNrichness_I,by = "Point",all.x = T)
head(FDModel);dim(FDModel)
colnames(FDModel)[10] <- "I_Rich"

FDModel$I_Rich[is.na(FDModel$I_Rich)] <- 0
head(FDModel);dim(FDModel)


#Now we check proportion of 0's

lapply(FDModel[,2:10], function(x){length(which(x==0))/length(x)})

#All groups except Group D are binomial

#update to pres/abs
FDModel$A_Rich <- ifelse(FDModel$A_Rich>0, yes = 1,no = 0)
FDModel$B_Rich <- ifelse(FDModel$B_Rich>0, yes = 1,no = 0)
FDModel$C_Rich <- ifelse(FDModel$C_Rich>0, yes = 1,no = 0)
FDModel$E_Rich <- ifelse(FDModel$E_Rich>0, yes = 1,no = 0)
FDModel$F_Rich <- ifelse(FDModel$F_Rich>0, yes = 1,no = 0)
FDModel$G_Rich <- ifelse(FDModel$G_Rich>0, yes = 1,no = 0)
FDModel$H_Rich <- ifelse(FDModel$H_Rich>0, yes = 1,no = 0)
FDModel$I_Rich <- ifelse(FDModel$I_Rich>0, yes = 1,no = 0)

colnames(FDModel)[2] <- "TG_A"
colnames(FDModel)[3] <- "TG_B"
colnames(FDModel)[4] <- "TG_C"
colnames(FDModel)[6] <- "TG_E"
colnames(FDModel)[7] <- "TG_F"
colnames(FDModel)[8] <- "TG_G"
colnames(FDModel)[9] <- "TG_H"
colnames(FDModel)[10] <- "TG_I"
head(FDModel);dim(FDModel)

#Diversity----


TG_D2 <- invert_trait_group[invert_trait_group$trait_group=="D",]
  
FUNdiversity_D <- aggregate(Morphospecies ~ Point, data = TG_D2, FUN = function(x) diversity(table(x), index = "invsimpson"))
  
FDModel <- merge(FDModel,FUNdiversity_D,by = "Point",all.x = T)
  
head(FDModel);dim(FDModel)

colnames(FDModel)[11] <- "D_Div"

FDModel$D_Div[is.na(FDModel$D_Div)] <- 0.00001


#Finally add variables----

FDModel <- merge(FDModel,variables,by = "Point")
head(FDModel);dim(FDModel)


