--
--
##**Title: Eastern Newt Vertebrae Analysis
--
  
library(geomorph) 
library(SlicerMorphR) 
library(shapes)
library(ellipsis)
library(ggplot2)
library(tidyverse)
library(MASS)
library(caret)
library(patchwork)
library(knitr)
library(ggpubr)


##**Data Import and Format**

lifestage<-factor(c("Adult", "Adult", "Adult", "Adult", "Adult", "Adult",  
                    "Eft", "Eft", "Eft", "Eft", "Eft", "Eft", "Eft",  
                    "Adult", "Adult", "Adult", "Eft", "Eft", "Adult", "Adult",  
                    "Juvenile", "Juvenile", "Juvenile", "Juvenile", "Paedomorph", 
                    "Paedomorph","Paedomorph","Paedomorph","Paedomorph","Juvenile", 
                    "Adult", "Adult", "Juvenile",	"Juvenile","Juvenile","Juvenile",	
                    "Juvenile","Juvenile","Paedomorph","Paedomorph","Paedomorph",	
                    "Paedomorph","Paedomorph"))

subspecies<-factor(c("N. v. viridescens", "N. v. viridescens", "N. v. viridescens", "N. v. viridescens",
                     "N. v. viridescens", "N. v. viridescens", "N. v. viridescens", "N. v. viridescens",
                     "N. v. viridescens", "N. v. viridescens", "N. v. viridescens", "N. v. viridescens",
                     "N. v. viridescens", "N. v. viridescens", "N. v. viridescens", "N. v. viridescens",
                     "N. v. viridescens", "N. v. viridescens", "N. v. viridescens", "N. v. viridescens", 
                     "N. v. louisianensis", "N. v. louisianensis", "N. v. louisianensis", "N. v. louisianensis",
                     "N. v. louisianensis", "N. v. louisianensis", "N. v. louisianensis", "N. v. louisianensis",
                     "N. v. louisianensis", "N. v. louisianensis", "N. v. viridescens", "N. v. viridescens", 
                     "N. v. louisianensis", "N. v. louisianensis", "N. v. louisianensis", "N. v. louisianensis", 
                     "N. v. louisianensis", "N. v. louisianensis", "N. v. piaropicola", "N. v. piaropicola", 
                     "N. v. piaropicola", "N. v. piaropicola", "N. v. piaropicola"))

#Caudal 2 has 2 specimens (ID28 & ID45) with haemal arch missing. Caudal 2 Analysis will have it's own 'lifestage' factor

Caud2lifestage<-factor(c("Adult", "Adult", "Adult", "Adult", "Adult", "Adult",  
                         "Eft", "Eft", "Eft", "Eft", "Eft", "Eft", "Eft",  
                         "Adult", "Adult", "Adult", "Eft", "Eft", "Adult", "Adult",  
                         "Juvenile", "Juvenile", "Juvenile", "Juvenile", "Paedomorph", 
                         "Paedomorph","Paedomorph","Paedomorph","Juvenile", 
                         "Adult", "Adult", "Juvenile",	"Juvenile","Juvenile","Juvenile",	
                         "Juvenile","Juvenile","Paedomorph","Paedomorph","Paedomorph",	
                         "Paedomorph"))

Caud2subspecies<-factor(c("N. v. viridescens", "N. v. viridescens", "N. v. viridescens", "N. v. viridescens",
                          "N. v. viridescens", "N. v. viridescens", "N. v. viridescens", "N. v. viridescens",
                          "N. v. viridescens", "N. v. viridescens", "N. v. viridescens", "N. v. viridescens",
                          "N. v. viridescens", "N. v. viridescens", "N. v. viridescens", "N. v. viridescens",
                          "N. v. viridescens", "N. v. viridescens", "N. v. viridescens", "N. v. viridescens", 
                          "N. v. louisianensis", "N. v. louisianensis", "N. v. louisianensis", "N. v. louisianensis",
                          "N. v. louisianensis", "N. v. louisianensis", "N. v. louisianensis", "N. v. louisianensis",
                          "N. v. louisianensis", "N. v. viridescens", "N. v. viridescens", 
                          "N. v. louisianensis", "N. v. louisianensis", "N. v. louisianensis", "N. v. louisianensis", 
                          "N. v. louisianensis", "N. v. louisianensis", "N. v. piaropicola", "N. v. piaropicola", 
                          "N. v. piaropicola", "N. v. piaropicola"))


#Atlas
setwd("F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/Oriented Fixed Landmarks/Atlas - Oriented")
files=dir(patt='json')
Atlas.Samples= gsub(".mrk.json", '', fixed=T, files)

Atlas.LMs=array(dim=c(13,3,43))
for (i in 1:43) Atlas.LMs[,,i] = read.markups.json(files[i])
dimnames(Atlas.LMs) = list(paste0("LM_",1:13), c("x", "y", "z"), Atlas.Samples)

#T1
setwd("F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/Oriented Fixed Landmarks/T1 - Oriented")
files=dir(patt='json')
T1.Samples= gsub(".mrk.json", '', fixed=T, files)

T1.LMs=array(dim=c(19,3,43))
for (i in 1:43) T1.LMs[,,i] = read.markups.json(files[i])
dimnames(T1.LMs) = list(paste0("LM_",1:19), c("x", "y", "z"), T1.Samples)

#T4
setwd("F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/Oriented Fixed Landmarks/T4 - Oriented")
files=dir(patt='json')
T4.Samples= gsub(".mrk.json", '', fixed=T, files)

T4.LMs=array(dim=c(19,3,43))
for (i in 1:43) T4.LMs[,,i] = read.markups.json(files[i])
dimnames(T4.LMs) = list(paste0("LM_",1:19), c("x", "y", "z"), T4.Samples)

#T7
setwd("F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/Oriented Fixed Landmarks/T7 - Oriented")
files=dir(patt='json')
T7.Samples= gsub(".mrk.json", '', fixed=T, files)

T7.LMs=array(dim=c(19,3,43))
for (i in 1:43) T7.LMs[,,i] = read.markups.json(files[i])
dimnames(T7.LMs) = list(paste0("LM_",1:19), c("x", "y", "z"), T7.Samples)

#T10
setwd("F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/Oriented Fixed Landmarks/T10 - Oriented")
files=dir(patt='json')
T10.Samples= gsub(".mrk.json", '', fixed=T, files)

T10.LMs=array(dim=c(19,3,43))
for (i in 1:43) T10.LMs[,,i] = read.markups.json(files[i])
dimnames(T10.LMs) = list(paste0("LM_",1:19), c("x", "y", "z"), T10.Samples)

#T13
setwd("F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/Oriented Fixed Landmarks/T13 - Oriented")
files=dir(patt='json')
T13.Samples= gsub(".mrk.json", '', fixed=T, files)

T13.LMs=array(dim=c(19,3,43))
for (i in 1:43) T13.LMs[,,i] = read.markups.json(files[i])
dimnames(T13.LMs) = list(paste0("LM_",1:19), c("x", "y", "z"), T13.Samples)

#Sacral
setwd("F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/Oriented Fixed Landmarks/Sacral - Oriented")
files=dir(patt='json')
Sacral.Samples= gsub(".mrk.json", '', fixed=T, files)

Sacral.LMs=array(dim=c(19,3,43))
for (i in 1:43) Sacral.LMs[,,i] = read.markups.json(files[i])
dimnames(Sacral.LMs) = list(paste0("LM_",1:19), c("x", "y", "z"), Sacral.Samples)

#Caudal 1
setwd("F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/Oriented Fixed Landmarks/Caud1 - Oriented")
files=dir(patt='json')
Caud1.Samples= gsub(".mrk.json", '', fixed=T, files)

Caud1.LMs=array(dim=c(19,3,43))
for (i in 1:43) Caud1.LMs[,,i] = read.markups.json(files[i])
dimnames(Caud1.LMs) = list(paste0("LM_",1:19), c("x", "y", "z"), Caud1.Samples)

#Caudal 2
setwd("F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/Oriented Fixed Landmarks/Caud2 - Oriented")
files=dir(patt='json')
Caud2.Samples= gsub(".mrk.json", '', fixed=T, files)

Caud2.LMs=array(dim=c(22,3,41))
for (i in 1:41) Caud2.LMs[,,i] = read.markups.json(files[i])
dimnames(Caud2.LMs) = list(paste0("LM_",1:22), c("x", "y", "z"), Caud2.Samples)

#Caudal 3
setwd("F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/Oriented Fixed Landmarks/Caud3 - Oriented")
files=dir(patt='json')
Caud3.Samples= gsub(".mrk.json", '', fixed=T, files)

Caud3.LMs=array(dim=c(22,3,43))
for (i in 1:43) Caud3.LMs[,,i] = read.markups.json(files[i])
dimnames(Caud3.LMs) = list(paste0("LM_",1:22), c("x", "y", "z"), Caud3.Samples)


##**Analyses**

#Generalized Procrustes Analysis (GPA) + Format - Align coordinates and format into a geomorph data frame
Atlas.gpa <- gpagen(Atlas.LMs)
Atlas_gdf <- geomorph.data.frame(Shape=Atlas.gpa$coords, ind=Atlas.Samples, Size=log(Atlas.gpa$Csize), lifestage=lifestage)

T1.gpa <- gpagen(T1.LMs)
T1_gdf <- geomorph.data.frame(Shape=T1.gpa$coords, ind=T1.Samples, Size=log(T1.gpa$Csize), lifestage=lifestage)

T4.gpa <- gpagen(T4.LMs)
T4_gdf <- geomorph.data.frame(Shape=T4.gpa$coords, ind=T4.Samples, Size=log(T4.gpa$Csize), lifestage=lifestage)

T7.gpa <- gpagen(T7.LMs)
T7_gdf <- geomorph.data.frame(Shape=T7.gpa$coords, ind=T7.Samples, Size=log(T7.gpa$Csize), lifestage=lifestage)

T10.gpa <- gpagen(T10.LMs)
T10_gdf <- geomorph.data.frame(Shape=T10.gpa$coords, ind=T10.Samples, Size=log(T10.gpa$Csize), lifestage=lifestage)

T13.gpa <- gpagen(T13.LMs)
T13_gdf <- geomorph.data.frame(Shape=T13.gpa$coords, ind=T13.Samples, Size=log(T13.gpa$Csize), lifestage=lifestage)

Sacral.gpa <- gpagen(Sacral.LMs)
Sacral_gdf <- geomorph.data.frame(Shape=Sacral.gpa$coords, ind=Sacral.Samples, Size=log(Sacral.gpa$Csize), lifestage=lifestage)

Caud1.gpa <- gpagen(Caud1.LMs)
Caud1_gdf <- geomorph.data.frame(Shape=Caud1.gpa$coords, ind=Caud1.Samples, Size=log(Caud1.gpa$Csize), lifestage=lifestage)

Caud2.gpa <- gpagen(Caud2.LMs)
Caud2_gdf <- geomorph.data.frame(Shape=Caud2.gpa$coords, ind=Caud2.Samples, Size=log(Caud2.gpa$Csize), lifestage=Caud2lifestage)

Caud3.gpa <- gpagen(Caud3.LMs)
Caud3_gdf <- geomorph.data.frame(Shape=Caud3.gpa$coords, ind=Caud3.Samples, Size=log(Caud3.gpa$Csize), lifestage=lifestage)


#Principal Component Analysis (PCA) - Ordinate the dataset
Atlas.PCA<-gm.prcomp(Atlas.gpa$coords)
summary(Atlas.PCA) 

T1.PCA<-gm.prcomp(T1.gpa$coords)
summary(T1.PCA)

T4.PCA<-gm.prcomp(T4.gpa$coords)
summary(T4.PCA) 

T7.PCA<-gm.prcomp(T7.gpa$coords)
summary(T7.PCA) 

T10.PCA<-gm.prcomp(T10.gpa$coords)
summary(T10.PCA) 

T13.PCA<-gm.prcomp(T13.gpa$coords)
summary(T13.PCA) 

Sacral.PCA<-gm.prcomp(Sacral.gpa$coords)
summary(Sacral.PCA)

Caud1.PCA<-gm.prcomp(Caud1.gpa$coords)
summary(Caud1.PCA) 

Caud2.PCA<-gm.prcomp(Caud2.gpa$coords)
summary(Caud2.PCA) 

Caud3.PCA<-gm.prcomp(Caud3.gpa$coords)
summary(Caud3.PCA) 


#Procrustes ANOVA

Atlas_proc_ANOVA_GDF<-geomorph.data.frame(Shape=Atlas.gpa$coords, ind=Atlas.Samples, Size=log(Atlas.gpa$Csize), lifestage=lifestage, subspecies=subspecies)
Atlas_proc_ANOVA<-procD.lm(Shape~lifestage*subspecies+Size, data=Atlas_proc_ANOVA_GDF, iter = 9999, RRPP = TRUE, print.progress = FALSE)
anova(Atlas_proc_ANOVA)

T1_proc_ANOVA_GDF<-geomorph.data.frame(Shape=T1.gpa$coords, ind=T1.Samples, Size=log(T1.gpa$Csize), lifestage=lifestage, subspecies=subspecies)
T1_proc_ANOVA<-procD.lm(Shape~lifestage*subspecies+Size, data=T1_proc_ANOVA_GDF, iter = 9999, RRPP = TRUE, print.progress = FALSE)
anova(T1_proc_ANOVA)

T4_proc_ANOVA_GDF<-geomorph.data.frame(Shape=T4.gpa$coords, ind=T4.Samples, Size=log(T4.gpa$Csize), lifestage=lifestage, subspecies=subspecies)
T4_proc_ANOVA<-procD.lm(Shape~lifestage*subspecies+Size, data=T4_proc_ANOVA_GDF, iter = 9999, RRPP = TRUE, print.progress = FALSE)
anova(T4_proc_ANOVA)

T7_proc_ANOVA_GDF<-geomorph.data.frame(Shape=T7.gpa$coords, ind=T7.Samples, Size=log(T7.gpa$Csize), lifestage=lifestage, subspecies=subspecies)
T7_proc_ANOVA<-procD.lm(Shape~lifestage*subspecies+Size, data=T7_proc_ANOVA_GDF, iter = 9999, RRPP = TRUE, print.progress = FALSE)
anova(T7_proc_ANOVA)

T10_proc_ANOVA_GDF<-geomorph.data.frame(Shape=T10.gpa$coords, ind=T10.Samples, Size=log(T10.gpa$Csize), lifestage=lifestage, subspecies=subspecies)
T10_proc_ANOVA<-procD.lm(Shape~lifestage*subspecies+Size, data=T10_proc_ANOVA_GDF, iter = 9999, RRPP = TRUE, print.progress = FALSE)
anova(T10_proc_ANOVA)

T13_proc_ANOVA_GDF<-geomorph.data.frame(Shape=T13.gpa$coords, ind=T13.Samples, Size=log(T13.gpa$Csize), lifestage=lifestage, subspecies=subspecies)
T13_proc_ANOVA<-procD.lm(Shape~lifestage*subspecies+Size, data=T13_proc_ANOVA_GDF, iter = 9999, RRPP = TRUE, print.progress = FALSE)
anova(T13_proc_ANOVA)

Sacral_proc_ANOVA_GDF<-geomorph.data.frame(Shape=Sacral.gpa$coords, ind=Sacral.Samples, Size=log(Sacral.gpa$Csize), lifestage=lifestage, subspecies=subspecies)
Sacral_proc_ANOVA<-procD.lm(Shape~lifestage*subspecies+Size, data=Sacral_proc_ANOVA_GDF, iter = 9999, RRPP = TRUE, print.progress = FALSE)
anova(Sacral_proc_ANOVA)

Caud1_proc_ANOVA_GDF<-geomorph.data.frame(Shape=Caud1.gpa$coords, ind=Caud1.Samples, Size=log(Caud1.gpa$Csize), lifestage=lifestage, subspecies=subspecies)
Caud1_proc_ANOVA<-procD.lm(Shape~lifestage*subspecies+Size, data=Caud1_proc_ANOVA_GDF, iter = 9999, RRPP = TRUE, print.progress = FALSE)
anova(Caud1_proc_ANOVA)

Caud2_proc_ANOVA_GDF<-geomorph.data.frame(Shape=Caud2.gpa$coords, ind=Caud2.Samples, Size=log(Caud2.gpa$Csize), lifestage=Caud2lifestage, subspecies=Caud2subspecies)
Caud2_proc_ANOVA<-procD.lm(Shape~lifestage*subspecies+Size, data=Caud2_proc_ANOVA_GDF, iter = 9999, RRPP = TRUE, print.progress = FALSE)
anova(Caud2_proc_ANOVA)

Caud3_proc_ANOVA_GDF<-geomorph.data.frame(Shape=Caud3.gpa$coords, ind=Caud3.Samples, Size=log(Caud3.gpa$Csize), lifestage=lifestage, subspecies=subspecies)
Caud3_proc_ANOVA<-procD.lm(Shape~lifestage*subspecies+Size, data=Caud3_proc_ANOVA_GDF, iter = 9999, RRPP = TRUE, print.progress = FALSE)
anova(Caud3_proc_ANOVA)


#Canonical Variate Analysis (CVA) + Leave One Out Cross Validation (LOOCV)

#Atlas

#Store PC Scores and format
Atlas_PCA_loadings <- Atlas.PCA$rotation
Atlas_PCA_scores <- Atlas.PCA$x
Atlas_pdf<-data.frame(Atlas_PCA_scores, ind=Atlas.Samples, Size=log(Atlas.gpa$Csize), lifestage=lifestage)

#95% Cumulative variance ~95% at PC Score 13
Atlas_LDA<-lda(lifestage~.,data=Atlas_pdf[,c(1:13, 34,35)])
Atlas_LDA_predicted<-predict(Atlas_LDA, Atlas_pdf[,c(1:13,34,35)])

Atlas_ctrl <- trainControl(method = "LOOCV")
Atlas_LDA_LOOCV <- train(lifestage ~ ., data = Atlas_pdf[, c(1:13, 34, 35)], method = "lda", trControl = Atlas_ctrl)
Atlas_LDA_LOOCV_cm <-confusionMatrix(Atlas_LDA_LOOCV$pred$pred, Atlas_LDA_LOOCV$pred$obs)
Atlas_LDA_LOOCV_cm

Atlas_LDA_LOOCV_cm_table<-as.table(Atlas_LDA_LOOCV_cm)
Atlas_LDA_LOOCV_cm_table


#Trunk 1
T1_PCA_loadings <- T1.PCA$rotation
T1_PCA_scores <- T1.PCA$x
T1_pdf<-data.frame(T1_PCA_scores, ind=T1.Samples, Size=log(T1.gpa$Csize), lifestage=lifestage)
T1_LDA<-lda(lifestage~.,data=T1_pdf[,c(1:19, 44,45)])
T1_LDA_predicted<-predict(T1_LDA, T1_pdf[,c(1:19,44,45)])

T1_ctrl <- trainControl(method = "LOOCV")
T1_LDA_LOOCV <- train(lifestage ~ ., data = T1_pdf[, c(1:19, 44, 45)], method = "lda", trControl = T1_ctrl)
T1_LDA_LOOCV_cm <-confusionMatrix(T1_LDA_LOOCV$pred$pred, T1_LDA_LOOCV$pred$obs)
T1_LDA_LOOCV_cm
T1_LDA_LOOCV_cm_table<-as.table(T1_LDA_LOOCV_cm)
T1_LDA_LOOCV_cm_table


#Trunk 4
T4_PCA_loadings <- T4.PCA$rotation
T4_PCA_scores <- T4.PCA$x
T4_pdf<-data.frame(T4_PCA_scores, ind=T4.Samples, Size=log(T4.gpa$Csize), lifestage=lifestage)
T4_LDA<-lda(lifestage~.,data=T4_pdf[,c(1:18, 44,45)])
T4_LDA_predicted<-predict(T4_LDA, T4_pdf[,c(1:18,44,45)])

T4_ctrl <- trainControl(method = "LOOCV")
T4_LDA_LOOCV <- train(lifestage ~ ., data = T4_pdf[, c(1:18, 44, 45)], method = "lda", trControl = T4_ctrl)
T4_LDA_LOOCV_cm <-confusionMatrix(T4_LDA_LOOCV$pred$pred, T4_LDA_LOOCV$pred$obs)
T4_LDA_LOOCV_cm
T4_LDA_LOOCV_cm_table<-as.table(T4_LDA_LOOCV_cm)
T4_LDA_LOOCV_cm_table


#Trunk 7
T7_PCA_loadings <- T7.PCA$rotation
T7_PCA_scores <- T7.PCA$x
T7_pdf<-data.frame(T7_PCA_scores, ind=T7.Samples, Size=log(T7.gpa$Csize), lifestage=lifestage)
T7_LDA<-lda(lifestage~.,data=T7_pdf[,c(1:15, 44,45)])
T7_LDA_predicted<-predict(T7_LDA, T7_pdf[,c(1:15,44,45)])

T7_ctrl <- trainControl(method = "LOOCV")
T7_LDA_LOOCV <- train(lifestage ~ ., data = T7_pdf[, c(1:15, 44, 45)], method = "lda", trControl = T7_ctrl)
T7_LDA_LOOCV_cm <-confusionMatrix(T7_LDA_LOOCV$pred$pred, T7_LDA_LOOCV$pred$obs)
T7_LDA_LOOCV_cm
T7_LDA_LOOCV_cm_table<-as.table(T7_LDA_LOOCV_cm)
T7_LDA_LOOCV_cm_table


#Trunk 10
T10_PCA_loadings <- T10.PCA$rotation
T10_PCA_scores <- T10.PCA$x
T10_pdf<-data.frame(T10_PCA_scores, ind=T10.Samples, Size=log(T10.gpa$Csize), lifestage=lifestage)
T10_LDA<-lda(lifestage~.,data=T10_pdf[,c(1:15, 44,45)])
T10_LDA_predicted<-predict(T10_LDA, T10_pdf[,c(1:15,44,45)])

T10_ctrl <- trainControl(method = "LOOCV")
T10_LDA_LOOCV <- train(lifestage ~ ., data = T10_pdf[, c(1:15, 44, 45)], method = "lda", trControl = T10_ctrl)
T10_LDA_LOOCV_cm <-confusionMatrix(T10_LDA_LOOCV$pred$pred, T10_LDA_LOOCV$pred$obs)
T10_LDA_LOOCV_cm
T10_LDA_LOOCV_cm_table<-as.table(T10_LDA_LOOCV_cm)
T10_LDA_LOOCV_cm_table


#Trunk 13
T13_PCA_loadings <- T13.PCA$rotation
T13_PCA_scores <- T13.PCA$x
T13_pdf<-data.frame(T13_PCA_scores, ind=T13.Samples, Size=log(T13.gpa$Csize), lifestage=lifestage)
T13_LDA<-lda(lifestage~.,data=T13_pdf[,c(1:16, 44,45)])
T13_LDA_predicted<-predict(T13_LDA, T13_pdf[,c(1:16,44,45)])

T13_ctrl <- trainControl(method = "LOOCV")
T13_LDA_LOOCV <- train(lifestage ~ ., data = T13_pdf[, c(1:16, 44, 45)], method = "lda", trControl = T13_ctrl)
T13_LDA_LOOCV_cm <-confusionMatrix(T13_LDA_LOOCV$pred$pred, T13_LDA_LOOCV$pred$obs)
T13_LDA_LOOCV_cm
T13_LDA_LOOCV_cm_table<-as.table(T13_LDA_LOOCV_cm)
T13_LDA_LOOCV_cm_table


#Sacral
Sacral_PCA_loadings <- Sacral.PCA$rotation
Sacral_PCA_scores <- Sacral.PCA$x
Sacral_pdf<-data.frame(Sacral_PCA_scores, ind=Sacral.Samples, Size=log(Sacral.gpa$Csize), lifestage=lifestage)
Sacral_LDA<-lda(lifestage~.,data=Sacral_pdf[,c(1:17, 44,45)])
Sacral_LDA_predicted<-predict(Sacral_LDA, Sacral_pdf[,c(1:17,44,45)])

Sacral_ctrl <- trainControl(method = "LOOCV")
Sacral_LDA_LOOCV <- train(lifestage ~ ., data = Sacral_pdf[, c(1:17, 44, 45)], method = "lda", trControl = Sacral_ctrl)
Sacral_LDA_LOOCV_cm <-confusionMatrix(Sacral_LDA_LOOCV$pred$pred, Sacral_LDA_LOOCV$pred$obs)
Sacral_LDA_LOOCV_cm
Sacral_LDA_LOOCV_cm_table<-as.table(Sacral_LDA_LOOCV_cm)
Sacral_LDA_LOOCV_cm_table


#Caudal 1
Caud1_PCA_loadings <- Caud1.PCA$rotation
Caud1_PCA_scores <- Caud1.PCA$x
Caud1_pdf<-data.frame(Caud1_PCA_scores, ind=Caud1.Samples, Size=log(Caud1.gpa$Csize), lifestage=lifestage)
Caud1_LDA<-lda(lifestage~.,data=Caud1_pdf[,c(1:17, 44,45)])
Caud1_LDA_predicted<-predict(Caud1_LDA, Caud1_pdf[,c(1:17,44,45)])

Caud1_ctrl <- trainControl(method = "LOOCV")
Caud1_LDA_LOOCV <- train(lifestage ~ ., data = Caud1_pdf[, c(1:17, 44, 45)], method = "lda", trControl = Caud1_ctrl)
Caud1_LDA_LOOCV_cm <-confusionMatrix(Caud1_LDA_LOOCV$pred$pred, Caud1_LDA_LOOCV$pred$obs)
Caud1_LDA_LOOCV_cm
Caud1_LDA_LOOCV_cm_table<-as.table(Caud1_LDA_LOOCV_cm)
Caud1_LDA_LOOCV_cm_table

#Caudal 2
Caud2_PCA_loadings <- Caud2.PCA$rotation
Caud2_PCA_scores <- Caud2.PCA$x
Caud2_pdf<-data.frame(Caud2_PCA_scores, ind=Caud2.Samples, Size=log(Caud2.gpa$Csize), lifestage=Caud2lifestage)
Caud2_LDA<-lda(lifestage~.,data=Caud2_pdf[,c(1:15, 42,43)])
Caud2_LDA_predicted<-predict(Caud2_LDA, Caud2_pdf[,c(1:15,42,43)])

Caud2_ctrl <- trainControl(method = "LOOCV")
Caud2_LDA_LOOCV <- train(lifestage ~ ., data = Caud2_pdf[, c(1:15, 42, 43)], method = "lda", trControl = Caud2_ctrl)
Caud2_LDA_LOOCV_cm <-confusionMatrix(Caud2_LDA_LOOCV$pred$pred, Caud2_LDA_LOOCV$pred$obs)
Caud2_LDA_LOOCV_cm
Caud2_LDA_LOOCV_cm_table<-as.table(Caud2_LDA_LOOCV_cm)
Caud2_LDA_LOOCV_cm_table

#Caudal 3
Caud3_PCA_loadings <- Caud3.PCA$rotation
Caud3_PCA_scores <- Caud3.PCA$x
Caud3_pdf<-data.frame(Caud3_PCA_scores, ind=Caud3.Samples, Size=log(Caud3.gpa$Csize), lifestage=lifestage)
Caud3_LDA<-lda(lifestage~.,data=Caud3_pdf[,c(1:15, 44,45)])
Caud3_LDA_predicted<-predict(Caud3_LDA, Caud3_pdf[,c(1:15,44,45)])

Caud3_ctrl <- trainControl(method = "LOOCV")
Caud3_LDA_LOOCV <- train(lifestage ~ ., data = Caud3_pdf[, c(1:15, 44, 45)], method = "lda", trControl = Caud3_ctrl)
Caud3_LDA_LOOCV_cm <-confusionMatrix(Caud3_LDA_LOOCV$pred$pred, Caud3_LDA_LOOCV$pred$obs)
Caud3_LDA_LOOCV_cm

Caud3_LDA_LOOCV_cm_table<-as.table(Caud3_LDA_LOOCV_cm)
Caud3_LDA_LOOCV_cm_table



#Morphological Disparity - Calculate Partial Disparities

Atlas_m.d<-morphol.disparity(Atlas_gdf$Shape~1, groups=lifestage, partial=TRUE, data=Atlas_gdf, iter=9999, print.progress = FALSE)
Atlas_m.d
Atlas_m.d_groups<-Atlas_m.d$Procrustes.var
Atlas_m.d_groups

T1_m.d<-morphol.disparity(T1_gdf$Shape~1, groups=lifestage, partial=TRUE, data=T1_gdf, iter=9999, print.progress = FALSE)
T1_m.d
T1_m.d_groups<-T1_m.d$Procrustes.var
T1_m.d_groups

T4_m.d<-morphol.disparity(T4_gdf$Shape~1, groups=lifestage, partial=TRUE, data=T4_gdf, iter=9999, print.progress = FALSE)
T4_m.d
T4_m.d_groups<-T4_m.d$Procrustes.var
T4_m.d_groups

T7_m.d<-morphol.disparity(T7_gdf$Shape~1, groups=lifestage, partial=TRUE, data=T7_gdf, iter=9999, print.progress = FALSE)
T7_m.d
T7_m.d_groups<-T7_m.d$Procrustes.var
T7_m.d_groups

T10_m.d<-morphol.disparity(T10_gdf$Shape~1, groups=lifestage, partial=TRUE, data=T10_gdf, iter=9999, print.progress = FALSE)
T10_m.d
T10_m.d_groups<-T10_m.d$Procrustes.var
T10_m.d_groups

T13_m.d<-morphol.disparity(T13_gdf$Shape~1, groups=lifestage, partial=TRUE, data=T13_gdf, iter=9999, print.progress = FALSE)
T13_m.d
T13_m.d_groups<-T13_m.d$Procrustes.var
T13_m.d_groups

Sacral_m.d<-morphol.disparity(Sacral_gdf$Shape~1, groups=lifestage, partial=TRUE, data=Sacral_gdf, iter=9999, print.progress = FALSE)
Sacral_m.d
Sacral_m.d_groups<-Sacral_m.d$Procrustes.var
Sacral_m.d_groups

Caud1_m.d<-morphol.disparity(Caud1_gdf$Shape~1, groups=lifestage, partial=TRUE, data=Caud1_gdf, iter=9999, print.progress = FALSE)
Caud1_m.d
Caud1_m.d_groups<-Caud1_m.d$Procrustes.var
Caud1_m.d_groups

Caud2_m.d<-morphol.disparity(Caud2_gdf$Shape~1, groups=Caud2lifestage, partial=TRUE, data=Caud2_gdf, iter=9999, print.progress = FALSE)
Caud2_m.d
Caud2_m.d_groups<-Caud2_m.d$Procrustes.var
Caud2_m.d_groups

Caud3_m.d<-morphol.disparity(Caud3_gdf$Shape~1, groups=lifestage, partial=TRUE, data=Caud3_gdf, iter=9999, print.progress = FALSE)
Caud3_m.d
Caud3_m.d_groups<-Caud3_m.d$Procrustes.var
Caud3_m.d_groups



#Total Morphological Disparity
Atlas_m.d<-morphol.disparity(Atlas_gdf$Shape~1, groups=NULL, data=Atlas_gdf, iter=9999, print.progress = FALSE)
Atlas_m.d

T1_m.d<-morphol.disparity(T1_gdf$Shape~1, groups=NULL, data=T1_gdf, iter=9999, print.progress = FALSE)
T1_m.d

T4_m.d<-morphol.disparity(T4_gdf$Shape~1, groups=NULL, data=T4_gdf, iter=9999, print.progress = FALSE)
T4_m.d

T7_m.d<-morphol.disparity(T7_gdf$Shape~1, groups=NULL, data=T7_gdf, iter=9999, print.progress = FALSE)
T7_m.d

T10_m.d<-morphol.disparity(T10_gdf$Shape~1, groups=NULL, data=T10_gdf, iter=9999, print.progress = FALSE)
T10_m.d

T13_m.d<-morphol.disparity(T13_gdf$Shape~1, groups=NULL, data=T13_gdf, iter=9999, print.progress = FALSE)
T13_m.d

Sacral_m.d<-morphol.disparity(Sacral_gdf$Shape~1, groups=NULL, data=Sacral_gdf, iter=9999, print.progress = FALSE)
Sacral_m.d

Caud1_m.d<-morphol.disparity(Caud1_gdf$Shape~1, groups=NULL, data=Caud1_gdf, iter=9999, print.progress = FALSE)
Caud1_m.d

Caud2_m.d<-morphol.disparity(Caud2_gdf$Shape~1, groups=NULL, data=Caud2_gdf, iter=9999, print.progress = FALSE)
Caud2_m.d

Caud3_m.d<-morphol.disparity(Caud3_gdf$Shape~1, groups=NULL, data=Caud3_gdf, iter=9999, print.progress = FALSE)
Caud3_m.d


