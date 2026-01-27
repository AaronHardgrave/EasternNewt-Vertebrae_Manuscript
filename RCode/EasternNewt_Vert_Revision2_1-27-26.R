##Title: Eastern Newt Vertebrae Analysis

##Overview
# This code is to accompany the manuscript
# "Terrestriality as a Constraint of Vertebral Shape in the Eastern Newt"
#
#
# Authors: Aaron Hardgrave & Sandy Kawano 
# Version - 2.0 - Second round of revisions
## bilat.symmetry used
## allometry plots produced
## PCA - mesh warping done in R
## Subspecies pooling justification removed
## CVA - Size removed from data frame
#SECTIONS
#### Load libraries (and install, if needed) ####
pkgs <- c('geomorph',    # Core geometric morphometrics package
          'devtools',    # For installing GitHub packages
          'shapes',      # Additional shape analysis tools
          'ellipsis',    # Utility functions
          'ggplot2',     # Publication-quality plots
          'MASS',        # For Linear Discriminant Analysis (LDA/CVA)
          'caret',       # For cross-validation and classification metrics
          'patchwork',   # Combining multiple plots
          'knitr',       # Report generation
          'ggpubr',      # Publication-ready plots and tables
          'vegan',       # Multivariate statistics
          'MuMIn')       # Model selection tools

# Automatically install and load packages if not present
for (i in pkgs) {
  if(!require(i, character.only = TRUE)) {
    install.packages(i)
    library(i, character.only = TRUE)
  }
  
  #No else needed - require() already loaded it if it exists
}

# SlicerMorphR must be downloaded from GitHub (not on CRAN)
# This package reads SlicerMorph JSON landmark files
if(!require("SlicerMorphR", character.only = TRUE)) {
  devtools::install_github('SlicerMorph/SlicerMorphR')
  library("SlicerMorphR")
} else {
  library("SlicerMorphR")
}



#### Data Import and Formatting ####

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

habitat <- factor(c("Semi-aquatic", "Semi-aquatic", "Semi-aquatic", "Semi-aquatic", "Semi-aquatic", "Semi-aquatic",  
                    "Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial",  
                    "Semi-aquatic", "Semi-aquatic", "Semi-aquatic", "Terrestrial", "Terrestrial", "Semi-aquatic", "Semi-aquatic",  
                    "Aquatic", "Aquatic", "Aquatic", "Aquatic", "Aquatic", 
                    "Aquatic","Aquatic","Aquatic","Aquatic","Aquatic", 
                    "Semi-aquatic", "Semi-aquatic", "Aquatic",	"Aquatic","Aquatic","Aquatic",	
                    "Aquatic","Aquatic","Aquatic","Aquatic","Aquatic",	
                    "Aquatic","Aquatic"))

ageGroup <-factor(c("Adult", "Adult", "Adult", "Adult", "Adult", "Adult",  
                    "Juvenile", "Juvenile", "Juvenile", "Juvenile", "Juvenile", "Juvenile", "Juvenile",  
                    "Adult", "Adult", "Adult", "Juvenile", "Juvenile", "Adult", "Adult",  
                    "Juvenile", "Juvenile", "Juvenile", "Juvenile", "Adult", 
                    "Adult","Adult","Adult","Adult","Juvenile", 
                    "Adult", "Adult", "Juvenile",	"Juvenile","Juvenile","Juvenile",	
                    "Juvenile","Juvenile","Adult","Adult","Adult",	
                    "Adult","Adult"))

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

Caud2habitat<-factor(c("Semi-aquatic", "Semi-aquatic", "Semi-aquatic", "Semi-aquatic", "Semi-aquatic", "Semi-aquatic",  
                         "Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial",  
                         "Semi-aquatic", "Semi-aquatic", "Semi-aquatic", "Terrestrial", "Terrestrial", "Semi-aquatic", "Semi-aquatic",  
                         "Aquatic", "Aquatic", "Aquatic", "Aquatic", "Aquatic", 
                         "Aquatic","Aquatic","Aquatic","Aquatic", 
                         "Semi-aquatic", "Semi-aquatic", "Aquatic",	"Aquatic","Aquatic","Aquatic",	
                         "Aquatic","Aquatic","Aquatic","Aquatic","Aquatic",	
                         "Aquatic"))

Caud2ageGroup<-factor(c("Adult", "Adult", "Adult", "Adult", "Adult", "Adult",  
                        "Juvenile", "Juvenile", "Juvenile", "Juvenile", "Juvenile", "Juvenile", "Juvenile",  
                        "Adult", "Adult", "Adult", "Juvenile", "Juvenile", "Adult", "Adult",  
                        "Juvenile", "Juvenile", "Juvenile", "Juvenile", "Adult", 
                        "Adult","Adult","Adult","Juvenile", 
                        "Adult", "Adult", "Juvenile",	"Juvenile","Juvenile","Juvenile",	
                        "Juvenile","Juvenile","Adult","Adult","Adult",	
                        "Adult"))



setwd("F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/Oriented Fixed Landmarks")

#Atlas
setwd("./Atlas - Oriented")
files=dir(patt='json')
Atlas.Samples= gsub(".mrk.json", '', fixed=T, files)

Atlas.LMs=array(dim=c(13,3,43))
for (i in 1:43) Atlas.LMs[,,i] = read.markups.json(files[i])
dimnames(Atlas.LMs) = list(paste0("LM_",1:13), c("x", "y", "z"), Atlas.Samples)

#T1
setwd("../")
setwd("./T1 - Oriented")
files=dir(patt='json')
T1.Samples= gsub(".mrk.json", '', fixed=T, files)

T1.LMs=array(dim=c(19,3,43))
for (i in 1:43) T1.LMs[,,i] = read.markups.json(files[i])
dimnames(T1.LMs) = list(paste0("LM_",1:19), c("x", "y", "z"), T1.Samples)

#T4
setwd("../")
setwd("./T4 - Oriented")
files=dir(patt='json')
T4.Samples= gsub(".mrk.json", '', fixed=T, files)

T4.LMs=array(dim=c(19,3,43))
for (i in 1:43) T4.LMs[,,i] = read.markups.json(files[i])
dimnames(T4.LMs) = list(paste0("LM_",1:19), c("x", "y", "z"), T4.Samples)

#T7
setwd("../")
setwd("./T7 - Oriented")
files=dir(patt='json')
T7.Samples= gsub(".mrk.json", '', fixed=T, files)

T7.LMs=array(dim=c(19,3,43))
for (i in 1:43) T7.LMs[,,i] = read.markups.json(files[i])
dimnames(T7.LMs) = list(paste0("LM_",1:19), c("x", "y", "z"), T7.Samples)

#T10
setwd("../")
setwd("./T10 - Oriented")
files=dir(patt='json')
T10.Samples= gsub(".mrk.json", '', fixed=T, files)

T10.LMs=array(dim=c(19,3,43))
for (i in 1:43) T10.LMs[,,i] = read.markups.json(files[i])
dimnames(T10.LMs) = list(paste0("LM_",1:19), c("x", "y", "z"), T10.Samples)

#T13
setwd("../")
setwd("./T13 - Oriented")
files=dir(patt='json')
T13.Samples= gsub(".mrk.json", '', fixed=T, files)

T13.LMs=array(dim=c(19,3,43))
for (i in 1:43) T13.LMs[,,i] = read.markups.json(files[i])
dimnames(T13.LMs) = list(paste0("LM_",1:19), c("x", "y", "z"), T13.Samples)

#Sacral
setwd("../")
setwd("./Sacral - Oriented")
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


#### Generalized Procrustes Analysis (GPA) ####
## + Format - Align coordinates and format into a geomorph data frame
#NOTE: I am writing this after running the bilat.symmetry analysis - We still need to run GPA to capture Centroid Size (CS)
Atlas.gpa <- gpagen(Atlas.LMs)
Atlas_gdf <- geomorph.data.frame(Shape=Atlas.gpa$coords, ind=Atlas.Samples, Size=log(Atlas.gpa$Csize), lifestage=lifestage, subspecies = subspecies, habitat = habitat, ageGroup = ageGroup)

T1.gpa <- gpagen(T1.LMs)
T1_gdf <- geomorph.data.frame(Shape=T1.gpa$coords, ind=T1.Samples, Size=log(T1.gpa$Csize), lifestage=lifestage, subspecies = subspecies, habitat = habitat, ageGroup = ageGroup)

T4.gpa <- gpagen(T4.LMs)
T4_gdf <- geomorph.data.frame(Shape=T4.gpa$coords, ind=T4.Samples, Size=log(T4.gpa$Csize), lifestage=lifestage, subspecies = subspecies, habitat = habitat, ageGroup = ageGroup)

T7.gpa <- gpagen(T7.LMs)
T7_gdf <- geomorph.data.frame(Shape=T7.gpa$coords, ind=T7.Samples, Size=log(T7.gpa$Csize), lifestage=lifestage, subspecies = subspecies, habitat = habitat, ageGroup = ageGroup)

T10.gpa <- gpagen(T10.LMs)
T10_gdf <- geomorph.data.frame(Shape=T10.gpa$coords, ind=T10.Samples, Size=log(T10.gpa$Csize), lifestage=lifestage, subspecies = subspecies, habitat = habitat, ageGroup = ageGroup)

T13.gpa <- gpagen(T13.LMs)
T13_gdf <- geomorph.data.frame(Shape=T13.gpa$coords, ind=T13.Samples, Size=log(T13.gpa$Csize), lifestage=lifestage, subspecies = subspecies, habitat = habitat, ageGroup = ageGroup)

Sacral.gpa <- gpagen(Sacral.LMs)
Sacral_gdf <- geomorph.data.frame(Shape=Sacral.gpa$coords, ind=Sacral.Samples, Size=log(Sacral.gpa$Csize), lifestage=lifestage, subspecies = subspecies, habitat = habitat, ageGroup = ageGroup)

Caud1.gpa <- gpagen(Caud1.LMs)
Caud1_gdf <- geomorph.data.frame(Shape=Caud1.gpa$coords, ind=Caud1.Samples, Size=log(Caud1.gpa$Csize), lifestage=lifestage, subspecies = subspecies, habitat = habitat, ageGroup = ageGroup)

Caud2.gpa <- gpagen(Caud2.LMs)
Caud2_gdf <- geomorph.data.frame(Shape=Caud2.gpa$coords, ind=Caud2.Samples, Size=log(Caud2.gpa$Csize), lifestage=Caud2lifestage, subspecies = Caud2subspecies, habitat = Caud2habitat, ageGroup = Caud2ageGroup)

Caud3.gpa <- gpagen(Caud3.LMs)
Caud3_gdf <- geomorph.data.frame(Shape=Caud3.gpa$coords, ind=Caud3.Samples, Size=log(Caud3.gpa$Csize), lifestage=lifestage, subspecies = subspecies, habitat = habitat, ageGroup = ageGroup)

#### Bilateral Symmetry Test ####
#Define Landmark Pairing Schemes
#
# Atlas (13 landmarks)
# Bilateral pairs: (1,2), (3,4), (7,10), (8,9)
# Midline: 5, 6, 11, 12, 13
Atlas_land.pairs <- matrix(c(
  1, 2,    # Atlantal cotyles
  3, 4,    # Odontoid process lateral margins
  7, 10,   # Postzygapophyses anterolateral
  8, 9     # Postzygapophyses posterolateral
), ncol = 2, byrow = TRUE)

# Trunk, Sacral, and Caudal 1 vertebrae (19 landmarks)
# Bilateral pairs: (1,2), (3,4), (5,6), (7,8), (17,16), (19,18)
# Convention: odd=left, even=right
# Midline: 9, 10, 11, 12, 13, 14, 15
Trunk_land.pairs <- matrix(c(
  1, 2,    # Prezygapophyses anterolateral
  3, 4,    # Neural arch lateral anterior to rib processes
  5, 6,    # Neural arch lateral at rib processes
  7, 8,    # Postzygapophyses posterolateral
  17, 16,  # Diapophyses (odd=left, even=right) #Transverse processes (dorsal) in Caud1
  19, 18   # Parapophyses (odd=left, even=right) #Transverse processes (ventral) in Caud1
), ncol = 2, byrow = TRUE)

# Caudal 2 and 3 vertebrae (22 landmarks)
# Bilateral pairs: (1,2), (3,4), (5,6), (7,8), (16,17), (18,19), (21,22)
# Midline: 9, 10, 11, 12, 13, 14, 15, 20
Caudal_land.pairs <- matrix(c(
  1, 2,    # Prezygapophyses anterolateral
  3, 4,    # Neural arch lateral anterior to transverse processes
  5, 6,    # Neural arch lateral at transverse processes
  7, 8,    # Postzygapophyses posterolateral
  17, 16,  # Transverse processes dorsal
  19, 18,  # Transverse processes ventral
  21, 22   # Haemal spine posterolateral
), ncol = 2, byrow = TRUE)


## Run Bilateral Symmetry Analyses
# Using object.sym = TRUE because landmarks from both sides are on each specimen

cat("\n========== BILATERAL SYMMETRY ANALYSIS ==========\n")
cat("Running bilat.symmetry() for all vertebral regions...\n\n")

# Atlas
cat("--- Atlas ---\n")
Atlas_bilat <- bilat.symmetry(A = Atlas.LMs, 
                              ind = factor(Atlas.Samples), 
                              object.sym = TRUE, 
                              land.pairs = Atlas_land.pairs,
                              iter = 9999,
                              print.progress = FALSE)
summary(Atlas_bilat)
cat("\n")

# T1
cat("--- T1 ---\n")
T1_bilat <- bilat.symmetry(A = T1.LMs, 
                           ind = factor(T1.Samples), 
                           object.sym = TRUE, 
                           land.pairs = Trunk_land.pairs,
                           iter = 9999,
                           print.progress = FALSE)
summary(T1_bilat)
cat("\n")

# T4
cat("--- T4 ---\n")
T4_bilat <- bilat.symmetry(A = T4.LMs, 
                           ind = factor(T4.Samples), 
                           object.sym = TRUE, 
                           land.pairs = Trunk_land.pairs,
                           iter = 9999,
                           print.progress = FALSE)
summary(T4_bilat)
cat("\n")

# T7
cat("--- T7 ---\n")
T7_bilat <- bilat.symmetry(A = T7.LMs, 
                           ind = factor(T7.Samples), 
                           object.sym = TRUE, 
                           land.pairs = Trunk_land.pairs,
                           iter = 9999,
                           print.progress = FALSE)
summary(T7_bilat)
cat("\n")

# T10
cat("--- T10 ---\n")
T10_bilat <- bilat.symmetry(A = T10.LMs, 
                            ind = factor(T10.Samples), 
                            object.sym = TRUE, 
                            land.pairs = Trunk_land.pairs,
                            iter = 9999,
                            print.progress = FALSE)
summary(T10_bilat)
cat("\n")

# T13
cat("--- T13 ---\n")
T13_bilat <- bilat.symmetry(A = T13.LMs, 
                            ind = factor(T13.Samples), 
                            object.sym = TRUE, 
                            land.pairs = Trunk_land.pairs,
                            iter = 9999,
                            print.progress = FALSE)
summary(T13_bilat)
cat("\n")

# Sacral
cat("--- Sacral ---\n")
Sacral_bilat <- bilat.symmetry(A = Sacral.LMs, 
                               ind = factor(Sacral.Samples), 
                               object.sym = TRUE, 
                               land.pairs = Trunk_land.pairs,
                               iter = 9999,
                               print.progress = FALSE)
summary(Sacral_bilat)
cat("\n")

# Caudal 1
cat("--- Caudal 1 ---\n")
Caud1_bilat <- bilat.symmetry(A = Caud1.LMs, 
                              ind = factor(Caud1.Samples), 
                              object.sym = TRUE, 
                              land.pairs = Trunk_land.pairs,
                              iter = 9999,
                              print.progress = FALSE)
summary(Caud1_bilat)
cat("\n")

# Caudal 2
cat("--- Caudal 2 ---\n")
Caud2_bilat <- bilat.symmetry(A = Caud2.LMs, 
                              ind = factor(Caud2.Samples), 
                              object.sym = TRUE, 
                              land.pairs = Caudal_land.pairs,
                              iter = 9999,
                              print.progress = FALSE)
summary(Caud2_bilat)
cat("\n")

# Caudal 3
cat("--- Caudal 3 ---\n")
Caud3_bilat <- bilat.symmetry(A = Caud3.LMs, 
                              ind = factor(Caud3.Samples), 
                              object.sym = TRUE, 
                              land.pairs = Caudal_land.pairs,
                              iter = 9999,
                              print.progress = FALSE)
summary(Caud3_bilat)
cat("\n")

#SUMMARY
# Directional Asymmetry (DA) accounts for 0.3-6.9% of total shape variation
# DA is also significant at 9/10 vertebral positions
# We remove the effects of DA

##Extract Symmetric Components and Rebuild Data Frames

# The $symm.shape output from bilat.symmetry() contains Procrustes-aligned
# symmetric coordinates. We use these instead of the original gpagen() output.

# Note: Centroid size comes from the original GPA since bilat.symmetry() 
# uses the same superimposition. We keep the log-transformed Csize.

cat("\n========== EXTRACTING SYMMETRIC COMPONENTS ==========\n")

# Atlas
Atlas_gdf_sym <- geomorph.data.frame(
  Shape = Atlas_bilat$symm.shape,
  ind = Atlas.Samples,
  Size = log(Atlas.gpa$Csize),
  lifestage = lifestage,
  subspecies = subspecies,
  habitat = habitat,
  ageGroup = ageGroup
)
cat("Atlas: Symmetric coordinates extracted\n")

# T1
T1_gdf_sym <- geomorph.data.frame(
  Shape = T1_bilat$symm.shape,
  ind = T1.Samples,
  Size = log(T1.gpa$Csize),
  lifestage = lifestage,
  subspecies = subspecies,
  habitat = habitat,
  ageGroup = ageGroup
)
cat("T1: Symmetric coordinates extracted\n")

# T4
T4_gdf_sym <- geomorph.data.frame(
  Shape = T4_bilat$symm.shape,
  ind = T4.Samples,
  Size = log(T4.gpa$Csize),
  lifestage = lifestage,
  subspecies = subspecies,
  habitat = habitat,
  ageGroup = ageGroup
)
cat("T4: Symmetric coordinates extracted\n")

# T7
T7_gdf_sym <- geomorph.data.frame(
  Shape = T7_bilat$symm.shape,
  ind = T7.Samples,
  Size = log(T7.gpa$Csize),
  lifestage = lifestage,
  subspecies = subspecies,
  habitat = habitat,
  ageGroup = ageGroup
)
cat("T7: Symmetric coordinates extracted\n")

# T10
T10_gdf_sym <- geomorph.data.frame(
  Shape = T10_bilat$symm.shape,
  ind = T10.Samples,
  Size = log(T10.gpa$Csize),
  lifestage = lifestage,
  subspecies = subspecies,
  habitat = habitat,
  ageGroup = ageGroup
)
cat("T10: Symmetric coordinates extracted\n")

# T13
T13_gdf_sym <- geomorph.data.frame(
  Shape = T13_bilat$symm.shape,
  ind = T13.Samples,
  Size = log(T13.gpa$Csize),
  lifestage = lifestage,
  subspecies = subspecies,
  habitat = habitat,
  ageGroup = ageGroup
)
cat("T13: Symmetric coordinates extracted\n")

# Sacral
Sacral_gdf_sym <- geomorph.data.frame(
  Shape = Sacral_bilat$symm.shape,
  ind = Sacral.Samples,
  Size = log(Sacral.gpa$Csize),
  lifestage = lifestage,
  subspecies = subspecies,
  habitat = habitat,
  ageGroup = ageGroup
)
cat("Sacral: Symmetric coordinates extracted\n")

# Caudal 1
Caud1_gdf_sym <- geomorph.data.frame(
  Shape = Caud1_bilat$symm.shape,
  ind = Caud1.Samples,
  Size = log(Caud1.gpa$Csize),
  lifestage = lifestage,
  subspecies = subspecies,
  habitat = habitat,
  ageGroup = ageGroup
)
cat("Caud1: Symmetric coordinates extracted\n")

# Caudal 2 (uses Caud2-specific factors due to missing specimens)
Caud2_gdf_sym <- geomorph.data.frame(
  Shape = Caud2_bilat$symm.shape,
  ind = Caud2.Samples,
  Size = log(Caud2.gpa$Csize),
  lifestage = Caud2lifestage,
  subspecies = Caud2subspecies,
  habitat = Caud2habitat,
  ageGroup = Caud2ageGroup
)
cat("Caud2: Symmetric coordinates extracted\n")

# Caudal 3
Caud3_gdf_sym <- geomorph.data.frame(
  Shape = Caud3_bilat$symm.shape,
  ind = Caud3.Samples,
  Size = log(Caud3.gpa$Csize),
  lifestage = lifestage,
  subspecies = subspecies,
  habitat = habitat,
  ageGroup = ageGroup
)
cat("Caud3: Symmetric coordinates extracted\n")


# Overwrite the original _gdf objects (to retain downstream code)

cat("Replacing original _gdf objects with symmetric versions...\n")
Atlas_gdf <- Atlas_gdf_sym
T1_gdf <- T1_gdf_sym
T4_gdf <- T4_gdf_sym
T7_gdf <- T7_gdf_sym
T10_gdf <- T10_gdf_sym
T13_gdf <- T13_gdf_sym
Sacral_gdf <- Sacral_gdf_sym
Caud1_gdf <- Caud1_gdf_sym
Caud2_gdf <- Caud2_gdf_sym
Caud3_gdf <- Caud3_gdf_sym
cat("Done. Original _gdf objects now contain symmetric coordinates.\n")


# Verify Dimensions
cat("\nVerifying coordinate dimensions:\n")
cat("Atlas_gdf_sym$Shape:", paste(dim(Atlas_gdf_sym$Shape), collapse=" x "), "\n")
cat("T1_gdf_sym$Shape:", paste(dim(T1_gdf_sym$Shape), collapse=" x "), "\n")
cat("Caud2_gdf_sym$Shape:", paste(dim(Caud2_gdf_sym$Shape), collapse=" x "), "\n")
cat("Caud3_gdf_sym$Shape:", paste(dim(Caud3_gdf_sym$Shape), collapse=" x "), "\n")


#### Principal Component Analysis (PCA) ####
## Ordinate the dataset using symmetrized shape coordinates

Atlas.PCA <- gm.prcomp(Atlas_bilat$symm.shape)
summary(Atlas.PCA) 

T1.PCA <- gm.prcomp(T1_bilat$symm.shape)
summary(T1.PCA)

T4.PCA <- gm.prcomp(T4_bilat$symm.shape)
summary(T4.PCA) 

T7.PCA <- gm.prcomp(T7_bilat$symm.shape)
summary(T7.PCA) 

T10.PCA <- gm.prcomp(T10_bilat$symm.shape)
summary(T10.PCA) 

T13.PCA <- gm.prcomp(T13_bilat$symm.shape)
summary(T13.PCA) 

Sacral.PCA <- gm.prcomp(Sacral_bilat$symm.shape)
summary(Sacral.PCA)

Caud1.PCA <- gm.prcomp(Caud1_bilat$symm.shape)
summary(Caud1.PCA) 

Caud2.PCA <- gm.prcomp(Caud2_bilat$symm.shape)
summary(Caud2.PCA) 

Caud3.PCA <- gm.prcomp(Caud3_bilat$symm.shape)
summary(Caud3.PCA)

## Export PCA Variance Summary ##

output_dir <- "F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_SecondRevisions/Results"

regions <- c("Atlas", "T1", "T4", "T7", "T10", "T13", "Sacral", "Caud1", "Caud2", "Caud3")

# Initialize list
PCA_variance_list <- list()

for (region in regions) {
  
  # Get the PCA object
  pca_name <- paste0(region, ".PCA")
  pca <- get(pca_name)
  
  # Extract variance summary
  pca_summary <- summary(pca)$PC.summary
  
  # Get proportion of variance (row 2)
  prop_var <- as.numeric(pca_summary[2, ])
  
  # Build rows for each PC
  for (i in seq_along(prop_var)) {
    PCA_variance_list[[length(PCA_variance_list) + 1]] <- data.frame(
      Region = region,
      PC = i,
      Variance_Proportion = round(prop_var[i], 5),
      Variance_Percent = round(prop_var[i] * 100, 2)
    )
  }
}

# Combine using do.call
PCA_variance_summary <- do.call(rbind, PCA_variance_list)

# Export
write.csv(PCA_variance_summary, 
          file.path(output_dir, "PCA_Variance_Summary.csv"), 
          row.names = FALSE)

cat("Exported: PCA_Variance_Summary.csv\n")


#### Procrustes ANOVA/ANCOVA ####

## Atlas
Atlas_proc_ANOVA_m1 <- procD.lm(Shape ~ Size + habitat + ageGroup, data = Atlas_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
Atlas_proc_ANOVA_m2 <- procD.lm(Shape ~ Size * ageGroup + habitat, data = Atlas_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
Atlas_proc_ANOVA_m3 <- procD.lm(Shape ~ Size + lifestage, data = Atlas_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
Atlas_proc_ANOVA_m4 <- procD.lm(Shape ~ Size * lifestage, data = Atlas_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)

anova(Atlas_proc_ANOVA_m3, Atlas_proc_ANOVA_m4)
anova(Atlas_proc_ANOVA_m4, effect.type = "F")

Atlas_gdf_gp <- interaction(Atlas_gdf$lifestage)

# Pairwise comparisons of LS means (size-adjusted, estimated at mean Size from model)
Atlas_proc_ANOVA_PW <- pairwise(Atlas_proc_ANOVA_m4, groups = Atlas_gdf_gp)

# Pairwise comparisons of allometric slopes (tests whether shape~size trajectories differ)
Atlas_proc_ANCOVA_PW <- pairwise(Atlas_proc_ANOVA_m4, groups = Atlas_gdf_gp, covariate = Atlas_gdf$Size)

summary(Atlas_proc_ANOVA_PW)
summary(Atlas_proc_ANCOVA_PW)

plotAllometry(Atlas_proc_ANOVA_m4, size = Atlas_gdf$Size, logsz = TRUE, method = "PredLine", pch = 19, col = as.numeric(Atlas_gdf$lifestage))
legend("topright", legend = unique(Atlas_gdf$lifestage), pch = 19, col = 1:nlevels(Atlas_gdf$lifestage), cex = 0.7)


## T1
T1_proc_ANOVA_m1 <- procD.lm(Shape ~ Size + habitat + ageGroup, data = T1_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
T1_proc_ANOVA_m2 <- procD.lm(Shape ~ Size * ageGroup + habitat, data = T1_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
T1_proc_ANOVA_m3 <- procD.lm(Shape ~ Size + lifestage, data = T1_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
T1_proc_ANOVA_m4 <- procD.lm(Shape ~ Size * lifestage, data = T1_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)

anova(T1_proc_ANOVA_m3, T1_proc_ANOVA_m4)
anova(T1_proc_ANOVA_m4, effect.type = "F")

T1_gdf_gp <- interaction(T1_gdf$lifestage)
T1_proc_ANOVA_PW <- pairwise(T1_proc_ANOVA_m4, groups = T1_gdf_gp)
T1_proc_ANCOVA_PW <- pairwise(T1_proc_ANOVA_m4, groups = T1_gdf_gp, covariate = T1_gdf$Size)

summary(T1_proc_ANOVA_PW)
summary(T1_proc_ANCOVA_PW)

plotAllometry(T1_proc_ANOVA_m4, size = T1_gdf$Size, logsz = TRUE, method = "PredLine", pch = 19, col = as.numeric(T1_gdf$lifestage))
legend("bottomright", legend = unique(T1_gdf$lifestage), pch = 19, col = 1:nlevels(T1_gdf$lifestage), cex = 0.7)


## T4
T4_proc_ANOVA_m1 <- procD.lm(Shape ~ Size + habitat + ageGroup, data = T4_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
T4_proc_ANOVA_m2 <- procD.lm(Shape ~ Size * ageGroup + habitat, data = T4_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
T4_proc_ANOVA_m3 <- procD.lm(Shape ~ Size + lifestage, data = T4_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
T4_proc_ANOVA_m4 <- procD.lm(Shape ~ Size * lifestage, data = T4_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)

anova(T4_proc_ANOVA_m3, T4_proc_ANOVA_m4)
anova(T4_proc_ANOVA_m4, effect.type = "F")

T4_gdf_gp <- interaction(T4_gdf$lifestage)
T4_proc_ANOVA_PW <- pairwise(T4_proc_ANOVA_m4, groups = T4_gdf_gp)
T4_proc_ANCOVA_PW <- pairwise(T4_proc_ANOVA_m4, groups = T4_gdf_gp, covariate = T4_gdf$Size)

summary(T4_proc_ANOVA_PW)
summary(T4_proc_ANCOVA_PW)

plotAllometry(T4_proc_ANOVA_m4, size = T4_gdf$Size, logsz = TRUE, method = "PredLine", pch = 19, col = as.numeric(T4_gdf$lifestage))
legend("bottomright", legend = unique(T4_gdf$lifestage), pch = 19, col = 1:nlevels(T4_gdf$lifestage), cex = 0.7)


## T7
T7_proc_ANOVA_m1 <- procD.lm(Shape ~ Size + habitat + ageGroup, data = T7_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
T7_proc_ANOVA_m2 <- procD.lm(Shape ~ Size * ageGroup + habitat, data = T7_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
T7_proc_ANOVA_m3 <- procD.lm(Shape ~ Size + lifestage, data = T7_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
T7_proc_ANOVA_m4 <- procD.lm(Shape ~ Size * lifestage, data = T7_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)

anova(T7_proc_ANOVA_m3, T7_proc_ANOVA_m4)
anova(T7_proc_ANOVA_m4, effect.type = "F")

T7_gdf_gp <- interaction(T7_gdf$lifestage)
T7_proc_ANOVA_PW <- pairwise(T7_proc_ANOVA_m4, groups = T7_gdf_gp)
T7_proc_ANCOVA_PW <- pairwise(T7_proc_ANOVA_m4, groups = T7_gdf_gp, covariate = T7_gdf$Size)

summary(T7_proc_ANOVA_PW)
summary(T7_proc_ANCOVA_PW)

plotAllometry(T7_proc_ANOVA_m4, size = T7_gdf$Size, logsz = TRUE, method = "PredLine", pch = 19, col = as.numeric(T7_gdf$lifestage))
legend("bottomright", legend = unique(T7_gdf$lifestage), pch = 19, col = 1:nlevels(T7_gdf$lifestage), cex = 0.7)


## T10
T10_proc_ANOVA_m1 <- procD.lm(Shape ~ Size + habitat + ageGroup, data = T10_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
T10_proc_ANOVA_m2 <- procD.lm(Shape ~ Size * ageGroup + habitat, data = T10_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
T10_proc_ANOVA_m3 <- procD.lm(Shape ~ Size + lifestage, data = T10_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
T10_proc_ANOVA_m4 <- procD.lm(Shape ~ Size * lifestage, data = T10_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)

anova(T10_proc_ANOVA_m3, T10_proc_ANOVA_m4)
anova(T10_proc_ANOVA_m4, effect.type = "F")

T10_gdf_gp <- interaction(T10_gdf$lifestage)
T10_proc_ANOVA_PW <- pairwise(T10_proc_ANOVA_m4, groups = T10_gdf_gp)
T10_proc_ANCOVA_PW <- pairwise(T10_proc_ANOVA_m4, groups = T10_gdf_gp, covariate = T10_gdf$Size)

summary(T10_proc_ANOVA_PW)
summary(T10_proc_ANCOVA_PW)

plotAllometry(T10_proc_ANOVA_m4, size = T10_gdf$Size, logsz = TRUE, method = "PredLine", pch = 19, col = as.numeric(T10_gdf$lifestage))
legend("bottomright", legend = unique(T10_gdf$lifestage), pch = 19, col = 1:nlevels(T10_gdf$lifestage), cex = 0.7)


## T13
T13_proc_ANOVA_m1 <- procD.lm(Shape ~ Size + habitat + ageGroup, data = T13_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
T13_proc_ANOVA_m2 <- procD.lm(Shape ~ Size * ageGroup + habitat, data = T13_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
T13_proc_ANOVA_m3 <- procD.lm(Shape ~ Size + lifestage, data = T13_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
T13_proc_ANOVA_m4 <- procD.lm(Shape ~ Size * lifestage, data = T13_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)

anova(T13_proc_ANOVA_m3, T13_proc_ANOVA_m4)
anova(T13_proc_ANOVA_m4, effect.type = "F")

T13_gdf_gp <- interaction(T13_gdf$lifestage)
T13_proc_ANOVA_PW <- pairwise(T13_proc_ANOVA_m4, groups = T13_gdf_gp)
T13_proc_ANCOVA_PW <- pairwise(T13_proc_ANOVA_m4, groups = T13_gdf_gp, covariate = T13_gdf$Size)

summary(T13_proc_ANOVA_PW)
summary(T13_proc_ANCOVA_PW)

plotAllometry(T13_proc_ANOVA_m4, size = T13_gdf$Size, logsz = TRUE, method = "PredLine", pch = 19, col = as.numeric(T13_gdf$lifestage))
legend("bottomright", legend = unique(T13_gdf$lifestage), pch = 19, col = 1:nlevels(T13_gdf$lifestage), cex = 0.7)


## Sacral
Sacral_proc_ANOVA_m1 <- procD.lm(Shape ~ Size + habitat + ageGroup, data = Sacral_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
Sacral_proc_ANOVA_m2 <- procD.lm(Shape ~ Size * ageGroup + habitat, data = Sacral_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
Sacral_proc_ANOVA_m3 <- procD.lm(Shape ~ Size + lifestage, data = Sacral_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
Sacral_proc_ANOVA_m4 <- procD.lm(Shape ~ Size * lifestage, data = Sacral_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)

anova(Sacral_proc_ANOVA_m3, Sacral_proc_ANOVA_m4)
anova(Sacral_proc_ANOVA_m4, effect.type = "F")

Sacral_gdf_gp <- interaction(Sacral_gdf$lifestage)
Sacral_proc_ANOVA_PW <- pairwise(Sacral_proc_ANOVA_m4, groups = Sacral_gdf_gp)
Sacral_proc_ANCOVA_PW <- pairwise(Sacral_proc_ANOVA_m4, groups = Sacral_gdf_gp, covariate = Sacral_gdf$Size)

summary(Sacral_proc_ANOVA_PW)
summary(Sacral_proc_ANCOVA_PW)

plotAllometry(Sacral_proc_ANOVA_m4, size = Sacral_gdf$Size, logsz = TRUE, method = "PredLine", pch = 19, col = as.numeric(Sacral_gdf$lifestage))
legend("bottomright", legend = unique(Sacral_gdf$lifestage), pch = 19, col = 1:nlevels(Sacral_gdf$lifestage), cex = 0.7)


## Caud1
Caud1_proc_ANOVA_m1 <- procD.lm(Shape ~ Size + habitat + ageGroup, data = Caud1_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
Caud1_proc_ANOVA_m2 <- procD.lm(Shape ~ Size * ageGroup + habitat, data = Caud1_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
Caud1_proc_ANOVA_m3 <- procD.lm(Shape ~ Size + lifestage, data = Caud1_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
Caud1_proc_ANOVA_m4 <- procD.lm(Shape ~ Size * lifestage, data = Caud1_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)

anova(Caud1_proc_ANOVA_m3, Caud1_proc_ANOVA_m4)
anova(Caud1_proc_ANOVA_m4, effect.type = "F")

Caud1_gdf_gp <- interaction(Caud1_gdf$lifestage)
Caud1_proc_ANOVA_PW <- pairwise(Caud1_proc_ANOVA_m4, groups = Caud1_gdf_gp)
Caud1_proc_ANCOVA_PW <- pairwise(Caud1_proc_ANOVA_m4, groups = Caud1_gdf_gp, covariate = Caud1_gdf$Size)

summary(Caud1_proc_ANOVA_PW)
summary(Caud1_proc_ANCOVA_PW)

plotAllometry(Caud1_proc_ANOVA_m4, size = Caud1_gdf$Size, logsz = TRUE, method = "PredLine", pch = 19, col = as.numeric(Caud1_gdf$lifestage))
legend("bottomright", legend = unique(Caud1_gdf$lifestage), pch = 19, col = 1:nlevels(Caud1_gdf$lifestage), cex = 0.7)


## Caud2
Caud2_proc_ANOVA_m1 <- procD.lm(Shape ~ Size + habitat + ageGroup, data = Caud2_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
Caud2_proc_ANOVA_m2 <- procD.lm(Shape ~ Size * ageGroup + habitat, data = Caud2_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
Caud2_proc_ANOVA_m3 <- procD.lm(Shape ~ Size + lifestage, data = Caud2_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
Caud2_proc_ANOVA_m4 <- procD.lm(Shape ~ Size * lifestage, data = Caud2_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)

anova(Caud2_proc_ANOVA_m3, Caud2_proc_ANOVA_m4)
anova(Caud2_proc_ANOVA_m4, effect.type = "F")

Caud2_gdf_gp <- interaction(Caud2_gdf$lifestage)
Caud2_proc_ANOVA_PW <- pairwise(Caud2_proc_ANOVA_m4, groups = Caud2_gdf_gp)
Caud2_proc_ANCOVA_PW <- pairwise(Caud2_proc_ANOVA_m4, groups = Caud2_gdf_gp, covariate = Caud2_gdf$Size)

summary(Caud2_proc_ANOVA_PW)
summary(Caud2_proc_ANCOVA_PW)

plotAllometry(Caud2_proc_ANOVA_m4, size = Caud2_gdf$Size, logsz = TRUE, method = "PredLine", pch = 19, col = as.numeric(Caud2_gdf$lifestage))
legend("bottomright", legend = unique(Caud2_gdf$lifestage), pch = 19, col = 1:nlevels(Caud2_gdf$lifestage), cex = 0.7)


## Caud3
Caud3_proc_ANOVA_m1 <- procD.lm(Shape ~ Size + habitat + ageGroup, data = Caud3_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
Caud3_proc_ANOVA_m2 <- procD.lm(Shape ~ Size * ageGroup + habitat, data = Caud3_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
Caud3_proc_ANOVA_m3 <- procD.lm(Shape ~ Size + lifestage, data = Caud3_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)
Caud3_proc_ANOVA_m4 <- procD.lm(Shape ~ Size * lifestage, data = Caud3_gdf, iter = 9999, RRPP = TRUE, print.progress = FALSE)

anova(Caud3_proc_ANOVA_m3, Caud3_proc_ANOVA_m4)
anova(Caud3_proc_ANOVA_m4, effect.type = "F")

Caud3_gdf_gp <- interaction(Caud3_gdf$lifestage)
Caud3_proc_ANOVA_PW <- pairwise(Caud3_proc_ANOVA_m4, groups = Caud3_gdf_gp)
Caud3_proc_ANCOVA_PW <- pairwise(Caud3_proc_ANOVA_m4, groups = Caud3_gdf_gp, covariate = Caud3_gdf$Size)

summary(Caud3_proc_ANOVA_PW)
summary(Caud3_proc_ANCOVA_PW)

plotAllometry(Caud3_proc_ANOVA_m4, size = Caud3_gdf$Size, logsz = TRUE, method = "PredLine", pch = 19, col = as.numeric(Caud3_gdf$lifestage))
legend("bottomright", legend = unique(Caud3_gdf$lifestage), pch = 19, col = 1:nlevels(Caud3_gdf$lifestage), cex = 0.7)



#### Canonical Variate Analysis (CVA) + Leave One Out Cross Validation (LOOCV) ####
# NOTE: For this analysis - I'm removing size to be consistent
# LS Means comparison is size adjusted
#
#Atlas

#Store PC Scores and format
Atlas_PCA_loadings <- Atlas.PCA$rotation
Atlas_PCA_scores <- Atlas.PCA$x
Atlas_pdf<-data.frame(Atlas_PCA_scores, ind=Atlas.Samples, Size=log(Atlas.gpa$Csize), lifestage=lifestage)

#95% Cumulative variance ~95% at PC Score 9
Atlas_LDA<-lda(lifestage~.,data=Atlas_pdf[,c(1:9, 21)])
Atlas_LDA_predicted<-predict(Atlas_LDA, Atlas_pdf[,c(1:9,21)])

Atlas_ctrl <- trainControl(method = "LOOCV")
Atlas_LDA_LOOCV <- train(lifestage ~ ., data = Atlas_pdf[, c(1:9, 21)], method = "lda", trControl = Atlas_ctrl)
Atlas_LDA_LOOCV_cm <-confusionMatrix(Atlas_LDA_LOOCV$pred$pred, Atlas_LDA_LOOCV$pred$obs)
Atlas_LDA_LOOCV_cm

Atlas_LDA_LOOCV_cm_table<-as.table(Atlas_LDA_LOOCV_cm)
Atlas_LDA_LOOCV_cm_table


#Trunk 1
T1_PCA_loadings <- T1.PCA$rotation
T1_PCA_scores <- T1.PCA$x
T1_pdf<-data.frame(T1_PCA_scores, ind=T1.Samples, Size=log(T1.gpa$Csize), lifestage=lifestage)
T1_LDA<-lda(lifestage~.,data=T1_pdf[,c(1:14, 31)])
T1_LDA_predicted<-predict(T1_LDA, T1_pdf[,c(1:14, 31)])

T1_ctrl <- trainControl(method = "LOOCV")
T1_LDA_LOOCV <- train(lifestage ~ ., data = T1_pdf[, c(1:14, 31)], method = "lda", trControl = T1_ctrl)
T1_LDA_LOOCV_cm <-confusionMatrix(T1_LDA_LOOCV$pred$pred, T1_LDA_LOOCV$pred$obs)
T1_LDA_LOOCV_cm
T1_LDA_LOOCV_cm_table<-as.table(T1_LDA_LOOCV_cm)
T1_LDA_LOOCV_cm_table


#Trunk 4
T4_PCA_loadings <- T4.PCA$rotation
T4_PCA_scores <- T4.PCA$x
T4_pdf<-data.frame(T4_PCA_scores, ind=T4.Samples, Size=log(T4.gpa$Csize), lifestage=lifestage)
T4_LDA<-lda(lifestage~.,data=T4_pdf[,c(1:12, 31)])
T4_LDA_predicted<-predict(T4_LDA, T4_pdf[,c(1:12, 31)])

T4_ctrl <- trainControl(method = "LOOCV")
T4_LDA_LOOCV <- train(lifestage ~ ., data = T4_pdf[, c(1:12, 31)], method = "lda", trControl = T4_ctrl)
T4_LDA_LOOCV_cm <-confusionMatrix(T4_LDA_LOOCV$pred$pred, T4_LDA_LOOCV$pred$obs)
T4_LDA_LOOCV_cm
T4_LDA_LOOCV_cm_table<-as.table(T4_LDA_LOOCV_cm)
T4_LDA_LOOCV_cm_table


#Trunk 7
T7_PCA_loadings <- T7.PCA$rotation
T7_PCA_scores <- T7.PCA$x
T7_pdf<-data.frame(T7_PCA_scores, ind=T7.Samples, Size=log(T7.gpa$Csize), lifestage=lifestage)
T7_LDA<-lda(lifestage~.,data=T7_pdf[,c(1:12, 31)])
T7_LDA_predicted<-predict(T7_LDA, T7_pdf[,c(1:12, 31)])

T7_ctrl <- trainControl(method = "LOOCV")
T7_LDA_LOOCV <- train(lifestage ~ ., data = T7_pdf[, c(1:12, 31)], method = "lda", trControl = T7_ctrl)
T7_LDA_LOOCV_cm <-confusionMatrix(T7_LDA_LOOCV$pred$pred, T7_LDA_LOOCV$pred$obs)
T7_LDA_LOOCV_cm
T7_LDA_LOOCV_cm_table<-as.table(T7_LDA_LOOCV_cm)
T7_LDA_LOOCV_cm_table


#Trunk 10
T10_PCA_loadings <- T10.PCA$rotation
T10_PCA_scores <- T10.PCA$x
T10_pdf<-data.frame(T10_PCA_scores, ind=T10.Samples, Size=log(T10.gpa$Csize), lifestage=lifestage)
T10_LDA<-lda(lifestage~.,data=T10_pdf[,c(1:12, 31)])
T10_LDA_predicted<-predict(T10_LDA, T10_pdf[,c(1:12, 31)])

T10_ctrl <- trainControl(method = "LOOCV")
T10_LDA_LOOCV <- train(lifestage ~ ., data = T10_pdf[, c(1:12, 31)], method = "lda", trControl = T10_ctrl)
T10_LDA_LOOCV_cm <-confusionMatrix(T10_LDA_LOOCV$pred$pred, T10_LDA_LOOCV$pred$obs)
T10_LDA_LOOCV_cm
T10_LDA_LOOCV_cm_table<-as.table(T10_LDA_LOOCV_cm)
T10_LDA_LOOCV_cm_table


#Trunk 13
T13_PCA_loadings <- T13.PCA$rotation
T13_PCA_scores <- T13.PCA$x
T13_pdf<-data.frame(T13_PCA_scores, ind=T13.Samples, Size=log(T13.gpa$Csize), lifestage=lifestage)
T13_LDA<-lda(lifestage~.,data=T13_pdf[,c(1:12, 33)])
T13_LDA_predicted<-predict(T13_LDA, T13_pdf[,c(1:12, 33)])

T13_ctrl <- trainControl(method = "LOOCV")
T13_LDA_LOOCV <- train(lifestage ~ ., data = T13_pdf[, c(1:12, 33)], method = "lda", trControl = T13_ctrl)
T13_LDA_LOOCV_cm <-confusionMatrix(T13_LDA_LOOCV$pred$pred, T13_LDA_LOOCV$pred$obs)
T13_LDA_LOOCV_cm
T13_LDA_LOOCV_cm_table<-as.table(T13_LDA_LOOCV_cm)
T13_LDA_LOOCV_cm_table


#Sacral
Sacral_PCA_loadings <- Sacral.PCA$rotation
Sacral_PCA_scores <- Sacral.PCA$x
Sacral_pdf<-data.frame(Sacral_PCA_scores, ind=Sacral.Samples, Size=log(Sacral.gpa$Csize), lifestage=lifestage)
Sacral_LDA<-lda(lifestage~.,data=Sacral_pdf[,c(1:12, 31)])
Sacral_LDA_predicted<-predict(Sacral_LDA, Sacral_pdf[,c(1:12, 31)])

Sacral_ctrl <- trainControl(method = "LOOCV")
Sacral_LDA_LOOCV <- train(lifestage ~ ., data = Sacral_pdf[, c(1:12, 31)], method = "lda", trControl = Sacral_ctrl)
Sacral_LDA_LOOCV_cm <-confusionMatrix(Sacral_LDA_LOOCV$pred$pred, Sacral_LDA_LOOCV$pred$obs)
Sacral_LDA_LOOCV_cm
Sacral_LDA_LOOCV_cm_table<-as.table(Sacral_LDA_LOOCV_cm)
Sacral_LDA_LOOCV_cm_table


#Caudal 1
Caud1_PCA_loadings <- Caud1.PCA$rotation
Caud1_PCA_scores <- Caud1.PCA$x
Caud1_pdf<-data.frame(Caud1_PCA_scores, ind=Caud1.Samples, Size=log(Caud1.gpa$Csize), lifestage=lifestage)
Caud1_LDA<-lda(lifestage~.,data=Caud1_pdf[,c(1:12, 31)])
Caud1_LDA_predicted<-predict(Caud1_LDA, Caud1_pdf[,c(1:12, 31)])

Caud1_ctrl <- trainControl(method = "LOOCV")
Caud1_LDA_LOOCV <- train(lifestage ~ ., data = Caud1_pdf[, c(1:12, 31)], method = "lda", trControl = Caud1_ctrl)
Caud1_LDA_LOOCV_cm <-confusionMatrix(Caud1_LDA_LOOCV$pred$pred, Caud1_LDA_LOOCV$pred$obs)
Caud1_LDA_LOOCV_cm
Caud1_LDA_LOOCV_cm_table<-as.table(Caud1_LDA_LOOCV_cm)
Caud1_LDA_LOOCV_cm_table

#Caudal 2
Caud2_PCA_loadings <- Caud2.PCA$rotation
Caud2_PCA_scores <- Caud2.PCA$x
Caud2_pdf<-data.frame(Caud2_PCA_scores, ind=Caud2.Samples, Size=log(Caud2.gpa$Csize), lifestage=Caud2lifestage)
Caud2_LDA<-lda(lifestage~.,data=Caud2_pdf[,c(1:11, 37)])
Caud2_LDA_predicted<-predict(Caud2_LDA, Caud2_pdf[,c(1:11, 37)])

Caud2_ctrl <- trainControl(method = "LOOCV")
Caud2_LDA_LOOCV <- train(lifestage ~ ., data = Caud2_pdf[, c(1:11, 37)], method = "lda", trControl = Caud2_ctrl)
Caud2_LDA_LOOCV_cm <-confusionMatrix(Caud2_LDA_LOOCV$pred$pred, Caud2_LDA_LOOCV$pred$obs)
Caud2_LDA_LOOCV_cm
Caud2_LDA_LOOCV_cm_table<-as.table(Caud2_LDA_LOOCV_cm)
Caud2_LDA_LOOCV_cm_table

#Caudal 3
Caud3_PCA_loadings <- Caud3.PCA$rotation
Caud3_PCA_scores <- Caud3.PCA$x
Caud3_pdf<-data.frame(Caud3_PCA_scores, ind=Caud3.Samples, Size=log(Caud3.gpa$Csize), lifestage=lifestage)
Caud3_LDA<-lda(lifestage~.,data=Caud3_pdf[,c(1:11, 37)])
Caud3_LDA_predicted<-predict(Caud3_LDA, Caud3_pdf[,c(1:11, 37)])

Caud3_ctrl <- trainControl(method = "LOOCV")
Caud3_LDA_LOOCV <- train(lifestage ~ ., data = Caud3_pdf[, c(1:11, 37)], method = "lda", trControl = Caud3_ctrl)
Caud3_LDA_LOOCV_cm <-confusionMatrix(Caud3_LDA_LOOCV$pred$pred, Caud3_LDA_LOOCV$pred$obs)
Caud3_LDA_LOOCV_cm

Caud3_LDA_LOOCV_cm_table<-as.table(Caud3_LDA_LOOCV_cm)
Caud3_LDA_LOOCV_cm_table


#### Total Morphological Disparity ####
headerNames <- c('Adult - Semi-aquatic', 'Juvenile: Terrestrial', 'Juvenile: Aquatic', 'Adult: Aquatic')

## Atlas
# Morphological disparity for entire data set
Atlas_m.dt<-morphol.disparity(Atlas_gdf$Shape~1, groups=NULL, data=Atlas_gdf, iter=9999, print.progress = FALSE)
Atlas_m.dt
# Morphological disparity for entire data set, accounting for allometry
Atlas_m.dt2<-morphol.disparity(Atlas_gdf$Shape~Size, groups=NULL, data=Atlas_gdf, iter=9999, print.progress = FALSE)

# Morphological disparity for entire data set while accounting for allometry and using group means
# including pairwise comparisons to determine which group differences are "significant"
Atlas_m.dt3 <- morphol.disparity(Atlas_gdf$Shape~Size, groups= ~lifestage, data = Atlas_gdf, print.progress = FALSE)
Atlas_m.dt3

# Create table of morphological disparity with pairwise comparisons
Atlas_m.dt3_PV <- ifelse(Atlas_m.dt3$PV.dist.Pval<0.05, paste(round(Atlas_m.dt3$PV.dist,3), "*", sep=""), round(Atlas_m.dt3$PV.dist,3))
Atlas_m.dt3_table <- ggtexttable(Atlas_m.dt3_PV, cols = headerNames, rows = headerNames)
Atlas_m.dt3_table

##T1
T1_m.dt<-morphol.disparity(T1_gdf$Shape~1, groups=NULL, data=T1_gdf, iter=9999, print.progress = FALSE)
T1_m.dt

# Morphological disparity for entire data set while accounting for allometry and using group means
# including pairwise comparisons to determine which group differences are "significant"
T1_m.dt3 <- morphol.disparity(T1_gdf$Shape~Size, groups= ~lifestage, data = T1_gdf, print.progress = FALSE)
T1_m.dt3

# Create table of morphological disparity with pairwise comparisons
T1_m.dt3_PV <- ifelse(T1_m.dt3$PV.dist.Pval<0.05, paste(round(T1_m.dt3$PV.dist,3), "*", sep=""), round(T1_m.dt3$PV.dist,3))
T1_m.dt3_table <- ggtexttable(T1_m.dt3_PV, cols = headerNames, rows = headerNames)
T1_m.dt3_table


## T4
T4_m.dt<-morphol.disparity(T4_gdf$Shape~1, groups=NULL, data=T4_gdf, iter=9999, print.progress = FALSE)
T4_m.dt

# Morphological disparity for entire data set while accounting for allometry and using group means
# including pairwise comparisons to determine which group differences are "significant"
T4_m.dt3 <- morphol.disparity(T4_gdf$Shape~Size, groups= ~lifestage, data = T4_gdf, print.progress = FALSE)
T4_m.dt3

# Create table of morphological disparity with pairwise comparisons
T4_m.dt3_PV <- ifelse(T4_m.dt3$PV.dist.Pval<0.05, paste(round(T4_m.dt3$PV.dist,3), "*", sep=""), round(T4_m.dt3$PV.dist,3))
T4_m.dt3_table <- ggtexttable(T4_m.dt3_PV, cols = headerNames, rows = headerNames)
T4_m.dt3_table

## T7
T7_m.dt<-morphol.disparity(T7_gdf$Shape~1, groups=NULL, data=T7_gdf, iter=9999, print.progress = FALSE)
T7_m.dt

# Morphological disparity for entire data set while accounting for allometry and using group means
# including pairwise comparisons to determine which group differences are "significant"
T7_m.dt3 <- morphol.disparity(T7_gdf$Shape~Size, groups= ~lifestage, data = T7_gdf, print.progress = FALSE)
T7_m.dt3

# Create table of morphological disparity with pairwise comparisons
T7_m.dt3_PV <- ifelse(T7_m.dt3$PV.dist.Pval<0.05, paste(round(T7_m.dt3$PV.dist,3), "*", sep=""), round(T7_m.dt3$PV.dist,3))
T7_m.dt3_table <- ggtexttable(T7_m.dt3_PV, cols = headerNames, rows = headerNames)
T7_m.dt3_table

## T10
T10_m.dt<-morphol.disparity(T10_gdf$Shape~1, groups=NULL, data=T10_gdf, iter=9999, print.progress = FALSE)
T10_m.dt

# Morphological disparity for entire data set while accounting for allometry and using group means
# including pairwise comparisons to determine which group differences are "significant"
T10_m.dt3 <- morphol.disparity(T10_gdf$Shape~Size, groups= ~lifestage, data = T10_gdf, print.progress = FALSE)
T10_m.dt3

# Create table of morphological disparity with pairwise comparisons
T10_m.dt3_PV <- ifelse(T10_m.dt3$PV.dist.Pval<0.05, paste(round(T10_m.dt3$PV.dist,3), "*", sep=""), round(T10_m.dt3$PV.dist,3))
T10_m.dt3_table <- ggtexttable(T10_m.dt3_PV, cols = headerNames, rows = headerNames)
T10_m.dt3_table

## T13
T13_m.dt<-morphol.disparity(T13_gdf$Shape~1, groups=NULL, data=T13_gdf, iter=9999, print.progress = FALSE)
T13_m.dt

# Morphological disparity for entire data set while accounting for allometry and using group means
# including pairwise comparisons to determine which group differences are "significant"
T13_m.dt3 <- morphol.disparity(T13_gdf$Shape~Size, groups= ~lifestage, data = T13_gdf, print.progress = FALSE)
T13_m.dt3

# Create table of morphological disparity with pairwise comparisons
T13_m.dt3_PV <- ifelse(T13_m.dt3$PV.dist.Pval<0.05, paste(round(T13_m.dt3$PV.dist,3), "*", sep=""), round(T13_m.dt3$PV.dist,3))
T13_m.dt3_table <- ggtexttable(T13_m.dt3_PV, cols = headerNames, rows = headerNames)
T13_m.dt3_table

## Sacral
Sacral_m.dt<-morphol.disparity(Sacral_gdf$Shape~1, groups=NULL, data=Sacral_gdf, iter=9999, print.progress = FALSE)
Sacral_m.dt

# Morphological disparity for entire data set while accounting for allometry and using group means
# including pairwise comparisons to determine which group differences are "significant"
Sacral_m.dt3 <- morphol.disparity(Sacral_gdf$Shape~Size, groups= ~lifestage, data = Sacral_gdf, print.progress = FALSE)
Sacral_m.dt3

# Create table of morphological disparity with pairwise comparisons
Sacral_m.dt3_PV <- ifelse(Sacral_m.dt3$PV.dist.Pval<0.05, paste(round(Sacral_m.dt3$PV.dist,3), "*", sep=""), round(Sacral_m.dt3$PV.dist,3))
Sacral_m.dt3_table <- ggtexttable(Sacral_m.dt3_PV, cols = headerNames, rows = headerNames)
Sacral_m.dt3_table

## Caudal 1
Caud1_m.dt<-morphol.disparity(Caud1_gdf$Shape~1, groups=NULL, data=Caud1_gdf, iter=9999, print.progress = FALSE)
Caud1_m.dt

# Morphological disparity for entire data set while accounting for allometry and using group means
# including pairwise comparisons to determine which group differences are "significant"
Caud1_m.dt3 <- morphol.disparity(Caud1_gdf$Shape~Size, groups= ~lifestage, data = Caud1_gdf, print.progress = FALSE)
Caud1_m.dt3

# Create table of morphological disparity with pairwise comparisons
Caud1_m.dt3_PV <- ifelse(Caud1_m.dt3$PV.dist.Pval<0.05, paste(round(Caud1_m.dt3$PV.dist,3), "*", sep=""), round(Caud1_m.dt3$PV.dist,3))
Caud1_m.dt3_table <- ggtexttable(Caud1_m.dt3_PV, cols = headerNames, rows = headerNames)
Caud1_m.dt3_table

## Caudal 2
Caud2_m.dt<-morphol.disparity(Caud2_gdf$Shape~1, groups=NULL, data=Caud2_gdf, iter=9999, print.progress = FALSE)
Caud2_m.dt

# Morphological disparity for entire data set while accounting for allometry and using group means
# including pairwise comparisons to determine which group differences are "significant"
Caud2_m.dt3 <- morphol.disparity(Caud2_gdf$Shape~Size, groups= ~lifestage, data = Caud2_gdf, print.progress = FALSE)
Caud2_m.dt3

# Create table of morphological disparity with pairwise comparisons
Caud2_m.dt3_PV <- ifelse(Caud2_m.dt3$PV.dist.Pval<0.05, paste(round(Caud2_m.dt3$PV.dist,3), "*", sep=""), round(Caud2_m.dt3$PV.dist,3))
Caud2_m.dt3_table <- ggtexttable(Caud2_m.dt3_PV, cols = headerNames, rows = headerNames)
Caud2_m.dt3_table

## Caudal 3
Caud3_m.dt<-morphol.disparity(Caud3_gdf$Shape~1, groups=NULL, data=Caud3_gdf, iter=9999, print.progress = FALSE)
Caud3_m.dt

# Morphological disparity for entire data set while accounting for allometry and using group means
# including pairwise comparisons to determine which group differences are "significant"
Caud3_m.dt3 <- morphol.disparity(Caud3_gdf$Shape~Size, groups= ~lifestage, data = Caud3_gdf, print.progress = FALSE)
Caud3_m.dt3

# Create table of morphological disparity with pairwise comparisons
Caud3_m.dt3_PV <- ifelse(Caud3_m.dt3$PV.dist.Pval<0.05, paste(round(Caud3_m.dt3$PV.dist,3), "*", sep=""), round(Caud3_m.dt3$PV.dist,3))
Caud3_m.dt3_table <- ggtexttable(Caud3_m.dt3_PV, cols = headerNames, rows = headerNames)
Caud3_m.dt3_table

MorphoDisparity_total_tables1 <- ggarrange(Atlas_m.dt3_table, T1_m.dt3_table, T4_m.dt3_table, T7_m.dt3_table, 
                                           nrow = 4, ncol = 1)

MorphoDisparity_total_tables2 <- ggarrange(T10_m.dt3_table, T13_m.dt3_table, Sacral_m.dt3_table, Caud1_m.dt3_table, 
                                           nrow = 4, ncol = 1)

MorphoDisparity_total_tables3 <- ggarrange(Caud2_m.dt3_table, Caud3_m.dt3_table, 
                                           nrow = 4, ncol = 1)


#### Morphological Disparity - Calculate Partial Disparities ####
# Reveals which subgroups contribute the most towards the total disparity


## Atlas
Atlas_m.d<-morphol.disparity(Atlas_gdf$Shape~1, groups=lifestage, partial=TRUE, data=Atlas_gdf, iter=9999, print.progress = FALSE)
Atlas_m.d
Atlas_m.d_groups<-Atlas_m.d$Procrustes.var
Atlas_m.d_groups

Atlas_m.d_PV <- ifelse(Atlas_m.d$PV.dist.Pval<0.05, paste(round(Atlas_m.d$PV.dist,3), "*", sep=""), round(Atlas_m.d$PV.dist,3))
Atlas_m.d_table <- ggtexttable(Atlas_m.d_PV, cols = headerNames, rows = headerNames)
Atlas_m.d_table

## T1
T1_m.d<-morphol.disparity(T1_gdf$Shape~1, groups=lifestage, partial=TRUE, data=T1_gdf, iter=9999, print.progress = FALSE)
T1_m.d
T1_m.d_groups<-T1_m.d$Procrustes.var
T1_m.d_groups

T1_m.d_PV <- ifelse(T1_m.d$PV.dist.Pval<0.05, paste(round(T1_m.d$PV.dist,3), "*", sep=""), round(T1_m.d$PV.dist,3))
T1_m.d_table <- ggtexttable(T1_m.d_PV, cols = headerNames, rows = headerNames)
T1_m.d_table

## T4
T4_m.d<-morphol.disparity(T4_gdf$Shape~1, groups=lifestage, partial=TRUE, data=T4_gdf, iter=9999, print.progress = FALSE)
T4_m.d
T4_m.d_groups<-T4_m.d$Procrustes.var
T4_m.d_groups

T4_m.d_PV <- ifelse(T4_m.d$PV.dist.Pval<0.05, paste(round(T4_m.d$PV.dist,3), "*", sep=""), round(T4_m.d$PV.dist,3))
T4_m.d_table <- ggtexttable(T4_m.d_PV, cols = headerNames, rows = headerNames)
T4_m.d_table

# T7
T7_m.d<-morphol.disparity(T7_gdf$Shape~1, groups=lifestage, partial=TRUE, data=T7_gdf, iter=9999, print.progress = FALSE)
T7_m.d
T7_m.d_groups<-T7_m.d$Procrustes.var
T7_m.d_groups

T7_m.d_PV <- ifelse(T7_m.d$PV.dist.Pval<0.05, paste(round(T7_m.d$PV.dist,3), "*", sep=""), round(T7_m.d$PV.dist,3))
T7_m.d_table <- ggtexttable(T7_m.d_PV, cols = headerNames, rows = headerNames)
T7_m.d_table

# T10
T10_m.d<-morphol.disparity(T10_gdf$Shape~1, groups=lifestage, partial=TRUE, data=T10_gdf, iter=9999, print.progress = FALSE)
T10_m.d
T10_m.d_groups<-T10_m.d$Procrustes.var
T10_m.d_groups

T10_m.d_PV <- ifelse(T10_m.d$PV.dist.Pval<0.05, paste(round(T10_m.d$PV.dist,3), "*", sep=""), round(T10_m.d$PV.dist,3))
T10_m.d_table <- ggtexttable(T10_m.d_PV, cols = headerNames, rows = headerNames)
T10_m.d_table

# T13
T13_m.d<-morphol.disparity(T13_gdf$Shape~1, groups=lifestage, partial=TRUE, data=T13_gdf, iter=9999, print.progress = FALSE)
T13_m.d
T13_m.d_groups<-T13_m.d$Procrustes.var
T13_m.d_groups

T13_m.d_PV <- ifelse(T13_m.d$PV.dist.Pval<0.05, paste(round(T13_m.d$PV.dist,3), "*", sep=""), round(T13_m.d$PV.dist,3))
T13_m.d_table <- ggtexttable(T13_m.d_PV, cols = headerNames, rows = headerNames)
T13_m.d_table

# Sacral
Sacral_m.d<-morphol.disparity(Sacral_gdf$Shape~1, groups=lifestage, partial=TRUE, data=Sacral_gdf, iter=9999, print.progress = FALSE)
Sacral_m.d
Sacral_m.d_groups<-Sacral_m.d$Procrustes.var
Sacral_m.d_groups

Sacral_m.d_PV <- ifelse(Sacral_m.d$PV.dist.Pval<0.05, paste(round(Sacral_m.d$PV.dist,3), "*", sep=""), round(Sacral_m.d$PV.dist,3))
Sacral_m.d_table <- ggtexttable(Sacral_m.d_PV, cols = headerNames, rows = headerNames)
Sacral_m.d_table

# Caudal 1
Caud1_m.d<-morphol.disparity(Caud1_gdf$Shape~1, groups=lifestage, partial=TRUE, data=Caud1_gdf, iter=9999, print.progress = FALSE)
Caud1_m.d
Caud1_m.d_groups<-Caud1_m.d$Procrustes.var
Caud1_m.d_groups

Caud1_m.d_PV <- ifelse(Caud1_m.d$PV.dist.Pval<0.05, paste(round(Caud1_m.d$PV.dist,3), "*", sep=""), round(Caud1_m.d$PV.dist,3))
Caud1_m.d_table <- ggtexttable(Caud1_m.d_PV, cols = headerNames, rows = headerNames)
Caud1_m.d_table

# Caud2
Caud2_m.d<-morphol.disparity(Caud2_gdf$Shape~1, groups=Caud2lifestage, partial=TRUE, data=Caud2_gdf, iter=9999, print.progress = FALSE)
Caud2_m.d
Caud2_m.d_groups<-Caud2_m.d$Procrustes.var
Caud2_m.d_groups

Caud2_m.d_PV <- ifelse(Caud2_m.d$PV.dist.Pval<0.05, paste(round(Caud2_m.d$PV.dist,3), "*", sep=""), round(Caud2_m.d$PV.dist,3))
Caud2_m.d_table <- ggtexttable(Caud2_m.d_PV, cols = headerNames, rows = headerNames)
Caud2_m.d_table

# Caudal 3
Caud3_m.d<-morphol.disparity(Caud3_gdf$Shape~1, groups=lifestage, partial=TRUE, data=Caud3_gdf, iter=9999, print.progress = FALSE)
Caud3_m.d
Caud3_m.d_groups<-Caud3_m.d$Procrustes.var
Caud3_m.d_groups

Caud3_m.d_PV <- ifelse(Caud3_m.d$PV.dist.Pval<0.05, paste(round(Caud3_m.d$PV.dist,3), "*", sep=""), round(Caud3_m.d$PV.dist,3))
Caud3_m.d_table <- ggtexttable(Caud3_m.d_PV, cols = headerNames, rows = headerNames)
Caud3_m.d_table


MorphoDisparity_partial_tables1 <- ggarrange(Atlas_m.d_table, T1_m.d_table, T4_m.d_table, T7_m.d_table, 
                                             nrow = 4, ncol = 1)

MorphoDisparity_partial_tables2 <- ggarrange(T10_m.d_table, T13_m.d_table, Sacral_m.d_table, Caud1_m.d_table, 
                                             nrow = 4, ncol = 1)

MorphoDisparity_partial_tables3 <- ggarrange(Caud2_m.d_table, Caud3_m.d_table, 
                                             nrow = 2, ncol = 1)

#### Export Morphological Disparity Results to CSV ####

output_dir <- "F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_SecondRevisions/Results"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

regions <- c("Atlas", "T1", "T4", "T7", "T10", "T13", "Sacral", "Caud1", "Caud2", "Caud3")

# Initialize data frames
partial_disparity_summary <- data.frame()
total_disparity_pairwise <- data.frame()
partial_disparity_pairwise <- data.frame()

for (region in regions) {
  
  ## Partial Disparity (for Figure 7)
  partial_name <- paste0(region, "_m.d")
  if (exists(partial_name)) {
    obj <- get(partial_name)
    
    # Procrustes variances per group
    pv <- obj$Procrustes.var
    temp_summary <- data.frame(
      Region = region,
      Lifestage = names(pv),
      Procrustes_Variance = as.numeric(pv)
    )
    partial_disparity_summary <- rbind(partial_disparity_summary, temp_summary)
    
    # Pairwise comparisons - extract upper triangle only
    pw_dist <- as.matrix(obj$PV.dist)
    pw_pval <- as.matrix(obj$PV.dist.Pval)
    groups <- rownames(pw_dist)
    
    for (i in 1:(length(groups)-1)) {
      for (j in (i+1):length(groups)) {
        temp_pw <- data.frame(
          Region = region,
          Comparison = paste(groups[i], groups[j], sep = ":"),
          Distance = pw_dist[i, j],
          P_value = pw_pval[i, j]
        )
        partial_disparity_pairwise <- rbind(partial_disparity_pairwise, temp_pw)
      }
    }
  }
  
  ## Total Disparity Pairwise
  total_name <- paste0(region, "_m.dt3")
  if (exists(total_name)) {
    obj <- get(total_name)
    pw_dist <- as.matrix(obj$PV.dist)
    pw_pval <- as.matrix(obj$PV.dist.Pval)
    groups <- rownames(pw_dist)
    
    for (i in 1:(length(groups)-1)) {
      for (j in (i+1):length(groups)) {
        temp_pw <- data.frame(
          Region = region,
          Comparison = paste(groups[i], groups[j], sep = ":"),
          Distance = pw_dist[i, j],
          P_value = pw_pval[i, j]
        )
        total_disparity_pairwise <- rbind(total_disparity_pairwise, temp_pw)
      }
    }
  }
}

# Write CSVs
write.csv(partial_disparity_summary, 
          file.path(output_dir, "Partial_Disparity_Summary_ForFigure7.csv"), 
          row.names = FALSE)

write.csv(partial_disparity_pairwise, 
          file.path(output_dir, "Partial_Disparity_Pairwise.csv"), 
          row.names = FALSE)

write.csv(total_disparity_pairwise, 
          file.path(output_dir, "Total_Disparity_Pairwise.csv"), 
          row.names = FALSE)

cat("\n=== Disparity results exported to:", output_dir, "===\n")


#### Figures ####
# Intro/Methods
## Figure 1 Created in BioRender
## Figure 2 Created in BioRender
# Results
## Figure 3 - Atlas PCA
## Figure 4 - Trunk PCA 
## Figure 5 - Sacral PCA
## Figure 6 - Caudal PCA
## Figure 7 - Morphological Disparity

## Figures 3-6: PCAs

#Atlas PCA

Atlas.scores <- Atlas.PCA$x[, 1:2]
Atlas.df <- data.frame(Atlas.scores, Group = lifestage)

# Recode to match Figure 7 terminology
Atlas.df$Group <- factor(Atlas.df$Group,
                         levels = c("Adult", "Eft", "Juvenile", "Paedomorph"),
                         labels = c("Semi-aquatic adult", "Terr. juvenile", 
                                    "Aq. juvenile", "Aq. paedomorph"))

# Define colors to match Figure 7
stage_colors <- c("Semi-aquatic adult" = "chartreuse2",
                  "Terr. juvenile" = "darkorange1",
                  "Aq. juvenile" = "deepskyblue",
                  "Aq. paedomorph" = "darkmagenta")

Atlas.ggp <- ggplot(Atlas.df, aes(x = Atlas.scores[,1], y = Atlas.scores[,2], 
                                  color = Group, fill = Group)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom = "polygon", alpha = 0.2, lwd = 1) +
  xlab("PC1 - 63.71%") +
  ylab("PC2 - 10.21%") + 
  scale_color_manual(values = stage_colors) +
  scale_fill_manual(values = stage_colors) +
  theme_light() +
  theme(panel.grid = element_blank(), 
        axis.title.x = element_text(face = "bold", size = 13), 
        axis.title.y = element_text(face = "bold", size = 13),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        legend.position = "none") +
  labs(title = "Atlas", color = "Life Stage", fill = "Life Stage")

Atlas.ggp

ggsave("Figure3_Atlas.PCA.jpg", plot=last_plot(), device="jpg",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_SecondRevisions/Figures", scale=1, width = 100, height=100,
       units=c("mm"),dpi=300, limitsize = TRUE)


# Define colors once at the top (if not already defined)
stage_colors <- c("Semi-aquatic adult" = "chartreuse2",
                  "Terr. juvenile" = "darkorange1",
                  "Aq. juvenile" = "deepskyblue",
                  "Aq. paedomorph" = "darkmagenta")

# Trunk 1 PCA
T1.scores <- T1.PCA$x[, 1:2]
T1.df <- data.frame(T1.scores, Group = lifestage)
T1.df$Group <- factor(T1.df$Group,
                      levels = c("Adult", "Eft", "Juvenile", "Paedomorph"),
                      labels = c("Semi-aquatic adult", "Terr. juvenile", 
                                 "Aq. juvenile", "Aq. paedomorph"))

T1.ggp <- ggplot(T1.df, aes(x = T1.scores[,1], y = T1.scores[,2], 
                            color = Group, fill = Group)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom = "polygon", alpha = 0.2, lwd = 1) +
  xlab("PC1 (43.60%)") +
  ylab("PC2 (11.83%)") + 
  scale_color_manual(values = stage_colors) +
  scale_fill_manual(values = stage_colors) +
  theme_light() +
  theme(panel.grid = element_blank(), 
        axis.title.x = element_text(face = "bold", size = 13), 
        axis.title.y = element_text(face = "bold", size = 13),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        legend.position = "none") +
  labs(title = "T1")

T1.ggp

ggsave("Figure4_Trunk1.PCA.jpg", plot=last_plot(), device="jpg",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_SecondRevisions/Figures", scale=1, width = 100, height=100,
       units=c("mm"),dpi=300, limitsize = TRUE)


# Trunk 4 PCA
T4.scores <- T4.PCA$x[, 1:2]
T4.df <- data.frame(T4.scores, Group = lifestage)
T4.df$Group <- factor(T4.df$Group,
                      levels = c("Adult", "Eft", "Juvenile", "Paedomorph"),
                      labels = c("Semi-aquatic adult", "Terr. juvenile", 
                                 "Aq. juvenile", "Aq. paedomorph"))

T4.ggp <- ggplot(T4.df, aes(x = T4.scores[,1], y = T4.scores[,2], 
                            color = Group, fill = Group)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom = "polygon", alpha = 0.2, lwd = 1) +
  xlab("PC1 (50.12%)") +
  ylab("PC2 (10.48%)") + 
  scale_color_manual(values = stage_colors) +
  scale_fill_manual(values = stage_colors) +
  theme_light() +
  theme(panel.grid = element_blank(), 
        axis.title.x = element_text(face = "bold", size = 13), 
        axis.title.y = element_text(face = "bold", size = 13),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        legend.position = "none") +
  labs(title = "T4")

T4.ggp

ggsave("Figure4_Trunk4.PCA.jpg", plot=last_plot(), device="jpg",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_SecondRevisions/Figures", scale=1, width = 100, height=100,
       units=c("mm"),dpi=300, limitsize = TRUE)

# Trunk 7 PCA
T7.scores <- T7.PCA$x[, 1:2]
T7.df <- data.frame(T7.scores, Group = lifestage)
T7.df$Group <- factor(T7.df$Group,
                      levels = c("Adult", "Eft", "Juvenile", "Paedomorph"),
                      labels = c("Semi-aquatic adult", "Terr. juvenile", 
                                 "Aq. juvenile", "Aq. paedomorph"))

T7.ggp <- ggplot(T7.df, aes(x = T7.scores[,1], y = T7.scores[,2], 
                            color = Group, fill = Group)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom = "polygon", alpha = 0.2, lwd = 1) +
  xlab("PC1 (45.82%)") +
  ylab("PC2 (13.52%)") + 
  scale_color_manual(values = stage_colors) +
  scale_fill_manual(values = stage_colors) +
  theme_light() +
  theme(panel.grid = element_blank(), 
        axis.title.x = element_text(face = "bold", size = 13), 
        axis.title.y = element_text(face = "bold", size = 13),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        legend.position = "none") +
  labs(title = "T7")

T7.ggp

ggsave("Figure4_Trunk7.PCA.jpg", plot=last_plot(), device="jpg",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_SecondRevisions/Figures", scale=1, width = 100, height=100,
       units=c("mm"),dpi=300, limitsize = TRUE)

# Trunk 10 PCA
T10.scores <- T10.PCA$x[, 1:2]
T10.df <- data.frame(T10.scores, Group = lifestage)
T10.df$Group <- factor(T10.df$Group,
                       levels = c("Adult", "Eft", "Juvenile", "Paedomorph"),
                       labels = c("Semi-aquatic adult", "Terr. juvenile", 
                                  "Aq. juvenile", "Aq. paedomorph"))

T10.ggp <- ggplot(T10.df, aes(x = T10.scores[,1], y = T10.scores[,2], 
                              color = Group, fill = Group)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom = "polygon", alpha = 0.2, lwd = 1) +
  xlab("PC1 (41.10%)") +
  ylab("PC2 (19.28%)") + 
  scale_color_manual(values = stage_colors) +
  scale_fill_manual(values = stage_colors) +
  theme_light() +
  theme(panel.grid = element_blank(), 
        axis.title.x = element_text(face = "bold", size = 13), 
        axis.title.y = element_text(face = "bold", size = 13),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        legend.position = "none") +
  labs(title = "T10")

T10.ggp

ggsave("Figure4_Trunk10.PCA.jpg", plot=last_plot(), device="jpg",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_SecondRevisions/Figures", scale=1, width = 100, height=100,
       units=c("mm"),dpi=300, limitsize = TRUE)


# Trunk 13 PCA
T13.scores <- T13.PCA$x[, 1:2]
T13.df <- data.frame(T13.scores, Group = lifestage)
T13.df$Group <- factor(T13.df$Group,
                       levels = c("Adult", "Eft", "Juvenile", "Paedomorph"),
                       labels = c("Semi-aquatic adult", "Terr. juvenile", 
                                  "Aq. juvenile", "Aq. paedomorph"))

T13.ggp <- ggplot(T13.df, aes(x = T13.scores[,1], y = T13.scores[,2], 
                              color = Group, fill = Group)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom = "polygon", alpha = 0.2, lwd = 1) +
  xlab("PC1 (38.94%)") +
  ylab("PC2 (20.42%)") + 
  scale_color_manual(values = stage_colors) +
  scale_fill_manual(values = stage_colors) +
  theme_light() +
  theme(panel.grid = element_blank(), 
        axis.title.x = element_text(face = "bold", size = 13), 
        axis.title.y = element_text(face = "bold", size = 13),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        legend.position = "none") +
  labs(title = "T13")

T13.ggp

ggsave("Figure4_Trunk13.PCA.jpg", plot=last_plot(), device="jpg",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_SecondRevisions/Figures", scale=1, width = 100, height=100,
       units=c("mm"),dpi=300, limitsize = TRUE)


# Sacral PCA
Sacral.scores <- Sacral.PCA$x[, 1:2]
Sacral.df <- data.frame(Sacral.scores, Group = lifestage)
Sacral.df$Group <- factor(Sacral.df$Group,
                          levels = c("Adult", "Eft", "Juvenile", "Paedomorph"),
                          labels = c("Semi-aquatic adult", "Terr. juvenile", 
                                     "Aq. juvenile", "Aq. paedomorph"))

Sacral.ggp <- ggplot(Sacral.df, aes(x = Sacral.scores[,1], y = Sacral.scores[,2], 
                                    color = Group, fill = Group)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom = "polygon", alpha = 0.2, lwd = 1) +
  xlab("PC1 (40.79%)") +
  ylab("PC2 (21.66%)") + 
  scale_color_manual(values = stage_colors) +
  scale_fill_manual(values = stage_colors) +
  theme_light() +
  theme(panel.grid = element_blank(), 
        axis.title.x = element_text(face = "bold", size = 13), 
        axis.title.y = element_text(face = "bold", size = 13),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        legend.position = "none") +
  labs(title = "Sacral")

Sacral.ggp

ggsave("Figure5_Sacral.PCA.jpg", plot=last_plot(), device="jpg",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_SecondRevisions/Figures", scale=1, width = 100, height=100,
       units=c("mm"),dpi=300, limitsize = TRUE)

# Caud1 PCA
Caud1.scores <- Caud1.PCA$x[, 1:2]
Caud1.df <- data.frame(Caud1.scores, Group = lifestage)
Caud1.df$Group <- factor(Caud1.df$Group,
                         levels = c("Adult", "Eft", "Juvenile", "Paedomorph"),
                         labels = c("Semi-aquatic adult", "Terr. juvenile", 
                                    "Aq. juvenile", "Aq. paedomorph"))

Caud1.ggp <- ggplot(Caud1.df, aes(x = Caud1.scores[,1], y = Caud1.scores[,2], 
                                  color = Group, fill = Group)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom = "polygon", alpha = 0.2, lwd = 1) +
  xlab("PC1 (42.70%)") +
  ylab("PC2 (18.49%)") + 
  scale_color_manual(values = stage_colors) +
  scale_fill_manual(values = stage_colors) +
  theme_light() +
  theme(panel.grid = element_blank(), 
        axis.title.x = element_text(face = "bold", size = 13), 
        axis.title.y = element_text(face = "bold", size = 13),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        legend.position = "none") +
  labs(title = "Caudal 1")

Caud1.ggp

ggsave("Figure6_Caud1.PCA.jpg", plot=last_plot(), device="jpg",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_SecondRevisions/Figures", scale=1, width = 100, height=100,
       units=c("mm"),dpi=300, limitsize = TRUE)


# Caud2 PCA (uses Caud2lifestage due to missing specimens ID28 & ID45)
Caud2.scores <- Caud2.PCA$x[, 1:2]
Caud2.df <- data.frame(Caud2.scores, Group = Caud2lifestage)
Caud2.df$Group <- factor(Caud2.df$Group,
                         levels = c("Adult", "Eft", "Juvenile", "Paedomorph"),
                         labels = c("Semi-aquatic adult", "Terr. juvenile", 
                                    "Aq. juvenile", "Aq. paedomorph"))

Caud2.ggp <- ggplot(Caud2.df, aes(x = Caud2.scores[,1], y = Caud2.scores[,2], 
                                  color = Group, fill = Group)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom = "polygon", alpha = 0.2, lwd = 1) +
  xlab("PC1 (42.59%)") +
  ylab("PC2 (15.19%)") + 
  scale_color_manual(values = stage_colors) +
  scale_fill_manual(values = stage_colors) +
  theme_light() +
  theme(panel.grid = element_blank(), 
        axis.title.x = element_text(face = "bold", size = 13), 
        axis.title.y = element_text(face = "bold", size = 13),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        legend.position = "none") +
  labs(title = "Caudal 2")

Caud2.ggp

ggsave("Figure6_Caud2.PCA.jpg", plot=last_plot(), device="jpg",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_SecondRevisions/Figures", scale=1, width = 100, height=100,
       units=c("mm"),dpi=300, limitsize = TRUE)

# Caud3 PCA
Caud3.scores <- Caud3.PCA$x[, 1:2]
Caud3.df <- data.frame(Caud3.scores, Group = lifestage)
Caud3.df$Group <- factor(Caud3.df$Group,
                         levels = c("Adult", "Eft", "Juvenile", "Paedomorph"),
                         labels = c("Semi-aquatic adult", "Terr. juvenile", 
                                    "Aq. juvenile", "Aq. paedomorph"))

Caud3.ggp <- ggplot(Caud3.df, aes(x = Caud3.scores[,1], y = Caud3.scores[,2], 
                                  color = Group, fill = Group)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom = "polygon", alpha = 0.2, lwd = 1) +
  xlab("PC1 (43.08%)") +
  ylab("PC2 (24.32%)") + 
  scale_color_manual(values = stage_colors) +
  scale_fill_manual(values = stage_colors) +
  theme_light() +
  theme(panel.grid = element_blank(), 
        axis.title.x = element_text(face = "bold", size = 13), 
        axis.title.y = element_text(face = "bold", size = 13),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        legend.position = "none") +
  labs(title = "Caudal 3")

Caud3.ggp


ggsave("Figure6_Caud3.PCA.jpg", plot=last_plot(), device="jpg",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_SecondRevisions/Figures", scale=1, width = 100, height=100,
       units=c("mm"),dpi=300, limitsize = TRUE)


# Figure 7 - Morphological Disparity
# This is intended to display total morphological variation at each vertebral position
# setwd("F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_SecondRevisions/Results")

library(dplyr)

# Load data from CSV
data_long <- read.csv("Partial_Disparity_Summary_ForFigure7.csv")

# Rename columns and lifestage labels to match manuscript terminology
data_long <- data_long %>%
  rename(`Vertebral Position` = Region,
         LifeStage = Lifestage,
         Disparity = Procrustes_Variance) %>%
  mutate(LifeStage = case_when(
    LifeStage == "Adult" ~ "Semi-aquatic adult",
    LifeStage == "Eft" ~ "Terr. juvenile",
    LifeStage == "Juvenile" ~ "Aq. juvenile",
    LifeStage == "Paedomorph" ~ "Aq. paedomorph",
    TRUE ~ LifeStage
  ))

# Calculate Total Disparity per region (sum of partial disparities)
total_disparity <- data_long %>%
  group_by(`Vertebral Position`) %>%
  summarize(`Total Disparity` = sum(Disparity))

# Merge total disparity back for the line plot
data_total <- left_join(data_long, total_disparity, by = "Vertebral Position")

# Define colors
stage_colors <- c("Semi-aquatic adult" = "chartreuse2",
                  "Terr. juvenile" = "darkorange1",
                  "Aq. juvenile" = "deepskyblue",
                  "Aq. paedomorph" = "darkmagenta")

# Define order for Vertebral Position
order_positions <- c("Atlas", "T1", "T4", "T7", "T10", "T13", "Sacral", "Caud1", "Caud2", "Caud3")
data_long$`Vertebral Position` <- factor(data_long$`Vertebral Position`, levels = order_positions)
data_total$`Vertebral Position` <- factor(data_total$`Vertebral Position`, levels = order_positions)
total_disparity$`Vertebral Position` <- factor(total_disparity$`Vertebral Position`, levels = order_positions)

# Plot
Fig7.ggp <- ggplot() +
  geom_col(data = data_long,
           aes(x = `Vertebral Position`, y = Disparity, fill = LifeStage),
           position = position_dodge(width = 0.8), width = 0.7,
           color = "black") +
  geom_line(data = total_disparity,
            aes(x = `Vertebral Position`, y = `Total Disparity`, group = 1, color = "Total Disparity"),
            linewidth = 1.2) +
  geom_point(data = total_disparity,
             aes(x = `Vertebral Position`, y = `Total Disparity`, color = "Total Disparity"),
             size = 3) +
  # Set fill order and labels
  scale_fill_manual(values = stage_colors,
                    breaks = c("Semi-aquatic adult", "Terr. juvenile", "Aq. juvenile", "Aq. paedomorph"),
                    labels = c("Semi-aquatic adult", "Terr. juvenile", "Aq. juvenile", "Aq. paedomorph")) +
  scale_color_manual(values = c("Total Disparity" = "black")) +
  theme_minimal() +
  labs(x = "Vertebral Position", y = "Morphological Disparity (Partial Procrustes Variance)",
       fill = "Life Stage", color = "") +
  theme(
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text.x  = element_text(face = "bold", size = 12, angle = 45, hjust = 1),
    axis.text.y  = element_text(face = "bold", size = 12),
    plot.title   = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.title = element_text(face = "bold", size = 13),
    legend.text  = element_text(size = 12)
  )

Fig7.ggp




#### PCA Mesh Warping Visualization ####
#
# To visualize PCA mesh warpings, we will calculate the Procrustes distance 
# from each specimen to the mean shape and find the minimum.
#
# LM coordinates of the PC extremes are exported as fscv files, then used for 
# mesh warping in 3D Slicer
#
# Find closest specimen to mean using SYMMETRIZED coordinates
# List of bilateral symmetry objects
bilat_list <- list(
  Atlas = Atlas_bilat,
  T1 = T1_bilat,
  T4 = T4_bilat,
  T7 = T7_bilat,
  T10 = T10_bilat,
  T13 = T13_bilat,
  Sacral = Sacral_bilat,
  Caud1 = Caud1_bilat,
  Caud2 = Caud2_bilat,
  Caud3 = Caud3_bilat
)

# Find closest specimen for each region
closest_specimens <- data.frame(
  Region = character(),
  Specimen = character(),
  Procrustes_Distance = numeric(),
  stringsAsFactors = FALSE
)

for (region in names(bilat_list)) {
  # Use symmetrized coordinates
  coords <- bilat_list[[region]]$symm.shape
  mean_shape <- mshape(coords)
  n_spec <- dim(coords)[3]
  
  proc_dist <- numeric(n_spec)
  for (i in 1:n_spec) {
    proc_dist[i] <- sqrt(sum((coords[,,i] - mean_shape)^2))
  }
  names(proc_dist) <- dimnames(coords)[[3]]
  
  closest_specimens <- rbind(closest_specimens, data.frame(
    Region = region,
    Specimen = names(which.min(proc_dist)),
    Procrustes_Distance = min(proc_dist)
  ))
}

print(closest_specimens)

#Region   Specimen Procrustes_Distance
#1   Atlas  Atlas_008          0.04560877
#2      T1     T1_008          0.04535458
#3      T4     T4_014          0.04891488
#4      T7     T7_014          0.04702169
#5     T10  T10_008_1          0.05728301
#6     T13  T13_003_1          0.05644183
#7  Sacral Sacral_010          0.05681238
#8   Caud1  Caud1_008          0.05544964
#9   Caud2  Caud2_007          0.07536946
#10  Caud3  Caud3_007          0.06595227

output_dir <- "F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_SecondRevisions/Figures/PCA Visualization/Landmarks"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Reference specimens closest to mean
reference_specimens <- c(
  Atlas = "Atlas_008",
  T1 = "T1_008",
  T4 = "T4_014",
  T7 = "T7_014",
  T10 = "T10_008_1",
  T13 = "T13_003_1",
  Sacral = "Sacral_010",
  Caud1 = "Caud1_008",
  Caud2 = "Caud2_007",
  Caud3 = "Caud3_007"
)

# Store variance info
pc_variance <- data.frame(
  Region = character(),
  PC1_var = numeric(),
  PC2_var = numeric(),
  PC1_sd = numeric(),
  PC2_sd = numeric(),
  stringsAsFactors = FALSE
)

for (region in names(bilat_list)) {
  
  cat("\n========== Processing", region, "==========\n")
  
  # Get symmetrized coordinates
  coords <- bilat_list[[region]]$symm.shape
  
  # Get reference specimen name and index
  ref_spec <- reference_specimens[region]
  ref_index <- which(dimnames(coords)[[3]] == ref_spec)
  
  if (length(ref_index) == 0) {
    warning(paste("Reference specimen", ref_spec, "not found in", region))
    next
  }
  
  # Extract symmetrized coordinates for reference specimen
  ref_symmetrized <- coords[,,ref_index]
  
  # Calculate mean shape
  mean_shape <- mshape(coords)
  
  # Run PCA on symmetrized coordinates
  pca_result <- gm.prcomp(coords)
  
  # Get variance explained
  pc_var <- summary(pca_result)$PC.summary
  pc1_percent <- round(pc_var[2, 1] * 100, 2)
  pc2_percent <- round(pc_var[2, 2] * 100, 2)
  
  # Calculate standard deviations of PC scores
  pc1_sd <- sd(pca_result$x[, 1])
  pc2_sd <- sd(pca_result$x[, 2])
  
  pc_variance <- rbind(pc_variance, data.frame(
    Region = region,
    PC1_var = pc1_percent,
    PC2_var = pc2_percent,
    PC1_sd = pc1_sd,
    PC2_sd = pc2_sd
  ))
  
  cat("PC1:", pc1_percent, "% (SD =", round(pc1_sd, 5), ")\n")
  cat("PC2:", pc2_percent, "% (SD =", round(pc2_sd, 5), ")\n")
  
  # Get rotation vectors and reshape to landmark matrix
  n_landmarks <- dim(coords)[1]
  pc1_vector <- matrix(pca_result$rotation[, 1], nrow = n_landmarks, ncol = 3, byrow = TRUE)
  pc2_vector <- matrix(pca_result$rotation[, 2], nrow = n_landmarks, ncol = 3, byrow = TRUE)
  
  # Generate shapes at ±2 SD
  shape_negPC1 <- mean_shape + pc1_vector * (-2 * pc1_sd)
  shape_posPC1 <- mean_shape + pc1_vector * (2 * pc1_sd)
  shape_negPC2 <- mean_shape + pc2_vector * (-2 * pc2_sd)
  shape_posPC2 <- mean_shape + pc2_vector * (2 * pc2_sd)
  
  # ----- EXPORT ALL FCSV FILES -----
  
  # 1. Symmetrized coordinates for reference specimen
  write.markups.fcsv(ref_symmetrized, 
                     file.path(output_dir, paste0(region, "_", ref_spec, "_symmetrized.fcsv")))
  cat("Exported:", paste0(region, "_", ref_spec, "_symmetrized.fcsv"), "\n")
  
  # 2. Mean shape
  write.markups.fcsv(mean_shape, 
                     file.path(output_dir, paste0(region, "_mean_shape.fcsv")))
  cat("Exported:", paste0(region, "_mean_shape.fcsv"), "\n")
  
  # 3. PC1 extremes (±2 SD)
  write.markups.fcsv(shape_negPC1, 
                     file.path(output_dir, paste0(region, "_neg2SD_PC1.fcsv")))
  write.markups.fcsv(shape_posPC1, 
                     file.path(output_dir, paste0(region, "_pos2SD_PC1.fcsv")))
  cat("Exported:", paste0(region, "_neg2SD_PC1.fcsv"), "and", paste0(region, "_pos2SD_PC1.fcsv"), "\n")
  
  # 4. PC2 extremes (±2 SD)
  write.markups.fcsv(shape_negPC2, 
                     file.path(output_dir, paste0(region, "_neg2SD_PC2.fcsv")))
  write.markups.fcsv(shape_posPC2, 
                     file.path(output_dir, paste0(region, "_pos2SD_PC2.fcsv")))
  cat("Exported:", paste0(region, "_neg2SD_PC2.fcsv"), "and", paste0(region, "_pos2SD_PC2.fcsv"), "\n")
}


#### Supplemental Allometry Figures ####
# Two multi-panel figures:
# Figure S_A: PC1 vs log(Centroid Size)
# Figure S_B: Regression Score vs log(Centroid Size)
#
# Uses plotAllometry() from geomorph to extract regression scores,
# then builds publication-quality figures with ggplot2

# Define color scheme to match existing figures exactly
life_stage_colors <- c(
  "Adult" = "chartreuse2",
  "Eft" = "darkorange1",
  "Juvenile" = "deepskyblue",
  "Paedomorph" = "darkmagenta"
)

# Life stage labels for legend
life_stage_labels <- c(
  "Adult" = "Semi-aquatic adult",
  "Eft" = "Terr. juvenile",
  "Juvenile" = "Aq. juvenile",
  "Paedomorph" = "Aq. paedomorph"
)

# List of regions
regions <- c("Atlas", "T1", "T4", "T7", "T10", "T13", "Sacral", "Caud1", "Caud2", "Caud3")

# Storage for plot data
pc1_plot_data <- list()
regscore_plot_data <- list()

## Extract data for each region

for (region in regions) {
  
  cat("Processing", region, "...\n")
  
  # Get the appropriate data frame, PCA, and procD.lm model
  gdf <- get(paste0(region, "_gdf"))
  pca <- get(paste0(region, ".PCA"))
  model <- get(paste0(region, "_proc_ANOVA_m4"))  # Shape ~ Size * lifestage model
  
  # Extract PC1 scores from PCA
  pc1_scores <- pca$x[, 1]
  
  # Get size and lifestage
  size <- gdf$Size  # Already log-transformed
  lifestage <- gdf$lifestage
  
  # Store PC1 plot data
  pc1_plot_data[[region]] <- data.frame(
    Region = region,
    PC1 = pc1_scores,
    LogCS = size,
    Lifestage = lifestage
  )
  
  # Use plotAllometry to get regression scores
  # Open null PDF device to suppress the base R plot output
  pdf(NULL)
  allom_output <- plotAllometry(model, size = size, logsz = FALSE, method = "RegScore")
  invisible(dev.off())
  
  # Extract regression scores from the plotAllometry output
  reg_scores <- allom_output$RegScore
  
  # Store RegScore plot data
  regscore_plot_data[[region]] <- data.frame(
    Region = region,
    RegScore = as.numeric(reg_scores),
    LogCS = size,
    Lifestage = lifestage
  )
}

# Combine all data
pc1_all <- do.call(rbind, pc1_plot_data)
regscore_all <- do.call(rbind, regscore_plot_data)

# Set factor levels for consistent ordering
pc1_all$Region <- factor(pc1_all$Region, levels = regions)
pc1_all$Lifestage <- factor(pc1_all$Lifestage, levels = c("Adult", "Eft", "Juvenile", "Paedomorph"))

regscore_all$Region <- factor(regscore_all$Region, levels = regions)
regscore_all$Lifestage <- factor(regscore_all$Lifestage, levels = c("Adult", "Eft", "Juvenile", "Paedomorph"))


## Figure S_A: PC1 vs log(Centroid Size)

fig_pc1 <- ggplot(pc1_all, aes(x = LogCS, y = PC1, color = Lifestage, fill = Lifestage)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  facet_wrap(~ Region, ncol = 5, scales = "free") +
  scale_color_manual(values = life_stage_colors, labels = life_stage_labels) +
  scale_fill_manual(values = life_stage_colors, labels = life_stage_labels) +
  labs(
    x = "log(Centroid Size)",
    y = "PC1 Score",
    color = "Life Stage",
    fill = "Life Stage"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 9),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "white"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

fig_pc1

ggsave("Supplemental_Allometry_PC1-logCS.png", plot=last_plot(), device="png",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_SecondRevisions/Figures", scale=1, width = 200, height=150,
       units=c("mm"),dpi=300, limitsize = TRUE)

cat("Saved: Supplemental_Allometry_PC1-logCS.png\n")


## Figure S_B: Regression Score vs log(Centroid Size)

fig_regscore <- ggplot(regscore_all, aes(x = LogCS, y = RegScore, color = Lifestage, fill = Lifestage)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  facet_wrap(~ Region, ncol = 5, scales = "free") +
  scale_color_manual(values = life_stage_colors, labels = life_stage_labels) +
  scale_fill_manual(values = life_stage_colors, labels = life_stage_labels) +
  labs(
    x = "log(Centroid Size)",
    y = "Regression Score (Shape ~ Size)",
    color = "Life Stage",
    fill = "Life Stage"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 9),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "white"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

fig_regscore


ggsave("Supplemental_Allometry_RegScore-logCS.png", plot=last_plot(), device="png",
       path="F:/Eastern Newt Project/Vertebrae_Analysis_Corrections/JOA_SecondRevisions/Figures", scale=1, width = 200, height=150,
       units=c("mm"),dpi=300, limitsize = TRUE)

cat("Saved: FigureS_RegScore_vs_LogCS.png\n")


