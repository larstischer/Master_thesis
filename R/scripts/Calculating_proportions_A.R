## Calculating proportion data of table A
## 08/10/24
# working directory
setwd(".../R")

# read data
dat1 <- read.csv("data/new_datA.csv", sep = ";")

# add heigth and delta13C values to columns
heights <- c(200, 290, 380, 470, 590, 680, 760, 850, 940, 1030, 1120, 1210, 1300, 1390)
d13C <- c(2.09, 1.89, 1.9, 2.87, 2.71, 3.71, 3.5, 4.52, 5.06, 4.51, 5.06, 4.98, 4.72, 3.99)

dat1$Height_cm <- heights
dat1$Delta13C <- d13C
# new table for prop table
p_datA <- dat1[, c("Sample_ID", "Height_cm", "Delta13C")]

# calculating proportions
# proportion for each component
p_datA$Prop_Tab_Coral <- dat1$Tab_Coral / dat1$Sum
#p_datA$Prop_non_Tab_Coral <- dat1$non_Tab_Coral / dat1$Sum

p_datA$Prop_Rug_Coral <- dat1$Rug_Coral / dat1$Sum
#p_datA$Prop_non_Rug_Coral <- dat1$non_Rug_Coral / dat1$Sum

p_datA$Prop_Strom <- dat1$Strom / dat1$Sum
#p_datA$Prop_non_Strom <- dat1$non_Strom / dat1$Sum

p_datA$Prop_Spo <- dat1$Spo / dat1$Sum
#p_datA$Prop_non_Spo <- dat1$non_Spo / dat1$Sum

p_datA$Prop_Bry <- dat1$Bry / dat1$Sum
#p_datA$Prop_non_Bry <- dat1$non_Bry / dat1$Sum

p_datA$Prop_Mic <- dat1$Mic / dat1$Sum
#p_datA$Prop_non_Mic <- dat1$non_Mic / dat1$Sum

p_datA$Prop_Ech <- dat1$Ech / dat1$Sum
#p_datA$Prop_non_Ech <- dat1$non_Ech / dat1$Sum

p_datA$Prop_Brach <- dat1$Brach / dat1$Sum
#p_datA$Prop_non_Brach <- dat1$non_Brach / dat1$Sum

p_datA$Prop_Problem <- dat1$Problem / dat1$Sum
#p_datA$Prop_non_Problem <- dat1$non_Problem / dat1$Sum

p_datA$Prop_Moll <- dat1$Moll / dat1$Sum
#p_datA$Prop_non_Moll <- dat1$non_Moll / dat1$Sum

p_datA$Prop_Tri <- dat1$Tri / dat1$Sum
#p_datA$Prop_non_Tri <- dat1$non_Tri / dat1$Sum

p_datA$Prop_Ost <- dat1$Ost / dat1$Sum
#p_datA$Prop_non_Ost <- dat1$non_Ost / dat1$Sum

p_datA$Prop_Alg <- dat1$Alg / dat1$Sum
#p_datA$Prop_non_Alg <- dat1$non_Alg / dat1$Sum

p_datA$Prop_Tas <- dat1$Tas / dat1$Sum
#p_datA$Prop_non_Tas <- dat1$non_Tas / dat1$Sum

p_datA$Prop_Micrite <- dat1$Micrite / dat1$Sum
#p_datA$Prop_non_Micrite <- dat1$non_Micrite / dat1$Sum

p_datA$Prop_Spar <- dat1$Spar / dat1$Sum
#p_datA$Prop_non_Spar <- dat1$non_Spar / dat1$Sum

p_datA$Prop_Pyr <- dat1$Pyr / dat1$Sum
#p_datA$Prop_non_Pyr <- dat1$non_Pyr / dat1$Sum

p_datA$Prop_Org <- dat1$Org / dat1$Sum
#p_datA$Prop_non_Org <- dat1$non_Org / dat1$Sum

p_datA$Prop_nonId <- dat1$non_Identified / dat1$Sum
#p_datA$Prop_Id <- dat1$Identified / dat1$Sum

# save table with proportion data

write.csv(p_datA, file = "data/p_datA.csv", row.names = FALSE) 

###
# Proportion of ov_data
ov_dat <- read.csv("data/ov_datA.csv", sep = ";")

# add heigth and delta13C values to columns
heights <- c(200, 290, 380, 470, 590, 680, 760, 850, 940, 1030, 1120, 1210, 1300, 1390)
d13C <- c(2.09, 1.89, 1.9, 2.87, 2.71, 3.71, 3.5, 4.52, 5.06, 4.51, 5.06, 4.98, 4.72, 3.99)

ov_dat$Height_cm <- heights
ov_dat$Delta13C <- d13C

##
# new table for prop table
p_ovdatA <- ov_dat[, c("Sample_ID", "Height_cm", "Delta13C", "Sum")]

# calculations
p_ovdatA$Micrite <- ov_dat$Micrite / ov_dat$Sum

p_ovdatA$Sparite <- ov_dat$Spar / ov_dat$Sum

p_ovdatA$Biogenic <- ov_dat$biogenic / ov_dat$Sum

p_ovdatA$Other <- ov_dat$Others / ov_dat$Sum

p_ovdatA$Non_Identified <- ov_dat$Non_Identified / ov_dat$Sum

#####
# calculate the sum for each component for each sample 
p_ovdatA$Total <- rowSums(p_ovdatA[, c("Micrite", "Sparite", "Biogenic", "Other", "Non_Identified")], na.rm = TRUE)

# save table with proportions for overview data
write.csv2(p_ovdatA, file = "data/p_ovdatA.csv", row.names = FALSE)

####
# proportion of biogenic data
bio_dat <- read.csv("data/bio_datA.csv", sep = ";")

# add heigth and delta13C values to columns
heights <- c(200, 290, 380, 470, 590, 680, 760, 850, 940, 1030, 1120, 1210, 1300, 1390)
d13C <- c(2.09, 1.89, 1.9, 2.87, 2.71, 3.71, 3.5, 4.52, 5.06, 4.51, 5.06, 4.98, 4.72, 3.99)

bio_dat$Height_cm <- heights
bio_dat$Delta13C <- d13C

##
# new table for prop table
p_biodatA <- bio_dat[, c("Sample_ID", "Height_cm", "Delta13C", "Sum")]

# calculations
p_biodatA$Corals <- bio_dat$Corals / bio_dat$Sum

p_biodatA$Sponges <- bio_dat$Sponges / bio_dat$Sum

p_biodatA$Bryozoans <- bio_dat$Bry / bio_dat$Sum

p_biodatA$Microbes <- bio_dat$Mic / bio_dat$Sum

p_biodatA$Microproblematica <- bio_dat$Problem / bio_dat$Sum

p_biodatA$Green_Algae <- bio_dat$Alg / bio_dat$Sum

p_biodatA$Echinoderms <- bio_dat$Ech / bio_dat$Sum

p_biodatA$Brachiopods <- bio_dat$Brach / bio_dat$Sum

p_biodatA$Molluscs <- bio_dat$Moll / bio_dat$Sum

p_biodatA$Trilobites <- bio_dat$Tri / bio_dat$Sum

p_biodatA$Ostracods <- bio_dat$Ost / bio_dat$Sum

#p_biodatA$Tasmanites <- bio_dat$Tas / bio_dat$Sum

#####
# Calculate the sum of each component for each sample
p_biodatA$Total <- rowSums(p_biodatA[, c("Corals", "Sponges", "Bryozoans", "Microbes", "Microproblematica", 
                                         "Green_Algae", "Echinoderms", "Molluscs", "Brachiopods", "Trilobites", "Ostracods")], na.rm = TRUE)

# save table with proportions for biogenic data
write.csv2(p_biodatA, file = "data/p_biodatA.csv", row.names = FALSE)
