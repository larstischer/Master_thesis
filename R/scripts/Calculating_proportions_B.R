## Calculating proportion data of table A
## 08/10/24
# working directory
setwd(".../R")

# read data
dat1 <- read.csv("data/new_datB.csv", sep = ";")

# add heigth and delta13C values to columns
heights <- c(500, 590, 680, 770, 860, 950, 1040, 1130, 1220, 1310, 1400)
d13C <- c(3.56, 4.09, 4.17, 4.62, 4.63, 4.88, 4.83, 4.86, 5.34, 5.18, 5.5)

dat1$Height_cm <- heights
dat1$Delta13C <- d13C
# new table for prop table
p_datB <- dat1[, c("Sample_ID", "Height_cm", "Delta13C")]

# calculating proportions
# for each component
p_datB$Prop_Tab_Coral <- dat1$Tab_Coral / dat1$Sum
#p_datA$Prop_non_Tab_Coral <- dat1$non_Tab_Coral / dat1$Sum

p_datB$Prop_Rug_Coral <- dat1$Rug_Coral / dat1$Sum
#p_datA$Prop_non_Rug_Coral <- dat1$non_Rug_Coral / dat1$Sum

p_datB$Prop_Strom <- dat1$Strom / dat1$Sum
#p_datA$Prop_non_Strom <- dat1$non_Strom / dat1$Sum

p_datB$Prop_Spo <- dat1$Spo / dat1$Sum
#p_datA$Prop_non_Spo <- dat1$non_Spo / dat1$Sum

p_datB$Prop_Bry <- dat1$Bry / dat1$Sum
#p_datA$Prop_non_Bry <- dat1$non_Bry / dat1$Sum

p_datB$Prop_Mic <- dat1$Mic / dat1$Sum
#p_datA$Prop_non_Mic <- dat1$non_Mic / dat1$Sum

p_datB$Prop_Ech <- dat1$Ech / dat1$Sum
#p_datA$Prop_non_Ech <- dat1$non_Ech / dat1$Sum

p_datB$Prop_Brach <- dat1$Brach / dat1$Sum
#p_datA$Prop_non_Brach <- dat1$non_Brach / dat1$Sum

p_datB$Prop_Problem <- dat1$Problem / dat1$Sum
#p_datA$Prop_non_Problem <- dat1$non_Problem / dat1$Sum

p_datB$Prop_Moll <- dat1$Moll / dat1$Sum
#p_datA$Prop_non_Moll <- dat1$non_Moll / dat1$Sum

p_datB$Prop_Tri <- dat1$Tri / dat1$Sum
#p_datA$Prop_non_Tri <- dat1$non_Tri / dat1$Sum

p_datB$Prop_Ost <- dat1$Ost / dat1$Sum
#p_datA$Prop_non_Ost <- dat1$non_Ost / dat1$Sum

p_datB$Prop_Alg <- dat1$Alg / dat1$Sum
#p_datA$Prop_non_Alg <- dat1$non_Alg / dat1$Sum

#p_datB$Prop_Tas <- dat1$Tas / dat1$Sum
#p_datA$Prop_non_Tas <- dat1$non_Tas / dat1$Sum

p_datB$Prop_Micrite <- dat1$Micrite / dat1$Sum
#p_datA$Prop_non_Micrite <- dat1$non_Micrite / dat1$Sum

p_datB$Prop_Spar <- dat1$Spar / dat1$Sum
#p_datA$Prop_non_Spar <- dat1$non_Spar / dat1$Sum

p_datB$Prop_Pyr <- dat1$Pyr / dat1$Sum
#p_datA$Prop_non_Pyr <- dat1$non_Pyr / dat1$Sum

p_datB$Prop_Org <- dat1$Org / dat1$Sum
#p_datA$Prop_non_Org <- dat1$non_Org / dat1$Sum

p_datB$Prop_nonId <- dat1$non_Identified / dat1$Sum
#p_datA$Prop_Id <- dat1$Identified / dat1$Sum

# save table
#write.csv2(p_datA, file = "data/p1_datA.csv", row.names = FALSE) # complete data
write.csv(p_datB, file = "data/p_datB.csv", row.names = FALSE) # without non_data

###
# Proportion of ov_data
ov_dat <- read.csv("data/ov_datB.csv", sep = ";")

# add heigth and delta13C values to columns
heights <- c(500, 590, 680, 770, 860, 950, 1040, 1130, 1220, 1310, 1400)
d13C <- c(3.56, 4.09, 4.17, 4.62, 4.63, 4.88, 4.83, 4.86, 5.34, 5.18, 5.5)

ov_dat$Height_cm <- heights
ov_dat$Delta13C <- d13C

##
# new table for prop table
p_ovdatB <- ov_dat[, c("Sample_ID", "Height_cm", "Delta13C", "Sum")]

# calculations
p_ovdatB$Micrite <- ov_dat$Micrite / ov_dat$Sum

p_ovdatB$Sparite <- ov_dat$Spar / ov_dat$Sum

p_ovdatB$Biogenic <- ov_dat$biogenic / ov_dat$Sum

p_ovdatB$Other <- ov_dat$Others / ov_dat$Sum

p_ovdatB$Non_Identified <- ov_dat$Non_Identified / ov_dat$Sum

#####
# calculate the sum of each component for each sample
p_ovdatB$Total <- rowSums(p_ovdatB[, c("Micrite", "Sparite", "Biogenic", "Other", "Non_Identified")], na.rm = TRUE)

# save table with overview data
write.csv2(p_ovdatB, file = "data/p_ovdatB.csv", row.names = FALSE)

####
# proportion of biogenic data
bio_dat <- read.csv("data/bio_datB.csv", sep = ";")

# add heigth and delta13C values to columns
heights <- c(500, 590, 680, 770, 860, 950, 1040, 1130, 1220, 1310, 1400)
d13C <- c(3.56, 4.09, 4.17, 4.62, 4.63, 4.88, 4.83, 4.86, 5.34, 5.18, 5.5)

bio_dat$Height_cm <- heights
bio_dat$Delta13C <- d13C

##
# new table for prop table
p_biodatB <- bio_dat[, c("Sample_ID", "Height_cm", "Delta13C", "Sum")]

# calculations
p_biodatB$Corals <- bio_dat$Corals / bio_dat$Sum

p_biodatB$Sponges <- bio_dat$Sponges / bio_dat$Sum

p_biodatB$Bryozoans <- bio_dat$Bry / bio_dat$Sum

p_biodatB$Microbes <- bio_dat$Mic / bio_dat$Sum

p_biodatB$Microproblematica <- bio_dat$Problem / bio_dat$Sum

p_biodatB$Green_Algae <- bio_dat$Alg / bio_dat$Sum

p_biodatB$Echinoderms <- bio_dat$Ech / bio_dat$Sum

p_biodatB$Brachiopods <- bio_dat$Brach / bio_dat$Sum

p_biodatB$Molluscs <- bio_dat$Moll / bio_dat$Sum

p_biodatB$Trilobites <- bio_dat$Tri / bio_dat$Sum

p_biodatB$Ostracods <- bio_dat$Ost / bio_dat$Sum

#p_biodatA$Tasmanites <- bio_dat$Tas / bio_dat$Sum

#####
# calculate the sum of each component for each sample
p_biodatB$Total <- rowSums(p_biodatB[, c("Corals", "Sponges", "Bryozoans", "Microbes", "Microproblematica", 
                                         "Green_Algae", "Echinoderms", "Molluscs", "Brachiopods", "Trilobites", "Ostracods")], na.rm = TRUE)

# save table with biogenic data
write.csv2(p_biodatB, file = "data/p_biodatB.csv", row.names = FALSE)

