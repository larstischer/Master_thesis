# 04/10/24
# Preparing for statistical analysis

# set directory
setwd(".../R")

# read data
datA <- read.csv("data/Pointcount_A.csv", sep = ";")
datB <- read.csv("data/Pointcount_B.csv", sep = ";")

# prepare data
is.na(datA) <- datA == ""
is.na(datB) <- datB == ""


# new table with new columns for further analyses
new_dat <- data.frame(
  Sample_ID = colnames(datA),
  Height_cm = 0,
  Delta13C = 0,
  Sum = 0,
  Tab_Coral = 0, # Tabulate Coral
  non_Tab_Coral = 0,
  Rug_Coral = 0, # Rugose Coral
  non_Rug_Coral = 0,
  Strom = 0,  # Stromatoporoid
  non_Strom = 0,
  Spo = 0, # Siliceous Sponge
  non_Spo = 0,
  Bry = 0,    # Bryozoan
  non_Bry = 0,
  Mic = 0,    # Microbe
  non_Mic = 0,
  Ech = 0,    # Echinoderm und Crinoid
  non_Ech = 0,
  Brach = 0,  # Brachiopod und Pentameride Brachiopod
  non_Brach = 0,
  Problem = 0,  # Allonema, Rothpletzella, Girvanella
  non_Problem = 0,
  Moll = 0,   # Mollusc, Gastropod, Bivalve, Rostroconch
  non_Moll = 0,
  Tri = 0,    # Trilobite
  non_Tri = 0,
  Ost = 0,    # Ostracod
  non_Ost = 0,
  Alg = 0,    # Green Algae
  non_Alg = 0,
  Tas = 0, # Tasmanites
  non_Tas = 0,
  Micrite = 0, # Micrite
  non_Micrite = 0,
  Spar = 0, # Sparite
  non_Spar = 0,
  Pyr = 0, # Pyrite
  non_Pyr = 0,
  Org = 0, # Organic
  non_Org = 0,
  non_Identified = 0, # Unidentified Components
  Identified = 0
)

# Calculation of counts of the different components for each sample
for (i in 1:ncol(datA)) {
  sample_id <- colnames(datA)[i]
  
  # Sum of all entries that are not NA's
  new_dat$Sum[i] <- sum(!is.na(datA[[sample_id]]))
  
  # Component specific counts and their counterparts
  new_dat$Tab_Coral[i] <- sum(datA[[sample_id]] %in% c("Tabulate Coral", "Tabulate Coral_Oncoid"), na.rm = TRUE)
  new_dat$non_Tab_Coral[i] <- new_dat$Sum[i] - new_dat$Tab_Coral[i]
  
  new_dat$Rug_Coral[i] <- sum(datA[[sample_id]] %in% c("Rugose Coral"), na.rm = TRUE)
  new_dat$non_Rug_Coral[i] <- new_dat$Sum[i] - new_dat$Rug_Coral[i]
  
  new_dat$Strom[i] <- sum(datA[[sample_id]] %in% c("Stromatoporoid", "Stromatoporoid_Oncoid"), na.rm = TRUE)
  new_dat$non_Strom[i] <- new_dat$Sum[i] - new_dat$Strom[i]
  
  new_dat$Spo[i] <- sum(datA[[sample_id]] %in% c("Siliceous Sponge"), na.rm = TRUE)
  new_dat$non_Spo[i] <- new_dat$Sum[i] - new_dat$Spo[i]
  
  new_dat$Bry[i] <- sum(datA[[sample_id]] %in% c("Bryozoan"), na.rm = TRUE)
  new_dat$non_Bry[i] <- new_dat$Sum[i] - new_dat$Bry[i]
  
  new_dat$Mic[i] <- sum(datA[[sample_id]] %in% c("Microbe", "Microbe_Oncoid"), na.rm = TRUE)
  new_dat$non_Mic[i] <- new_dat$Sum[i] - new_dat$Mic[i]
  
  new_dat$Ech[i] <- sum(datA[[sample_id]] %in% c("Echinoderm", "Echinoderm ", "Crinoid", "Crinoid_Oncoid"), na.rm = TRUE)
  new_dat$non_Ech[i] <- new_dat$Sum[i] - new_dat$Ech[i]
  
  new_dat$Brach[i] <- sum(datA[[sample_id]] %in% c("Brachiopod", "Brachiopod_Oncoid", "Pentameride Brachiopod"), na.rm = TRUE)
  new_dat$non_Brach[i] <- new_dat$Sum[i] - new_dat$Brach[i]
  
  new_dat$Problem[i] <- sum(datA[[sample_id]] %in% c("Allonema", "Allonema_Oncoid", "Rothpletzella", "Girvanella"), na.rm = TRUE)
  new_dat$non_Problem[i] <- new_dat$Sum[i] - new_dat$Problem[i]
  
  new_dat$Moll[i] <- sum(datA[[sample_id]] %in% c("Mollusc", "Gastropod", "Bivalve", "Rostroconch", "Rostroconch_Oncoid"), na.rm = TRUE)
  new_dat$non_Moll[i] <- new_dat$Sum[i] - new_dat$Moll[i]
  
  new_dat$Tri[i] <- sum(datA[[sample_id]] %in% c("Trilobite", "Trilobite_Oncoid"), na.rm = TRUE)
  new_dat$non_Tri[i] <- new_dat$Sum[i] - new_dat$Tri[i]
  
  new_dat$Ost[i] <- sum(datA[[sample_id]] %in% c("Ostracod"), na.rm = TRUE)
  new_dat$non_Ost[i] <- new_dat$Sum[i] - new_dat$Ost[i]
  
  new_dat$Alg[i] <- sum(datA[[sample_id]] %in% c("Green Algae"), na.rm = TRUE)
  new_dat$non_Alg[i] <- new_dat$Sum[i] - new_dat$Alg[i]
  
  new_dat$Tas[i] <- sum(datA[[sample_id]] %in% c("Tasmanites"), na.rm = TRUE)
  new_dat$non_Tas[i] <- new_dat$Sum[i] - new_dat$Tas[i]
  
  new_dat$Micrite[i] <- sum(datA[[sample_id]] %in% c("Micrite"), na.rm = TRUE)
  new_dat$non_Micrite[i] <- new_dat$Sum[i] - new_dat$Micrite[i]
  
  new_dat$Spar[i] <- sum(datA[[sample_id]] %in% c("Sparite"), na.rm = TRUE)
  new_dat$non_Spar[i] <- new_dat$Sum[i] - new_dat$Spar[i]
  
  new_dat$Pyr[i] <- sum(datA[[sample_id]] %in% c("Pyrite"), na.rm = TRUE)
  new_dat$non_Pyr[i] <- new_dat$Sum[i] - new_dat$Pyr[i]
  
  new_dat$Org[i] <- sum(datA[[sample_id]] %in% c("Organic"), na.rm = TRUE)
  new_dat$non_Org[i] <- new_dat$Sum[i] - new_dat$Org[i]
  
  new_dat$non_Identified[i] <- sum(datA[[sample_id]] %in% c("?", "?_Oncoid", "0"), na.rm = TRUE)
  new_dat$Identified[i] <- new_dat$Sum[i] - new_dat$non_Identified[i]
  
}

# save new table
write.csv2(new_dat, file = "data/new_datA.csv", row.names = FALSE)

#### preparing data for barplots
# biogenic, micrite + sparite, others, unidentified
library(dplyr)

# read data
new_datA <- read.csv("data/new_datA.csv", sep = ";")

# remove non_columns + Identified
ov_datA <- new_datA %>%
  select(-c(non_Tab_Coral, non_Rug_Coral, non_Strom, non_Spo, 
            non_Bry, non_Mic, non_Ech, non_Brach, 
            non_Problem, non_Moll, non_Tri, 
            non_Ost, non_Alg, non_Tas, 
            non_Micrite, non_Spar, non_Pyr, 
            non_Org, Identified))

# combine biogenic components
bio <- c("Tab_Coral", "Rug_Coral", "Strom", "Spo", "Bry", 
                            "Mic", "Ech", "Brach", "Problem", "Moll", 
                            "Tri", "Ost", "Alg")

# new column biogenic
ov_datA$biogenic <- rowSums(ov_datA[bio], na.rm = TRUE)

# remove original columns
ov_datA <- ov_datA[, !names(ov_datA) %in% bio]

# combine other components
ov_datA$Others <- ov_datA$Pyr + ov_datA$Org

# remove columns of pyrite and organic
ov_datA <- ov_datA[, !names(ov_datA) %in% c("Pyr", "Org")]

# combine non_Identified components
ov_datA$Non_Identified <- ov_datA$non_Identified + ov_datA$Tas

# remove columns non_identified and Tasmanites
ov_datA <- ov_datA[, !names(ov_datA) %in% c("non_Identified", "Tas")]

# save new table with overview data of the components: Micrite, Sparite, Biogenic and other
write.csv2(ov_datA, "data/ov_datA.csv", row.names = FALSE)

## preparing data for barplots
# now for biogenic components
library(dplyr)


# read data
new_datA <- read.csv("data/new_datA.csv", sep = ";")

# remove non_columns + Identified, Micrite, Sparite, Pyrite, Organic, non_Identified
bio_datA <- new_datA %>%
  select(-c(Sum, non_Tab_Coral, non_Rug_Coral, non_Strom, non_Spo, 
            non_Bry, non_Mic, non_Ech, non_Brach, 
            non_Problem, non_Moll, non_Tri, 
            non_Ost, non_Alg, non_Tas, Tas,
            non_Micrite, non_Spar, non_Pyr, 
            non_Org, Identified, Micrite, Spar, Pyr, Org, non_Identified))

bio_datA$Sum <- rowSums(bio_datA[, c("Bry", "Mic", "Ech", "Brach", "Problem", "Moll", "Tri", "Ost", "Alg", "Tab_Coral", "Rug_Coral", "Strom","Spo")], na.rm = TRUE)

# combine groups
bio_datA <- bio_datA %>%
  mutate(
    Corals = Tab_Coral + Rug_Coral,  # Tab_Coral und Rug_Coral zu Corals
    Sponges = Strom + Spo             # Strom und Spo zu Sponges
  ) %>%
  select(-Tab_Coral, -Rug_Coral, -Strom, -Spo)  # Alte Spalten entfernen

# Sorting columns
bio_datA <- bio_datA[, c("Sample_ID", "Height_cm", "Delta13C", "Sum", "Corals", "Sponges", "Bry", "Mic", "Ech", "Brach", "Problem", "Moll", "Tri", "Ost", "Alg")]

# save table with biogenic data
write.csv2(bio_datA, "data/bio_datA.csv", row.names = FALSE)
