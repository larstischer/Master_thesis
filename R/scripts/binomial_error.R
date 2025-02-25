## Calculating the binomial error

# set working directory
setwd(".../R")

# read data
datA <- read.csv("data/new_datA.csv", sep = ";")
datB <- read.csv("data/new_datB.csv", sep = ";")

# calculation of binomial error + new table
library(dplyr)

## dat A -> LMA
bin_errorA <- datA %>%
  mutate(
    bin_error_Tab_Coral = sqrt((Tab_Coral / Sum) * (1 - (Tab_Coral / Sum)) / Sum),
    bin_error_Rug_Coral = sqrt((Rug_Coral / Sum) * (1 - (Rug_Coral / Sum)) / Sum),
    bin_error_Strom = sqrt((Strom / Sum) * (1 - (Strom / Sum)) / Sum),
    #bin_error_Spo = sqrt((Spo / Sum) * (1 - (Spo / Sum)) / Sum),
    bin_error_Bry = sqrt((Bry / Sum) * (1 - (Bry / Sum)) / Sum),
    bin_error_Mic = sqrt((Mic / Sum) * (1 - (Mic / Sum)) / Sum),
    bin_error_Ech = sqrt((Ech / Sum) * (1 - (Ech / Sum)) / Sum),
    bin_error_Brach = sqrt((Brach / Sum) * (1 - (Brach / Sum)) / Sum),
    bin_error_Problem = sqrt((Problem / Sum) * (1 - (Problem / Sum)) / Sum),
    bin_error_Moll = sqrt((Moll / Sum) * (1 - (Moll / Sum)) / Sum),
    bin_error_Tri = sqrt((Tri / Sum) * (1 - (Tri / Sum)) / Sum),
    bin_error_Ost = sqrt((Ost / Sum) * (1 - (Ost / Sum)) / Sum),
    bin_error_Alg = sqrt((Alg / Sum) * (1 - (Alg / Sum)) / Sum),
    bin_error_Micrite = sqrt((Micrite / Sum) * (1 - (Micrite / Sum)) / Sum),
    bin_error_Spar = sqrt((Spar / Sum) * (1 - (Spar / Sum)) / Sum),
    bin_error_Pyr = sqrt((Pyr / Sum) * (1 - (Pyr / Sum)) / Sum),
    bin_error_Org = sqrt((Org / Sum) * (1 - (Org / Sum)) / Sum),
    bin_error_non_Identified = sqrt((non_Identified / Sum) * (1 - (non_Identified / Sum)) / Sum)
  ) %>%
  select(Sample_ID, Sum, starts_with("bin_error_"))  # Behalte nur Sample_ID, Sum und die Fehler-Spalten

# save table with binomial errors for location A
write.csv2(bin_errorA, "data/bin_errA.csv", row.names = FALSE)

## dat B -> Reef
bin_errorB <- datB %>%
  mutate(
    bin_error_Tab_Coral = sqrt((Tab_Coral / Sum) * (1 - (Tab_Coral / Sum)) / Sum),
    bin_error_Rug_Coral = sqrt((Rug_Coral / Sum) * (1 - (Rug_Coral / Sum)) / Sum),
    bin_error_Strom = sqrt((Strom / Sum) * (1 - (Strom / Sum)) / Sum),
    bin_error_Spo = sqrt((Spo / Sum) * (1 - (Spo / Sum)) / Sum),
    bin_error_Bry = sqrt((Bry / Sum) * (1 - (Bry / Sum)) / Sum),
    bin_error_Mic = sqrt((Mic / Sum) * (1 - (Mic / Sum)) / Sum),
    bin_error_Ech = sqrt((Ech / Sum) * (1 - (Ech / Sum)) / Sum),
    bin_error_Brach = sqrt((Brach / Sum) * (1 - (Brach / Sum)) / Sum),
    bin_error_Problem = sqrt((Problem / Sum) * (1 - (Problem / Sum)) / Sum),
    bin_error_Moll = sqrt((Moll / Sum) * (1 - (Moll / Sum)) / Sum),
    bin_error_Tri = sqrt((Tri / Sum) * (1 - (Tri / Sum)) / Sum),
    bin_error_Ost = sqrt((Ost / Sum) * (1 - (Ost / Sum)) / Sum),
    bin_error_Alg = sqrt((Alg / Sum) * (1 - (Alg / Sum)) / Sum),
    bin_error_Micrite = sqrt((Micrite / Sum) * (1 - (Micrite / Sum)) / Sum),
    bin_error_Spar = sqrt((Spar / Sum) * (1 - (Spar / Sum)) / Sum),
    bin_error_Pyr = sqrt((Pyr / Sum) * (1 - (Pyr / Sum)) / Sum),
    bin_error_Org = sqrt((Org / Sum) * (1 - (Org / Sum)) / Sum),
    bin_error_non_Identified = sqrt((non_Identified / Sum) * (1 - (non_Identified / Sum)) / Sum)
  ) %>%
  select(Sample_ID, Sum, starts_with("bin_error_"))  # Behalte nur Sample_ID, Sum und die Fehler-Spalten

# save table with binomial errors for location B
write.csv2(bin_errorB, "data/bin_errB.csv", row.names = FALSE)
