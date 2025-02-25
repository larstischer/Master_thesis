## calculating and plot relationship between component distribution and heigths/delta13C

# set directory
setwd(".../R")

# required packages
library(ggplot2)
library(visreg)
library(pscl)
library(dplyr)

# read in data
dat <- read.csv("data/new_datB.csv", sep = ";")

## Prepare data
# adding heigth and delta13C
heights <- c(500, 590, 680, 770, 860, 950, 1040, 1130, 1220, 1310, 1400)
d13C <- c(3.56, 4.09, 4.17, 4.62, 4.63, 4.88, 4.83, 4.86, 5.34, 5.18, 5.5)

dat$Height_cm <- heights
dat$Delta13C <- d13C

# correlation test between Height and delta13C
# Pearson test
plot(dat$Height_cm, dat$Delta13C)
cor.test(dat$Height_cm, dat$Delta13C) # Pearson test

# combine specific columns
# Others
dat$Others <- dat$Pyr 
dat$non_Others <- dat$non_Pyr 

# remove columns
dat <- dat[, !names(dat) %in% c("Pyr")]
dat <- dat[, !names(dat) %in% c("non_Pyr")]

# Non_Identified components
dat$Non_Identified <- dat$non_Identified
dat$Identified <- dat$Identified
# remove columns
dat <- dat[, !names(dat) %in% c("non_Identified")]


# Corals and Sponges
dat <- dat %>%
  mutate(
    Corals = Tab_Coral + Rug_Coral,  # Tab_Coral and Rug_Coral to Corals
    Sponges = Strom + Spo             # Strom and Spo to Sponges
  ) %>%
  select(-Tab_Coral, -Rug_Coral, -Strom, -Spo)  # remove old columns

dat <- dat %>%
  mutate(
    non_Corals = non_Tab_Coral + non_Rug_Coral,  # Tab_Coral and Rug_Coral to Corals
    non_Sponges = non_Strom + non_Spo             # Strom and Spo to Sponges
  ) %>%
  select(-non_Tab_Coral, -non_Rug_Coral, -non_Strom, -non_Spo)  # remove old columns

# add biogenic components -> number of biogenic components
dat$bio <- rowSums(dat[, c("Corals", "Sponges","Bry", "Mic", "Ech", "Brach", "Problem", "Moll", "Tri", "Ost", "Alg")], na.rm = TRUE)

# add diversity


# sorting columns
dat <- dat[, c("Sample_ID", "Height_cm", "Delta13C", "Sum", "Corals", "non_Corals", "Sponges", "non_Sponges", "Bry", "non_Bry", "Mic", "non_Mic",
               "Ech", "non_Ech", "Brach", "non_Brach", "Problem", "non_Problem", "Moll", "non_Moll", "Tri", "non_Tri", "Ost", "non_Ost", "Alg", "non_Alg", 
               "Micrite", "non_Micrite", "Spar", "non_Spar", "Others", "non_Others", "Non_Identified", "Identified", "bio")]


#Testing influence of parameter "heigth" on skeletal components via binomial regression model (dat)
# assigning data
height<-dat$Height_cm/100
delta<-dat$Delta13C

cp1 <- dat$Corals
ncp1 <- dat$non_Corals

cp2 <- dat$Sponges
ncp2 <- dat$non_Sponges

cp3 <- dat$Bry
ncp3 <- dat$non_Bry

cp4 <- dat$Mic
ncp4 <- dat$non_Mic

cp5 <- dat$Ech
ncp5 <- dat$non_Ech

cp6 <- dat$Brach
ncp6 <- dat$non_Brach

cp7 <- dat$Problem
ncp7 <- dat$non_Problem

cp8 <- dat$Moll
ncp8 <- dat$non_Moll

cp9 <- dat$Tri
ncp9 <- dat$non_Tri

cp10 <- dat$Ost
ncp10 <- dat$non_Ost

cp11 <- dat$Alg
ncp11 <- dat$non_Alg

cp12 <- dat$Micrite
ncp12 <- dat$non_Micrite

cp13 <- dat$Spar
ncp13 <- dat$non_Spar

cp14 <- dat$Others
ncp14 <- dat$non_Others

# Testing influence of parameters on species richness (SR) = number of different taxa presented in the rhodolith matrix (Dat2)
#mod6<-lm(cp6 ~ depth,data=Dat2)
#summary(mod6)


#visreg(mod6,scale="response",xvar="depth",ylab=expression(bold("taxon richness")),xlab=expression(bold("water depth [m]")),xlim=c(11,50), rug=FALSE)

pdf("output/reef_trends.pdf", width = 10, height = 10)

# Stelle das Layout für 2 Zeilen und 2 Spalten ein
par(mfrow = c(4, 2), mar = c(3, 3, 1, 1), oma = c(1, 1, 1, 1), pty = "s")

# 12) skeletal component Micrite (cp12)

#binominal logistic regression model
mod12 <-glm(cbind(cp12,ncp12) ~ height, data=dat,family="binomial")
summary(mod12)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod12)['McFadden']


#visreg(mod12,scale="response",xvar="height",ylab=expression(bold("proportion of micrite")),xlim=c(1,15), rug=F,  xlab=expression(bold("height [m]")))
visreg(
  mod12,
  scale = "response",
  xvar = "height",
  ylab = expression(bold("proportion of micrite")),
  xlim = c(5, 14),
  rug = F,
  xlab = expression(bold("height [m]")),
  line.par = list(col = "#8B4726", lwd = 2),  
  fill.par = list(col = adjustcolor("#8B4726", alpha.f = 0.3))  
)
# 13) skeletal component Sparite (cp13)

#binominal logistic regression model
mod13 <-glm(cbind(cp13,ncp13) ~ height, data=dat,family="binomial")
summary(mod13)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod13)['McFadden']


#visreg(mod13,scale="response",xvar="height",ylab=expression(bold("proportion of sparite")),xlim=c(1,15), rug=F,  xlab=expression(bold("height [m]")))
visreg(
  mod13,
  scale = "response",
  xvar = "height",
  ylab = expression(bold("proportion of sparite")),
  xlim = c(5, 14),
  rug = F,
  xlab = expression(bold("height [m]")),
  line.par = list(col = "#C1FFC1", lwd = 2),  
  fill.par = list(col = adjustcolor("#C1FFC1", alpha.f = 0.3))  
)

# 4) skeletal component microbes (cp4)

#binominal logistic regression model
mod4 <-glm(cbind(cp4,ncp4) ~ height, data=dat,family="binomial")
summary(mod4)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod4)['McFadden']


#visreg(mod4,scale="response",xvar="height",ylab=expression(bold("proportion of microbes")),xlim=c(1,15), rug=F,  xlab=expression(bold("height [m]")))
visreg(
  mod4,
  scale = "response",
  xvar = "height",
  ylab = expression(bold("proportion of microbes")),
  xlim = c(5, 14),
  rug = F,
  xlab = expression(bold("height [m]")),
  line.par = list(col = "#6b8e23", lwd = 2), 
  fill.par = list(col 
                  = adjustcolor("#6b8e23", alpha.f = 0.3))  
)


# 2) skeletal component Sponges (cp2)

#binominal logistic regression model
mod2 <-glm(cbind(cp2,ncp2) ~ height, data=dat,family="binomial")
summary(mod2)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod2)['McFadden']


#visreg(mod2,scale="response",xvar="height",ylab=expression(bold("proportion of sponges")),xlim=c(1,15), rug=F,  xlab=expression(bold("height [m]")))
visreg(
  mod2,
  scale = "response",
  xvar = "height",
  ylab = expression(bold("proportion of sponges")),
  xlim = c(5, 14),
  rug = F,
  xlab = expression(bold("height [m]")),
  line.par = list(col = "#ffd700", lwd = 2),  
  fill.par = list(col = adjustcolor("#ffd700", alpha.f = 0.3)) 
)

# Problematica
#binominal logistic regression model
mod7 <-glm(cbind(cp7,ncp7) ~ height, data=dat,family="binomial")
summary(mod7)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod7)['McFadden']


#visreg(mod7,scale="response",xvar="height",ylab=expression(bold("proportion of Problematica")),xlim=c(1,15), rug=F,  xlab=expression(bold("height [m]")))
visreg(
  mod7,
  scale = "response",
  xvar = "height",
  ylab = expression(bold("proportion of problematica")),
  xlim = c(5, 14),
  rug = F,
  xlab = expression(bold("height [m]")),
  line.par = list(col = "#00cd66", lwd = 2), 
  fill.par = list(col = adjustcolor("#00cd66", alpha.f = 0.3))  
)

# 1) skeletal component Corals (cp1)

#binominal logistic regression model
mod1<-glm(cbind(cp1,ncp1) ~ height,family="binomial")
summary(mod1)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod1)['McFadden']


#visreg(mod1,scale="response",xvar="height",ylab=expression(bold("proportion of corals")),xlim=c(2,14), col = "#ff7256", rug=F ,xlab=expression(bold("height [m]")))
visreg(
  mod1,
  scale = "response",
  xvar = "height",
  ylab = expression(bold("proportion of corals")),
  xlim = c(5, 14),
  rug = F,
  xlab = expression(bold("height [m]")),
  line.par = list(col = "#ff7256", lwd = 2), 
  fill.par = list(col = adjustcolor("#ff7256", alpha.f = 0.3)) 
)

dev.off()
#binominal logistic regression model
#mod1<-glm(cbind(cp1,ncp1) ~ delta,family="binomial")
#summary(mod1)

#caculation of McFadden calpR2(mod1)['McFadden']
#pR2(mod1)['McFadden']


#visreg(mod1,scale="response",xvar="delta",ylab=expression(bold("proportion of corals")),xlim=c(1.5,5.5), rug=F,  xlab=expression(bold("delta13C")))

# part 2
pdf("output/reef_trends2.pdf", width = 10, height = 10)

# new layout
par(mfrow = c(4, 2), mar = c(3, 3, 1, 1), oma = c(1, 1, 1, 1), pty = "s")
# 3) skeletal component Bryozoans (cp3)

#binominal logistic regression model
mod3 <-glm(cbind(cp3,ncp3) ~ height, data=dat,family="binomial")
summary(mod3)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod3)['McFadden']


#visreg(mod3,scale="response",xvar="height",ylab=expression(bold("proportion of bryozoans")),xlim=c(1,15), rug=F,  xlab=expression(bold("height [m]")))
visreg(
  mod3,
  scale = "response",
  xvar = "height",
  ylab = expression(bold("proportion of bryozoans")),
  xlim = c(5, 14),
  rug = F,
  xlab = expression(bold("height [m]")),
  line.par = list(col = "#cd2990", lwd = 2),  
  fill.par = list(col = adjustcolor("#cd2990", alpha.f = 0.3))  
)






# 5) Echinoderms
#binominal logistic regression model
mod5 <-glm(cbind(cp5,ncp5) ~ height, data=dat,family="binomial")
summary(mod4)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod5)['McFadden']


#visreg(mod5,scale="response",xvar="height",ylab=expression(bold("proportion of echinoderms")),xlim=c(1,15), rug=F,  xlab=expression(bold("height [m]")))
visreg(
  mod5,
  scale = "response",
  xvar = "height",
  ylab = expression(bold("proportion of echinoderms")),
  xlim = c(5, 14),
  rug = F,
  xlab = expression(bold("height [m]")),
  line.par = list(col = "#ba55d3", lwd = 2), 
  fill.par = list(col = adjustcolor("#ba55d3", alpha.f = 0.3))  
)
# 6) Brachiopods
#binominal logistic regression model
mod6 <-glm(cbind(cp6,ncp6) ~ height, data=dat,family="binomial")
summary(mod6)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod6)['McFadden']


#visreg(mod6,scale="response",xvar="height",ylab=expression(bold("proportion of brachiopods")),xlim=c(1,15), rug=F,  xlab=expression(bold("height [m]")))
visreg(
  mod6,
  scale = "response",
  xvar = "height",
  ylab = expression(bold("proportion of brachiopods")),
  xlim = c(5, 14),
  rug = F,
  xlab = expression(bold("height [m]")),
  line.par = list(col = "#6959cd", lwd = 2),  
  fill.par = list(col = adjustcolor("#6959cd", alpha.f = 0.3))  
)

# Molluscs
#binominal logistic regression model
mod8 <-glm(cbind(cp8,ncp8) ~ height, data=dat,family="binomial")
summary(mod8)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod8)['McFadden']


#visreg(mod8,scale="response",xvar="height",ylab=expression(bold("proportion of Molluscs")),xlim=c(1,15), rug=F,  xlab=expression(bold("height [m]")))
visreg(
  mod8,
  scale = "response",
  xvar = "height",
  ylab = expression(bold("proportion of molluscs")),
  xlim = c(5, 14),
  rug = F,
  xlab = expression(bold("height [m]")),
  line.par = list(col = "#40e0d0", lwd = 2), 
  fill.par = list(col = adjustcolor("#40e0d0", alpha.f = 0.3))  
)
# Trilobites
#binominal logistic regression model
mod9 <-glm(cbind(cp9,ncp9) ~ height, data=dat,family="binomial")
summary(mod9)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod9)['McFadden']


#visreg(mod9,scale="response",xvar="height",ylab=expression(bold("proportion of Trilobites")),xlim=c(1,15), rug=F,  xlab=expression(bold("height [m]")))
visreg(
  mod9,
  scale = "response",
  xvar = "height",
  ylab = expression(bold("proportion of trilobites")),
  xlim = c(5, 14),
  rug = F,
  xlab = expression(bold("height [m]")),
  line.par = list(col = "#eecfa1", lwd = 2),  
  fill.par = list(col = adjustcolor("#eecfa1", alpha.f = 0.3)) 
)
# Algae
#binominal logistic regression model
mod11 <-glm(cbind(cp11,ncp11) ~ height, data=dat,family="binomial")
summary(mod11)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod11)['McFadden']


#visreg(mod11,scale="response",xvar="height",ylab=expression(bold("proportion of Algae")),xlim=c(1,15), rug=F,  xlab=expression(bold("height [m]")))
visreg(
  mod11,
  scale = "response",
  xvar = "height",
  ylab = expression(bold("proportion of algae")),
  xlim = c(5, 14),
  rug = F,
  xlab = expression(bold("height [m]")),
  line.par = list(col = "#7fff00", lwd = 2), 
  fill.par = list(col = adjustcolor("#7fff00", alpha.f = 0.3))  
)

dev.off()

# Ostracods
#binominal logistic regression model
mod10 <-glm(cbind(cp10,ncp10) ~ height, data=dat,family="binomial")
summary(mod10)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod10)['McFadden']


#visreg(mod10,scale="response",xvar="height",ylab=expression(bold("proportion of Ostracods")),xlim=c(1,15), rug=F,  xlab=expression(bold("height [m]")))
visreg(
  mod10,
  scale = "response",
  xvar = "height",
  ylab = expression(bold("proportion of ostracods")),
  xlim = c(5, 14),
  rug = F,
  xlab = expression(bold("height [m]")),
  line.par = list(col = "#ff7f00", lwd = 2),  
  fill.par = list(col = adjustcolor("#ff7f00", alpha.f = 0.3)) 
)
dev.off()
# Problematica
#binominal logistic regression model
mod7 <-glm(cbind(cp7,ncp7) ~ height, data=dat,family="binomial")
summary(mod7)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod7)['McFadden']


#visreg(mod7,scale="response",xvar="height",ylab=expression(bold("proportion of Problematica")),xlim=c(1,15), rug=F,  xlab=expression(bold("height [m]")))
visreg(
  mod7,
  scale = "response",
  xvar = "height",
  ylab = expression(bold("proportion of problematica")),
  xlim = c(4, 15),
  rug = F,
  xlab = expression(bold("height [m]")),
  line.par = list(col = "#00cd66", lwd = 2),  
  fill.par = list(col = adjustcolor("#00cd66", alpha.f = 0.3)) 
)




######################
# 1) skeletal component Corals (cp1)

#binominal logistic regression model
mod1<-glm(cbind(cp1,ncp1) ~ height,family="binomial")
summary(mod1)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod1)['McFadden']


visreg(mod1,scale="response",xvar="height",ylab=expression(bold("proportion of corals")),xlim=c(4,15), rug=F,  xlab=expression(bold("height [m]")))


#binominal logistic regression model
mod1<-glm(cbind(cp1,ncp1) ~ delta,family="binomial")
summary(mod1)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod1)['McFadden']


visreg(mod1,scale="response",xvar="delta",ylab=expression(bold("proportion of corals")),xlim=c(3.5,5.5), rug=F,  xlab=expression(bold("height [cm]")))

# 2) skeletal component Sponges (cp2)

#binominal logistic regression model
mod2 <-glm(cbind(cp2,ncp2) ~ height, data=dat,family="binomial")
summary(mod2)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod2)['McFadden']


visreg(mod2,scale="response",xvar="height",ylab=expression(bold("proportion of sponges")),xlim=c(4,15), rug=F,  xlab=expression(bold("height [cm]")))

# 3) skeletal component Bryozoans (cp3)

#binominal logistic regression model
mod3 <-glm(cbind(cp3,ncp3) ~ height, data=dat,family="binomial")
summary(mod3)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod3)['McFadden']


visreg(mod3,scale="response",xvar="height",ylab=expression(bold("proportion of bryozoans")),xlim=c(4,15), rug=F,  xlab=expression(bold("height [m]")))

# 4) skeletal component microbes (cp4)

#binominal logistic regression model
mod4 <-glm(cbind(cp4,ncp4) ~ height, data=dat,family="binomial")
summary(mod4)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod4)['McFadden']


visreg(mod4,scale="response",xvar="height",ylab=expression(bold("proportion of microbes")),xlim=c(4,15), rug=F,  xlab=expression(bold("height [m]")))


# 5) Echinoderms
#binominal logistic regression model
mod5 <-glm(cbind(cp5,ncp5) ~ height, data=dat,family="binomial")
summary(mod4)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod5)['McFadden']


visreg(mod5,scale="response",xvar="height",ylab=expression(bold("proportion of echinoderms")),xlim=c(4,15), rug=F,  xlab=expression(bold("height [m]")))

# 6) Brachiopods
#binominal logistic regression model
mod6 <-glm(cbind(cp6,ncp6) ~ height, data=dat,family="binomial")
summary(mod6)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod6)['McFadden']


visreg(mod6,scale="response",xvar="height",ylab=expression(bold("proportion of brachiopods")),xlim=c(4,15), rug=F,  xlab=expression(bold("height [m]")))

# Problematica
#binominal logistic regression model
mod7 <-glm(cbind(cp7,ncp7) ~ height, data=dat,family="binomial")
summary(mod7)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod7)['McFadden']


visreg(mod7,scale="response",xvar="height",ylab=expression(bold("proportion of Problematica")),xlim=c(4,15), rug=F,  xlab=expression(bold("height [m]")))

# Molluscs
#binominal logistic regression model
mod8 <-glm(cbind(cp8,ncp8) ~ height, data=dat,family="binomial")
summary(mod8)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod8)['McFadden']


visreg(mod8,scale="response",xvar="height",ylab=expression(bold("proportion of Molluscs")),xlim=c(4,15), rug=F,  xlab=expression(bold("height [m]")))

# Trilobites
#binominal logistic regression model
mod9 <-glm(cbind(cp9,ncp9) ~ height, data=dat,family="binomial")
summary(mod9)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod9)['McFadden']


visreg(mod9,scale="response",xvar="height",ylab=expression(bold("proportion of Trilobites")),xlim=c(4,15), rug=F,  xlab=expression(bold("height [m]")))

# Ostracods
#binominal logistic regression model
mod10 <-glm(cbind(cp10,ncp10) ~ height, data=dat,family="binomial")
summary(mod10)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod10)['McFadden']


visreg(mod10,scale="response",xvar="height",ylab=expression(bold("proportion of Ostracods")),xlim=c(4,15), rug=F,  xlab=expression(bold("height [m]")))

# Algae
#binominal logistic regression model
mod11 <-glm(cbind(cp11,ncp11) ~ height, data=dat,family="binomial")
summary(mod11)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod11)['McFadden']


visreg(mod11,scale="response",xvar="height",ylab=expression(bold("proportion of Algae")),xlim=c(4,15), rug=F,  xlab=expression(bold("height [m]")))



# 12) skeletal component Micrite (cp12)

#binominal logistic regression model
mod12 <-glm(cbind(cp12,ncp12) ~ height, data=dat,family="binomial")
summary(mod12)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod12)['McFadden']


visreg(mod12,scale="response",xvar="height",ylab=expression(bold("proportion of micrite")),xlim=c(4,15), rug=F,  xlab=expression(bold("height [m]")))

# 13) skeletal component Sparite (cp13)

#binominal logistic regression model
mod13 <-glm(cbind(cp13,ncp13) ~ height, data=dat,family="binomial")
summary(mod13)

#caculation of McFadden calpR2(mod1)['McFadden']
pR2(mod13)['McFadden']


visreg(mod13,scale="response",xvar="height",ylab=expression(bold("proportion of sparite")),xlim=c(4,15), rug=F,  xlab=expression(bold("height [m]")))

# save plots
pdf("output/Reef_trends.pdf", width = 10, height = 10)

# new layout
par(mfrow = c(3, 2))

# plots
visreg(mod1,scale="response",xvar="height",ylab=expression(bold("proportion of corals")),xlim=c(4,15), rug=F,  xlab=expression(bold("height [m]")))
visreg(mod2,scale="response",xvar="height",ylab=expression(bold("proportion of sponges")),xlim=c(4,15), rug=F,  xlab=expression(bold("height [m]")))
visreg(mod3,scale="response",xvar="height",ylab=expression(bold("proportion of bryozoans")),xlim=c(4,15), rug=F,  xlab=expression(bold("height [m]")))
visreg(mod4,scale="response",xvar="height",ylab=expression(bold("proportion of microbes")),xlim=c(4,15), rug=F,  xlab=expression(bold("height [m]")))
visreg(mod7,scale="response",xvar="height",ylab=expression(bold("proportion of problematica")),xlim=c(4,15), rug=F,  xlab=expression(bold("height [m]")))
visreg(mod12,scale="response",xvar="height",ylab=expression(bold("proportion of micrite")),xlim=c(4,15), rug=F,  xlab=expression(bold("height [m]")))


# close file
dev.off()

