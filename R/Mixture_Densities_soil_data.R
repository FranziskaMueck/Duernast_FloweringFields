################ Calculation Soil Data for comparison between years ####################

rm(list = ls())
setwd ("Y:/Dürnast_Blühstreifen/Veröffentlichung/")

library(dplyr)

##### - Calculate Mean and SE - #####

#data
soil <- read.csv("Y:/Dürnast_Blühstreifen/Veröffentlichung/DataFiles/data_raw_SoilParameter.csv", sep = ";")

# extract 2017 Data
s17 <- subset(soil, year == "2017")

# extract 2023 Data
s23 <- subset(soil, year == "2023")

#calculate means of all parameters
mean.pH <- aggregate(pH ~ year, data = soil, FUN = mean)
mean.pho <- aggregate(phosphor ~ year, data = soil, FUN = mean)
mean.pot <- aggregate(potassium ~ year, data = soil, FUN = mean)
mean.mg <- aggregate(magnesium ~ year, data = soil, FUN = mean)

sd.pH <- aggregate(pH ~ year, data = soil, FUN = sd)
sd01 <- reframe (mean.pH,
               sd.upr = (mean.pH$pH + sd.pH$pH),
               sd.lwr = (mean.pH$pH - sd.pH$pH))

sd.pho <- aggregate(phosphor ~ year, data = soil, FUN = sd)
sd02 <- reframe (mean.pho,
                 sd.upr = (mean.pho$phosphor + sd.pho$phosphor),
                 sd.lwr = (mean.pho$phosphor - sd.pho$phosphor))

sd.pot <- aggregate(potassium ~ year, data = soil, FUN = sd)
sd03 <- reframe (mean.pot,
                 sd.upr = (mean.pot$potassium + sd.pot$potassium),
                 sd.lwr = (mean.pot$potassium - sd.pot$potassium))

sd.mg <- aggregate(magnesium ~ year, data = soil, FUN = sd)
sd04 <- reframe (mean.mg,
                 sd.upr = (mean.mg$magnesium + sd.mg$magnesium),
                 sd.lwr = (mean.mg$magnesium - sd.mg$magnesium))
# combine all tables
full.table <- cbind(mean.pH, sd.pH, mean.pho, sd.pho, mean.pot, sd.pot)

View(full.table)

#t-test for the comparison of means of the years

t.test(pH~year, var.equal=TRUE, alternative="two.sided", data=soil)
t.test(phosphor~year, var.equal=TRUE, alternative="two.sided", data=soil)
t.test(potassium~year, var.equal=TRUE, alternative="two.sided", data=soil)
