################ Comparison seed mixtures seed densities ####################

rm(list = ls()) # clear workspace
setwd ("Y:/Dürnast_Blühstreifen/Veröffentlichung/")

#packages
library("ggplot2")      # used to create plots
library("gridExtra")    # used for combining graphs
library("glmmTMB")      # used for creating models
library("car")          # used for ANOVA
library("tidyr")        # used for usage of % symbols
library("tidyverse")    # used for mutate() function
library("emmeans")      # used for post-hoc -> variable within other variable

##### - main results - #####
#### - species numbers 2021 - ####
# data
aby1<-read.csv("Y:/Dürnast_Blühstreifen/Veröffentlichung/DataFiles/data_raw_Duernast_FloweringFields.csv", sep = ";")
View(aby1) # to check if table was imported correctly

# create subset for 2021 data
aby21 <- subset(aby1, year == "2021")

# encode density & mixture as factor and sort seedMixture by species richness
aby <- aby21 %>%
  mutate(
    seedDensity = as.factor(seedDensity),
    seedMixture = as.factor(seedMixture),
    seedMixture = fct_relevel(seedMixture, "L.nr","M.nr","M.r","I.nr","H.r")
  )

### - model species diversity - ###
## test for normality ##
# total plant species number
shapiro.test(aby$spec_nr_all) #distribution? -> normal
hist(aby$spec_nr_all)
# unsown species number
shapiro.test(aby$weed_nr) # distribution? -> NOT normal (p < 0.05)
hist(aby$weed_nr)
# established sown species number
shapiro.test(aby$estab_nr) # distribution? -> NOT normal (p < 0.05)
hist(aby$estab_nr)

## create generalized linear mixed models (glmm) ##
# total plant species number (psp)
glm.psp <- glmmTMB(spec_nr_all~seedDensity*seedMixture+(1|DensityBlock)+(1|replication)+(1|GrownBlock),
                   family = poisson(link = "log"), data = aby)
# unsown species number (weed)
glm.weed <- glmmTMB(weed_nr~seedDensity*seedMixture+(1|DensityBlock)+(1|replication)+(1|GrownBlock),
                   family = poisson(link = "log"), data = aby)
# established sown species number (est)
glm.est <- glmmTMB(estab_nr~seedDensity*seedMixture+(1|DensityBlock)+(1|replication)+(1|GrownBlock),
                    family = poisson(link = "log"), data = aby)

# Anova
Anova(glm.psp)
Anova(glm.weed)
Anova(glm.est)
# Post-hoc test for single variables (seed mixture & seed density)
summary(pairs(emmeans(glm.psp, "seedMixture")), adjust = "tukey")
summary(pairs(emmeans(glm.psp, "seedDensity")), adjust = "tukey")
summary(pairs(emmeans(glm.weed, "seedMixture")), adjust = "tukey")
summary(pairs(emmeans(glm.weed, "seedDensity")), adjust = "tukey")
summary(pairs(emmeans(glm.est, "seedMixture")), adjust = "tukey") 
summary(pairs(emmeans(glm.est, "seedDensity")), adjust = "tukey")
#example for bonferroni results of few pair comparisons - not included in the manuscript
summary(pairs(emmeans(glm.psp, "seedMixture")), adjust = "bonferroni")

#compare seed density within seed mixture - total number of species
emm1 <- emmeans(glm.psp, specs = pairwise ~ seedDensity | seedMixture, type = "response", adjust = "tukey")
emm1 <- emm1$contrasts %>%
  rbind(adjust = "tukey")
summary(emm1, infer = T)
#compare seed density within seed mixture - number of unsown species
emm2 <- emmeans(glm.weed, specs = pairwise ~ seedDensity | seedMixture, type = "response", adjust = "tukey")
emm2 <- emm2$contrasts %>%
  rbind(adjust = "tukey")
summary(emm2, infer = T)
#compare seed density within seed mixture - number of established sown species
emm3 <- emmeans(glm.est, specs = pairwise ~ seedDensity | seedMixture, type = "response", adjust = "tukey")
emm3 <- emm3$contrasts %>%
  rbind(adjust = "tukey")
summary(emm3, infer = T)

#compare seed mixture within seed densities - total number of species
emm4 <- emmeans(glm.psp, specs = pairwise ~ seedMixture | seedDensity, type = "response", adjust = "tukey")
emm4 <- emm4$contrasts %>%
  rbind(adjust = "tukey")
summary(emm4, infer = T)
#compare seed mixture within seed density - number of unsown species
emm5 <- emmeans(glm.weed, specs = pairwise ~ seedMixture | seedDensity, type = "response", adjust = "tukey")
emm5 <- emm5$contrasts %>%
  rbind(adjust = "tukey")
summary(emm5, infer = T)
#compare seed mixture within seed density - number of established sown species
emm6 <- emmeans(glm.est, specs = pairwise ~ seedMixture | seedDensity, type = "response", adjust = "tukey")
emm6 <- emm6$contrasts %>%
  rbind(adjust = "tukey")
summary(emm6, infer = T)


### - Create Graphs for species numbers - ###

#Total number of species
species <- ggplot(data=aby,aes(x=seedMixture,y=spec_nr_all,fill=seedDensity)) +         # The basic plot aesthetics 
  geom_boxplot()+
  xlab("") +
  ylab("Total Species number") +
  scale_fill_manual(name="Seed density", values=c("grey80","grey50","grey20"),
                    breaks=c("1","2","3"),
                    labels=c("-50%", "100%", "+100%")) +
  theme_classic(base_size = 15) +
  theme(strip.background=element_blank(),strip.text = element_blank(),
        axis.text.x = element_blank(), axis.text.y=element_text(size=15)) +
  geom_point(position = position_dodge(width = 0.9), colour = "black", alpha = 0.5)+
  guides(colour = "none", alpha = "none")+
  ggtitle("2021")+
  theme(plot.title = element_text(hjust = 0.5, size = 28, face = "bold"))+
  annotate("text", x = 1:5, y = 30, label = c("a","a","a","a","a"),size=5)
species

# Number of established sown species
est <- ggplot(data=aby,aes(x=seedMixture,y=estab_nr,fill=seedDensity)
) + 
  geom_boxplot()+
  ylim(0,20)+
  ylab("Number of established species") +
  xlab("") +
  scale_fill_manual(name="Seed density", values=c("grey80","grey50","grey20"),
                    breaks=c("1","2","3"),
                    labels=c("-50%", "100%", "+100%")
  ) +
  theme_classic(base_size = 15) +
  theme(strip.background=element_blank(),strip.text = element_blank(),
        axis.text.x = element_blank(), axis.text.y=element_text(size=15)
  ) +
  geom_point(position = position_dodge(width = 0.9), colour = "black", alpha = 0.5)+  annotate("text", x = 1:5, y = 20, label = c("b","a","a","b","a"),size=5)+
  #annotate("text", x = c(3.8, 4, 4.2), y = 12, label = c("a","a","b"),size=5)
  theme(plot.title = element_text(hjust = 0.5, size = 28, face = "bold"))
est

  
# Number of unsown species
weed <- ggplot(data=aby,aes(x=seedMixture,y=weed_nr,fill=seedDensity)
) + 
  geom_boxplot()+
  ylim(0,20)+
  ylab("Number of unsown species") +
  xlab("Seed mixture") +
  scale_fill_manual(name="Seed density", values=c("grey80","grey50","grey20"),
                    breaks=c("1","2","3"),
                    labels=c("-50%", "100%", "+100%")
  ) +
  theme_classic(base_size = 15) +
  theme(axis.text.y=element_text(size=15),axis.text.x=element_text(size=15)
  ) +
  geom_point(position = position_dodge(width = 0.9), colour = "black", alpha = 0.5)+  annotate("text", x = 1:5, y = 20, label = c("a","ab","b","ab","b"),size=5)+
  annotate("text", x = c(3.8, 4, 4.2), y = 10, label = c("a","a","b"),size=5)
weed

diversity<-grid.arrange(species, est, weed)
ggsave("glm_diversity2021_corrected_all.jpg",plot=diversity,width=10, height=9,dpi=300, path = "Y:/Dürnast_Blühstreifen/Veröffentlichung/Graphen")


#### - plant biomass converted to gram per square meter 2021 - ####
## normality ##

shapiro.test(aby$biomass_qm) #distribution? -> NOT normal
hist(aby$biomass_qm)
#data transformation
shapiro.test(log(aby$biomass_qm+1)) # normal distribution
hist(log(aby$biomass_qm+1)) 

## generalized linear mixed models (glmm) ##
# transformed data
glm.bm2 <- glmmTMB(log(biomass_qm+1)~seedDensity*seedMixture+(1|replication)+(1|DensityBlock)+(1|GrownBlock),
                  family = gaussian(link = "identity"), data = aby)

# Anova
Anova(glm.bm2)

#post-hoc for single significant variable (seed mixture)
summary(pairs(emmeans(glm.bm2, "seedMixture")), adjust = "tukey")

# compare seed density within seed mixture - biomass
emmbm2 <- emmeans(glm.bm2, specs = pairwise ~ seedDensity | seedMixture, type = "response", adjust = "tukey")
emmbm2 <- emmbm2$contrasts %>%
  rbind(adjust = "tukey")
summary(emmbm2, infer = T)


### - Create Graph for plant biomass per sqare meter - ###
biomass<-ggplot(data=aby,aes(x=seedMixture,y=biomass_qm,fill=seedDensity))+
  geom_boxplot()+
  scale_fill_manual(name="Seed density", values=c("grey80","grey50","grey20"),
                    breaks=c("1","2","3"),
                    labels=c("-50%", "100%", "+100%"))+
  ylab("Dried biomass (g)")+
  xlab("Seed mixture")+
  geom_point(position = position_dodge(width = 0.9), colour = "black", alpha = 0.5)+
  annotate("text", x = 1:5, y = 2000, label = c("ab","b","a","b","b"),size=5)+
  theme_classic(base_size = 15)+
  theme(axis.text.y=element_text(size=15),axis.text.x=element_text(size=15))

biomass
ggsave("glm_biomass_qm.jpg",plot=biomass,width=10,height=5,dpi=300, path = "Y:/Dürnast_Blühstreifen/Veröffentlichung/Graphen")

#### - model pollinated plants and grasses - ####
## normality ##

# Number of insect pollinated plants
shapiro.test(aby$nb_pollin) #distribution? -> normal
hist(aby$nb_pollin)

# Number of grass species (Poaceae)
shapiro.test(aby$nb_grasses) #distribution? -> NOT normal
hist(aby$nb_grasses)

#lots of 0s -> transformed data with each +1
shapiro.test(aby$nb_grasses1) #distribution? -> NOT normal
hist(aby$nb_grasses1)

## generalized linear mixed models (glmm) ##

# Number of insect pollinated plants
glm.polp <- glmmTMB(nb_pollin~seedDensity*seedMixture+(1|replication)+(1|DensityBlock)+(1|GrownBlock),
                  family = poisson(link = "log"), data = aby)

# number of grass species (Poaceae) with transformed data (each +1)
glm.grass <- glmmTMB(nb_grasses1~seedDensity*seedMixture+(1|replication)+(1|DensityBlock)+(1|GrownBlock),
                     family = poisson(link = "log"), data = aby)

# Anova
Anova(glm.polp)
Anova(glm.grass) # statistics with transformed data

#post-hoc for single significant variable (seed mixture) for pollinated plants
summary(pairs(emmeans(glm.polp, "seedMixture")), adjust = "tukey")
#post-hoc for single significant variable (seed mixture) for grasses
summary(pairs(emmeans(glm.grass, "seedMixture")), adjust = "tukey")


# compare seed mixture within seed densities - number of insect pollinated plants
emmpoll <- emmeans(glm.polp, specs = pairwise ~ seedDensity | seedMixture, type = "response", adjust = "tukey")
emmpoll <- emmpoll$contrasts %>%    
  rbind(adjust = "tukey")
summary(emmpoll, infer = T)

# compare seed mixture within seed densities - number of grass species (Poaceae)
emmgrass <- emmeans(glm.grass, specs = pairwise ~ seedDensity | seedMixture, type = "response", adjust = "tukey")
emmgrass <- emmgrass$contrasts %>%    
  rbind(adjust = "tukey")
summary(emmgrass, infer = T)


### - create graph for pollinated plants and grasses - ###
#number of insect pollinated plants
pollpla<-ggplot(data=aby,aes(x=seedMixture,y=nb_pollin,fill=seedDensity))+
  geom_boxplot()+
  scale_fill_manual(name="Seed density", values=c("grey80","grey50","grey20"),
                    breaks=c("1","2","3"),
                    labels=c("-50%", "100%", "+100%"))+
  xlab("")+
  ylab("Number of insect pollinated plants")+
  geom_point(position = position_dodge(width = 0.9), colour = "black", alpha = 0.5)+
  annotate("text", x = 1:5, y = 25, label = c("b","a","ab","ab","ab"),size=5)+
  theme_classic(base_size = 15)+
  ggtitle("2021")+
  theme(plot.title = element_text(hjust = 0.5, size = 28, face = "bold"))+
  theme(strip.background=element_blank(),strip.text = element_blank(),
        axis.text.x = element_blank(), axis.text.y=element_text(size=15))
pollpla

#number of grass species (Poaceae)
grass<-ggplot(data=aby,aes(x=seedMixture,y=nb_grasses,fill=seedDensity))+
  geom_boxplot()+
  scale_fill_manual(name="Seed density", values=c("grey80","grey50","grey20"),
                    breaks=c("1","2","3"),
                    labels=c("-50%", "100%", "+100%"))+
  ylab("Number of grasses")+
  xlab("Seed mixture")+
  geom_point(position = position_dodge(width=0.9), colour = "black", alpha = 0.5)+
  annotate("text", x = 1:5, y = 5, label = c("ab","b","b","ab","a"),size=5)+
  theme_classic(base_size = 15)+
  theme(axis.text.y=element_text(size=15),axis.text.x=element_text(size=15))
grass

grassespoll<-grid.arrange(pollpla, grass) 
ggsave("glm_pollgrasses.jpg",plot=grassespoll,width=10,height=7,dpi=300, path = "Y:/Dürnast_Blühstreifen/Veröffentlichung/Graphen")



#### - species numbers 2024 - ####

# create subset for 2024 data
aby24.1<- subset(aby1, year == "2024")

# encode density & mixture as factor and sort seedMixture by species richness
aby24 <- aby24.1 %>%
  mutate(
    seedDensity = as.factor(seedDensity),
    seedMixture = as.factor(seedMixture),
    seedMixture = fct_relevel(seedMixture, "L.nr","M.nr","M.r","I.nr","H.r")
  )

### - models species diversity - ###
## normality ##
# Total plant species number
shapiro.test(aby24$spec_nr_all) # distribution? -> normal
hist(aby24$spec_nr_all)
# Number of unsown species
shapiro.test(aby24$weed_nr) # distribution? -> normal distribution
hist(aby24$weed_nr)
# Number of established sown species
shapiro.test(aby24$estab_nr) # not normal distribution (p<0.05)
hist(aby24$estab_nr)

## create generalized linear mixed models (glmm) ##
# total plant species number (psp)
glm.psp24 <- glmmTMB(spec_nr_all~seedDensity*seedMixture+(1|replication)+(1|DensityBlock)+(1|GrownBlock),
                   family = poisson(link = "log"), data = aby24)
# unsown species number
glm.weed24 <- glmmTMB(weed_nr~seedDensity*seedMixture+(1|replication)+(1|DensityBlock)+(1|GrownBlock),
                    family = poisson(link = "log"), data = aby24)
#established sown species number
glm.est24 <- glmmTMB(estab_nr~seedDensity*seedMixture+(1|replication)+(1|DensityBlock)+(1|GrownBlock),
                      family = poisson(link = "log"), data = aby24)

# Anova
Anova(glm.psp24)
Anova(glm.weed24)
Anova(glm.est24)

# Post-hoc test for single variables
summary(pairs(emmeans(glm.psp24, "seedMixture")), adjust = "tukey")
summary(pairs(emmeans(glm.psp24, "seedDensity")), adjust = "tukey")
summary(pairs(emmeans(glm.weed24, "seedMixture")), adjust = "tukey")
summary(pairs(emmeans(glm.weed24, "seedDensity")), adjust = "tukey")
summary(pairs(emmeans(glm.est24, "seedMixture")), adjust = "tukey")
summary(pairs(emmeans(glm.est24, "seedDensity")), adjust = "tukey")


# Compare seed density within seed mixture - total number of species
emm1 <- emmeans(glm.psp24, specs = pairwise ~ seedDensity | seedMixture, type = "response", adjust = "tukey")
emm1 <- emm1$contrasts %>%
  rbind(adjust = "tukey")
summary(emm1, infer = T)
# Compare seed density within seed mixture - number of unsown species
emm2 <- emmeans(glm.weed24, specs = pairwise ~ seedDensity | seedMixture, type = "response", adjust = "tukey")
emm2 <- emm2$contrasts %>% 
  rbind(adjust = "tukey")
summary(emm2, infer = T)
# Compare seed density within seed mixture - number of established sown species
emm3 <- emmeans(glm.est24, specs = pairwise ~ seedDensity | seedMixture, type = "response", adjust = "tukey")
emm3 <- emm3$contrasts %>% 
  rbind(adjust = "tukey")
summary(emm3, infer = T)

# Compare seed mixture within seed densities - total number of species
emm4 <- emmeans(glm.psp24, specs = pairwise ~ seedMixture | seedDensity, type = "response", adjust = "tukey")
emm4 <- emm4$contrasts %>%
  rbind(adjust = "tukey")
summary(emm4, infer = T)
#compare seed mixture within seed density - number of unsown species
emm5 <- emmeans(glm.weed24, specs = pairwise ~ seedMixture | seedDensity, type = "response", adjust = "tukey")
emm5 <- emm5$contrasts %>%
  rbind(adjust = "tukey")
summary(emm5, infer = T)
#compare seed mixture within seed density - number of established sown species
emm6 <- emmeans(glm.est24, specs = pairwise ~ seedMixture | seedDensity, type = "response", adjust = "tukey")
emm6 <- emm6$contrasts %>%
  rbind(adjust = "tukey")
summary(emm6, infer = T)

###### - Create Graphs with models - #####

#part of graph for significance letters

### species Graphs  ###
#Number of all species
species24 <- ggplot(data=aby24,aes(x=seedMixture,y=spec_nr_all,fill=seedDensity)          # The basic plot aesthetics
) + 
  geom_boxplot()+
  xlab("") +
  ylab("Total Species number") +
  scale_fill_manual(name="Seed density", values=c("grey80","grey50","grey20"),
                    breaks=c("1","2","3"),
                    labels=c("-50%", "100%", "+100%")
  ) +
  scale_y_continuous(breaks = seq(0, 30, by = 5))+
  theme_classic(base_size = 15) +
  theme(strip.background=element_blank(),strip.text = element_blank(),
        axis.text.x = element_blank(), axis.text.y=element_text(size=15)
  ) +
  geom_point(position = position_dodge(width = 0.9), colour = "black", alpha = 0.5)+
  ggtitle("2024")+
  theme(plot.title = element_text(hjust = 0.5, size = 28, face = "bold"))+
  annotate("text", x = 1:5, y = 30, label = c("b","a","ac","a","bc"),size=5)
species24

# Number of established species
est24 <- ggplot(data=aby24,aes(x=seedMixture,y=estab_nr,fill=seedDensity)          # The basic plot aesthetics
) + 
  geom_boxplot()+
  ylim(0,20)+
  ylab("Number of established species") +
  xlab("") +
  scale_fill_manual(name="Seed density", values=c("grey80","grey50","grey20"),
                    breaks=c("1","2","3"),
                    labels=c("-50%", "100%", "+100%")
  ) +
  #expand_limits(y = 0) +                        # Expand y range
  theme_classic(base_size = 15) +
  theme(strip.background=element_blank(),strip.text = element_blank(),
        axis.text.x = element_blank(), axis.text.y=element_text(size=15)
  ) +
  geom_point(position = position_dodge(width = 0.9), colour = "black", alpha = 0.5)+
  theme(plot.title = element_text(hjust = 0.5, size = 28, face = "bold"))+
  annotate("text", x = 1:5, y = 20, label = c("b","a","a","a","a"),size=5)
est24

# Number of unsown species
weed24 <- ggplot(data=aby24,aes(x=seedMixture,y=weed_nr,fill=seedDensity)          # The basic plot aesthetics
) + 
  geom_boxplot()+
  ylim(0,20)+
  ylab("Number of unsown species") +
  xlab("Seed mixture") +
  scale_fill_manual(name="Seed density", values=c("grey80","grey50","grey20"),
                    breaks=c("1","2","3"),
                    labels=c("-50%", "100%", "+100%")
  ) +
  #expand_limits(y = 0) +                        # Expand y range
  theme_classic(base_size = 15) +
  theme(axis.text.y=element_text(size=15),axis.text.x=element_text(size=15)
  ) +
  geom_point(position = position_dodge(width = 0.9), colour = "black", alpha = 0.5)+
  annotate("text", x = 1:5, y = 20, label = c("a","a","a","a","b"),size=5)
weed24

diversity24<-grid.arrange(species24, est24, weed24) 
ggsave("glm_diversity2024_corrected_all.jpg",plot=diversity24,width=10, height=9,dpi=300, path = "Y:/Dürnast_Blühstreifen/Veröffentlichung/Graphen")

### - model pollinated plants 2024 - ###
## normality ##

# Insect pollinated plant species number
shapiro.test(aby24$nb_pollin) #distribution? -> normal
hist(aby24$nb_pollin)

#grass species number
shapiro.test(aby24$nb_grasses) #distribution? -> NOT normal
hist(aby24$nb_grasses)



## Create generalized linear mixed models (glmm) ##
# Total insect pollinated plant species number
glm.polp24 <- glmmTMB(nb_pollin~seedDensity*seedMixture+(1|replication)+(1|DensityBlock)+(1|GrownBlock),
                      family = poisson(link = "log"), data = aby24)

# Total grass species number (Poaceae)
glm.grass24 <- glmmTMB(nb_grasses~seedDensity*seedMixture+(1|replication)+(1|DensityBlock)+(1|GrownBlock),
                       family = poisson(link = "log"), data = aby24)


# Anova
Anova(glm.polp24)
Anova(glm.grass24)

#post-hoc for single significant variable (seed mixture) in insect pollinated plants
summary(pairs(emmeans(glm.polp24, "seedMixture")), adjust = "tukey")   # useless -> type not significant

#post-hoc for single significant variable (seed mixture) in grasses
summary(pairs(emmeans(glm.grass24, "seedMixture")), adjust = "tukey")   # useless -> type not significant


# check seed density within seed mixture - insect pollinated plants
emmpoll24 <- emmeans(glm.polp24, specs = pairwise ~ seedDensity | seedMixture, type = "response", adjust = "tukey")
emmpoll24 <- emmpoll24$contrasts %>%    # in this way you can put all comparisons together,
  rbind(adjust = "tukey")
summary(emmpoll24, infer = T)


# check seed density within seed mixture - grasses
emmgrass24 <- emmeans(glm.grass24, specs = pairwise ~ seedDensity | seedMixture, type = "response", adjust = "tukey")
emmgrass24 <- emmgrass24$contrasts %>%    # in this way you can put all comparisons together,
  rbind(adjust = "tukey")
summary(emmgrass24, infer = T)


### - plotting graphs - ###
# Insect pollinated plants
pollpla24<-ggplot(data=aby24,aes(x=seedMixture,y=nb_pollin,fill=seedDensity))+
  geom_boxplot()+
  scale_fill_manual(name="Seed density", values=c("grey80","grey50","grey20"),
                    breaks=c("1","2","3"),
                    labels=c("-50%", "100%", "+100%"))+
  xlab("")+
  ylab("Number of insect pollinated plants")+
  geom_point(position = position_dodge(width = 0.9), colour = "black", alpha = 0.5)+
  annotate("text", x = 1:5, y = 25, label = c("b","a","a","a","b"),size=5)+
  theme_classic(base_size = 15)+
  ggtitle("2024")+
  theme(plot.title = element_text(hjust = 0.5, size = 28, face = "bold"))+
  theme(strip.background=element_blank(),strip.text = element_blank(),
        axis.text.x = element_blank(), axis.text.y=element_text(size=15))

pollpla24

#grasses (Poaceae)
grass24<-ggplot(data=aby24,aes(x=seedMixture,y=nb_grasses,fill=seedDensity))+
  geom_boxplot()+
  scale_fill_manual(name="Seed density", values=c("grey80","grey50","grey20"),
                    breaks=c("1","2","3"),
                    labels=c("-50%", "100%", "+100%"))+
  ylab("Number of grasses")+
  xlab("Seed mixture")+
  geom_point(position = position_dodge(width = 0.9), colour = "black",alpha = 0.5)+
  annotate("text", x = 1:5, y = 5, label = c("a","a","a","a","a"),size=5)+
  theme_classic(base_size = 15)+
  theme(axis.text.y=element_text(size=15),axis.text.x=element_text(size=15))

grass24

# Combine both graphs
grassespoll24<-grid.arrange(pollpla24, grass24) 
ggsave("glm_pollgrasses24.jpg",plot=grassespoll24,width=10,height=7,dpi=300, path = "Y:/Dürnast_Blühstreifen/Veröffentlichung/Graphen")



##### Moran's I analysis #####

library(sf)    # for importing Shape files
library(spdep) # for Moran's I analysis
library(tmap)  # to check shp file data

shp.file <- st_read( "Y:/Dürnast_Blühstreifen/Veröffentlichung/DataFiles/Location_sites_Duernast_sepYears.shp")
# Check attribute table columns
names(shp.file)
View(shp.file) # check if data table was imported correctly

## Visualizing data
# Biomass
tm_shape(shp.file) + tm_fill(col="F21biomass", style="quantile", n=8, palette="Greens") +
  tm_legend(outside=TRUE)

# total species richness 2021
tm_shape(shp.file) + tm_fill(col="F21all_spe", style="quantile", n=8, palette="Greens") +
  tm_legend(outside=TRUE)

# total species richness 2024
tm_shape(shp.file) + tm_fill(col="F24all_spe", style="quantile", n=8, palette="Greens") +
  tm_legend(outside=TRUE)


#### - Moran's I analysis as a function of contiguous neighbour - ####
nb <- poly2nb(shp.file, queen=TRUE)
nb
# assign weight to neighbors
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
lw$weights[2]

### - Moran's I for biomass - ###
#visually check assumptions (distribution of attribute values and outliers)
hist(shp.file$F21biomass, main=NULL)
boxplot(shp.file$F21biomass, horizontal = TRUE)

# computing for Moran's I statistics
I.bm <- moran(shp.file$F21biomass, lw, length(nb), Szero(lw))[1]
I.bm

# performing hypothesis test with monte carlo approach
MC.bm <- moran.mc(shp.file$F21biomass, lw, nsim=999, alternative="greater")

MC.bm

plot(MC.bm, xlab = "Moran's I") 

### - Moran's I for overall species richness in 2021 - ###
#visually check assumptions (distribution of attribute values and outliers)
hist(shp.file$F21all_spe, main=NULL)
boxplot(shp.file$F21all_spe, horizontal = TRUE)

# computing for Moran's I statistics
I.snr <- moran(shp.file$F21all_spe, lw, length(nb), Szero(lw))[1]
I.snr

# performing hypothesis test with monte carlo approach
MC.snr<- moran.mc(shp.file$F21all_spe, lw, nsim=999, alternative="greater")

MC.snr

plot(MC.snr, xlab = "Moran's I") 

### - Moran's I for overall species richness in 2024 - ###
#visually check assumptions (distribution of attribute values and outliers)
hist(shp.file$F24all_spe, main=NULL)
boxplot(shp.file$F24all_spe, horizontal = TRUE)

# computing for Moran's I statistics
I.snr24 <- moran(shp.file$F24all_spe, lw, length(nb), Szero(lw))[1]
I.snr24

# performing hypothesis test with monte carlo approach
MC.snr24<- moran.mc(shp.file$F24all_spe, lw, nsim=999, alternative="greater")

MC.snr24

plot(MC.snr24, xlab = "Moran's I") 

#### - Moran's I analysis as a function of distance band - ####
# calculate centroid of each plot
s.center <- st_point_on_surface(shp.file)
s.coord <- st_coordinates(s.center)

# define neighbour as including all polygon centers within specific units rage

s.dist  <-  dnearneigh(s.coord, 0, 0.00025) # chosen distance: 25 m (including west and east next GrownBlocks as neighbours)
s.dist
s.dist[22]

# create list of neighbouring polygons
lw <- nb2listw(s.dist, style="W", zero.policy = TRUE)
lw

## for biomass
# run Monte Carlo simulation
MI.bm  <-  moran.mc(shp.file$F21biomass, lw, nsim=999, zero.policy = TRUE) 
MI.bm

plot(MI.bm, xlab = "Moran's I") 

## for species richness in 2021
# run Monte Carlo simulation
MI.snr  <-  moran.mc(shp.file$F21all_spe, lw, nsim=999, zero.policy = TRUE) 
MI.snr

plot(MI.snr, xlab = "Moran's I")

## for species richness in 2024
# run Monte Carlo simulation
MI.snr24  <-  moran.mc(shp.file$F24all_spe, lw, nsim=999, zero.policy = TRUE) 
MI.snr24

plot(MI.snr24, xlab = "Moran's I")
