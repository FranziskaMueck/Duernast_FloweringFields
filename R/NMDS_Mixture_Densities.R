####### ----- NMDS Plot Code ----- ######

# clear workspace from any previous entry
rm(list = ls())

library("vegan")        # used to calculate NMDS
library("ggplot2")      # used for plotting NMDS
library("tidyr")        # used for usage of % symbols
library("tidyverse")    # used for mutate() function

######  all species (per month) ####
specm<-read.csv("Y:/Dürnast_Blühstreifen/Veröffentlichung/DataFiles/data_processed_NMDS_NoOutliers.csv", check.names=FALSE, sep = ";")
rownames(specm)<-specm$species

#delete first column with rownames
specm$species<-NULL

#remove empty lines
specmz<-specm[rowSums(specm)!=0,]
specmz<-as.data.frame(t(specmz))
specmzy<-specmz[rowSums(specmz)!=0,]

View(specmzy) # check if data was imported correctly

#second matrix
abm1<-read.csv2("Y:/Dürnast_Blühstreifen/Veröffentlichung/DataFiles/data_raw_Duernast_NMDS_FieldData.csv", sep = ";")
abm1$seed_density<-as.factor(abm1$seed_density)
abm1$seed_mixture<-as.factor(abm1$seed_mixture)
abm <- abm1 %>%
  mutate(
    seed_density = as.factor(seed_density),
    seed_mixture = as.factor(seed_mixture),
    sampling_year = as.factor(sampling_year),
    seed_mixture = fct_relevel(seed_mixture, "L.nr","M.nr","M.r","I.nr","H.r"))

#specify rownames
rownames(abm)<-abm$code

#delete first column with rownames
abm$code<-NULL

#combine matrix 1 & 2
abmord<-abm[,c("year_mix_group","seed_mixture","seed_density","sampling_period","sampling_year","spec_nr", "id")]

#prepare table for NMDS
species<-cbind(abmord[1],specmz)

#NMDS
NMDS <- metaMDS(species, k = 2, distance = "bray", trymax = 10000) # with k = number of dimensions
stressplot(NMDS) # Large scatter around the line suggests that original dissimilarities are not well preserved
varTrans <- ordisurf(NMDS ~ year_mix_group, data = species, method = "REML", selected = TRUE)
summary(varTrans)

#transform NMDS to data Frame
nmds.scores <- species %>%
  mutate(NMDS1 = NMDS$points[, 1], NMDS2 = NMDS$points[, 2])


# scores function from vegan to extract site scores and convert to a data.frame
nmds.scores$site <- rownames(nmds.scores)  # create a column of site names, from the rownames of data.scores
nmds.scores <- cbind(nmds.scores, abmord[2])  #  add the grp variable created earlier
nmds.scores <- cbind(nmds.scores, abmord[5])

species.scores <- as.data.frame(scores(NMDS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
species.only <- species.scores[-1,]

#creating hulls for each mixture/sampling year
grp.1 <- nmds.scores[nmds.scores$year_mix_group == "1", ][chull(nmds.scores[nmds.scores$year_mix_group == 
                                                                              "1", c("NMDS1", "NMDS2")]), ]  # hull values for first group
grp.2 <- nmds.scores[nmds.scores$year_mix_group == "2", ][chull(nmds.scores[nmds.scores$year_mix_group == 
                                                                   "2", c("NMDS1", "NMDS2")]), ]  # hull values for first group
grp.3 <- nmds.scores[nmds.scores$year_mix_group == "3", ][chull(nmds.scores[nmds.scores$year_mix_group == 
                                                                              "3", c("NMDS1", "NMDS2")]), ]  # hull values for first group
grp.4 <- nmds.scores[nmds.scores$year_mix_group == "4", ][chull(nmds.scores[nmds.scores$year_mix_group == 
                                                                              "4", c("NMDS1", "NMDS2")]), ]  # hull values for first group
grp.5 <- nmds.scores[nmds.scores$year_mix_group == "5", ][chull(nmds.scores[nmds.scores$year_mix_group == 
                                                                              "5", c("NMDS1", "NMDS2")]), ]  # hull values for first group
grp.6 <- nmds.scores[nmds.scores$year_mix_group == "6", ][chull(nmds.scores[nmds.scores$year_mix_group == 
                                                                              "6", c("NMDS1", "NMDS2")]), ]  # hull values for first group
grp.7 <- nmds.scores[nmds.scores$year_mix_group == "7", ][chull(nmds.scores[nmds.scores$year_mix_group == 
                                                                              "7", c("NMDS1", "NMDS2")]), ]  # hull values for first group
grp.8 <- nmds.scores[nmds.scores$year_mix_group == "8", ][chull(nmds.scores[nmds.scores$year_mix_group == 
                                                                              "8", c("NMDS1", "NMDS2")]), ]  # hull values for first group
grp.9 <- nmds.scores[nmds.scores$year_mix_group == "9", ][chull(nmds.scores[nmds.scores$year_mix_group == 
                                                                              "9", c("NMDS1", "NMDS2")]), ]  # hull values for first group
grp.10 <- nmds.scores[nmds.scores$year_mix_group == "10", ][chull(nmds.scores[nmds.scores$year_mix_group == 
                                                                              "10", c("NMDS1", "NMDS2")]), ]  # hull values for first group

hull.data <- rbind(grp.1, grp.2, grp.3, grp.4, grp.5, grp.6, grp.7, grp.8, grp.9, grp.10)  #combine groups to one dataframe

nmds.plot <- ggplot() + 
            geom_text(data=species.only,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
            #geom_text_repel(data=species.only, aes(x = NMDS1, y = NMDS2, label = species))+
            geom_point(data=nmds.scores,aes(x=NMDS1,y=NMDS2,colour = seed_mixture, shape = sampling_year),size=2) + # add the point markers for each plot
            geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=seed_mixture, group=year_mix_group),alpha=0.30) + # add the convex hulls
            #geom_text(data=nmds.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the plot labels
            scale_colour_manual(values=c("L.nr" = "yellow", "M.nr" = "pink","M.r" = "firebrick", "I.nr" ="forestgreen","H.r" ="darkblue")) +
            scale_shape_manual(values=c("2021" = 16, "2024" = 17))+
            scale_fill_manual(values=c("L.nr" = "yellow", "M.nr" = "pink","M.r" = "firebrick", "I.nr" ="forestgreen","H.r" ="darkblue")) +
            coord_equal() +
            theme_bw()+
            theme(panel.background = element_blank(), 
                  panel.grid.major = element_blank(),  #remove major-grid labels
                  panel.grid.minor = element_blank(),
                  plot.background = element_blank())
nmds.plot
ggsave("NMDS_plot.jpg",plot=nmds.plot,width=7,height=5,dpi=300, path = "Y:/Dürnast_Blühstreifen/Veröffentlichung/Graphen/")


### - Permanova statistics - ###

distm1 <- vegdist(species)

ado <- adonis2(distm1 ~ year_mix_group, data = species, permutations = 10000, method = "bray")
ado


