################ Overview Graph of whole site ####################

rm(list = ls()) # clear workspace

#used packages
library(ggplot2)

### ---- Bar Plot Species total Numbers ---- ###

# ----- Data 2021 (No May) ----- #

summary.all20<-read.csv2("Y:/Dürnast_Blühstreifen/Veröffentlichung/RCodes/data_processed_summary_2021_NoMay.csv")
summary.all20$seed_mixture<-factor(summary.all20$seed_mixture,levels=c("L.nr","M.nr","M.r","I.nr","H.r","weed")) # sort x-axis
summaryall20<-summary.all20[1:5,] # only use 5 seed mixtures

#creating dataframe for barplot
library(reshape2)
df1all20<-data.frame(summaryall20$weed,summaryall20$estab,summaryall20$seed_mixture)
df2all20<-melt(df1all20, id.vars='summaryall20.seed_mixture')
sown<-c(26,47,42,41,61,26,47,42,41,61)
df3all20<-cbind(df2all20,sown)

#final graph
allstackall20<-ggplot(df3all20,aes(summaryall20.seed_mixture,value,fill=variable))+
  geom_bar(position="stack",stat="identity")+
  scale_fill_manual(name="", values=c("grey70","grey30"),
                    breaks=c("summaryall20.weed","summaryall20.estab"),
                    labels=c("unsown", "sown & observed"))+
  ylab("Total species number (all plots)")+
  xlab("Seed mixture")+
  theme_classic(base_size = 15)+
  geom_errorbar(aes(ymax=sown,ymin=sown),linetype="dashed",
                position=position_dodge())+
  annotate("text",5.4,63,label="sown")+
  annotate("text",x=1:5,y=3,label=c("69%","60%","61%","43%","46%"),colour="white")+
  annotate("text",x=1,y=22,label="39%")+annotate("text",x=2,y=37,label="36%")+annotate("text",x=3,y=31,label="29%")+
  annotate("text",x=4,y=34,label="44%")+annotate("text",x=5,y=37,label="28%")+
  theme(axis.text.y=element_text(size=15),axis.text.x=element_text(size=15))+
  ggtitle("2021")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

allstackall20
ggsave("Fig1a_Barplot2021.jpg",plot=allstackall20,width=7,height=5,dpi=300, path = "Y:/Dürnast_Blühstreifen//Veröffentlichung/Graphen/")

# ----- Data 2024 (June & August) ----- #

summary.all24<-read.csv2("Y:/Dürnast_Blühstreifen/Veröffentlichung/RCodes/data_processed_summary_2024.csv")
summary.all24$seed_mixture<-factor(summary.all24$seed_mixture,levels=c("L.nr","M.nr","M.r","I.nr","H.r","weed")) # sort x-axis
summaryall24<-summary.all24[1:5,] # only use 5 seed mixtures

#creating dataframe for barplot
library(reshape2)
df1all24<-data.frame(summaryall24$weed,summaryall24$estab,summaryall24$seed_mixture)
df2all24<-melt(df1all24, id.vars='summaryall24.seed_mixture')
sown<-c(26,47,42,41,61,26,47,42,41,61)
df3all24<-cbind(df2all24,sown)

#final graph
allstackall24<-ggplot(df3all24,aes(summaryall24.seed_mixture,value,fill=variable))+
  geom_bar(position="stack",stat="identity")+
  scale_fill_manual(name="", values=c("grey70","grey30"),
                    breaks=c("summaryall24.weed","summaryall24.estab"),
                    labels=c("unsown", "sown & observed"))+
  ylab("Total species number (all plots)")+
  xlab("Seed mixture")+
  theme_classic(base_size = 15)+
  geom_errorbar(aes(ymax=sown,ymin=sown),linetype="dashed",
                position=position_dodge())+
  annotate("text",5.4,63,label="sown")+
  annotate("text",x=1:5,y=3,label=c("31%","52%","49%","38%","38%"),colour="white")+
  annotate("text",x=1,y=32,label="77%")+annotate("text",x=2,y=47,label="59%")+annotate("text",x=3,y=47,label="60%")+
  annotate("text",x=4,y=41,label="59%")+annotate("text",x=5,y=35,label="39%")+
  theme(axis.text.y=element_text(size=15),axis.text.x=element_text(size=15))+
  ggtitle("2024")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

allstackall24
ggsave("Fig1b_barplot2024.jpg",plot=allstackall24,width=7,height=5,dpi=300, path = "Y:/Dürnast_Blühstreifen/Veröffentlichung/Graphen")
