##  

##   Figures for lidar paper



# mean max canopy height

library(ggplot2)

lid<- as.data.frame(read_excel("BEF_forest_structure_data.xlsx"))

##generating new columns depending on treatment (N vs No_N) (P vs No_P)
lid$N_treatment=ifelse(lid$Treatment %in% c("NP","N"), "With_N","Without_N") 
lid$P_treatment=ifelse(lid$Treatment %in% c("NP","P"), "With_P","Without_P")


head(lid)
lid$N_treatment<-factor(lid$N_treatment,levels=c("Without_N","With_N"))
lid$Pgroup<-paste(lid$P_treatment, lid$Stand, lid$Year)

table(lid$Treatment)

lid$Treatment<-factor(lid$Treatment, levels=c("Control","N","P","NP"))

head(lid)
lid$Years<-as.factor(lid$Years)

ggplot(lid, aes(x=N_treatment, y=mean.max.canopy.ht.aop, shape=Years, group=Pgroup))+geom_point(aes(col=Treatment))+geom_line()+
  facet_wrap(~Stand, scales="free_y")+scale_color_manual(values=c("black","blue","red","purple"))+
  ylab("mean max tree height (m)")+theme_classic()


ggplot(lid, aes(x=N_treatment, y= rumple.aop  , shape=Years, group=Pgroup))+geom_point(aes(col=Treatment))+geom_line()+
  facet_wrap(~Stand, scales="free_y")+scale_color_manual(values=c("black","blue","red","purple"))+
  ylab("mean max tree height (m)")+theme_classic()

o1 <- anova( lme( rumple.aop ~ N_treatment*P_treatment+Year+Age, random=~1|Stand, data=data3))


c1 <- emmeans(lme( rumple.aop ~ N_treatment*P_treatment+Year+Age, random=~1|Stand, data=data3), pairwise ~ N_treatment+P_treatment)
c1


rump<-as.data.frame(c1$emmeans)
rump

rump$Treatment<-c("Control","N","P","N+P")
rump$Treatment<-factor(rump$Treatment, levels=c("Control","N","P","N+P"))

f1<-ggplot(rump, aes(x=Treatment, y=emmean, fill=Treatment))+geom_bar(stat="identity", col="black")+
  scale_fill_manual(values=c("black","blue","red","purple"))+theme_classic()+
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0.3)+ylab("Rumple (%)")


######
head(lid)
h1 <- emmeans(lme( mean.max.canopy.ht.aop ~ N_treatment*P_treatment+Year+Age, random=~1|Stand, data=data3), pairwise ~ N_treatment+P_treatment)
h1


mmch<-as.data.frame(h1$emmeans)
mmch

mmch$Treatment<-c("Control","N","P","N+P")
mmch$Treatment<-factor(mmch$Treatment, levels=c("Control","N","P","N+P"))

f2<-ggplot(mmch, aes(x=Treatment, y=emmean, fill=Treatment))+geom_bar(stat="identity", col="black")+
  scale_fill_manual(values=c("black","blue","red","purple"))+theme_classic()+
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0.3)+ylab("Mean max canopy height (m)")

library(ggpubr)
ggarrange(f1, f2, common.legend=T, legend="bottom")







