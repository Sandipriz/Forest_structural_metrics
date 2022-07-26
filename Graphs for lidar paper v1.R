##  

##   Figures for lidar paper



# mean max canopy height


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
table(lid$Treatment)
lid$Years<-as.factor(lid$Years)
ggplot(lid, aes(x=N_treatment, y=mean.max.canopy.ht.aop, shape=Years, group=Pgroup))+geom_point(aes(col=Treatment))+geom_line()+
  facet_wrap(~Stand, scales="free_y")+scale_color_manual(values=c("black","blue","red","purple"))+
  ylab("mean max tree height (m)")+theme_classic()





