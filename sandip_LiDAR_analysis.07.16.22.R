getwd()
library(data.table)
library(tidyr)
library(nlme)
library(lmerTest)
library(readxl)
library(emmeans)

lid<- read_excel("BEF_forest_structure_data.xlsx")
lid<-as.data.frame(lid)
##read in data
head(lid)
names(lid)

# turn years into 'year' for year since treatment
lid$Year <- lid$Years-2011
lid$Year <- as.factor(lid$Year)


## N addition and P addition
##generating new columns depending on treatment (N vs No_N) (P vs No_P)
lid$N_treatment=ifelse(lid$Treatment %in% c("NP","N"), "With_N","Without_N") 
lid$P_treatment=ifelse(lid$Treatment %in% c("NP","P"), "With_P","Without_P")


head(lid)

lid$Stand <- as.factor(lid$Stand)
lid$N_treatment <- as.factor(lid$N_treatment)
lid$P_treatment <- as.factor(lid$P_treatment)

lid$N_treatment <- factor(lid$N_treatment,
                            levels= c("Without_N", "With_N"))

lid$P_treatment <- factor(lid$P_treatment,
                            levels= c("Without_P", "With_P"))

lid$Age <- factor(lid$Age,
                  levels=c("~30 years old", "~60 years old", "~100 years old"),
                  labels= c("young", "medium", "mature"))

## Specify a function that specifies the mixed effect model you'd like to use for each canopy metric
## this gets the coefficients for the fixed effects
aov.mixed <- function(y, Stand, N_treatment, P_treatment, Year, Age){
  #1 aov for linear mixed effect model
  mixed1<-anova( lme(y ~ N_treatment*P_treatment+Year+Age, random=~1|Stand, data=lid))
  return(mixed1)}

names(lid)
# we want to loop through the dependent variables. These are columns 6:18
names(lid)



# essentially, you are going through each column 6:18 and generating this output
<<<<<<< HEAD
library(emmeans)
m1 <- ( lme( mean.max.canopy.ht.aop ~ N_treatment*P_treatment+Year+Age, random=~1|Stand, data=lid))
anova(m1)
=======


m1 <- anova(( lme( mean.max.canopy.ht.aop ~ N_treatment*P_treatment+Year+Age, random=~1|Stand, data=data3)))
m1
>>>>>>> 62261e521cb68a0815324fc81377cd7d615bf068
a1 <- emmeans(m1, pairwise ~ N_treatment+P_treatment)
a11 <- emmeans(m1, pairwise ~ Age+Year)
a1
a11
n1 <- ( lme( entropy.aop ~ N_treatment*P_treatment+Year+Age, random=~1|Stand, data=lid))
b1 <- emmeans(n1, pairwise ~ N_treatment+P_treatment)
b11 <- emmeans(n1, pairwise ~ Age)
b1
b11
o1 <- ( lme( rumple.aop ~ N_treatment*P_treatment+Year+Age, random=~1|Stand, data=lid))
c1 <- emmeans(o1, pairwise ~ N_treatment+P_treatment)
c1
<<<<<<< HEAD
p1 <- ( lme( sd.sd.aop.aop ~ N_treatment*P_treatment+Year+Age, random=~1|Stand, data=lid))
=======
p1 <- ( lme( sd.sd.aop ~ N_treatment*P_treatment+Year+Age, random=~1|Stand, data=data3))

>>>>>>> 62261e521cb68a0815324fc81377cd7d615bf068
d1 <- emmeans(p1, pairwise ~ N_treatment+P_treatment)
d1
q1 <- ( lme(VAI.AOP.aop ~ N_treatment*P_treatment+Year+Age, random=~1|Stand, data=lid))
e1 <- emmeans(q1, pairwise ~ N_treatment+P_treatment)
e1
r1 <- ( lme(VCI.AOP.aop ~ N_treatment*P_treatment+Year+Age, random=~1|Stand, data=lid))
g1 <- emmeans(r1, pairwise ~ N_treatment+P_treatment)
g1

<<<<<<< HEAD
for(i in c(6:18)){ 
  y = lid[,i]
  Stand= lid$Stand
  N_treatment=lid$N_treatment
  P_treatment=lid$P_treatment
  output.mixed[[i-5]] <- aov.mixed(y, Stand, N_treatment, P_treatment, Year, Age)}
=======

rbind(m1,n1,o1, p1, q1, r1)


names(data3) # start the list of columns where the dependent variables start
#make an empty 'list' to hold the output

output.mixed<-list()


for(i in c(8:21)){ 
  y = data3[,i]
  Stand= data3$Stand
  N_treatment=data3$N_treatment
  P_treatment=data3$P_treatment
  output.mixed[[i-7]] <- aov.mixed(y, Stand, N_treatment, P_treatment, Year, Age)}


>>>>>>> 62261e521cb68a0815324fc81377cd7d615bf068


output.mixed

# so some formatting
d.int<- as.data.frame(rbindlist(output.mixed))
d.int
d.int$Source<-rep(c("intercept","N addition","P addition","Year","Age","N*P"))
dim(d.int)

d.int

# this adds in row names to d.int
rep(names(lid)[c(6:18)], each=6)
# lets add these rows into d.int, the dataframe of model outputs (p values and df)
d.int$variable<-rep(names(lid)[c(6:18)], each=6)
names(d.int)

names(d.int)
# some re-ordering of columns to make it easier to interpret
d.int<-d.int[ ,c(6,5, 1, 2, 3, 4)]
names(d.int)

# for an easier time of formatting names for tables
pism<-d.int
pism

fp<-spread(pism[ , c(1,2,6)], "Source","p-value")

head(fp)

# re-order again
fp.print<-fp[,c(1,7,2,4,6,5)]

head(fp.print)

# this writes a .csv to the directory you are in.
setwd(dir ="E:/forest structure data" )
write.csv(fp.print, file="p_values_structural_metrics_6_25.csv")

##graphs

library(ggplot2)

lid$Age <- factor(lid$Age,
                    levels=c("~30 years old", "~60 years old", "~100 years old"),
                    labels= c("young", "medium", "mature"))

lid$staplo<-paste(lid$Stand, lid$Treatment)
ggplot(lid, aes(x=Years, y=mean.max.canopy.ht.aop, col=Treatment,group=staplo))+geom_point()+
  facet_wrap(~Age)+scale_color_manual(values=c(Control="black",N="blue", P="red",NP="purple"))+
    geom_smooth(method="lm",aes(col=Treatment))

lid$statP<-paste(lid$Years, lid$Stand, lid$P_treatment)
lid$statN<-paste(lid$Years, lid$Stand, lid$N_treatment)
lid$N_treatment<-factor(lid$N_treatment, levels=c("Without_N","With_N"))


ggplot(lid, aes(x=N_treatment, y=mean.max.canopy.ht.aop, col=Treatment,group=statP))+
  geom_point(size=2)+geom_line()+
  facet_wrap(~Age)+
  scale_shape_manual(values = c(0, 1, 2,8))+
  scale_color_manual(values=c(Control="grey",N="blue", P="red",NP="purple"))+
  theme_classic()+ylab("Max canopy height (m)")+xlab("Addition of N")+
  theme(legend.position = "bottom")




##For Nitrogen Only
ggplot(lid, aes(x=N_treatment, y=mean.max.canopy.ht.aop,group=statP))+
  geom_point(aes(shape=Treatment, color=Treatment, size=2))+
  scale_shape_manual(values=c(15, 16, 17,18))+
  geom_line()+
  facet_wrap(~Age)+scale_color_manual(values=c(Control="grey",N="blue", P="red",NP="purple"))+
  theme_classic()+ylab("Mean Max canopy height (m)")+xlab("Addition of N")+
  theme(legend.position = "bottom")+
  guides(colour=guide_legend(override.aes = list(size = 4)),shape="none", size="none")



##For Phosphorus only
ggplot(lid, aes(x=P_treatment, y=mean.max.canopy.ht.aop,group=statN))+
  geom_point(aes(shape=Treatment, color=Treatment, size=2))+
  scale_shape_manual(values=c(15, 16, 17,18))+
  geom_line()+
  facet_wrap(~Age)+scale_color_manual(values=c(Control="grey",N="blue", P="red",NP="purple"))+
  theme_classic()+ylab("Mean Max canopy height (m)")+xlab("Addition of P")+
  theme(legend.position = "bottom")+
  guides(colour=guide_legend(override.aes = list(size = 4)), shape="none", size="none")


### Different graphics##################################################################
data1=lid
colnames(data1)
For_N <-data1 %>% select(Years, 
                           Age,
                           Treatment,
                           Stand,
                           N,
                           mean.max.canopy.ht.aop,
                           VAI.AOP.aop, 
                           rumple.aop,
                           entropy.aop,
                           sd.sd.aop) %>% 
  rename(Year=Years,
         Age1=Age,
         Stands=Stand,
         Treatments=Treatment,
         N_treat=N,
         Mean_Canopy_Height= mean.max.canopy.ht.aop,
         VAI=VAI.AOP.aop,
         Rumple=rumple.aop,
         Entropy=entropy.aop,
         Std_std=sd.sd.aop) %>% 
  filter(N_treat=="1")

No_N <-data1 %>% select(Years, 
                          Age,
                          Stand,
                          Treatment,
                          N,
                          mean.max.canopy.ht.aop,
                          VAI.AOP.aop,
                          rumple.aop,
                          entropy.aop,
                          sd.sd.aop) %>% 
  filter(N=="0")
head(heights)
heights$Years

For_P <-data1 %>% select(Years, 
                           Age,
                           Treatment,
                           Stand,
                           P,
                           VCI.AOP.aop, 
                           rumple.aop) %>%
  rename(years=Years,
         Age2=Age,
         Stand2=Stand,
         treatments=Treatment,
         P_treat=P,
         VCI=VCI.AOP.aop,
         Rumple=rumple.aop) %>% 
  filter(P_treat=="1")

No_P <-data1 %>% select(Years, 
                          Age,
                          Stand,
                          Treatment,
                          P,
                          VCI.AOP.aop,
                          rumple.aop) %>% 
  filter(P=="0")


The_N <- cbind(For_N, No_N)
The_P <- cbind(For_P, No_P)
head(The_N)

##For Nitrogen
colnames(The_N)
The_N$Year <- as.factor(The_N$Year)
p <- ggplot(data = The_N, aes(x =mean.max.canopy.ht.aop,  y = Mean_Canopy_Height )) + 
  geom_point(aes(colour = Stand, shape = Year), size=3) +
  theme_bw() + 
  ylab(" Canopy Mean Height with N addition") +
  xlab("Canopy Mean Height without N addition") +
  (geom_abline(intercept=0, slope=1)) 
p<- p + theme(axis.title = element_text(size = 15))  
p

colnames(The_N)
q <- ggplot(data = The_N, aes(x =VAI.AOP.aop,  y = VAI)) + 
  geom_point(aes(colour = Stand, shape = Year), size=3) +
  theme_bw() + 
  ylab(" VAI with N addition") +
  xlab("VAI without N addition") +
  (geom_abline(intercept=0, slope=1)) 
q<- q + theme(axis.title = element_text(size = 15))  
q

m <- ggplot(data = The_N, aes(x =entropy.aop,  y = Entropy)) + 
  geom_point(aes(colour = Stand, shape = Year), size=3) +
  theme_bw() + 
  ylab(" Entropy with N addition") +
  xlab("Entropy without N addition") +
  (geom_abline(intercept=0, slope=1)) 
m<- m + theme(axis.title = element_text(size = 15))  
m

n <- ggplot(data = The_N, aes(x =sd.sd.aop,  y = Std_std)) + 
  geom_point(aes(colour = Stand, shape = Year), size=3) +
  theme_bw() + 
  ylab(" Std of std with N addition") +
  xlab("Std of std without N addition") +
  (geom_abline(intercept=0, slope=1)) 
n<- n + theme(axis.title = element_text(size = 15))  
n

##For Phosphorus
The_P$Years <- as.factor(The_P$Years)
The_P$Stand2 <- as.factor(The_P$Stand2)
r <- ggplot(data = The_P, aes(x = rumple.aop,  y = Rumple))+
  geom_point(aes(colour = Stand2, shape = Years), size=4) +
  xlim(1,3)+
  theme_bw()+
  ylab("Rumple with P addition") +
  xlab("Rumple without P addition") +
  geom_abline(intercept=0, slope=1)
r <- r + theme(axis.title = element_text(size = 15))

s <- ggplot(data = The_P, aes(x = VCI.AOP.aop,  y = VCI))+
  geom_point(aes(colour = Stand2, shape = Years), size=4) +
  theme_bw()+
  ylab("VCI with P addition") +
  xlab("VCI without P addition") +
  geom_abline(intercept=0, slope=1)
s <- s + theme(axis.title = element_text(size = 15))
s

