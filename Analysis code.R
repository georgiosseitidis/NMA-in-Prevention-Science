#Load libraries
library(netmeta)
library(ggplot2)

#Load data
data=read.csv("Alcohol prevention dataset.csv")
#es.tot refer to the comparison treat2 vs treat1

#Network Meta-Analysis Graph theory  model
NMA=netmeta(TE=es_tot,seTE=se_tot,treat1=treat2, treat2 =treat1,studlab=studyid,reference.group ="AO-CT" ,sm="SMD",tol.multiarm = 0.05,comb.random = T,data=data)

#Network's geometry
netgraph(NMA,number.of.studies = T,plastic = F)

#Results of Network Meta-Analsis (Treatment effect, Heterogeneity, Inconsistency)
summary(NMA)

#League Table
netleague(NMA)

#Check for inconsistency
#Globally

#Design-based decomposition of Cochran's Q
decomp.design(NMA)

#Generalized Q statistic
NMA$pval.Q

#Locally

#Net heat plot (No intense colours identified) 
netheat(NMA) 

#Node splitting
netsplit(NMA)

#Ranking (P-scores)
netrank(NMA,small.values = "good")

#Forest plot from netmeta
forest.netmeta(NMA,smlab = "Random Effect Model",xlab="Quantity of Alcohol Use: 0-3 Months",leftlabs = "",
               reference.group =c("AO-CT","BASICS"),xlim=c(-1,1) )

#Forest plot from gg-plot
te.random=as.data.frame(NMA$TE.random) #NMA treatment estimate
lb.random=as.data.frame(NMA$lower.random) #NMA lower bound 
ub.random=as.data.frame(NMA$upper.random) #NMA upper bound
dat.forest=data.frame(matrix(nrow = dim(te.random)[1]))
dat.forest$node=names(te.random) #Names of the nodes
dat.forest=as.data.frame(dat.forest[,2]) #Omit first column since it has only NAs

#AO-CT is the reference category (column 4)
for(i in 1:dim(te.random)[1]){
  dat.forest[i,2]=te.random[i,which(colnames(te.random)=="AO-CT")] #Treatment estimate
  dat.forest[i,3]=lb.random[i,which(colnames(te.random)=="AO-CT")] #Lower bound
  dat.forest[i,4]=ub.random[i,which(colnames(te.random)=="AO-CT")] #Upper bound
}
colnames(dat.forest)=c("Treatment","TE","lb","ub")
#Omit the line with the reference Treatment
dat.forest=dat.forest[-which(dat.forest$Treatment=="AO-CT"),]

#Forest plot
ggplot(dat.forest, aes(y=Treatment, x=TE))+
  #Add the CI error bars
  geom_errorbarh(height=0.1, size=1, aes(xmin=lb, xmax=ub))+
  #Add data points and color them 
  geom_point(size=4.5,col="blue",aes(y=Treatment, x=TE))+
  #Add labels in points
  geom_text(data = dat.forest, aes( label =round(TE,2)),vjust = -0.8 , size = 4) +
  geom_text(data = dat.forest, aes( label =round(lb,2),x=lb),vjust = -0.3,hjust=1.15 , size = 4) +
  geom_text(data = dat.forest, aes( label =round(ub,2),x=ub),vjust = -0.3,hjust=-0.1, size = 4) +
  #Specify the limits of the x-axis and relabel it to something more meaningful
  scale_x_continuous(limits=c(-1,1),breaks=seq(-1, 1, 0.2), name='Quantity of Alcohol Use: 0-3 Months')+
  #Set y-axis label
  ylab('')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=0, color='red', linetype='dashed')+
  #Title
  ggtitle("")+
  labs(subtitle="",caption  = "Reference treatment: AO-CT",size=14)+
  #Apply theme
  theme(legend.position = "none",
        axis.text.x = element_text( size=12),
        axis.text.y = element_text( size=12),
        axis.title.x = element_text(size=14,margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y=element_text(size=14,margin = margin(t = 0, r = 15, b = 0, l = 0)),
        panel.grid.minor.x = element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(size = 13),
        plot.title =  element_text(hjust = 0.5))

#Save ggplot
tiff("forestplot.tiff",units="in", width=10, height=6, res=400)
dev.off()
ggsave("forestplot.tiff", units="in", width=10, height=6, dpi=400, compression = 'lzw')

