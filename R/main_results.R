library(readr)
library(estimatr)
library(dplyr)
setwd("/Users/ziv.e/github/ganimals_cscw") #replace for with your path
ci2 <- function(data){
  n <- sum(!is.na(data))
  s <- sd(data,na.rm = T)
  return(1.96*s/sqrt(n))
}
error.bar <- function(x, y, upper, lower=upper, length=0.05,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}


####Inequality
inequality_all <- read_csv("data/inequality_model1.csv")
inequality_all$social <- 1-inequality_all$condition %%2 
inequality_all$cloud <- as.numeric(inequality_all$condition > 2)
#table 2
summary(lm("count ~ social+cloud", data = inequality_all))

summary(lm("count ~ social", data = inequality_all%>% filter(cloud==0)))

#pre-registered fisherian randomization inference robustness check
social_p <- c()
for( i in 1:10000){
  inequality_all$soc_boot <-sample(0:1,16, replace=T)
  inequality_all$cloud_boot <-sample(0:1,16, replace=T)
  fit <- lm("count ~ soc_boot+cloud_boot", data = inequality_all)
  social <-summary(fit)$coefficients[,3][2]
  social_p<- c(social_p, social)
}
fit_a <- lm("count ~ social+cloud", data = inequality_all)
t<- summary(fit_a)$coefficients[,3][2]
sum(social_p > t) / 10000
#ßt.test(social_p)

#additional bootstrap robustness check for SI
social_p <- c()
for( i in 1:10000){
  inequality_all$gini_boot <- c(sample(inequality_all$count[1:4], replace = T), sample(inequality_all$count[5:8], replace = T) ,sample(inequality_all$count[9:12], replace = T), sample(inequality_all$count[13:16], replace = T))
  social <-mean(inequality_all[inequality_all$social==1,]$gini_boot) - mean(inequality_all[inequality_all$social==0,]$gini_boot) # summary(fit)$coefficients[,4][3]
  social_p<- c(social_p, social)
}
sum(social_p > 0)

#Figure 3
barplot(inequality_all[inequality_all$condition==1,]$count, ylim = c(0.5,0.8), col='#F38630')#","#69D2E7
abline(h=mean(inequality_all[inequality_all$condition==1,]$count))
barplot(inequality_all[inequality_all$condition==2,]$count, ylim = c(0.5,0.8), col='#69D2E7')
abline(h=mean(inequality_all[inequality_all$condition==2,]$count))
barplot(inequality_all[inequality_all$condition==3,]$count, ylim = c(0.5,0.8), col='#F38630')
abline(h=mean(inequality_all[inequality_all$condition==3,]$count))
barplot(inequality_all[inequality_all$condition==4,]$count, ylim = c(0.5,0.8), col='#69D2E7')
abline(h=mean(inequality_all[inequality_all$condition==4,]$count))


###Unpredictability
unpredictability_all1 <- read_csv("data/unpredictability_model4.csv") 
unpredictability_all1 <-data.frame(rbind(cbind(t(unpredictability_all1[1,2:7]),0,0), cbind(t(unpredictability_all1[2,2:7]),1,0) ,cbind(t(unpredictability_all1[3,2:7]),0,1), cbind(t(unpredictability_all1[4,2:7]),1,1)))
colnames(unpredictability_all1) <- c("u", "social", "cloud")
summary(lm("u ~ social+cloud", data =unpredictability_all1))

summary(lm("u ~ social", data =unpredictability_all1 %>% filter(cloud==0)))

plot_unpredictability <- function(up){
  nplot<-matrix(c(mean(up[(up$social==0) & (up$eco==0),]$u,na.rm=T),mean(up[(up$social==1) & (up$eco==0),]$u,na.rm=T),
                  mean(up[(up$social==0) & (up$eco==1),]$u,na.rm=T),mean(up[(up$social==1) & (up$eco==1),]$u,na.rm=T)),2,2,byrow=TRUE)   #with 2 being replaced by the number of genes!
  tplot<-t(nplot)
  ee<-matrix(c(ci2(up[(up$social==0) & (up$eco==0),]$u),ci2(up[(up$social==1) & (up$eco==0),]$u),
               ci2(up[(up$social==0) & (up$eco==1),]$u),ci2(up[(up$social==1) & (up$eco==1),]$u)),2,2,byrow=TRUE)
  tee<-t(ee)
  BarPlot <- barplot(tplot, beside=TRUE,ylab="Unpredictability U",main = "", ylim =c(0,max(tplot)+max(tee)) , names.arg=c("Feed" , "Ecosystem"), col=c("#F38630","#69D2E7"))
  legend("topleft", cex = 0.75,
         legend = c("Independent", "Social"), 
         fill = c("#F38630","#69D2E7"))
  #add error bars
  
  error.bar(BarPlot,tplot,tee)
  
}

#figure 4
plot_unpredictability(unpredictability_all1)


###Engagement
engagement <- read_csv("data/engagement_per_person.csv")
engagement$social <- 1-engagement$user_condition %%2 
engagement$cloud <- as.numeric(engagement$user_condition > 2)
lm_robust(data=engagement, formula=count ~ social+cloud, clusters=engagement$world)

plot_engagement <- function(){
  
  pd = as.matrix(engagement %>% group_by(user_condition) %>% summarise( count = mean(count)))[,"count"]
  se = as.matrix(engagement %>% group_by(user_condition) %>% summarise( count = sd(count)))[,"count"]
  n = as.matrix(engagement %>% count(user_condition))[,"n"]
  
  nplot<-matrix(pd,2,2,byrow=TRUE)   #with 2 being replaced by the number of genes!
  tplot<-t(nplot)
  BarPlot <- barplot(tplot, beside=TRUE,ylab="Average number of votes",main = "", ylim = c(0,6), names.arg=c("Feed" , "Ecosystem"), col=c("#F38630","#69D2E7"))
  legend("topleft", cex = 0.75,
         legend = c("Independent", "Social"), 
         fill = c("#F38630","#69D2E7"))
  #add error bars
  ee<-matrix(se/sqrt(n),2,2,byrow=TRUE)
  tee<-t(ee)
  error.bar(BarPlot,tplot,tee)
  
}

#figure 7
plot_engagement()

social_p <- c()
for( i in 1:10000){
  engagement$soc_boot <-sample(0:1,nrow(engagement), replace=T)
  engagement$eco_boot <-sample(0:1,nrow(engagement), replace=T)
  fit <- lm_robust(data=engagement, formula=count ~ soc_boot+eco_boot, clusters=engagement$world)
  social <-summary(fit)$coefficients[,3][3]
  social_p<- c(social_p, social)
}
fit_a <-lm_robust(data=engagement, formula=count ~ social+eco, clusters=engagement$world)
t<- summary(fit_a)$coefficients[,3][3]
sum(social_p > t) / 10000



###Diversity
diversity <- read_csv("Data/diversity.csv")
diversity$world = diversity$X1 + 1
diversity$social <-1 - ceiling(diversity$world /4) %% 2
diversity$cloud <- as.numeric(diversity$world >= 9)
diversity = data.frame(diversity)
colnames(diversity) <- c("X1", "entropy", "world", "social", "cloud")

#table 4
summary(lm(data=diversity, formula="entropy ~ social+ cloud"))

t.test(diversity[(diversity$cloud==0) &(diversity$social==0) ,],diversity[(diversity$cloud==0) &(diversity$social==1) ,])


#figure 6 left
nplot<-matrix(c(mean(diversity[(diversity$social==0) & (diversity$cloud==0),]$entropy),mean(diversity[(diversity$social==1) & (diversity$cloud==0),]$entropy),
                mean(diversity[(diversity$social==0) & (diversity$cloud==1),]$entropy),mean(diversity[(diversity$social==1) & (diversity$cloud==1),]$entropy)),2,2,byrow=TRUE)   #with 2 being replaced by the number of genes!
tplot<-t(nplot)
BarPlot <- barplot(tplot, beside=TRUE,ylab="Average entropy",main = "", ylim = c(0,11), names.arg=c("Feed" , "Ecosystem"), col=c("#F38630","#69D2E7"))
#add error bars
ee<-matrix(c(ci2(diversity[(diversity$social==0) & (diversity$cloud==0),]$entropy),ci2(diversity[(diversity$social==1) & (diversity$cloud==0),]$entropy),
             ci2(diversity[(diversity$social==0) & (diversity$cloud==1),]$entropy),ci2(diversity[(diversity$social==1) & (diversity$cloud==1),]$entropy)),2,2,byrow=TRUE)
tee<-t(ee)
error.bar(BarPlot,tplot,tee)
abline(h = 7.006197031998495)

divergence <- read_csv("data/divergence.csv") 
divergence$cloud = as.numeric(divergence$condition >1)
divergence$social = as.numeric(divergence$condition %% 2 == 1)

#table 4
summary(lm("pca ~ social+cloud", data =divergence))
summary(lm("norm ~ social+cloud", data =divergence))
summary(lm("w2d ~ social+cloud", data =divergence))

summary(lm("pca ~ social", data =divergence %>% filter(cloud==0)))
summary(lm("norm ~ social", data =divergence %>% filter(cloud==0)))
summary(lm("w2d ~ social", data =divergence %>% filter(cloud==0)))

divergence$u = divergence$pca
plot_unpredictability(divergence)
divergence$u = divergence$norm
plot_unpredictability(divergence)
divergence$u = divergence$w2d
plot_unpredictability(divergence)

wm <- read_csv("data/world_means.csv") 
wm <- wm %>% mutate(
  color = case_when(
    X1 ==16 ~ "black",
    social ==0 ~ "orange",
    social==1 ~"blue"
  ),
  pch = case_when(
    X1 ==16 ~ 24,
    eco ==0 ~ 22,
    eco==1 ~21
  )
)
#figure 5
plot(wm$P1,wm$P2, pch=wm$pch, bg=wm$color, cex=1.5, bty="n",xlab="",ylab="", xlim=c(-0.5,0.5),ylim=c(-0.5,0.5))
mc <- read_csv("data/morphology_coords.csv") 
for (i in 1:10){
  abline(a=0,b=mc[i,'1']/mc[i,'0'])
}
abline(a=0,b=)
gc <- read_csv("data/ganimal_coords.csv") 
text(x=gc$P1, y=gc$P2, labels=gc$species, cex=0.5)
