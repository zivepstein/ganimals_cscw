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

#inequality
##Gini measure with all ganimals with more than 1 vote (model 2)
inequality_vote1 <- read_csv("data/inequality_model2.csv")
inequality_vote1$social <- 1-inequality_vote1$condition %%2 
inequality_vote1$cloud <- as.numeric(inequality_vote1$condition > 2)
summary(lm("count ~ social+cloud", data = inequality_vote1))
summary(lm("count ~ social", data = inequality_vote1 %>% filter(cloud==0)))


##Gini measure with seed ganimals (model 3)
inequality_seed <- read_csv("data/inequality_model3.csv")
inequality_seed$social <- 1-inequality_seed$condition %%2 
inequality_seed$cloud <- as.numeric(inequality_seed$condition > 2)
summary(lm("count ~ social*cloud", data = inequality_seed))
summary(lm("count ~ social", data = inequality_seed %>% filter(cloud==0)))

##Gini measure with all ganimals with more than 1 vote (model 4)
inequality_seed1 <- read_csv("data/inequality_model4.csv")
inequality_seed1$social <- 1-inequality_seed1$condition %%2 
inequality_seed1$cloud <- as.numeric(inequality_seed1$condition > 2)
summary(lm("count ~ social+cloud", data = inequality_seed1))

summary(lm("count ~ social", data = inequality_seed1 %>% filter(eco==0)))

##Coefficient of variation Mesaure (model 5)
inequality_cov <- read_csv("data/inequality_model5.csv")
inequality_cov$social <- 1-inequality_cov$condition %%2 
inequality_cov$eco <- as.numeric(inequality_cov$condition > 2)
summary(lm("count ~ social+eco", data = inequality_cov))

summary(lm("count ~ social", data = inequality_cov %>% filter(eco==0)))

##Herfindahl Mesaure (model 6)
inequality_herfindah <- read_csv("data/inequality_model6.csv")
inequality_herfindah$social <- 1-inequality_herfindah$condition %%2 
inequality_herfindah$eco <- as.numeric(inequality_herfindah$condition > 2)
summary(lm("count ~ social+eco", data = inequality_herfindah))
summary(lm("count ~ social", data = inequality_herfindah %>% filter(eco==0)))

summary(lm("count ~ social", data = inequality_all %>% filter(eco==0))) #0.004
summary(lm("count ~ social", data = inequality_vote1 %>% filter(eco==0))) #0.000101
summary(lm("count ~ social", data = inequality_seed %>% filter(eco==0)))  #0.308  
summary(lm("count ~ social", data = inequality_seed1 %>% filter(eco==0))) #0.00661
summary(lm("count ~ social", data = inequality_cov %>% filter(eco==0)))#0.000105
summary(lm("count ~ social", data = inequality_herfindah %>% filter(eco==0)))#0.00154


#unpredictability
unpredictability_seed <- read_csv("data/unpredictability_model1.csv") 
unpredictability_seed <-data.frame(rbind(cbind(t(unpredictability_seed[1,2:7]),0,0), cbind(t(unpredictability_seed[2,2:7]),1,0) ,cbind(t(unpredictability_seed[3,2:7]),0,1), cbind(t(unpredictability_seed[4,2:7]),1,1)))
colnames(unpredictability_seed) <- c("u", "social", "eco")
summary(lm("u ~ social+eco", data =unpredictability_seed))

unpredictability_seed1 <- read_csv("data/unpredictability_model2.csv") 
unpredictability_seed1 <-data.frame(rbind(cbind(t(unpredictability_seed1[1,2:7]),0,0), cbind(t(unpredictability_seed1[2,2:7]),1,0) ,cbind(t(unpredictability_seed1[3,2:7]),0,1), cbind(t(unpredictability_seed1[4,2:7]),1,1)))
colnames(unpredictability_seed1) <- c("u", "social", "eco")
summary(lm("u ~ social+eco", data =unpredictability_seed1))

unpredictability_all <- read_csv("data/unpredictability_model3.csv") 
unpredictability_all <-data.frame(rbind(cbind(t(unpredictability_all[1,2:7]),0,0), cbind(t(unpredictability_all[2,2:7]),1,0) ,cbind(t(unpredictability_all[3,2:7]),0,1), cbind(t(unpredictability_all[4,2:7]),1,1)))
colnames(unpredictability_all) <- c("u", "social", "eco")
summary(lm("u ~ social+eco", data =unpredictability_all))
summary(lm("u ~ social", data =unpredictability_all %>% filter(eco==0)))