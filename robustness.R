library(tidyverse)
library(fixest)

datS2 <- readRDS(file = "C:/Users/wb481167/OneDrive - WBG/Charldocs/Research/Pollution/Data/Stata/Forrobust.rds")

datS2 %>% summarize(minPol = min(min_pm25),
                    maxPol = max(max_pm25),
                    meanPol = mean(mean_pm25),
                    minSales = min(VentasanualesenUF), # Sales (annual)
                    maxSales = max(VentasanualesenUF),
                    meanSales = mean(VentasanualesenUF),
                    minWorkers = min(Trabajadoresponderadospormese), # Employees (monthly weighted averages)
                    maxWorkers = max(Trabajadoresponderadospormese),
                    meanWorkers = mean(Trabajadoresponderadospormese),
                    minWage = min(Rentanetainformada), # Wages
                    maxWage = max(Rentanetainformada),
                    meanWage = mean(Rentanetainformada),
                    Numbers = n())

NROW(unique(datS2$Númerodeempresas)) # Number of firm

datS2$Rentanetainformada[datS2$Rentanetainformada==0]<-0.0001
datS2$lnRWage <- log(datS2$Rentanetainformada/datS2$CPI) # Real wage
datS2$lnEMP <- log(datS2$Trabajadoresponderadospormese)
datS2$lnsales <- log(datS2$sales)


datS3 <- datS2 %>% group_by(id) %>% mutate(lagyl = lag(yl, order_by=id),
                                           leadyl = lead(yl, order_by=id))
datS3$build <- factor(datS3$dummy)



# ==================================
# ROBUSTNESS CHECKS
# ==================================

# PROPENSITY SCORE: CONTROL FOR RW FAR AND NEAR POWER STATIONS
    # https://rstudio-pubs-static.s3.amazonaws.com/284461_5fabe52157594320921fc9e4d539ebc2.html

datS3$DistDum <- case_when(datS3$min <= 2.5 ~ 1, # 2.5 is one standard deviation sd(datS3$min)
                           datS3$min > 2.5 ~ 0)

datS3$DistDum <- factor(datS3$DistDum)
datS3$dummy <- factor(datS3$dummy)

 # Standardize data

datS3 <- datS3 %>%
  mutate(lnRWageDM = (lnRWage - mean(lnRWage)) / sd(lnRWage),
         lpmDM = (lpm - mean(lpm)) / sd(lpm),
         ylDM = (yl - mean(yl)) / sd(yl)) 



datS3 %>% select(id,DistDum,lnRWageDM,lpmDM,ylDM) %>%
  group_by(DistDum) %>%
  summarise(mean_wage = mean(lnRWageDM,na.rm=TRUE),
            mean_pollution = mean(lpmDM,na.rm=TRUE),
            mean_yl = mean(ylDM,na.rm=TRUE))

  # Check whether means are statistically significant
with(datS3, t.test(lnRWageDM ~ DistDum)) # If P-Values < 0.001 then means are not statistically different
with(datS3, t.test(lpmDM ~ DistDum)) # If P-Values < 0.001 then means are not statistically different
with(datS3, t.test(ylDM ~ DistDum)) # If P-Values < 0.001 then means are not statistically different

# -----------------------------------
# Method using Logit

simp1 <-  glm(DistDum~lnRWage,family = binomial(), data = datS3)
summary(simp1)

# Now we can derive the propensity score: It is simply the probability of being treated given the estimates from the logit.
prs_df <- data.frame(pr_score = predict(simp1, type = "response"),DistDum = simp1$model$DistDum)
head(prs_df)

labs <- paste("Distance from power station:", c("Close to power station", "Far from power station"))
prs_df %>%
  mutate(DistDum = ifelse(DistDum == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~DistDum) +
  xlab("Probability of being close to a power station") +
  theme_bw()

# -----------------------------------
# Emthod using MATCHIT - note no missing values allowed

library(MatchIt)
#library(cobalt) 
#library(patchwork)

datS3_nomiss <- datS3 %>%  # MatchIt does not allow missing values
  select(id,city,year,dummy,DistDum,sector,lnRWageDM,lpmDM,ylDM) %>%
  na.omit()

set.seed(100)
m.out = matchit(dummy ~ lnRWageDM+lpmDM+ylDM+factor(sector)+factor(year), data = datS3_nomiss, method = "nearest", ratio = 1,replace=TRUE,caliper = .10)
summary(m.out) # Use for detecting how successful the math was
plot(m.out)
#bal.plot(m.out,var.name = "distance", which = "both")
#plot(m.out, type = "density", interactive = FALSE,which.xs = c("lnRWageDM", "lpmDM", "ylDM"))
#pdf(file = "my_plot.pdf", onefile = FALSE, width = 15, height = 7.5)
#my_plot <- plot(summary(m.out1))
#dev.off()

# Data frame with only the matched units [treated = close to power station; control is far from power station]: 
# If also tells us how many observations are discarded

dta_m <- match.data(m.out) 
dim(dta_m) # New variable distance is the propensity score
plot(summary(m.out), abs = FALSE) # See if model is balanced. Before matching should see large differences; after matching should see less

#love.plot(bal.tab(m.out,m.threshold=0.1),stat="mean.diffs",grid=TRUE,stars="raw",abs=F)

cj <- data.frame(Distance = c(0.9463,0),
                 Wages = c(0.0597,0.1), # -0.0199
                 Pollution = c(-0.1216,-0.043), # 0.0039
                 Productivity = c(-0.0708,0.0186), # 0.0233
                 Experiment = c("All","Macthed")) # Well matched if SMD < 0.1

cj1 <- cj %>% pivot_longer(!Experiment,names_to="Desc",values_to="vals")

pdf(file = "C:/Users/wb481167/OneDrive - WBG/Charldocs/Research/Pollution/Paper/bal_plot.pdf", onefile = FALSE, width = 15, height = 7.5)
ggplot(cj1,aes(y=Desc,x=vals,color=Experiment))+geom_point(size = 3)+theme_bw()+labs(title="",x="Standardized mean differences",y="")+
  geom_vline(xintercept = 0.1, linetype="dotted",color = "black", size=0.5)+
  geom_vline(xintercept = -0.1, linetype="dotted",color = "black", size=0.5)+
  geom_vline(xintercept = 0,color = "black", size=1)
dev.off()



# ==================================
# INSTRUMENT VALIDITY
# ==================================
# PROBABILITY OF BUILDING A COAL POWER PLANT INDEPENDANT OF LAGGED Y/L


datRob <- dta_m %>% select(id,city,year,sector,lnRWageDM,lpmDM,weights) # ,subclass
datSmall <- datS3 %>% select(id,city,year,sector,DistDum,lagyl,yl,leadyl,build,lpm,prov,Númerodeempresas)
datRob <- datRob %>% left_join(datSmall)

mylogit1 <- glm(dummy ~ lagyl+lnRWage+I(year)+I(sector4)+I(prov), data = datS3,family=binomial(logit))
mylogit1 <- glm(build ~ lagyl+I(year)+I(sector)+I(prov), data = datRob,family=binomial(logit))
summary(mylogit1) 
nobs(mylogit1)

coef(summary(mylogit1))["lagyl","Estimate"]
coef(summary(mylogit1))["lagyl","Std. Error"]

mylogit2 <- glm(build ~ yl+I(year)+I(sector)+I(prov), data = datRob,family=binomial(logit)) # I(year)+I(sector4)+I(prov)
summary(mylogit2)

coef(summary(mylogit2))["yl","Estimate"]
coef(summary(mylogit2))["yl","Std. Error"]

#mylogit3 <- glm(build ~ leadyl, data = datRob,family=binomial(logit))
mylogit3 <- glm(dummy ~ leadyl+lnRWage+I(year)+I(sector4)+I(prov), data = datS3,family=binomial(logit))
mylogit3 <- glm(build ~ leadyl+I(year)+I(sector)+I(prov), data = datRob,family=binomial(logit))
summary(mylogit3)
coef(summary(mylogit3))["leadyl","Estimate"]
coef(summary(mylogit3))["leadyl","Std. Error"]
nobs(mylogit3)

# -----------------------------------
# -----------------------------------
# -----------------------------------
library(lmtest)
library(sandwich)

# Now  we are ready to run difference in difference
res <- lm(lnRWageDM ~ lpmDM:build+I(year)+I(sector)+I(prov), data = datRob)
coeftest(res, vcov = vcovHC(res, type = "HC0"))
coefci(res, vcov. = vcovHC, level = 0.95)
coeftest(res, vcov. = vcovCL, cluster = ~subclass) # For clustered se

coefci(res, vcov. = vcovCL, cluster = ~subclass, level = 0.95)

#Test the coefficient using cluster robust standard error
res3 <- lm(lnRWageDM ~ lpmDM:dummy+I(year)+I(sector)+I(city), data = dta_m, weights = weights)
coeftest(res3, vcov. = vcovCL, cluster = ~subclass)
#Calculate the confidence intervals based on cluster robust standard error
coefci(res3, vcov. = vcovCL, cluster = ~subclass, level = 0.95)

res1 <- lm(Númerodeempresas ~ lpmDM:build+I(year)+I(sector)+I(prov), data = datRob)
coeftest(res1, vcov = vcovHC(res1, type = "HC0"))
coefci(res1, vcov. = vcovHC, level = 0.95)
coeftest(res1, vcov. = vcovCL, cluster = ~subclass) # For clustered se
coefci(res1, vcov. = vcovCL, cluster = ~subclass, level = 0.95)

#ROBUST CHECK 2: NUMBER OF FIRMS: H(0): coal fired power station has no impact on changing the number of firms