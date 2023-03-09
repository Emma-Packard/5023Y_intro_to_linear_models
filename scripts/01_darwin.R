# 📦 PACKAGES ----
library(tidyverse)
library(here)
library(kableExtra)

library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
library(broom.helpers)
#library(broom.helper)

darwin <- read.csv(here("data", "darwin.csv"))

# checking the structure of the data 
glimpse(darwin)

# checking data is in a tidy format
head(darwin)

# checking varible names
colnames(darwin)

# clean up column names 

darwin <- janitor::clean_names(darwin)

# check for duplication
darwin %>% 
  duplicated() %>% 
  sum()

# check for typos - by looking at impossible values
darwin %>% 
summarise(min=min(height, na.rm=TRUE), # min
          max=max(height, na.rm=TRUE)) # max

# check for typos by looking at distinct characters/values

darwin %>% 
  distinct(pair)

darwin %>% 
  distinct(type)

# missing values
darwin %>% 
  is.na() %>% 
  sum()

# quick summary

summary(darwin)

# data visualisation ----

darwin %>% 
  ggplot(aes(x=type,
             y=height))+
  geom_point() # scatter

darwin %>% 
  ggplot(aes(x=type,
             y=height))+
  geom_violin() # volin

# you could also substitute (or combine) other geoms including
# geom_boxplot()
# geom_violin()
# geom_histogram()
# Why not have a go and see what you can make?

#  comparing groups ----
darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height)) # standard devation 

# make a new object
darwin_summary <-darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

# make a summary plot
darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()


# use kable extra functions to make a nice table (could be replaced with kable() if needed)
darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

# height of the plant as a proxy for fitness and whether there is a difference between the mean heights of the plants

# Our goal is to:
  
# Estimate the mean heights of the plants in these two groups

# Estimate the mean difference in heights between these two groups

# Quantify our confidence in these difference



# pivot_wider ----
# pivot data to wide format then subtract Selfed plant heights from Crossed plant heights

darwin_wide <- darwin %>% # what the new data will be called
  pivot_wider(names_from = type, values_from = height) %>% # where the values are going to
  mutate(difference = Cross - Self) # cross takeaways self

# can use ^ to cal the mean difference between paired plants 
# + the amount of variance (as standard deviation)
# cal the average height dif and the Sd of the difference ----

difference_summary <- darwin_wide %>% # using the wide data
  summarise(mean=mean(difference),
            sd=sd(difference), # sd means sd 
            n=n()) # making a new varible that mean n and it equals to n

difference_summary # saved as 


# standard error ----
difference_summary %>% 
  mutate(se = sd/sqrt(n)) 
# need a measure of uncertainty, example standard error 

# the average difference in height was 2.62 ± 1.22 inches (mean ± SE).

# Sd example using norm ----
#Create a sequence of 100 equally spaced numbers between -4 and 4
x <- seq(-4, 4, length=100)

#create a vector of values that shows the height of the probability distribution
#for each value in x
y <- dnorm(x) # norm meaning normal dis

#plot x and y as a scatterplot with connected lines (type = "l") and add
#an x-axis with custom labels
plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))

# confidance interval range ----

lowerCI <- 2.62-(2*1.22)

upperCI <- 2.62+(2*1.22)

lowerCI
upperCI

# CI means -> would capture the true mean in 95% of experiments

#________________________________-----
# Lab_13_lm ----

lsmodel0 <- lm(formula = height ~ 1, data = darwin)
# simple models ignores explanatory values, 1= the intercept
# without the explanatory values the formula will estimate the mean of the whole data set

# Summaries for models ----
# linear model, summary of model summary()

# Broom 🧹 ----
# summary tool of 🔑 information in tibble

# broom::tidy() > summarizes info about model components
# broom::glance() > report info about whole model
# broom::augment() > adds info individual obs to a data set/ 
# it can be used to model predictions into a new data 

# Model summary ----
summary(lsmodel0) # text form of coefficients
broom::tidy(lsmodel0) # makes the table of coefficients 

mean(darwin$height)


# Compare means ----

lsmodel1 <- lm(height ~ type, data=darwin)# difference in the average plant height, as a result of pollination type

# note that the following is identical

# lsmodel1 <- lm(height ~ 1 + type, data=darwin)

broom::tidy(lsmodel1)# prints in a table 

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height))
# showing that the means haven't changed the question has

summary(lsmodel1)

# superimpose means to the plot 

darwin %>% 
  ggplot(aes(x=type, 
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()

# SE of difference (SED)----
# SE of values (estimates)
# our module is the pooled approach> assumes variance is roughly the same

#Confidence intervals ----
confint(lsmodel1) # base r
broom::tidy(lsmodel1, conf.int=T) # table 
# first row Cl
# second row interval 

# Answering the question ----

# Darwin's original hypothesis was that self-pollination would reduce fitness (using height as a proxy for this)
# reject the null hypothesis -> a difference of 0  - lies outside the 95% CL
# for the mean 
# CI contain difference or no difference - cannot see the difference 

GGally::ggcoef_model(lsmodel1, # function makes graph estimated mean difference approx 95^ Cl
                     show_p_values=FALSE, 
                     conf.level=0.95)

broom::tidy(lsmodel1, conf.int=T, conf.level=0.99)

# Getting the other treatment mean and standard error ----
# cal the other mean and SE
# self treament is now the mean



darwin %>% 
  mutate(type=factor(type)) %>% 
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>% 
  lm(height~type, data=.) %>% 
  broom::tidy()


# Emmeans 

#this packages does a sim thing to ^

means <- emmeans::emmeans(lsmodel1, specs = ~ type)

means

# provides > mean, SD, 95% CI

means %>% 
  as_tibble() %>% 
  ggplot(aes(x=type, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL))

# Assumption checking ----

# residual/unexplained variance in our data is approximately norm dis
# residual/unexplained variance is approx equal between our groups 

# def >
# residuals differences between obs values/fitted values produces by the model

performance::check_model(lsmodel1) # base r > showing residuals
plot(lsmodel1)

#Normal distribution 

performance::check_model(lsmodel1, check=c("normality","qq"))
plot(lsmodel1, which=c(2,2))

# QQ plots -----
# checks the sample dis is the same as another (or theoretical dis)
# data on y-axis 
# theoretical norm dis on x-axis 
# if follows a norm dis they meet to produce perfect diagonal line 


# Equal variance -----

performance::check_model(lsmodel1, check="homogeneity")
plot(lsmodel1, which=c(1,3))

# Outliers 

performance::check_model(lsmodel1, check="outliers")
plot(lsmodel1, which=c(4,4))

# Summary ----

darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-2.617), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)

#___________________________________----

#Lab_14-----
# students tests ----
# small sized version of norm dis
# one t test, mean of sample/compares null hypothersis 
# two t test compares difference between means of 2 samples against null

x <- seq(-4, 4, length=100)
z_dist <- dnorm(x)

values <- tibble(x,z_dist)

# map_dfc combines values returned into a dataframe

# base r version 
x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 30)# defining degf
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)

df <- c(1:30) # defining df
#_____________________________________________________----
#tidyverse version, more comp

x <- seq(-4, 4, length=100)
z_dist <- dnorm(x)

values <- tibble(x,z_dist)

# map_dfc combines values returned into a dataframe
t <- map_dfc(degf, ~dt(x, .x))
colnames(t) <- degf

combined <- cbind(values,t)

combined %>% 
  pivot_longer(cols=!x, names_to="distribution") %>% 
  mutate(distribution=factor(distribution, levels=c("z_dist", "1", "3", "8", "30"))) %>%  
  mutate(distribution=fct_recode(distribution, "z distribution" = "z_dist", "df = 1" = "1", "df = 3" = "3", "df = 8" = "8", "df = 30" = "30")) %>% 
  ggplot(aes(x=x, y=value, colour=distribution))+
  geom_line(linetype="dashed")+
  theme_classic()

#_____________________________________________________________----
df <- c(1:30)
# map_dbl forces returned values to be a single vector of numbers (rather than a list)
critical_t <- map_dbl(df, ~qt(p=0.05/2, df=.x, lower.tail=FALSE))

tibble(df,critical_t) %>% 
  ggplot(aes(x=df, y=critical_t))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept=1.96), linetype="dashed", colour="red")+
  labs(x= "Degrees of Freedom",
       y= expression(paste("Critical value of ", italic("t"))))


lsmodel1 <- lm(height ~ type, data = darwin) # the two varibles, height and type
summary(lsmodel1) 


tidy_model1 <- broom::tidy(lsmodel1)# table of info

tidy_model1[[2,2]] / tidy_model1[[2,3]]# specific rows in the table 



#____________________________----

#Paired t ----
# multiple means, for a specific reason, say at this time is is different 


lsmodel_darwin <- lm(height ~ type + factor(pair), data = darwin) # factor = multiple means and then find the mean of the whole thing
summary(lsmodel_darwin) # print

#confidence intervals 
# in this case we have lowered the uncertainty by adding paired parameter
lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% # makes a table of info
  slice(1:2) # just show first two rows


m1 <- lm(height ~ type, data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="unpaired")

m2 <- lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="paired")

rbind(m1,m2) %>% 
  ggplot(aes(model, estimate))+ 
  geom_pointrange(aes(ymin=conf.high, ymax=conf.low))+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme_minimal()+
  coord_flip() # flip the cords 

#_____________________----

#Type 1 (rejecting the null hyp) and Type 2 errors (rejecting the alternative) -----

#β = statistical power

#____________________----

# Repeatably 
# to avoid errors 1/2

set.seed(1234)

myList <- vector("list", 20)
y <- tibble()

for (i in 1:length(myList)) { 
  
  x <-  rnorm(n=12, mean=2.6, sd=2.83)
  data <- tibble(x)
  temp <- lm(x~1, data=data) %>% 
    broom::tidy(conf.int=T) 
  y <- rbind(y,temp)  
  
}

y$`experiment number` <- rep(1:20)

# the new dataframe y contains the results of 20 new experiments

# Activity 1: Experimental Repeatabitliy 

y %>% 
  mutate(`p value < 0.05` = if_else(p.value > 0.049, "non-significant", "significant")) %>% # the threshold
  group_by(`p value < 0.05`) %>% 
  summarise(`number of experiments`=n())


# comparing the estimates and the confidence intervals 
y %>% 
  ggplot(aes(x=`experiment number`, y=estimate))+
  geom_pointrange(aes(ymin = conf.low, ymax=conf.high))+
  labs(y = "Estimated mean effect of outcrossing")+
  geom_hline(linetype="dashed", yintercept=0.05)+
  theme_minimal()

#____________________________________----

# Lab 16 ----

# one-way ANOVA ----

anova(lsmodel1)

pf(5.9395, 1, 28, lower.tail=FALSE)# f value 

# The self pollinated maize plants measured an average of 17.6 [16-19.1] (mean[95% CI]) inches high, while the cross-pollinated plants had a mean height of 20.2 [18.6-21.7] inches - a difference of 2.6 [-0.4-4.8] inches (one-way ANOVA: F1,28 = 5.9, P = 0.02).

#Two-way ANOVA ----
# two varibles of interest 
lsmodel2 <- lm(height ~ type + as.factor(pair), data = darwin)
anova(lsmodel2)
