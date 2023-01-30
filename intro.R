# 📦 PACKAGES ----
library(tidyverse)
library(here)
library(kableExtra)

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
 