#Linear regression ----
# the change in mean values according to the explanatory variable
#y= a + bx
#y is the predicted value of the response variable
#a is the regression intercept (the value of y when x = 0)
#b is the slope of the regression line
#x is the value of the explanatory variable

# ANOVA ----
# the difference between means 
# quaintfiy the overall variability and and divide by within and outside the groups 
#'signal-to-noise' ratio'
## one way ANOVA ----
# one explanatory variable #

# ordary least squares ----
# the more varation that can be explained the the model the more confident can be that we have detected a real effect in our estimates of mean differences
#___________________________________________________----



# Packages ----
library(tidyverse)
library(rstatix)
library(performance)
library(rstatix)

#loading in the data ----

janka<- read.csv(here("data", "janka.csv"))

# cleaning the data ----

glimpse(janka) # looking at the data 

# checking data is in a tidy format
head(janka)

# checking variable names
colnames(janka)

# clean up column names 

janka <- janitor::clean_names(janka) # snakecase

# check for duplication
janka %>% 
  duplicated() %>% 
  sum()

# check for typos - by looking at impossible values
janka %>% 
  summarise(min=min(hardness, na.rm=TRUE), # min
            max=max(hardness, na.rm=TRUE)) # max

janka %>% 
  summarise(min=min(dens, na.rm=TRUE), # min
            max=max(dens, na.rm=TRUE)) # max

# check for typos by looking at distinct characters/values

janka %>% 
  distinct(dens)

janka %>% 
  distinct(hardness)

# missing values
janka %>% 
  is.na() %>% # no NAs 
  sum()

# type of data

typeof(dens)

# quick summary

summary(janka)

#___________________________________----

#Lab15----

# Activity 1: Exploratory analysis ----

janka %>% 
  ggplot(aes(x=dens, y=hardness))+ # three different groups? 
  geom_point()
# positively related, wood density and timber hardness 


#Activity 2: Correlation - Generate Pearson's R ----


janka %>% 
  cor_test(dens, hardness)
# postive relationship - cor~0.97 (1 to-1)
# the density of the wood postively increases the hardness of the wood

##Regression ----
# predictor variable is continuous

janka_ls1 <- lm(hardness ~ dens, data = janka) 
# This linear model will estimate a 'line of best fit' using the method of 'least squares'

# hardness - predictor
# dens - response  

# specify linear model method for line fitting

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm") # regression line
#high and low estimates, so the uncertanty becomes higher 

##Summary ----

janka_ls1 %>% 
  broom::tidy()

# Activity 3: Mean centered regression ----

# subtracting the mean from each of the data points 

dens_mean <- janka %>% 
  summarise(mean_dens=mean(dens))
# 45.73333

janka %>% 
  mutate(centered_dens = dens-pull(dens_mean)) %>% 
  lm(hardness ~ centered_dens, data = .) %>% 
  broom::tidy()

# the second row
# Density is our explanatory variable, and the slope is estimated against it
# so timber hardness increases by 57.5 for every unit change in density

confint(janka_ls1) # upper and lower confidance intervals 

#  α= 0.05 we think there is at least a 52.9 unit increase on the janka scale for every unit increase in density (ρ)
# intervals do not span 0 so there is a sig relatioship at 0.05

## Effect size ----

# R2 = the protion of varation in the data which is explianed by the linnear model
summary(janka_ls1)

## Assumptions ----

janka_ls1 %>% 
  broom::augment() %>% # adds informations about observations to a dataset
  head() #  used to display the first n rows present in the input data frame

### Assumptions graph ----

augmented_ls1 <- janka_ls1 %>% 
  broom::augment()

augmented_ls1 %>% 
  ggplot(aes(x=dens, 
             y=.fitted))+
  geom_line()+ 
  geom_point(aes(x=dens, 
                 y=hardness))+
  geom_segment(aes(x=dens, 
                   xend=dens, 
                   y=.fitted, 
                   yend=hardness), # draws a straight line between points (x, y) 
               linetype="dashed", colour="red")

### graph 2 ----

# A line connecting all the data points in order 
p1 <- augmented_ls1 %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_line()+
  ggtitle("Full Data")

# Plotting the fitted values against the independent e.g. our regression line
p2 <- augmented_ls1 %>% 
  ggplot(aes(x=dens, y=.fitted))+
  geom_line()+
  ggtitle("Linear trend")

# Plotting the residuals against the fitted values e.g. remaining variance
p3 <- augmented_ls1 %>% 
  ggplot(aes(x=.fitted, y=.resid))+
  geom_hline(yintercept=0, colour="white", size=5)+
  geom_line()+
  ggtitle("Remaining \npattern")


library(patchwork)
p1+p2+p3

#### function graph 2 ----

model_plot <- function(data=augmented_ls1, 
                       x="dens", 
                       y="hardness", 
                       title="Full data"){
  ggplot(aes(x=.data[[x]], 
             y=.data[[y]]), 
         data=data)+
    geom_line()+
    theme_bw()+
    ggtitle(title)
}

p1 <- model_plot()
p2 <- model_plot(y=".fitted", title="Linear prediction")
p3 <- model_plot(y=".resid", title="Remaining pattern")

#### normal dis ----

performance::check_model(janka_ls1, check=c("normality","qq"))
# some residuals


### Equal variance ----
# 'standardized residuals' - this is the raw residual divided by the standard deviation.
# less confidence in our predictions at high values of density, as there are more residuals 


performance::check_model(janka_ls1, check="homogeneity") # checking the variance 

## Outliers ----

plot(janka_ls1, which=c(4,5))
#  positional order in the dataframe -32

# Prediction ----

coef(janka_ls1) # the coefficients of the intercept and the slope we can make predictions on new data

# a + bx -> equation mannal (coefficients)

-1160.49970 + 57.50667 * 65


coef(janka_ls1)[1] + coef(janka_ls1)[2] * 65 # coefficients using code

predict(janka_ls1, newdata=list(dens=c(22,35,65))) # coefficients via function

## Adding confidence intervals ----
# SD

broom::augment(janka_ls1, newdata = tibble(dens=c(22,35,65)), se=TRUE)


## 95% Confidence Intervals ----

broom::augment(janka_ls1, newdata=tibble(dens=c(22,35,65)), interval="confidence")

## quick predictions for categorical data ----

emmeans::emmeans(janka_ls1, 
                 specs = "dens", 
                 at = list(dens = c(22, 35, 65)))

pred_newdata <- broom::augment(janka_ls1, 
                               newdata=tibble(dens=c(22,35,65)))

# Activity 4: Prediction ----
## graph ----

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_point(data=pred_newdata, aes(y=.fitted, x=dens), colour="red")+
  geom_label(data=pred_newdata, (aes(y=(.fitted+10), x=(dens+3), label=round(.fitted, digits=0))))+
  theme_bw()+
  labs(x="Density", y="Timber Hardness")+
  scale_x_continuous(limits=c(20,80), expand=expansion(add=c(0,5)))

# Summary -----

# where the relationship between the explanatory variable and response variable are modelled with the equation for a straight line

# The intercept is the value of y when x = 0, often this isn't that useful, and we can use 'mean-centered' values if we wish to make the intercept more intuitive

# As with all linear models, regression assumes that the unexplained variability around the regression line, is normally distributed and has constant variance.

# Once the regression has been fitted it is possible to predict values of y from values of x, the uncertainty around these predictions can be captured with confidence intervals.

#_______________________________________________________________-----

# Lab 16 ----

















