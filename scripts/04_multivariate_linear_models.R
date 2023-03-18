#___________________----
#Setting up ----

## the response of above ground plant biomass production of grassland plots in relation to two resource addition treatment

#___________________----
#importing the data 

biomass <- read_csv("data/biomass.csv")

#___________________----

# check the structure of the data
glimpse(biomass)

# check data is in a tidy format
head(biomass)

# check variable names
colnames(biomass)

# check for duplication
biomass %>% 
  duplicated() %>% 
  sum()

# check for typos - by looking at impossible values
biomass %>% 
  summarise(min=min(Biomass.m2, na.rm=TRUE), # min and max
            max=max(Biomass.m2, na.rm=TRUE))

# check for typos by looking at distinct characters/values
biomass %>% 
  distinct(Fert)

biomass %>% 
  distinct(Light)

biomass %>% 
  distinct(FL)

# missing values
biomass %>% 
  is.na() %>% 
  sum()

# quick summary

summary(biomass)

# one-way ANOVA ---- 

ls_1 <- lm(Biomass.m2 ~ FL, data = biomass)
 
summary(ls_1)



GGally::ggcoef_model(ls_1,
                     show_p_values=FALSE,
                     signif_stars = FALSE,
                     conf.level=0.95) # the confidence interval include 0 meaning that we cant properly say light make the biomass higher 

# combine the average mean differences of the light effect and fertiliser effect
coef(ls_1)[2] + coef(ls_1)[3] 
# if there was no interaction they the values would add up to the combined effect

# compare this to the average difference of the combined treatment
coef(ls_1)[4]
# however they do not add up to the same value, shows that there could be a great effect if the 2 values are together 

# testing the interactions ----
# adding or sub different effect should change the slope of the graph as they have different effects 

# plot of light and no light -----
biomass %>% ggplot(aes(x=Fert, y=Biomass.m2, colour = Light, fill = Light, group = Light))+
  geom_jitter(width=0.1) +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 3,
    shape = 23
  )+stat_summary(
    geom = "line",
    fun = "mean",
    size = 1, linetype = "dashed"
  )


# interaction we add this to the moddle ----


ls_2 <- lm(Biomass.m2 ~ Fert + # main effect
             Light + # main effect
             Fert:Light, # interaction term ----
           data = biomass)

summary(ls_2)
# the fourth line is the interaction effect, the sd is bigger, the estimate is the interaction effect 

# ls_2 graph -----
GGally::ggcoef_model(ls_2,
                     show_p_values=FALSE,
                     signif_stars = FALSE,
                     conf.level=0.95)
# light and fert interaction effect is 95, ^ interaction effect if not it would be 0 -in order to work out the estimated biomass for both we need to sum additve effects (light + fert) and and the interaction effect (light:fert)


# comparing model 1/2 ----
# when comparing to the ANOVA we must add the single terms and the interaction term - this then should add up to the combined term of for the first model


# model 1
coef(ls_1)[4]

# model 2
coef(ls_2)[2] + coef(ls_2)[3] + coef(ls_2)[4] # single terms and interaction terms 
# add up to the same 


# ANOVA tables ----



## interaction effect ----
# interaction between two categorical variables ----
drop1(ls_2, test = "F") # testing the f value with and without the interaction effect
# shows that there is an interaction effect

# There was an interactive effect of light and fertiliser treatments (ANOVA F1,60 = 4.25, P = 0.044) in which combining treatments produced substantially more biomass (95.4g [95% CI: 2.8 - 188]) than expected from the additive effects alone (Fertiliser 93.7g [28.2 - 159.2], Light 30.1g [-35.3 - 95.6]).

# make sure to include the (estimate) and the uncertainty (confidence intervals)

## unbalanced ----

# we have to remove the interaction term before we can keep using drop1()

ls_3 <- lm(Biomass.m2 ~ Fert + Light, data = biomass)

drop1(ls_3, test = "F")

# can use this module to get the f stats but the estimates must come from the full module

# order matters 

## deliberately unbalanced dataset ----


# make three vectors and combine them into a new tibble

height <- c(50,57,91,94,102,110,57,71,85,105,120)
size <- c(rep("small", 2), rep("large", 4), rep("small", 3), rep("large", 2))
treatment <- c(rep("Control", 6), rep("Removal", 5))

unbalanced <- tibble(height, size, treatment)

unbalanced

# Activity 1: Sums of Squares ----
# changing order changes the way sums of squares is cal

model_01 <- lm(height ~ treatment + size, data = unbalanced) 
anova(model_01)

model_2 <- lm(height ~ size + treatment, data = unbalanced)
anova(model_2)

#comparing the models
drop1(model_01)
drop1(model_2)
# this function does not depend on order 

# post hoc ----
# comparing means - not nesscary in this are the most important is the interaction effect
emmeans::emmeans(ls_2, specs = pairwise ~ Light + Fert + Light:Fert) %>% 
  confint()
# including the argument pairwise in front of the ~ prompts the post-hoc pairwise comparisons.
# $emmeans contains the estimate mean values for each possible combination (with confidence intervals)
# $ contrasts contains tukey test post hoc comparisons between levels

#_________________________________________________________----
# ACNOVA ----
## between a factor and a continuous variable ----

# The experiment aimed to see how the yields soya bean (William variety), were affected by stress and Ozone levels.









