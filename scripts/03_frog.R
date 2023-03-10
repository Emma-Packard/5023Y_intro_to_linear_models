
#___________________________----
# Setting up ----

## An analysis of the development time of frogspawn in response to water temperature ----


#___________________________----

# 📦 PACKAGES ----

library(tidyverse)

#___________________________----

# IMPORTING DATA ----

frogs <- read_csv("data/frogs_messy_data.csv")

#___________________________----

# TIDY_DATA ----

glimpse(frogs)

frogs <- frogs %>% 
  rename("13" = Temperature13,
         "18" = Temperature18,
         "25" = Temperature25,
         frogspawn_id = `Frogspawn sample id`) %>% 
  pivot_longer(`13`:`25`, names_to="temperature", values_to="days") %>% 
  drop_na(days)

#___________________________----

lsmodel_frogs1 <- lm(days ~ temperature, data = frogs)

# summary -----

summary(lsmodel_frogs1)

anova(lsmodel_frogs1)

broom::tidy(lsmodel_frogs1, conf.int = T)

#___________________________----

# check assumptions 

performance::check_model(lsmodel_frogs1,
                         check = c("qq", "outliers", "homogeneity"))

lsmodel_frogs1 <- lm(days ~ temperature, data = frogs)

frog_col <- c("#57CC99", "#80ED99", "#38A3A5")

lsmodel_frogs1 %>%
  ggplot(aes(x = temperature,
             y = days,
             fill = temperature,
             colour= temperature)) +
  geom_violin(alpha = 0.2)+
  geom_boxplot(width = 0.2,
               alpha = 0.6)+
  scale_fill_manual(values = frog_col)+
  scale_colour_manual(values = frog_col) +
  theme_classic()+
    labs(
    x = "Temperature (°C)",
    y = "Amount of days taken to hatch",
    title = "",
    subtitle = "",
    legend = "Temperature") 
  

p + guides(fill=guide_legend(title="Temperature"))


# Figure 10.4: Time to hatching is inversely related to temperature in frogspawn. Circles represent estimated mean hatching times with 95% confidence intervals from a one-way ANOVA (F1,28 = 385.9, P < 0.001). Dashed lines indicate the slope of the mean difference between 13-18 degrees and 13-25 degrees Celsius. Faded points represent individual data points.





 