### R workshop                             ###
### Applied statistics 2                   ###
### Linear Mixed effect models - LMMs 2026 ###

# Calling the needed packages

library (stats)
library (psych)
library (dplyr)
library (ggplot2)
library (lme4) # this one is the basic for LMMs
library(lmerTest) # for calculation of p values
library(gridExtra) # for managing graphs
library(languageR) 
library(lattice) # for graphs
library(MuMIn) # for R2 for LMMs
# importing data
library(readr)


# Information about working directory

getwd()

# read data file, giving the object name - "dat"

dat <- read_delim("lmm_data.csv", delim = ";")

# Verify data import

View(dat)

# checking the matrix dimensions and the structure of the data

dim(dat)
str(dat)

# telling the R that Subject_ID, Trial_ID, NoS_cat, Frequency_cat and Lexicality
# should be treated as factors (categorical variables)

dat$Subject_ID <- as.factor (dat$Subject_ID)
dat$Trial_ID <- as.factor (dat$Trial_ID)
dat$NoS_cat <- as.factor (dat$NoS_cat)
dat$Frequency_cat<- as.factor (dat$Frequency_cat)
dat$Lexicality <- as.factor (dat$Lexicality)

str(dat)

# We have the lexical decision latencies, high and low frequency words, and words that
# have few, medium and many number of senses
# This matrix contains only RTs for words (pseudo words are excluded)

# Checking the distribution of DV - RT

qqnorm(dat$RT)

# Standard procedure - Transforming RT via log transformation
# But in demonstration, we will use regular RTs

dat$RT_log <- log(dat$RT)

# And check the distribution

qqnorm(dat$RT_log)

#Checking the distribution of by subject RTs


ggplot(dat, aes(sample = RT)) +
  stat_qq(alpha = 0.4, size = 0.5, color = "darkblue") + 
  # Referent line
  stat_qq_line(color = "red") + 
  # by Subject
  facet_wrap(~ Subject_ID) + 
  theme_bw() + 
  labs(title = "QQ-plot",
       x = "qqnorm",
       y = "RT") +
  theme(strip.text = element_text(size = 7))

#version 2

ggplot(dat, aes(sample = RT)) +
  stat_qq(alpha = 0.5, size = 0.5, color = "black") + 
  facet_wrap(~ Subject_ID) + 
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    strip.background = element_blank(), 
    strip.text = element_text(size = 8) 
  ) +
  labs(x = "qqnorm",
       y = "RT")

# Log-transforming word frequency due to its logarithmic relationship with RT
# (Recall the linearity assumption)
# Linearity will be formally tested later

dat$logF= log(dat$Frequency)

qqnorm(dat$Frequency)
qqnorm(dat$logF)

# Additionally, continuous predictors should be mean-centered
# to ensure the intercept is meaningful
# Furthermore, it is even better to normalize (standardize) the values:

# Normalization (centering + scaling)

dat$log_F_scaled <- scale(dat$logF)
dat$NoS_scaled <- scale(dat$NoS)

# Checking the distributions

qqnorm(dat$log_F_scaled)
qqnorm(dat$NoS_scaled)

# Demonstrating the RT and Frequency relationship - for all the data

# Graph with one regresion line

ggplot(dat, aes(x = log_F_scaled, y = RT)) +
  geom_point(alpha = 0.3) +             
  geom_smooth(method = "lm", col = "red") + 
  theme_minimal() +
  labs(title = "Ordinal LM",
       x = "logFscale",
       y = "RT")

LM1<-lm(RT ~ log_F_scaled, data=dat)
summary(LM1)


# Building a mixed-effects model
# We start with a random intercept for subjects because, even without 
# prior knowledge of the design, we know that participants inherently 
# differ from one another in most characteristics


ran_mod1 <- lmer( RT ~ 1 + (1|Subject_ID), data = dat) #intercept only model
# ran_mod1<- lmer( RT ~ (1|Subject_ID), data = dat) #different way of annotation for IOM


# Besides expecting subjects to differ in speed, 
# we also expect that reaction times will not be the same for all words.

# We build a model informed by the expectation of a different intercept for 
# each word; i.e., we expect words to differ from one another in 
# the speed of the response they elicit.

# Since we haven't presented all the words in the Serbian language and, 
# furthermore, we want to generalize our findings to the entire population 
# of words in our language, we cannot treat words as fixed effects, 
# but rather as random effects.

ran_mod2 <- lmer( RT ~ 1 + (1|Trial_ID), data = dat)

# We can build a model that simultaneously accounts for
# both differences between subjects and differences between words.

ran_mod3 <- lmer( RT ~ 1 + (1|Subject_ID) + (1|Trial_ID), data = dat)

# We can also check whether including each of 
# these two random effects is justified.
# Remember – this is a "data-driven" approach.

# Compare the model containing only subjects with 
# the model containing both subjects and stimuli
# to check if including stimuli as a random effect is justified.

anova(ran_mod1, ran_mod3)

# It is.

# Compare the model containing only stimuli with 
# the model containing both subjects and stimuli
# to check if including subjects as a random effect is justified.

anova(ran_mod2, ran_mod3)

# It is.

# Therefore, the model with both sources of random effects is justified 
# by both the design (two sources of measurement dependency: subjects and words) 
# and the data.



# HOW DO WE ADD FIXED EFFECTS?

# In the same way as in ordinary linear models

###################################################
####                                           ####
#### LMMs with ONE continuous predictor (logF) ####
####                                           ####
###################################################

# Adding the predictor in LLMs is the same as in OLR, with the addition of random effects
# So, after modeling random intercepts, we add a continuous predictor

model1 <- lmer( RT ~ log_F_scaled + (1|Subject_ID) + (1|Trial_ID), data = dat)

# Does adding frequency as a fixed effect improve the model's 
# fit to the data? Is it justified by the data, or does it unnecessarily 
# complicate the model?
# Uncontrolled addition of predictors can lead to so-called "over fitting."

anova(ran_mod3, model1)

# Adding frequency improves fit of the model
# Now we can see the summary

summary(model1)

# If we want to see R2, we use the function from MuMin package
# Marginal - variance explained by frequency, Conditional- variance explained by
# fixed and random effects

r.squaredGLMM(model1)


# Visualisation of Model 1

# 1. Extracting coefficients for each participant
# Since it is a Random Intercept-only model, the slope (logF) will be the same for everyone


df_coefs <- coef(model1)$Subject_ID

df_coefs$Subject_ID <- rownames(df_coefs)
colnames(df_coefs) <- c("Intercept", "Slope", "Subject_ID")

ggplot(dat, aes(x = logF, y = RT)) +
  # Optionally - adding the raw data as gray dots 
  geom_point(alpha = 0.1, size = 0.8) +
  # Regression lines for each participant
  geom_abline(data = df_coefs, 
              aes(intercept = Intercept, slope = Slope, color = Subject_ID), 
              linewidth=1, alpha = 0.9) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_text(size = 16, face = "bold"), 
        axis.text = element_text(size = 14),                
        plot.title = element_text(size = 18, face = "bold") 
  ) +
  labs(title = "Random Intercept Model: Linear Fit",
       x = "logF",
       y = "RT") 


# 2. Extracting coefficients for each trial
# Since it is a Random Intercept-only model, the slope (logF) will be the same for each tiral df_coefs1 <- coef(model1)$Trial_ID

df_coefs_trials <- coef(model1)$Trial_ID
df_coefs_trials$Trial_ID <- rownames(df_coefs_trials)
colnames(df_coefs_trials) <- c("Intercept", "Slope", "Word")

ggplot(dat, aes(x = logF, y = RT)) +
  geom_point(alpha = 0.1, size = 0.7, color = "gray") + 
  geom_abline(data = df_coefs_trials, 
              aes(intercept = Intercept, slope = Slope, color = Word), 
              linewidth = 0.5, alpha = 0.7) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14)
  ) +
  labs(title = "Random Intercept Model: Item Variability",
       x = "logF",
       y = "RT") 


# Modelling random slopes

# Now we want to check wether our participants react differently on word frequency

model2 <- lmer(RT ~ log_F_scaled + (1 + log_F_scaled | Subject_ID) + (1 | Trial_ID), data = dat)


summary(model2)

anova (model2, model1)

# Because we obtained the message that boudary is singular
# We specified a model with unlinked random effects, where the random intercept
# and the random slope were constrained to be uncorrelated to prevent overparameterization.

model3 <- lmer(RT ~ log_F_scaled + (1 | Subject_ID) + (0 + log_F_scaled | Subject_ID) + (1 | Trial_ID), data = dat)

# or shorter

# model3 <- lmer(RT ~ log_F_scaled + (1 + log_F_scaled || Subject_ID) + (1 | Trial_ID), data = dat)


summary(model3)

# we can see that the random slope is unnecessary
# and that the simpler random structure with random intercepts is sufficient

anova(model3, model1)

# Now we can add another continuous predictor and so on...

model4 <- lmer(RT ~ log_F_scaled + NoS_scaled + (1 | Subject_ID) + (1 | Trial_ID), data = dat)


summary(model4)

anova(model4, model1)

qqnorm(residuals(model4))

# Checking the linear model assumptions:

# 1. Making the column with the predicted values of RT

dat$RT.fitted = predict(model4)

# percent of explained variance

cor(dat$RT, dat$RT.fitted)^2

# or 

r.squaredGLMM(model4)

# 2. Making the column with the residuals of model4:

dat$RT.res = residuals(model4)

# Plotting the correlation between fitted values and residuals
# To check for homoscedasticity (homogeneity of variance)
# This should ideally look like a well-distributed "egg-shaped" cloud

plot(predict(model4),residuals(model4), xlab="residuals",ylab="fitted values",
     abline(0,0))
# ggplot

ggplot(dat, aes(x=RT.fitted, y=RT.res)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) 

# Checking whether the residuals are normally distributed
# This should be as close to a straight line as possible

qqnorm(residuals(model4))
qqline(residuals(model4)) #adding line

# or on the same graph homoscedasticity and normality of residuals

par(mfcol=c(1,2))
qqnorm(resid(model4))
plot(fitted(model4), resid(model4))
par(mfcol=c(1,1))

# Now we will exclude observations with large residuals
# To check if they have an undue influence on the model
# Refitting the model on the subset of data
# This procedure allowed us to determine whether the observed frequency effect
# was a general property of the entire distribution or if it was driven primarily 
# by observations in the upper tail (i.e., the slowest response times)

model4t <- lmer( RT ~ log_F_scaled + NoS_scaled + (1|Subject_ID) + (1|Trial_ID), 
                 data = dat, subset=abs(scale(resid(model4)))<2.5)

summary(model4t)

#checking the residuals
par(mfcol=c(1,2))
qqnorm(resid(model4t))
plot(fitted(model4t), resid(model4t))
par(mfcol=c(1,1))

# Effects of frequency and NoS remains even after trimming the residuals

###################################################
####                                           ####
#### CATEGORICAL VARIABLES IN LMMs             ####
####                                           ####
###################################################

dat <- read.csv("lmm_data.csv", T, sep = ";")

dim(dat)
colnames(dat)
table(dat$NoS_cat)
table(dat$Frequency_cat)

dat$logF <- log(dat$Frequency)

# plot -- cont freq
dat %>%
  group_by(Trial_ID) %>%
  mutate(mean_RT = mean(RT, na.rm = TRUE),
         logF = mean(logF, na.rm = TRUE)) %>%
  ggplot(aes(x = logF, y = mean_RT)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_bw()

# plot -- cat freq

dat %>%
  group_by(Frequency_cat) %>%
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    se = sd(RT, na.rm = TRUE) / sqrt(n()),
    ci = 1.96 * se,
    .groups = "drop"
  ) %>%
  ggplot(aes(x = Frequency_cat, y = mean_RT)) +
  geom_col(width = 0.3) +
  geom_errorbar(aes(ymin = mean_RT - ci, ymax = mean_RT + ci),
                width = 0.15) +
  theme_bw()


dat %>%
  group_by(Frequency_cat) %>%
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    se = sd(RT, na.rm = TRUE) / sqrt(n()),
    ci = 1.96 * se,
    .groups = "drop"
  ) %>%
  ggplot(aes(x = Frequency_cat, y = mean_RT, group = 1)) +
  geom_point(size = 1) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_RT - ci, ymax = mean_RT + ci),
                width = 0.1) +
  ylim(0,700) +
  theme_bw()

dat %>%
  group_by(Frequency_cat) %>%
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    se = sd(RT, na.rm = TRUE) / sqrt(n()),
    ci = 1.96 * se,
    .groups = "drop"
  ) %>%
  ggplot(aes(x = Frequency_cat, y = mean_RT, group = 1)) +
  geom_point(size = 1) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_RT - ci, ymax = mean_RT + ci),
                width = 0.1) +
  theme_bw()

# treatment contrast

## housekeeping
dat$Frequency_cat <- factor(dat$Frequency_cat)

## see which values R gives to factor levels
contrasts(dat$Frequency_cat)

lmer_cat_1 <- lmer(RT ~ Frequency_cat +
                     (1|Subject_ID) + (1|Trial_ID),
                   data = dat)
summary(lmer_cat_1)

# releveling -- choosing reference group
dat$Frequency_cat <- relevel(dat$Frequency_cat, ref = "LF")

# run the model again
lmer_cat_1_relevel <- lmer(RT ~ Frequency_cat +
                             (1|Subject_ID) + (1|Trial_ID),
                           data = dat)
summary(lmer_cat_1_relevel)

# plot -- relevelled
dat$Frequency_cat <- factor(dat$Frequency_cat, levels = c("LF", "HF"))
dat %>%
  group_by(Frequency_cat) %>%
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    se = sd(RT, na.rm = TRUE) / sqrt(n()),
    ci = 1.96 * se,
    .groups = "drop"
  ) %>%
  ggplot(aes(x = Frequency_cat, y = mean_RT, group = 1)) +
  geom_point(size = 1) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_RT - ci, ymax = mean_RT + ci),
                width = 0.1) +
  theme_bw()

### treatment contrast with three groups

dat$NoS_cat <- factor(dat$NoS_cat,
                      levels = c("few", "medium", "many"))

contrasts(dat$NoS_cat)

lmer_cat_2 <- lmer(RT ~ NoS_cat +
                     (1|Subject_ID) + (1|Trial_ID),
                   data = dat)
summary(lmer_cat_2)

dat$NoS_cat <- relevel(dat$NoS_cat, ref = "medium")

lmer_cat_2_relevel <- lmer(RT ~ NoS_cat +
                             (1|Subject_ID) + (1|Trial_ID),
                           data = dat)
summary(lmer_cat_2_relevel)

### sum contrast

# frequency

# just in case
dat$Frequency_cat_sum <- dat$Frequency_cat

# defining sum contrast
contrasts(dat$Frequency_cat_sum) <- contr.sum(2)
contrasts(dat$Frequency_cat_sum)

lmer_cat_3 <- lmer(RT ~ Frequency_cat_sum +
                     (1|Subject_ID) + (1|Trial_ID),
                   data = dat)
summary(lmer_cat_3)

### sum contrast with three groups

# just in case
dat$NoS_cat_sum <- dat$NoS_cat
dat$NoS_cat_sum <- factor(dat$NoS_cat_sum,
                          levels = c("few", "medium", "many"))
contrasts(dat$NoS_cat_sum) <- contr.sum(3)
contrasts(dat$NoS_cat_sum)

lmer_cat_4 <- lmer(RT ~ NoS_cat_sum +
                     (1|Subject_ID) + (1|Trial_ID),
                   data = dat)
summary(lmer_cat_4)

# plot
dat %>%
  mutate(NoS_cat = factor(NoS_cat, levels = c("few", "medium", "many"))) %>%
  group_by(NoS_cat) %>%
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    se = sd(RT, na.rm = TRUE) / sqrt(n()),
    ci = 1.96 * se,
    .groups = "drop"
  ) %>%
  ggplot(aes(x = NoS_cat, y = mean_RT, group = 1)) +
  geom_point(size = 1) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_RT - ci, ymax = mean_RT + ci),
                width = 0.1) +
  theme_bw()

### polynomial contrast

dat$NoS_cat_poly <- dat$NoS_cat
dat$NoS_cat_poly <- factor(dat$NoS_cat_poly,
                           levels = c("few", "medium", "many"))
contrasts(dat$NoS_cat_poly) <- contr.poly(3)
contrasts(dat$NoS_cat_poly)

lmer_cat_5 <- lmer(RT ~ NoS_cat_poly +
                     (1|Subject_ID) + (1|Trial_ID),
                   data = dat)
summary(lmer_cat_5)

### Interactions

dat %>%
  mutate(
    NoS_cat = factor(NoS_cat, levels = c("few", "medium", "many")),
    Frequency_cat = factor(Frequency_cat, levels = c("LF", "HF")) 
  ) %>%
  # 2. Group by BOTH variables to see the interaction
  group_by(NoS_cat, Frequency_cat) %>%
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    se = sd(RT, na.rm = TRUE) / sqrt(n()),
    ci = 1.96 * se,
    .groups = "drop"
  ) %>%
  ggplot(aes(x = NoS_cat, y = mean_RT, 
             group = Frequency_cat, 
             color = Frequency_cat)) +
  geom_point(size = 2) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_RT - ci, ymax = mean_RT + ci),
                width = 0.1) +
  labs(
    y = "Mean Reaction Time",
    x = "Number of Senses (NoS)") +
  theme_bw()

dat$NoS_cat <- factor(dat$NoS_cat,
                      levels = c("few", "medium", "many"))
dat$Frequency_cat <- factor(dat$Frequency_cat,
                            levels = c("LF", "HF"))

lmer_cat_6 <- lmer(RT ~ NoS_cat * Frequency_cat +
                     (1|Subject_ID) + (1|Trial_ID),
                   data = dat)
summary(lmer_cat_6)

# tranformations

plot(density(dat$RT))

dat$invRT <- -1000/(dat$RT)
plot(density(dat$invRT))

dat$logRT <- log(dat$RT)
plot(density(dat$logRT))

lmer_cat_7 <- lmer(RT ~ NoS_cat + Frequency_cat +
                     (1|Subject_ID) + (1|Trial_ID),
                   data = dat)
summary(lmer_cat_7)

anova(lmer_cat_7, lmer_cat_6)
