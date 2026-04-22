### R workshop            ###
### Basic operations in R ###
### Variables, t- test, ANOVA ###

#Calling the needed packages

library(stats)
library(psych)
library(tidyverse)

# Giving the name of the dataset 

data1 <- simulated_data

# Checking the data structure

str (data1)


# Setting the proper measurement level

data1$Concreteness_category <- as.factor (data1$Concreteness_category)

data1$EV_cat_label <- as.factor (data1$EV_cat_label)

str(data1)


#Checking the distribution of the RT 

data1 %>%
  group_by(data1$Concreteness_category) %>%
  do(describe(.$Emotional_valence))

data1 %>%
  group_by(data1$Concreteness_category) %>%
  do(describe(.$logF))


hist(data1$RT_1000)
hist(data1$RT_1500)

describe(data1$RT_1000)
describe(data1$RT_1500)

ggplot(data1, aes(x = RT_1000)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black", alpha = 0.7) + 
  labs(title = "Histograms of RTs in 1000ms Condition",
       x = "Reaction Time (ms)",
       y = "Frequency") +
  theme_minimal()

ggplot(data1, aes(x = RT_1500)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black", alpha = 0.7) + 
  labs(title = "Histograms of RTs in 1500ms Condition",
       x = "Reaction Time (ms)",
       y = "Frequency") +
  theme_minimal()

data1 %>%
  group_by(Concreteness_category) %>%
  summarise(shapiro_p = shapiro.test(RT_1000)$p.value)

data1 %>%
  group_by(Concreteness_category) %>%
  summarise(shapiro_p = shapiro.test(RT_1500)$p.value)


ggplot(data1, aes(sample = RT_1000)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of RTs in 1000ms Condition",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

ggplot(data1, aes(sample = RT_1500)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of RTs in 1500ms Condition",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()


# Transforming the RT data to obtain distribution normality

data1$rt1000_log <- log(data1$RT_1000)
data1$rt1500_log <- log(data1$RT_1500)

#cheking new distribution

ggplot(data1, aes(sample = rt1000_log)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of RTs in 1000ms Condition",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

ggplot(data1, aes(sample = rt1500_log)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of RTs in 1500ms Condition",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()


ggplot(data1, aes(x = rt1000_log)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Log-transformed RTs",
       x = "Log-transformed Reaction Time (ms)",
       y = "Frequency") +
  theme_minimal()

ggplot(data1, aes(x = rt1500_log)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Log-transformed RTs",
       x = "Log-transformed Reaction Time (ms)",
       y = "Frequency") +
  theme_minimal()


describe(data1$rt1000_log)

describe(data1$rt1500_log)

shapiro.test(data1$rt1000_log)
shapiro.test(data1$rt1500_log)

# inverse transformation

data1$rt1000_inv <- 1/data1$RT_1000
data1$rt1500_inv <- 1/data1$RT_1500

ggplot(data1, aes(x = rt1000_inv)) +
  geom_histogram(binwidth = 0.0001, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Inverse RTs (1/RT) in 1000ms Condition",
       x = "Inverse Reaction Time (1/RT)",
       y = "Frequency") +
  theme_minimal()

ggplot(data1, aes(x = rt1500_inv)) +
  geom_histogram(binwidth = 0.0001, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Inverse RTs (1/RT) in 1500ms Condition",
       x = "Inverse Reaction Time (1/RT)",
       y = "Frequency") +
  theme_minimal()

shapiro.test(data1$rt1000_inv)
shapiro.test(data1$rt1500_inv)

ggplot(data1, aes(sample = rt1000_inv)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of RTs in 1000ms Condition",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

ggplot(data1, aes(sample = rt1500_inv)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of RTs in 1500ms Condition",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()


#The inverse transformation is better, however, the 1500 condition still has not-normal distribution

############################ FACTORIAL DESIGNS ############################################################

######################## t-test (Testing the difference between 2 groups) ###############################

# 1. Checking whether the two groups of stimuli are equal by some feature - averaging

# We have two groups of words: concrete and abstract, and we want to check whether they are 
# averaged by frequency
# For this case, we are using t test for independent samples

t.test(logF~ Concreteness_category , data = data1)

# We can see that two conditions are not statisticaly different by word frequency, because the p value
# is above 0.05

# 2. Checking the differences in RT between two independant groups, in order to answer the research question
# For instance, whether concrete words are easier for processing

t.test(rt1000_inv~ Concreteness_category , data = data1)

t.test(RT_1000 ~ Concreteness_category , data = data1)

#there is concreteness effect in the 1000 ms condition

t.test(rt1500_inv~ Concreteness_category , data = data1)

t.test(RT_1500 ~ Concreteness_category , data = data1)

#there is also concreteness effect in the 1500 ms condition since in both analysis we obtained p value below 0.05

# Box plots of the meanRTs for concrete and abstract words

ggplot(data1, aes(x = Concreteness_category, y = RT_1000, fill = Concreteness_category)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 4, color = "red") +
  labs(title = "Boxplot of RTs by word concreteness with 1000ms stimuli exposure",
       x = "Condition",
       y = "Reaction Time (ms)") +
  theme_minimal()

ggplot(data1, aes(x = Concreteness_category, y = RT_1500, fill = Concreteness_category)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 4, color = "red") +
  labs(title = "Boxplot of RTs by word concreteness with 1500 ms stimuli exposure",
       x = "Condition",
       y = "Reaction Time (ms)") +
  theme_minimal()

# 3. Checking the defferences in RT between two dependant groups, in order to answer the research question
# such as: Does the stimuli presentation duration have the impact on word recognition?
# For this purpose, we are using t test for dependant samples
# Why these are dependant samples - condition with 1000 and 1500 ms? Well, because the same stimuli (words) were used
# in both of the conditions
# This is why in the matrix we have those RTs in separate columns

t.test(data1$RT_1000, data1$RT_1500, paired = TRUE)

# since the t test is positive, this means that group with 1000ms stimuli exposure had slower (higher) RTs
# this can be checked by reversing the order of variables in the code

t.test(data1$RT_1500, data1$RT_1000, paired = TRUE)

# Now the t test is negative, but still this means the sam thing, that the group with the 1000ms exposure had
# slower RTs, but t test is negative, since the first group in the R code now has lower RTs, so when substracting
# the value is negative

# box plot of RTs by stimuli exposure conditions

ggplot(data1, aes(x = factor(0), y = RT_1000)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  geom_boxplot(data = data1, aes(x = factor(1), y = RT_1500), fill = "lightgreen", color = "black") +
  scale_x_discrete(labels = c("1000ms", "1500ms")) +
  labs(title = "Boxplot of RTs for Condition 1000 and Condition 1500ms",
       x = "Condition",
       y = "Reaction Time (ms)") +
  theme_minimal()

################ ONE WAY ANOVA (Testing the difference between more than 2 groups) ###########################

# So, we want to see whether EV influence word processing, thus, we have three groups of words:
# negative, neutral and positive
# We are using one way ANOVA for independent samples

# Perform one-way ANOVA
anova_result <- aov(RT_1000 ~ EV_cat_label, data = data1)

# Display the summary of the ANOVA
summary(anova_result)

# Perform Tukey's HSD post-hoc test - to check between which of the groups differences are statistically significant

data1$EV_cat_label<- as.factor (data1$EV_cat_label)

tukey_result <- TukeyHSD(anova_result)

is.factor(data1$EV_cat_label)

# Display the Tukey test results

tukey_result

plot(tukey_result)

# Check normality for each group using Shapiro-Wilk test
shapiro.test(data1$RT_1000[data1$EV_cat_label == "negative"])
shapiro.test(data1$RT_1000[data1$EV_cat_label == "neutral"])
shapiro.test(data1$RT_1000[data1$EV_cat_label == "positive"])

#You can use Levene's Test to check if the variances across groups are equal:

library (car)


leveneTest(RT_1000 ~ EV_cat_label, data = data1)

# Graph - bar plots

ggplot(data1, aes(x = EV_cat_label, y = RT_1000, fill = EV_cat_label)) +
  stat_summary(fun = "mean", geom = "bar", color = "black", width = 0.7) +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.2) +
  labs(title = "Mean RTs with 95% CI by EV",
       x = "Condition",
       y = "Mean Reaction Time (ms)") +
  theme_minimal()

####### Two way ANOVA - exploring the INTERACTION ########

# We are crossing the EV factor and concreteness factor
# asterix in the code denotes the interaction, thusthe output will have main effects of EV and concreteness
# and their interaction

anova_interaction <- aov(RT_1000 ~ EV_cat_label * Concreteness_category, data = data1)

summary(anova_interaction)

#Results show no interaction

#Post hoc

library(emmeans)

# Estimated marginal means and pairwise comparisons
emmeans_result <- emmeans(anova_interaction, ~ EV_cat_label * Concreteness_category)

# Pairwise comparisons with Tukey adjustment

pairs(emmeans_result, adjust = "tukey")


library(ggplot2)

#Bar plot interaction

ggplot(data1, aes(x = EV_cat_label, y = RT_1000, fill = Concreteness_category)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge", color = "black") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               position = position_dodge(0.9), width = 0.2) +
  labs(title = "Interaction Plot: RT1000 by EV Category and Concreteness",
       x = "EV Category", y = "Reaction Time") +
  theme_minimal()

# Line plot interaction

library(ggplot2)
library(dplyr)

# Calculate group means and standard errors

summary_data <- data1 %>%
  group_by(EV_cat_label, Concreteness_category) %>%
  summarise(
    mean_RT = mean(RT_1000, na.rm = TRUE),
    se_RT = sd(RT_1000, na.rm = TRUE) / sqrt(n())
  )

# Line plot

ggplot(summary_data, aes(x = EV_cat_label, y = mean_RT, group = Concreteness_category, color = Concreteness_category)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_RT - se_RT, ymax = mean_RT + se_RT), width = 0.2) +
  labs(
    title = "Reaction Time by EV Category and Concreteness",
    x = "EV Category",
    y = "Mean Reaction Time",
    color = "Concreteness"
  ) +
  theme_minimal()


####### MIXED ANOVA - exploring the INTERACTION between independent and repeated measures ########

# We want to explore the interaction between two factors, one is independent (between groups) and other
# is dependent or repeated (within groups)

# We are using now different matrix - in long data format, so the RT is in one column, and we added
# the factor stimulus duration, and denoted wich RT belongs to each of the conditions (1000 ms or 1500 ms)

install.packages("afex")
library(afex)
data2<- simulated_data_long

data2$Stim_duration <- as.factor(data2$Stim_duration)
data2$Concreteness_category <- as.factor(data2$Concreteness_category)
data2$Word <- as.factor(data2$Word)  

anova_mixed <- aov_ez(
  id = "Word",
  dv = "RT",
  within = "Stim_duration",
  between = "Concreteness_category",
  data = data2
)

anova_mixed

# making the summarisation of the data for the plot

plot_data <- data2 %>%
  group_by(Stim_duration, Concreteness_category) %>%
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    se_RT = sd(RT, na.rm = TRUE) / sqrt(n())
  )

# Line interaction plot

ggplot(plot_data, aes(x = Stim_duration, y = mean_RT, group = Concreteness_category, color = Concreteness_category)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_RT - se_RT, ymax = mean_RT + se_RT), width = 0.2) +
  labs(
    title = "RT by Stim Duration and Concreteness",
    x = "Stimulus Duration",
    y = "Mean RT",
    color = "Concreteness"
  ) +
  theme_minimal()

### Correlation, regression ###

# load packages
library(readxl)
library(lme4)

# read wide data
dat <- read_excel("simulated_data.xlsx")

# some things to get to know your data
summary(dat) # descriptive statistics of your data
head(dat) # check first 6 rows of a dataset
colnames(dat) # get the list of the names of the columns -- your variables
dim(dat) # see the dimensions of the data (number of rows, number of columns)

### Correlations

# To get a specific variable, you need to instruct R to get the data, and the name of the column
# here, we will use the method that looks like:
# [NAME OF THE DATA]$[NAME OF THE COLUMN]
# in our case you write: dat$logF

# testing correlation between (log) Frequency and Concreteness
cor.test(dat$logF, dat$Concreteness)

# plotting the data for (log) Frequency and Concreteness
png("c_Freq_Conc.png", width = 400, height = 400) # create a file you want to plot to ("create new file")
plot(dat$logF, dat$Concreteness) # plotting
dev.off() # informing R to stop writing to the file you opened two lines ago

# Spearman correlation
cor.test(dat$Arousal, dat$Concreteness, method = "spearman")
# method argument allows you to vary the correlation method
# "pearson" will give you the same method as in the first correlation we tested

# Point-biserial correlation
cor.test(dat$Concreteness_category, dat$logF)
# although no method is specified, R knows that one variable has only two levels

# Practice for later: try to correlate other variables in the dataset

### Regression

# you will need so-called long form data, where each participant response in an experiment will be one row
# read long data
datl <- read_excel("simulated_data_long.xlsx")

# some things to get to know your data
summary(datl) # descriptive statistics of your data
head(datl) # check first 6 rows of a dataset
colnames(datl) # get the list of the names of the columns -- your variables
dim(datl) # see the dimensions of the data (number of rows, number of columns)

# for now, we will just consider RTs for words exposed for 1500 ms
# here we create a new dataset that contains just 1500 ms exposition
datl_1500 <- datl[datl$Stim_duration == "1500ms",]

reg_model_1 <- lm(RT ~ Concreteness, data = datl_1500)
summary(reg_model_1)

png("r_Conc_RT.png", width = 400, height = 400) # create a file you want to plot to ("create new file")
plot(datl_1500$Concreteness, datl_1500$RT)
abline(reg_model_1, col = "green", lwd = 2)
dev.off() # informing R to stop writing to the file you opened two lines ago

### Multiple regression

reg_model_2 <- lm(RT ~ logF + Arousal, data = datl_1500)
summary(reg_model_2)

# with interaction parameter
reg_model_3 <- lm(RT ~ logF * Arousal, data = datl_1500)
summary(reg_model_3)

# with SIGNIFICANT interaction
reg_model_4 <- lm(RT ~ Concreteness * logF, data = datl_1500)
summary(reg_model_4)

reg_model_5 <- lm(RT ~ Emotional_valence * logF, data = datl_1500)
summary(reg_model_5)
