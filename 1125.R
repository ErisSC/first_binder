library(tidyverse)
library(afex)
library(emmeans)
library(visdat)

#Q1++++++++++++++++++++++++++++++
data1 <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data1.csv")
head(data1)

data1_tidied <- data1 %>%
  mutate(Condition = factor(Condition))
head(data1_tidied)

data1_tidied %>%
  group_by(Condition) %>%
  summarise(mean = mean(RT), sd = sd(RT))

set.seed(1000)
data1_tidied %>% 
  ggplot(aes(x = Condition, y = RT, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .1) +
  guides(colour = "none") +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  ylab("RT(ms)")

model1 <- aov_4(RT ~ Condition + (1 | Subject), data = data1_tidied)

summary(model1)

emmeans(model1, pairwise ~ Condition, adjust = "bonferroni")

#F (1,22) = 91.22, p < .001, generalised η2 = .81
#people responded faster to words that were high lexical frequency than low lexical frequency words p<.0001

#Q2++++++++++++++++++++++++++++++
data2 <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data2.csv")
head(data2)

data2_tidied <- data2 %>%
  mutate(Condition = factor(Condition))
head(data2_tidied)

data2_tidied %>%
  group_by(Condition) %>%
  summarise(mean = mean(RT), sd = sd(RT))

set.seed(1000)
data2_tidied %>% 
  ggplot(aes(x = fct_reorder(Condition, RT), y = RT, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .1) +
  guides(colour = "none") +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Condition", y = "RT(ms)")

model2 <- aov_4(RT ~ Condition + (1 | Subject), data = data2_tidied)

summary(model2)

emmeans(model2, pairwise ~ Condition, adjust = "bonferroni")

#F (3,44) = 203.21, p < .001, generalised η2 = .93
#faster<<<very high, high, low, very low>>>slower

#Q3++++++++++++++++++++++++++++++
data3 <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data3.csv")
head(data3)

data3_tidied <- data3 %>%
  mutate(Size = factor(Size), Colour = factor(Colour))
head(data3_tidied)

data3_tidied %>%
  group_by(Size, Colour) %>%
  summarise(mean = mean(RT), sd = sd(RT))

data3_tidied %>%
  ggplot(aes(x = fct_reorder(Size:Colour, RT), y = RT, colour = Size:Colour)) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .25) +
  guides(colour = "none") +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Size X Colour", y = "RT (ms)")

model3 <- aov_4(RT ~ Size * Colour + (1 + Size * Colour | Subject), data = data3_tidied, na.rm = TRUE)

anova(model3)

emmeans(model3, pairwise ~ Size * Colour, adjust = "bonferroni")

#The ANOVA revealed effect of Size (F(1, 23) = 198.97, p<.001, ηG2 = .62), effect of Colours (F(1, 23) = 524.27, p<.001, ηG2 = .87), and an interaction between Size and Colour (F(1, 23) = 11.08, p = .003, ηG2 = .14)
#faster<<<large-color,small-color,large-BW,small-BW>>>slower

#Q4++++++++++++++++++++++++++++++++++++
data4 <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data4.csv")
head(data4)

data4_tidied <- data4 %>%
  mutate(Difficulty = factor(Difficulty), Time_Pressure = factor(Time_Pressure), Group = factor(Group))
head(data4_tidied)

data4_tidied %>%
  group_by(Difficulty, Time_Pressure, Group) %>%
  summarise(mean = mean(RT), sd = sd(RT))

data4_tidied %>%
  ggplot(aes(x = fct_reorder(Difficulty:Time_Pressure, RT), y = RT, colour = Difficulty:Time_Pressure)) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .25) +
  guides(colour = "none") +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = .5)) +
  theme(text = element_text(size = 13)) +
  labs(x = "Difficulty X Time Pressure", y = "RT (ms)") +
  facet_wrap(~ Group)

model4 <- aov_4(RT ~ Difficulty * Time_Pressure * Group + (1 + Difficulty * Time_Pressure | Subject), data = data4_tidied, na.rm = TRUE)
anova(model4)
emmeans(model4, pairwise ~ Difficulty * Time_Pressure * Group, adjust = "bonferroni")
#effect of group, difficulty, time pressure; interaction between group & difficulty, group & time pressure, 3-way interaction

data_Arts = data4_tidied %>% filter(Group == "Arts_Students")
data_Maths = data4_tidied %>% filter(Group == "Maths_Students")
data_Psyc = data4_tidied %>% filter(Group == "Psychology_Students")

model_Arts <- aov_4(RT ~ Difficulty * Time_Pressure + (1 + Difficulty * Time_Pressure | Subject), data = data_Arts, na.rm = TRUE)
anova(model_Arts)
emmeans(model_Arts, pairwise ~ Difficulty * Time_Pressure, adjust = "bonferroni")
#no effect of difficulty(p = 0.06), no effect of time pressure(F<1), no interaction(F<1) among Arts students

model_Maths <- aov_4(RT ~ Difficulty * Time_Pressure + (1 + Difficulty * Time_Pressure | Subject), data = data_Maths, na.rm = TRUE)
anova(model_Maths)
emmeans(model_Maths, pairwise ~ Difficulty * Time_Pressure, adjust = "bonferroni")
#no effect of difficulty(p = 0.06), no effect of time pressure(F<1), no interaction(F<1) among Maths students

model_Psyc <- aov_4(RT ~ Difficulty * Time_Pressure + (1 + Difficulty * Time_Pressure | Subject), data = data_Psyc, na.rm = TRUE)
anova(model_Psyc)
emmeans(model_Psyc, pairwise ~ Difficulty * Time_Pressure, adjust = "bonferroni")
#effect of difficulty(F(1,23) = 524.27, p<.001, ηG2 = .87), effect of time pressure(F(1,23) = 198.97, p<.001, ηG2 = .62), interaction(F(1,23) = 11.08, p=.003, ηG2 = .14)
#for psychology students:faster<<<easy-nopressure,easy-pressure,hard-nopressure,hard-pressure>>>slower
