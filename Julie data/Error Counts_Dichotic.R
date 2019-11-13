library(tidyverse)
library(readxl)
#Import Dataset (number values only)
s1 <- read_xlsx(file.choose(), sheet = 2)
names(s1)<-make.names(names(s1),unique = TRUE)
age.data <- read_xlsx(file.choose(), sheet = 2)
names(age.data)<-make.names(names(age.data),unique = TRUE) 
s1<- left_join(s1, age.data, by = "Subject.Number")

#Save as a new df
Error <- s1

#Make new column with combined Score
#Error$Score <- rowSums(Error[,c("Score_1", "Score_2")], na.rm=TRUE)

Error <- Error %>% 
  rename(TMR = Target.to.Masker.Ratio, TrialNum = Trial.Number.Within.Block)

#Subset to Monaural Data
Error_di <- Error %>%
  filter(Condition > 3 & Condition < 8)

#Masker - Target Ear = 1, Masker - Opposite Ear = 2, Other = 3, Correct = 4

Error_di1 <- Error_di %>% mutate(., Error.Type.Verb = with(., case_when(
  (Condition == 4 & Score_1 <1 & Response.Verb == Masker1.Verb) ~ 1,
  (Condition == 4 & Score_1 <1 & Response.Verb == Masker2.Verb) ~ 2,
  (Condition == 4 & Score_1 <1 & Response.Verb != Masker1.Verb & Masker2.Verb) ~ 3,
  (Condition == 5 & Score_1 <1 & Response.Verb == Masker1.Verb) ~ 1,
  (Condition == 5 & Score_1 <1 & Response.Verb == Masker2.Verb) ~ 2,
  (Condition == 5 & Score_1 <1 & Response.Verb != Masker1.Verb & Masker2.Verb) ~ 3,
  (Condition == 6 & Score_1 <1 & Response.Verb == Masker1.Verb) ~ 1,
  (Condition == 6 & Score_1 <1 & Response.Verb == Masker2.Verb) ~ 2,
  (Condition == 6 & Score_1 <1 & Response.Verb != Masker1.Verb & Masker2.Verb) ~ 3,
  (Condition == 7 & Score_1 <1 & Response.Verb == Masker1.Verb) ~ 1,
  (Condition == 7 & Score_1 <1 & Response.Verb == Masker2.Verb) ~ 2,
  (Condition == 7 & Score_1 <1 & Response.Verb != Masker1.Verb & Masker2.Verb) ~ 3,
  TRUE ~ 4)))

Error_di1 <- Error_di1 %>% mutate(., Error.Type.Number = with(., case_when(
  (Condition == 4 & Score_1 <1 & Response.Number == Masker1.Number) ~ 1,
  (Condition == 4 & Score_1 <1 & Response.Number == Masker2.Number) ~ 2,
  (Condition == 4 & Score_1 <1 & Response.Number != Masker1.Number & Masker2.Number) ~ 3,
  (Condition == 5 & Score_1 <1 & Response.Number == Masker1.Number) ~ 1,
  (Condition == 5 & Score_1 <1 & Response.Number == Masker2.Number) ~ 2,
  (Condition == 5 & Score_1 <1 & Response.Number != Masker1.Number & Masker2.Number) ~ 3,
  (Condition == 6 & Score_1 <1 & Response.Number == Masker1.Number) ~ 1,
  (Condition == 6 & Score_1 <1 & Response.Number == Masker2.Number) ~ 2,
  (Condition == 6 & Score_1 <1 & Response.Number != Masker1.Number & Masker2.Number) ~ 3,
  (Condition == 7 & Score_1 <1 & Response.Number == Masker1.Number) ~ 1,
  (Condition == 7 & Score_1 <1 & Response.Number == Masker2.Number) ~ 2,
  (Condition == 7 & Score_1 <1 & Response.Number != Masker1.Number & Masker2.Number) ~ 3,
  TRUE ~ 4)))
                                  
Error_di1 <- Error_di1 %>% mutate(., Error.Type.Adjective = with(., case_when(
  (Condition == 4 & Score_1 <1 & Response.Adjective == Masker1.Adjective) ~ 1,
  (Condition == 4 & Score_1 <1 & Response.Adjective == Masker2.Adjective) ~ 2,
  (Condition == 4 & Score_1 <1 & Response.Adjective != Masker1.Adjective & Masker2.Adjective) ~ 3,
  (Condition == 5 & Score_1 <1 & Response.Adjective == Masker1.Adjective) ~ 1,
  (Condition == 5 & Score_1 <1 & Response.Adjective == Masker2.Adjective) ~ 2,
  (Condition == 5 & Score_1 <1 & Response.Adjective != Masker1.Adjective & Masker2.Adjective) ~ 3,
  (Condition == 6 & Score_1 <1 & Response.Adjective == Masker1.Adjective) ~ 1,
  (Condition == 6 & Score_1 <1 & Response.Adjective == Masker2.Adjective) ~ 2,
  (Condition == 6 & Score_1 <1 & Response.Adjective != Masker1.Adjective & Masker2.Adjective) ~ 3,
  (Condition == 7 & Score_1 <1 & Response.Adjective == Masker1.Adjective) ~ 1,
  (Condition == 7 & Score_1 <1 & Response.Adjective == Masker2.Adjective) ~ 2,
  (Condition == 7 & Score_1 <1 & Response.Adjective != Masker1.Adjective & Masker2.Adjective) ~ 3,
  TRUE ~ 4)))

Error_di1 <- Error_di1 %>% mutate(., Error.Type.Object = with(., case_when(
  (Condition == 4 & Score_1 <1 & Response.Object == Masker1.Object) ~ 1,
  (Condition == 4 & Score_1 <1 & Response.Object == Masker2.Object) ~ 2,
  (Condition == 4 & Score_1 <1 & Response.Object != Masker1.Object & Masker2.Object) ~ 3,
  (Condition == 5 & Score_1 <1 & Response.Object == Masker1.Object) ~ 1,
  (Condition == 5 & Score_1 <1 & Response.Object == Masker2.Object) ~ 2,
  (Condition == 5 & Score_1 <1 & Response.Object != Masker1.Object & Masker2.Object) ~ 3,
  (Condition == 6 & Score_1 <1 & Response.Object == Masker1.Object) ~ 1,
  (Condition == 6 & Score_1 <1 & Response.Object == Masker2.Object) ~ 2,
  (Condition == 6 & Score_1 <1 & Response.Object != Masker1.Object & Masker2.Object) ~ 3,
  (Condition == 7 & Score_1 <1 & Response.Object == Masker1.Object) ~ 1,
  (Condition == 7 & Score_1 <1 & Response.Object == Masker2.Object) ~ 2,
  (Condition == 7 & Score_1 <1 & Response.Object != Masker1.Object & Masker2.Object) ~ 3,
  TRUE ~ 4)))



#Factor and Leveling
Error_di1$Group <- as.factor(Error_di1$Group)
Error_di1$Condition <- as.factor(Error_di1$Condition)
Error_di1$Group <- relevel(Error_di1$Group, "YN", "ON")


#Gather columns into "Long" column
Error_diLong <- gather(Error_di1, WordCat, Error.Type, Error.Type.Verb:Error.Type.Object, factor_key=TRUE)


### Data Factors/cleanup
Error_diLong$WordCat <- factor(Error_diLong$WordCat) #Rename Word Category Lables
levels(Error_diLong$WordCat) <- gsub('Error.Type.', '', levels(Error_diLong$WordCat))

mean_Error_diL <- Error_diLong %>%
  count(Subject.Number, Group, Condition, WordCat, Error.Type, name = "NumError")

#Make Error type asfactor (dont do this until after NumError is calculated)
mean_Error_diL$Error.Type <- factor(mean_Error_diL$Error.Type)


## Summary Tables for all Figures #####
mean_Error_allwords <- mean_Error_diL %>%
  group_by(Group, Condition, Error.Type) %>%
  summarize(error = mean(NumError/20)) %>%
  filter(Error.Type != "4")


mean_Error_Verb <- mean_Error_diL %>%
  group_by(Group, Condition, WordCat, Error.Type) %>%
  filter(WordCat == "Verb") %>%
  summarize(error = mean(NumError/20)) %>%
  filter(Error.Type != "4")

mean_Error_Number <- mean_Error_diL %>%
  group_by(Group, Condition, WordCat, Error.Type) %>%
  filter(WordCat == "Number") %>%
  summarize(error = mean(NumError/20)) %>%
  filter(Error.Type != "4")

mean_Error_Adjective <- mean_Error_diL %>%
  group_by(Group, Condition, WordCat, Error.Type) %>%
  filter(WordCat == "Adjective") %>%
  summarize(error = mean(NumError/20)) %>%
  filter(Error.Type != "4")

mean_Error_object <- mean_Error_diL %>%
  group_by(Group, Condition, WordCat, Error.Type) %>%
  filter(WordCat == "Object") %>%
  summarize(error = mean(NumError/20)) %>%
  filter(Error.Type != "4")
 

### Figures #####

#Make new Facet Labels
cond.facet.d <- c(
  "4" = "Familiar Target", 
  "5" = "Familiar Masker -\nTarget Ear", 
  "6" = "Familiar Masker -\nOpposite Ear",
  "7" = "All Unfamiliar")
# tmr.facet <- c(
#   "-5" = "- 5 SNR",
#   "0" = "0 SNR")

 
#ALL WORDS
ggplot(mean_Error_allwords, aes(Error.Type, error)) + 
  geom_col(aes(fill = factor(Error.Type, labels = c("Masker - Target Ear", "Masker - Opposite Ear", "Other"))), color = "black", position = position_dodge2()) +
  scale_y_continuous(expand = c(0,0), limits = c(0, .75), breaks = c(.2, .4, .6)) +
  facet_grid(Condition ~ Group, labeller = labeller(Condition = cond.facet.d)) +
  labs(title = "Dichotic Errors Averaged Across Word Categories", x = "Error Type", y = "Error Proportion", 
       fill = "Target Error \nConfusion") +
    theme_bw(base_size = 16) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
    scale_fill_viridis_d()

ggsave("Dichotic_Errors.jpeg", device = "jpeg", scale = 2)

#VERBS
ggplot(mean_Error_Verb, aes(Error.Type, error)) + 
  geom_col(aes(fill = factor(Error.Type, labels = c("Masker - Target Ear", "Masker - Opposite Ear", "Other"))), color = "black", position = position_dodge2()) +
  scale_y_continuous(expand = c(0,0), limits = c(0, .75), breaks = c(.2, .4, .6)) +
  facet_grid(Condition ~ Group, labeller = labeller(Condition = cond.facet)) +
  labs(title = "Dichotic Errors - Verbs", x = "Error Type", y = "Error Proportion", 
       fill = "Target Error \nConfusion") +
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_fill_viridis_d()


#NUMBERS
ggplot(mean_Error_Number, aes(Error.Type, error)) + 
  geom_col(aes(fill = factor(Error.Type, labels = c("Masker - Target Ear", "Masker - Opposite Ear", "Other"))), color = "black", position = position_dodge2()) +
  scale_y_continuous(expand = c(0,0), limits = c(0, .75), breaks = c(.2, .4, .6)) +
  facet_grid(Condition ~ Group, labeller = labeller(Condition = cond.facet)) +
  labs(title = "Dichotic Errors - Numbers", x = "Error Type", y = "Error Proportion", 
       fill = "Target Error \nConfusion") +
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_fill_viridis_d()

#ADJECTIVES
ggplot(mean_Error_Adjective, aes(Error.Type, error)) + 
  geom_col(aes(fill = factor(Error.Type, labels = c("Masker - Target Ear", "Masker - Opposite Ear", "Other"))), color = "black", position = position_dodge2()) +
  scale_y_continuous(expand = c(0,0), limits = c(0, .75), breaks = c(.2, .4, .6)) +
  facet_grid(Condition ~ Group, labeller = labeller(Condition = cond.facet)) +
  labs(title = "Dichotic Errors - Adjectives", x = "Error Type", y = "Error Proportion", 
       fill = "Target Error \nConfusion") +
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_fill_viridis_d()

#Objects
ggplot(mean_Error_object, aes(Error.Type, error)) + 
  geom_col(aes(fill = factor(Error.Type, labels = c("Masker - Target Ear", "Masker - Opposite Ear", "Other"))), color = "black", position = position_dodge2()) +
  scale_y_continuous(expand = c(0,0), limits = c(0, .75), breaks = c(.2, .4, .6)) +
  facet_grid(Condition ~ Group, labeller = labeller(Condition = cond.facet)) +
  labs(title = "Dichotic Errors - Objects", x = "Error Type", y = "Error Proportion", 
       fill = "Target Error \nConfusion") +
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_fill_viridis_d()


#ANOVA

anova_allwords <- mean_Error_diL %>%
  group_by(Subject.Number, Group, Condition, Error.Type, WordCat) %>%
  summarize(error = mean(NumError/20)) %>%
  filter(Error.Type != "4")

#P
anova1 <- aov(error ~ Condition * WordCat * Error.Type * Group + (1/Subject.Number), data = anova_allwords)
summary(anova1)

#Final model. Main effects of condition/tmr/group, and a significant interaction of condition x group
anova2 <- aov(error ~ Condition + TMR + Group + Condition * Group + TMR * Error.Type + TMR * Error.Type * Group + (1/Subject.Number), data = anova_allwords)
summary(anova2)

#Pairwise t-tests re: group differences

TukeyHSD(anova1, ("Group:Error.Type"))
TukeyHSD(anova1, ("Condition:Error.Type"))
TukeyHSD(anova1, ("Error.Type:Group"), ordered = FALSE)

#Group x Error.Type Interaction
post_error.group <- mean_Error_diL %>%
  group_by(Group, Error.Type) %>%
  summarize(error = mean(NumError/20)) %>%
  filter(Error.Type != "4")

ggplot(post_error.group, aes(Error.Type, error)) + 
  geom_col(aes(fill = factor(Error.Type, labels = c("Masker - Target Ear", "Masker - Opposite Ear", "Other"))), color = "black", position = position_dodge2()) +
  scale_y_continuous(expand = c(0,0), limits = c(0, .75), breaks = c(.2, .4, .6)) +
  facet_wrap( ~ Group, labeller = labeller(Condition = cond.facet.d)) +
  labs(title = "Group x Error.Type Interaction", x = "Error Type", y = "Error Proportion", 
       fill = "Target Error \nConfusion") +
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_fill_viridis_d()

ggsave("Dichotic_Errors_GroupxType.jpeg", device = "jpeg", scale = 1)

#Condition x Error.Type Interaction
post_error.condition <- mean_Error_diL %>%
  group_by(Condition, Error.Type) %>%
  summarize(error = mean(NumError/20)) %>%
  filter(Error.Type != "4")

ggplot(post_error.condition, aes(Error.Type, error)) + 
  geom_col(aes(fill = factor(Error.Type, labels = c("Masker - Target Ear", "Masker - Opposite Ear", "Other"))), color = "black", position = position_dodge2()) +
  scale_y_continuous(expand = c(0,0), limits = c(0, .75), breaks = c(.2, .4, .6)) +
  facet_wrap( ~ Condition, labeller = labeller(Condition = cond.facet.d), nrow = 1) +
  labs(title = "Condition x Error.Type Interaction", x = "Error Type", y = "Error Proportion", 
       fill = "Target Error \nConfusion") +
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_fill_viridis_d()

ggsave("Dichotic_Errors_CondxType.jpeg", device = "jpeg", scale = 2)

#all conditions are sig diff from each other ( 2 has more errors than 1, 3 has more errors than 1, 2 has more erros than 3)
#1) YN sig diff on all conditions. ON does not differ between 2 and 3
#2) ON sig worse than YN on all conditions.

#ANOVA Interactions
#Effect of Group x Condition

anova_interaction1 <- mean_Error_monoL %>%
  group_by(Group, Condition, Error.Type, WordCat) %>%
  summarize(error = mean(NumError/20)) %>%
  filter(Error.Type != "4")