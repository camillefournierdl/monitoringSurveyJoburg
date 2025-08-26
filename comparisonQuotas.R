library(tidyverse)
library(RColorBrewer)

"%ni%" = Negate("%in%")

set.seed(123)

dataJoburg <- read.csv("data/data_joh_person.csv")

# make quotas simpler 
dataJoburg <- subset(dataJoburg, quota_gender.0 %in% c(1,2))
dataJoburg <- subset(dataJoburg, !is.na(quota_gender.0))

dataJoburg$gender <- ifelse(dataJoburg$quota_gender.0 == 1, "male",
                            "female")
quotas <- read.csv("data/quotas_long.csv", sep = ";")

## start with age

age <- subset(quotas, variable_type == "newAge")

observed <- (table(dataJoburg$quota_age_, dataJoburg$gender)/nrow(dataJoburg)) %>% as.data.frame()
colnames(observed) <- c("age_group", "gender", "value")
observed$type <- "observed"

quotasTh <- age %>% select(gender, age_group, value) %>% 
  mutate(type = "quotas")

age_long <- rbind(observed, quotasTh)

age_long$age_group_cat <- ifelse(age_long$age_group == "1", "18-29",
                                 ifelse(age_long$age_group == "2", "30-39",
                                        ifelse(age_long$age_group == "3", "40-49",
                                               ifelse(age_long$age_group == "4", "50-60", NA))))

age_long$gender <- ifelse(age_long$gender == "male", "Man", "Woman")
age_long$type <- ifelse(age_long$type == "observed", "Sample", "Census")

# --- Plot ---
p1 <- ggplot(age_long, aes(x = factor(age_group_cat), y = value,
                     fill = type)) +
  geom_col(position = "dodge") +
  facet_wrap(~gender) +
  scale_fill_manual(values = brewer.pal(5, "BuPu")[c(2,4)])+
  labs(x = "Age group", y = "Proportion", fill = "") +
  theme_minimal()

p1

ggsave(plot = p1, filename = paste("plots/quotaAge.png", sep = ""),
       dpi=600, width = 12, height = 8, units='cm')


## then education

education <- subset(quotas, variable_type == "education")

observed <- (table(dataJoburg$quota_education)/nrow(dataJoburg)) %>% as.data.frame()

colnames(observed) <- c("education_level", "value")
observed$type <- "observed"

quotasTh <- education %>% select(ed_level_code, value) %>% 
  mutate(type = "quotas") %>% 
  rename(education_level = ed_level_code)

education_long <- rbind(observed, quotasTh)

education_long$education_level_cat <- ifelse(education_long$education_level == "1", "noEd",
                                 ifelse(education_long$education_level == "2", "grade 1-7",
                                        ifelse(education_long$education_level == "3", "grade 8-11",
                                               ifelse(education_long$education_level == "4", "grade 12",
                                                      ifelse(education_long$education_level == "5", "post matric",
                                                             ifelse(education_long$education_level == "6", "bachelor",
                                                                    ifelse(education_long$education_level == "7", "master+", NA)))))))

education_long$type <- ifelse(education_long$type == "observed", "Sample", "Census")


# --- Plot ---
p2 <- ggplot(education_long, aes(x = factor(education_level_cat, levels = c("noEd","grade 1-7","grade 8-11","grade 12",
                                                                            "post matric","bachelor","master+")),
                                 y = value,
                                 fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = brewer.pal(5, "BuPu")[c(2,4)])+
  labs(x = "Education level", y = "Proportion", fill = "") +
  theme_minimal()

ggsave(plot = p2, filename = paste("plots/quotaEducation.png", sep = ""),
       dpi=600, width = 15, height = 8, units='cm')
