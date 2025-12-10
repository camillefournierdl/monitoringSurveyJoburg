library(tidyverse)
library(stringr)

`%ni%` <- Negate(`%in%`)

censusGEO <- read.csv("data/sa-census-2022-geography-v1.csv")

QID_joburg <- subset(censusGEO, Municipality == "78. JHB")$QID %>% unique()

census <- read.csv("data/sa-census-2022-person-v1.csv")

censusJoburg <- subset(census, QID %in% QID_joburg)

age <- censusJoburg %>%
  mutate(age_group = cut(P04_AGE, 
                         breaks = c(18, 29, 39, 49, 60), 
                         labels = c("18-29", "30-39", "40-49", "50-60"),
                         right = TRUE)) %>%
  filter(!is.na(age_group)) %>% 
  count(age_group, P02_SEX) %>%
  mutate(percentage = n / sum(n))

age$age_group_number <- rep(1:4, each = 2)

write.csv(age, "data/ageCensus.csv", row.names = F)

recode_education <- function(x) {
  # normalize to simple tokens
  ed <- x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", " ") |>
    str_squish()
  
  has <- function(terms) grepl(paste(terms, collapse = "|"), ed)
  
  unknown   <- c("do not know", "unspecified", "not applicable")
  no_school <- c("no schooling")
  
  primary   <- c("grade r","grade 0","grade 1","grade 2","grade 3","grade 4","grade 5","grade 6","grade 7")
                 # "standard 1","standard 2","standard 3","standard 4","standard 5","abet 1","abet 2","abet 3","aet 1","aet 2","aet 3")
  
  some_sec  <- c("grade 8","grade 9","grade 10","grade 11")
                 # "standard 6","standard 7","standard 8","standard 9",
                 # "form 1","form 2","form 3","form 4",
                 # "ncv level 2","ncv level 3","nqf level 2","nqf level 3","abet 4",
                 # "certificate with less than grade 12","diploma with less than grade 12")
  
  matric    <- c("grade 12","standard 10","form 5","matric","ncv level 4","nqf level 4")
  
  postmatric <- c(
    # TVET/NTC/N-levels and NQF 5–7
    "ntc i","ntc ii","ntc iii"," n1 "," n2 "," n3 "," n4 "," n5 "," n6 ",
    "nqf level 5","nqf level 6","nqf level 7",
    # explicit post-matric credentials
    "higher diploma","advanced diploma",
    "diploma with grade 12","certificate with grade 12",
    "occupational certificate nqf level 5","occupational certificate nqf level 6","occupational certificate nqf level 7"
  )
  
  bachelor  <- c("bachelor")
  postgrad  <- c("honours","honors","postgraduate diploma","masters","professional masters","phd","doctor")
  
  case_when(
    has(unknown)                 ~ "Missing/Unknown",
    has(postgrad) | has(c("nqf level 9","nqf level 10")) ~ "Postgraduate degree",
    has(bachelor)                ~ "Bachelor’s degree",
    has(postmatric)              ~ "Diploma or certificate after matric (e.g., TVET college)",
    has(matric) & !has(postmatric) & !has(bachelor) & !has(postgrad)
    ~ "Matric/Grade 12 (National Senior Certificate)",
    has(some_sec)                ~ "Some secondary school (Grades 8–11)",
    has(primary)                 ~ "Primary school (Grades 1–7)",
    has(no_school)               ~ "No formal education",
    grepl("other", ed)           ~ "Other",
    TRUE                         ~ "Missing/Unknown"
  )
}

# get education category from census to compare quota
censusJoburg_out <- censusJoburg %>%
  filter(P04_AGE > 17) %>% 
  mutate(edu_category = recode_education(P21_EDULEVEL)) %>%
  filter(edu_category %ni% c("Other", "Missing/Unknown")) %>% # comment this line out for testing
  count(edu_category, P02_SEX, name = "n") %>%
  mutate(pct = n / sum(n)) %>%
  arrange(match(edu_category, c(
    "No formal education",
    "Primary school (Grades 1–7)",
    "Some secondary school (Grades 8–11)",
    "Matric/Grade 12 (National Senior Certificate)",
    "Diploma or certificate after matric (e.g., TVET college)",
    "Bachelor’s degree (university)",
    "Postgraduate degree (Honours/Postgrad Dip/Masters/Doctorate)",
    "Other",
    "Missing/Unknown"
  )))

censusJoburg_out$edu_category_number <- rep(1:7, each = 2)

write.csv(censusJoburg_out, "data/educationCensus.csv", row.names = F)
