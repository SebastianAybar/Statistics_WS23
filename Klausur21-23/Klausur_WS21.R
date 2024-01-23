#KLAUSUR WS21/22
library("tidyverse")
library("tidyr")
library("dplyr")
library("ggplot2")

#Aufgabe 1
#(a) Import
path_dataset <- "C:/Users/sebas/OneDrive/Dokumente/GitHub/Statistics_WS23/Klausur21-23/melanoma.csv"
dataset <- read.csv(path_dataset) %>% as_tibble()
#(c) Change the values of the variables sex, status, ulcer to strings describing their values 
#and add a new variable live.status describing whether the patient is alive or dead.
dataset$sex <- ifelse(dataset$sex == 1, "male", "female")
dataset$status <- case_when(dataset$status == 1 ~ "died from melanoma",
                            dataset$status == 2 ~ "still alive",
                            dataset$status == 3 ~ "died unrelated to melanoma")
dataset$ulcer <- ifelse(dataset$ulcer == 1, "present", "absent")
#mutate column still.alive
dataset <- dataset %>% mutate(live.status = ifelse(status == "still alive", "alive", "dead"))
#(d) Create a contingency table for the variables sex and live.status.
contigency_table_observed <- chisq.test(dataset$sex, dataset$live.status)$observed %>% addmargins() #sobald ich observed in variable packen will klappts nicht
contigency_table_expected <- chisq.test(dataset$sex, dataset$live.status)$expected %>% addmargins()
Chi_squared <- chisq.test(dataset$sex, dataset$live.status)$statistic

#So klappts alternativ
contigency_table <- chisq.test(dataset$sex, dataset$live.status)

observed_values <- contigency_table$observed
observed_values %>% addmargins()

expected_values <- contigency_table$expected
expected_values %>% addmargins()

#(e) Evaluate the relative risks to survive at least 3 years for the variable sex and interpret the values.
dataset <- dataset %>%
  mutate(lived.longer.than.3.years = ifelse(time >= 1095, 1, 0))
  
risk_to_survive <- dataset %>% 
  group_by(sex) %>%
  summarise(rel_risk_to_survive_min_3_years = round(sum(lived.longer.than.3.years) / length(dataset), 2))
#Woman have a higher chance to survive 3 years or longer

#(f) Create a summary describing the distribution of the variable age
#containing min, max, mean, the three quartiles depending on the variable sex.
age_measures <- dataset %>%
  group_by(sex) %>%
  summarise(mean = mean(age),
            min = min(age),
            max = max(age),
            q1 = quantile(age, 0.25, type = 1),
            q2 = quantile(age, 0.50, type = 1),
            q3 = quantile(age, 0.75, type = 1))

#(g) Create side by side boxplots for the age of persons depending on their sex and interpret the diagram.
boxplot(dataset$age ~ dataset$sex, 
        data = dataset,
        main = "Side by Side Boxplots",
        xlab = "",
        ylab = "age")

#(h) The csv file add.data.melanoma.csv contains data from another study. 
#Import the dataset as a tibble called add.data.melanoma.
path_dataset <- "C:/Users/sebas/OneDrive/Dokumente/GitHub/Statistics_WS23/Klausur21-23/add.data.melanoma.csv"
dataset <- read.csv(path_dataset) %>% as_tibble()
dataset <- dataset %>% separate(sex_age_year, into = c("sex", "age", "year"), sep = "/")






