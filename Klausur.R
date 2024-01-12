library("tidyverse")
library("tidyr")
library("dplyr")
library("ggplot2")

#read the csv file into a tibble
path_dataset <- "C:/Users/sebas/OneDrive/Dokumente/GitHub/Statistics_WS23/exam_data.csv"
data_set <- read.csv(path_dataset) %>% as_tibble()
#split the Variable exam into mat.nr and course
tidy_data <- data_set %>%
  separate(exam, into = c("mat.nr", "course"), sep = "/")
#add new variable grade
new_variable <- tidy_data %>%
  mutate(grade = case_when(score < 50 ~ 5, 
                           score >= 50 & score < 65 ~ 4,
                           score >= 65 & score < 80 ~ 3,
                           score >= 80 & score < 90 ~ 2,
                           score >= 90 ~ 1))
  
#determine the total number of tests in each exam and number of students participating
total_number_of_tests <- new_variable %>% 
  group_by(course) %>%
  summarise(n = n())

unique_students <- new_variable %>%
  group_by(mat.nr) %>% 
  summarise(n = n())

count(unique_students)
total_number_of_students <- count(unique_students)

#For each subject, determine the absolute frequencies of the grades
#and store the result in a tibble with the variables grade, Computer
#Networks, Data Bases, Formal Languages, Mathematics, OOP and
#Software Engineering.
notenverteilungen <- new_variable %>%
  count(course, grade) %>%
  spread(key = course, value = n)

#For each subject, determine the minimum, maximum, the three
#quartiles, the mean of the variable score, the number of 
#participants and the dropout rates.
measures <- new_variable %>%
  group_by(course) %>%
   summarise(minimum = min(score),
             maximum = max(score),
             mean = mean(score),
             participants = n(),
             q1 = quantile(score, 0.25, type = 1),
             q2 = quantile(score, 0.50, type = 1),
             q3 = quantile(score, 0.75, type = 1))

dropouts <- new_variable %>%
  filter(attempt == 3, grade == 5)

number_of_dropouts_as_tibble <- count(dropouts)
#oder
number_of_dropouts_as_int <- as.integer(count(dropouts))

measures <- new_variable %>%
  mutate(dropout = if_else(grade <= 4, 0, 1)) %>%
  group_by(course) %>%
  summarise(minimum = min(score),
            maximum = max(score),
            mean = mean(score),
            participants = n(),
            q1 = quantile(score, 0.25, type = 1),
            q2 = quantile(score, 0.50, type = 1),
            q3 = quantile(score, 0.75, type = 1),
            dropout_rate = sum(dropout)/participants)




