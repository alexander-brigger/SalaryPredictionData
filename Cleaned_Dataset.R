# Alex Brigger Final Project 5/3/2024

rm(list=ls())

# Importing Libraries
library(readr)
library(tidyverse)
library(ggplot2)
library(janitor)

# Reading in data
salary_predict = read_csv("salary_prediction_data.csv")


# Inspecting the data
glimpse(salary_predict)

# Turning education levels into a factor for the graph
salary_predict$Education = factor(salary_predict$Education,levels = c("High School", "Bachelor", "Master", "PhD"))

# This checks for missing values\
missing_values <- sum(is.na(salary_predict))
missing_values

# Here I created a median salary column that finds the median salary for each job
salary_predict <- salary_predict %>%
  group_by(Job_Title) %>%
  mutate(median_for_job = median(Salary, na.rm = TRUE))

# I then created another column seeing if they are above the median or not. I figured I could then see different factors and then see if they were above the median or not.
salary_predict <- salary_predict %>%
  group_by(Job_Title) %>%
  mutate(relation_to_median = ifelse(Salary > median_for_job, "Above Median", "Below Median"))

# Here I used the janitor library to fix all the names and make the more standardized

salary_predict <- clean_names(salary_predict)

#salary_predict

# First graph that shows the age distribution of the data
salary_predict %>%
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = 2.5, fill = "red", color = "black") +
  labs(title = "Age Distribution",
       x = "Age",
       y = "Frequency")

# This graph shows the distribution of education by gender in the dataset.
salary_predict %>%
  ggplot(aes(x = gender, fill = education)) +
  geom_bar(position = "stack") +
  labs(title = "Education Level by Gender",
       x = "Gender",
       y = "Frequency")

# This shows the salaries range based on education levels.
salary_predict %>%
  ggplot(aes(x = education, y = salary, fill = education)) +
  geom_boxplot() +
  labs(title = "Salary by Education Level",
       x = "Education Level",
       y = "Salary")

# This shows the distribution of salaries in the dataset.
salary_predict %>%
  ggplot(aes(x = salary)) +
  geom_histogram(binwidth = 10000, fill = "lightblue", color = "black") +
  labs(title = "Salary Distribution",
       x = "Salary",
       y = "Frequency")

# This shows the number of each job there is in the dataset
salary_predict %>%
  ggplot(aes(x = job_title)) +
  geom_bar(fill = "orange", color = "black") +
  coord_flip() +
  labs(title = "Frequency of Job Titles",
       x = "Job Title",
       y = "Frequency")


# This graph shows salary based on gender
salary_predict %>%
  ggplot(aes(x = gender, y = salary, fill = gender)) +
  geom_boxplot() +
  labs(title = "Salary by Gender",
       x = "Gender",
       y = "Salary")

# This shows the salary and experience chart showing the gender as well
salary_predict %>%
  ggplot(aes(x = experience, y = salary, color = gender)) +
  geom_point() +
  labs(title = "Salary vs. Experience by Gender",
       x = "Experience",
       y = "Salary",
       color = "Gender")

# plot on age and experience
salary_predict %>%
  ggplot(aes(x = age, y = experience)) +
  geom_point() +
  labs(title = "Age vs. Experience",
       x = "Age",
       y = "Experience")

# plot on salary and age
salary_predict %>%
  ggplot(aes(x = age, y = salary)) +
  geom_point() +
  labs(title = "Age vs. Salary",
       x = "Age",
       y = "Salary")

# plot on salary and experience
salary_predict %>%
  ggplot(aes(x = experience, y = salary)) +
  geom_point() +
  labs(title = "Salary vs. Experience",
       x = "Experience",
       y = "Salary")

# here I get the average salary based on location and job title to then compare in the next graph
average_salary <- salary_predict %>%
  group_by(job_title, location) %>%
  summarize(avg_salary = mean(salary))

# bar plot that shows average salaries per job for each location
average_salary %>%
  ggplot(aes(x = job_title, y = avg_salary, fill = location)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Salary by Job Title and Location",
       x = "Job Title",
       y = "Average Salary",
       fill = "Location") +
  scale_y_continuous(labels = scales::comma_format())


# bar plot for gender and relation to median salary based on education level
salary_predict %>%
  ggplot(aes(x = education, fill = relation_to_median)) +
  geom_bar(position = "dodge") +
  labs(title = "Relation to Median Salary by Education Level",
       x = "Education Level",
       y = "Count",
       fill = "Relation to Median") +
  scale_fill_manual(values = c("Above Median" = "lightblue", "Below Median" = "salmon")) +
  facet_wrap(~ gender)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# calculating the average salary per job title and education level same as before but for education and not location
average_salary_by_job_and_education <- salary_predict %>%
  group_by(job_title, education) %>%
  summarize(avg_salary = mean(salary))

# here I am comparing salary by job title and education level
average_salary_by_job_and_education %>%
  ggplot(aes(x = job_title, y = avg_salary, fill = education)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Salary by Job Title and Education Level",
       x = "Job Title",
       y = "Average Salary",
       fill = "Education Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("High School" = "lightblue", "Bachelor" = "salmon", "Master" = "lightgreen", "PhD" = "purple"))



