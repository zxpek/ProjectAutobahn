#- Description of the dataset (dimensions, content of each of the variables).
#- Descriptive statistics (univariate descriptive statistics and bivariate).
#- Correlation analyses
#- Regression
#- _Above all_: you should tell me something interesting about what is going on in the data.

# MA: Add Data Set Information
# Source: Paulo Cortez, University of Minho, GuimarÃ£es, Portugal, http://www3.dsi.uminho.pt/pcortez
# This data approach student achievement in secondary education of two Portuguese schools. 
# The data attributes include student grades, demographic, social and school related features). 
# The data was collected by using school reports and questionnaires. 
# Two datasets are provided regarding the performance in two distinct subjects: Mathematics (mat) and Portuguese language (por). 
# The two datasets were modeled under binary/five-level classification and regression tasks.

library(ggplot2)
# MA: Change variable names
mat_ds <- read.csv("student/student-mat.csv", header=TRUE)
por_ds <- read.table("student/student-por.csv", sep=";", header=TRUE)

# MA: Print first 10 rows of each data set
head(mat_ds, n = 10)
head(por_ds, n = 10)

student_ds <- rbind(mat_ds, por_ds)
print(nrow(student_ds)) # 1044 students

head(student_ds, n = 10)
# MA: Description of variables in the final data set
str(student_ds)

#uhh guys there's 386 students in both datasets so the rbind didn't exactly work
# to do any analysis, we should convert rows to numeric as opposed to binary/categorical
# nvm the regression can automatically convert categorical so we good fam.

# The combined student dataset contains 32 variables
# School      | Student's school (binary: "GP" - Gabriel Pereira or "MS" - Mousinho da Silveira)  #need to convert to 1/0
# Sex         | Student's sex (binary: "F" - female or "M" - male)                                #need to convert to 1/0
# Age         | Student's age (numeric: from 15 to 22)
# Address     | Student's home address type (binary: "U" - urban or "R" - rural)                  #need to convert to 1/0
# Famsize     | Family size (binary: "LE3" - less or equal to 3 or "GT3" - greater than 3)        #need to convert to 1/0
# Pstatus     | Parent's cohabitation status (binary: "T" - living together or "A" - apart)       #need to convert to 1/0
# Medu        | Mother's education (numeric: 0 - none,  1 - primary education (4th grade), 2 – 5th to 9th grade, 3 – secondary education or 4 – higher education)
# Fedu        | Father's education (numeric: 0 - none,  1 - primary education (4th grade), 2 – 5th to 9th grade, 3 – secondary education or 4 – higher education)
# Mjob        | Mother's job (nominal: "teacher", "health" care related, civil "services" (e.g. administrative or police), "at_home" or "other")
# Fjob        | Father's job (nominal: "teacher", "health" care related, civil "services" (e.g. administrative or police), "at_home" or "other")
# Reason      | Reason to choose this school (categorical: close to "home", school "reputation", "course" preference or "other") 
# Guardian    | Student's guardian (nominal: "mother", "father" or "other")
# Traveltime - home to school travel time (numeric: 1 - <15 min., 2 - 15 to 30 min., 3 - 30 min. to 1 hour, or 4 - >1 hour)
# Studytime - weekly study time (numeric: 1 - <2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, or 4 - >10 hours)
# Failures - number of past class failures (numeric: n if 1<=n<3, else 4)
# Schoolsup - extra educational support (binary: yes or no)
# Famsup - family educational support (binary: yes or no)
# paid - extra paid classes within the course subject (Math or Portuguese) (binary: yes or no)
# activities - extra-curricular activities (binary: yes or no)
# nursery - attended nursery school (binary: yes or no)
# higher - wants to take higher education (binary: yes or no)
# internet - Internet access at home (binary: yes or no)
# romantic - with a romantic relationship (binary: yes or no)
# famrel - quality of family relationships (numeric: from 1 - very bad to 5 - excellent)
# freetime - free time after school (numeric: from 1 - very low to 5 - very high)
# goout - going out with friends (numeric: from 1 - very low to 5 - very high)
# Dalc - workday alcohol consumption (numeric: from 1 - very low to 5 - very high)
# Walc - weekend alcohol consumption (numeric: from 1 - very low to 5 - very high)
# health - current health status (numeric: from 1 - very bad to 5 - very good)
# absences - number of school absences (numeric: from 0 to 93)

# these grades are related with the course subject, Math or Portuguese:
#31 G1 - first period grade (numeric: from 0 to 20)
#31 G2 - second period grade (numeric: from 0 to 20)
#32 G3 - final grade (numeric: from 0 to 20, output target)

# univariate statistics. we can use bar plots to do some bivariate analysis
summary(student_ds)
# MA: Some basic analysis on the data
# Most of the students in this data set study at Gabriel Pereira (772 out of 1044)
# Female Percent
f_pct <- paste0(nrow(student_ds[student_ds[,"sex"] == "F",]) / nrow(student_ds) * 100, "%")
# 56.6% female students
# Male Percent
m_pct <- paste0(nrow(student_ds[student_ds[,"sex"] == "M",]) / nrow(student_ds) * 100, "%")
# 43.4% male students
# Average grade of female students
f_grade <- mean(student_ds[student_ds[,"sex"] == "F", "G3"])
# 11.44839
# Average grade of male students
m_grade <- mean(student_ds[student_ds[,"sex"] == "M", "G3"])
# 11.20309
#do a t.test between male and female grades.
# t.test between the two schoools. 

# Difference between female and male proportions with respect to certain characteristics
prop.test(table(student_ds$sex, student_ds$activities), correct = FALSE)
#prop test make sure the y/n is on the columns
prop.test(table(student_ds$nursery, student_ds$sex), correct = FALSE)
prop.test(table(student_ds$higher, student_ds$sex), correct = FALSE)
prop.test(table(student_ds$internet, student_ds$sex), correct = FALSE)
prop.test(table(student_ds$romantic, student_ds$sex), correct = FALSE)

# barplots for scores based on various characteristics is the general idea. 
# forgetting how to do it where instead of count, the bar graph shows the mean based on various characterists
# gaaaah train has no internet....
ggplot(student_ds, aes(x = school, fill = G3)) + geom_bar()

pairs(student_ds)
pairs

#what's correlated with each other??? how to correlate categorical variables?
#answer why they are not correlated? do we have anything completely uncorrelated?
#any negative correlations?
# ?pairs should calculate them all but then we'd get a huge 30x30 matrix which is hard to parse


#giant linear model of what best predicts G3 score?
model1 <- lm(student_ds$G3 ~ student_ds$address + student_ds$school + student_ds$romantic + student_ds$famsize + student_ds$Medu)
summary(model1)
# questions to ask. does mom or dad's education status and/or job affect scores more
# looks like having a romantic interest distracts students from studying
# also looks like urban students do better than rural ones
# one of the schools does way better than the other? can we find out how much of that school variation is due to the school vs other characteristics

# MA model
# Negative relationships
summary(lm(formula = student_ds$G3 ~ student_ds$health, data = student_ds))
# p-value: 0.00964 , slope: -0.21723
summary(lm(formula = student_ds$G3 ~ student_ds$Walc, data = student_ds))
# p-value: 0.000178 , slope: -0.34807
summary(lm(formula = student_ds$G3 ~ student_ds$Dalc, data = student_ds))
# p-value: 2.65e-05 , slope: -0.5496
summary(lm(formula = student_ds$G3 ~ student_ds$goout, data = student_ds))
# p-value: 0.00154 , slope: -0.3282
summary(lm(formula = student_ds$G3 ~ student_ds$freetime, data = student_ds))
# p-value: 0.0361 , slope: -0.2431
summary(lm(formula = student_ds$G3 ~ student_ds$romantic, data = student_ds))
# p-value: 0.00146 , slope: -0.7939
summary(lm(formula = student_ds$G3 ~ student_ds$traveltime, data = student_ds))
# p-value: 0.000898 , slope: -0.5421

# Positive relationships
summary(lm(formula = student_ds$G3 ~ student_ds$internet, data = student_ds))
# p-value: 0.0005299 , slope: 1.0192
summary(lm(formula = student_ds$G3 ~ student_ds$higher, data = student_ds))
# p-value: 9.55e-15 , slope: 3.2726
summary(lm(formula = student_ds$G3 ~ student_ds$studytime, data = student_ds))
# p-value: 1.52e-07 , slope: 0.7487
summary(lm(formula = student_ds$G3 ~ student_ds$Fjob, data = student_ds))
# p-value: 0.007 , slope: 1.8454 (Fjobteacher)
summary(lm(formula = student_ds$G3 ~ student_ds$Mjob, data = student_ds))
# p-value: 1.16e-05 , slope: 2.2139 (Mjobhealth)
# p-value: 0.00126 , slope: 1.1920 (Mjobservices)
# p-value: 6.21e-05 , slope: 1.7386 (Mjobteacher)
summary(lm(formula = student_ds$G3 ~ student_ds$Fedu, data = student_ds))
# p-value: 2.1e-07 , slope: 0.5615
summary(lm(formula = student_ds$G3 ~ student_ds$Medu, data = student_ds))
# p-value: 5.05e-11 , slope: 0.6922

# should we convert categorical variables to factors?
mod2 <- lm(formula = student_ds$G3 ~ student_ds$higher + student_ds$studytime + 
             student_ds$Mjob + student_ds$Fedu + student_ds$Medu + student_ds$Walc + 
             student_ds$Dalc + student_ds$traveltime, data = student_ds)
summary(mod2)




















