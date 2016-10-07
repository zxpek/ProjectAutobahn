#- Description of the dataset (dimensions, content of each of the variables).
#- Descriptive statistics (univariate descriptive statistics and bivariate).
#- Correlation analyses
#- Regression
#- _Above all_: you should tell me something interesting about what is going on in the data.
library(ggplot2)
d1=read.csv("student-mat.csv",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d1
d2

d3=rbind(d1,d2)
print(nrow(d3)) # 1044 students
d3
#uhh guys there's 386 students in both datasets so the rbind didn't exactly work
# to do any analysis, we should convert rows to numeric as opposed to binary/categorical
# nvm the regression can automatically convert categorical so we good fam.

#The combined student dataset contains 32 variables
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
summary(d3)
str(d3)

# barplots for scores based on various characteristics is the general idea. 
# forgetting how to do it where instead of count, the bar graph shows the mean based on various characterists
# gaaaah train has no internet....
ggplot(d3,aes(x = school, fill = G3)) + geom_bar()

pairs(d3)
pairs

#what's correlated with each other??? how to correlate categorical variables?
#answer why they are not correlated? do we have anything completely uncorrelated?
#any negative correlations?
# ?pairs should calculate them all but then we'd get a huge 30x30 matrix which is hard to parse


#giant linear model of what best predicts G3 score?
model1 <- lm(d3$G3 ~ d3$address + d3$school + d3$romantic + d3$famsize + d3$Medu)
summary(model1)
# questions to ask. does mom or dad's education status and/or job affect scores more
# looks like having a romantic interest distracts students from studying
# also looks like urban students do better than rural ones
# one of the schools does way better than the other? can we find out how much of that school variation is due to the school vs other characteristics
