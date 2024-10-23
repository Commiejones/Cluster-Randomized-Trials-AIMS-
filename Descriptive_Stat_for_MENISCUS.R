library(table1)
library(knitr)
library(kableExtra)
library(stargazer)


# Load the data
load("CSJMeniscus.RData")
df= data.frame(meni)
str(df)
#remove na's
which(is.na(df)==T)
df <- df[complete.cases(df),] ## removes na's
##factor school column 
df1 <-df[order(df$school),] ###reorder the 
head(df1$school, 50)
length(unique(df1$school)) ##checking the unique school num
df1$school<- factor(df1$school) ##factor school column
levels(df1$school)<- seq(from=1, to=60, by=1) ##the factor level from 1 to 60

#using table1 to create a summary table
#summary table for individual-level variables\
label(df1$Muganda)     <- "Muganda"
label(df1$uneb_strata)     <- "UNEB Strata"
label(df1$difficulty)     <- "Difficulty"
label(df1$knowledge)     <- "Knowledge"
label(df1$painmgt)     <- "PAin Management"
label(df1$math_conf)     <- "Math Confidence "
label(df1$science_conf)     <- "Science Confidence "
label(df1$hhsize)     <- "Household Size"
label(df1$cgeduc)     <- "Caregiver Education"
label(df1$religion)     <- "Religion"
label(df1$ageg)     <- "Age group"
label(df1$district)     <- "District"

a <- table1(~ ageg + cgeduc+religion+hhsize+difficulty+knowledge+painmgt+math_conf+science_conf+Muganda | district, data = df1, caption="Descriptive statistics of individual level variables")
print(a)

#summary table for school-level variables
label(df1$uneb_strata)     <- "UNEB Strata"
label(df1$ownership)     <- "School Ownership"
label(df1$dayschool)     <- "School Type"
label(df1$district)     <- "District"

b <- table1(~ uneb_strata+ownership+dayschool | district, data = df1,caption="Descriptive statistics of school level variables")
print(b)
