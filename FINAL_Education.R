library(regclass)

data("EDUCATION")
head(EDUCATION)

# X Variables
# Y Variables: CollegeGPA
colnames(EDUCATION)
#edu <- EDUCATION[,c("CollegeGPA","Gender","HSGPA", "ACT" ,"APHours" ,"JobHours" , "School","LanguagesSpoken","HSHonorsClasses",
#                    "SmokeInHS","PayCollegeNoLoans", "ClubsInHS" , "JobInHS" , "Churchgoer", "Height" , "Weight","Family" , "Pet")]
edu <- EDUCATION[,c("CollegeGPA","HSGPA", "ACT" ,"APHours" ,"JobHours" , "School","ClubsInHS" , "JobInHS" )]

dim(edu);head(edu)

# Summary
summary(edu)
boxplot(edu$JobHours)
boxplot(edu$APHours)
boxplot(edu$ClubsInHS)
### comment: almost 50% of students had jobs while they were in highschool.
### comment: JobHours median and mean are very different, box plot shows there are outliers, too.
### comment: APHours has a lot of ourliers, this might cause noise when modeling against CollegeGPA

# convert categorial to binary
### comment: there are binary variables; JobInHS and School, so converting them to 1 or 0 so that we can test out against CollegeGPA
JobInHS_b = c()
for (j in edu$JobInHS){
  if (j == 'Yes') {
    JobInHS_b = c(JobInHS_b, 1)
  }
  else {JobInHS_b = c(JobInHS_b, 0)}
}

School_b = c()
for (s in edu$School){
  if (s == 'Private') {
    School_b = c(School_b, 1)
  }
  else {School_b = c(School_b, 0)}
}

# combining binary converted variables with edu (selected columns from EDUCATION)
edu2 = cbind(edu, JobInHS_b, School_b)
head(edu2)

# drop columns that are not wanted
edu3 = edu2[,c('CollegeGPA', 'HSGPA', 'ACT', 'APHours', 'JobHours', 'School_b')]

# Collinearity, Correlation
pairs(edu3)
### comment: CollegeGPA and HSGPA seem positively correlated, other pairs seem not correlated looking at this plot
cor_matrix(edu3)

all_correlations(edu3,interest='CollegeGPA',sorted="magnitude")
### comment: CollegeGPA and HSGPA seem positively correlated then CollegeGPA against ACT seem having weak positive correlation
### there seem no specific correlation between CollegeGPA with other X variables than HSGPA and ACT

# regressions between collegeGPA vs X variables
### comment: we can start off modeling the highest correlated pair: collegeGPA and HSGPA
plot(edu2$CollegeGPA~edu2$HSGPA)
m_gpas = lm(CollegeGPA~HSGPA, data=edu2)
abline(m_gpas,col="red")
summary(m_gpas)
### comment: How does residual look like? They are spread around 0
plot(m_gpas$residuals~m_gpas$fitted.values,xlab="Fitted value",ylab="Residual"); abline(h=0,col='red')
### comment: The residuals seem following normality
qqnorm(m_gpas$residuals,main="Normal Q-Q plot of residuals"); qqline(m_gpas$residuals,col='red')


### comment: The summary shows that HSGPA is statistically significant. However, R-Squared seems very low.
# X transformation to increase R square
HSGPA_mc <- edu2$HSGPA - mean(edu2$HSGPA)
HSGPA_log <- log10(edu2$HSGPA)
HSGPA_sqrd <- edu2$HSGPA^2

m_gpas2 = lm(CollegeGPA~HSGPA+I(HSGPA^2), data=edu2); summary(m_gpas2)
m_gpas3 = lm(CollegeGPA~HSGPA+I(sqrt(HSGPA)), data=edu2); summary(m_gpas3)

summary(edu2$HSGPA)
which(edu2$HSGPA > 4.5)
which(edu2$HSGPA > 4.5)
summary(edu2$CollegeGPA)
plot(edu2$CollegeGPA~HSGPA_log)


# Add additional variables
m_JobHS = lm(CollegeGPA~HSGPA+JobInHS_b, data=edu2); summary(m_JobHS)
m_JobHS = lm(CollegeGPA~HSGPA+ACT, data=edu2); summary(m_JobHS) ### comment: both are statistically significant
m_clubHS = lm(CollegeGPA~HSGPA+ClubsInHS, data=edu2); summary(m_clubHS)
m_jobhours = lm(CollegeGPA~HSGPA+JobHours, data=edu2); summary(m_jobhours) ### comment: both are statistically significant

### comment: How does residual look like? They are spread around 0
plot(m_jobhours$residuals~m_jobhours$fitted.values,xlab="Fitted value",ylab="Residual"); abline(h=0,col='red')
### comment: The residuals seem following normality
qqnorm(m_jobhours$residuals,main="Normal Q-Q plot of residuals"); qqline(m_jobhours$residuals,col='red')




