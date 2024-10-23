############################# Load the neccessary packages ###############################################################################
library(sjPlot) # for tab_model() function that helps to put the model summaries in a nice table format
library(lme4)  # for lmer() function for fitting linear mixed models
library(jtools)  # summ() function for summary
library(lmerTest)
library(interactions) ##to plot the fixed effect
library(ggplot2)  #for visualizations
library(stargazer)    #used togenerate latex code for the model summary
library(dplyr)       # for cleaning the data 
library(report)     #generate report summary of the model
library(tidyverse)    # for cleaning the data
library(flexplot)  # for model.comparison() function, icc () function and visualize () function for model comparison and visualization of the lmer models
library(caret)  ###here we used the Preprocess function to normalize(rescale) the data from 0 to 1
library(data.table) #used in set in new column names and concatenating to the dataframe

# Load the data
load("CSJMeniscus.RData")
df= data.frame(meni)
str(df)
##############################Data Cleaning#############################################################################3
#remove na's
which(is.na(df)==T)
df <- df[complete.cases(df),]
##factor school column 
df1 <-df[order(df$school),] ###reorder the 
head(df1$school, 50)
length(unique(df1$school)) ##checking the unique school num
df1$school<- factor(df1$school) ##factor school column
levels(df1$school)<- seq(from=1, to=60, by=1) ##the factor level from 1 to 60

###checking missing values in the columns
colSums(is.na(df1))

#look at the dataset
head(df1,20)
#variables that vary within a cluster(school) in the dataset include
#ageg, cgeduc, age_first, dayschool, religion, mugandanda, hhsize, difficulty, knowledge , painmgt, math-conf and science_conf

#variables that don't vary are 
#uneb_strata, distrit, ownership. These variables must not be used to model random effects

########################Basic statistics##############################
#check the mean for the math conf 
mean(df1$math_conf)
#check the variance for the math conf
var(df1$math_conf)
sd(df1$math_conf)
range(df1$math_conf)
range(df1$difficulty)
range(df1$knowledge)
range(df1$science_conf)
#hence we need to standardize math confidence score to have a mean=0 and  sd=1
########################## Scaling the continous variables by normalizing it to take values from 0 to 1################################################33 
math_conf_new <- preProcess(as.data.frame(df1$math_conf), method = "range")
math_conf_new <- predict(math_conf_new, as.data.frame(df1$math_conf))


difficulty_new <- preProcess(as.data.frame(df1$difficulty), method = "range")
difficulty_new <- predict(difficulty_new, as.data.frame(df1$difficulty))

science_conf_new <- preProcess(as.data.frame(df1$science_conf), method = "range")
science_conf_new <- predict(science_conf_new, as.data.frame(df1$science_conf))

knowledge_new <- preProcess(as.data.frame(df1$knowledge), method = "range")
knowledge_new <- predict(knowledge_new, as.data.frame(df1$knowledge))

df1 <- cbind(df1, math_conf_new, difficulty_new, science_conf_new, knowledge_new)


# Specify the new column names
new_names <- c("math_conf_new", "difficulty_new", "science_conf_new", "knowledge_new")

# Rename columns using setnames()
setnames(df1, old = c("df1$math_conf", "df1$difficulty", "df1$science_conf", "df1$knowledge"), new = new_names)

#check the mean for the math conf 
mean(df1$math_conf_new)
#check the variance for the math conf
var(df1$math_conf_new)
sd(df1$math_conf_new)
range(df1$math_conf_new)
range(df1$difficulty_new)
range(df1$knowledge_new)
range(df1$science_conf_new)


###########################Fitting Random effect models######################################################################
#research question: What variables affect the math confidence scores of girls

#random anova model(null model)with no explanatory variable
null_model = lmer(math_conf~1+(1|school), data=df1)
summary(null_model)
icc(null_model)
tab_model(null_model)
visualize(null_model,plot="model") ##this fit a mean(average) math confidence score for the entire clusters and for each individual
report(null_model)


# first we want to see if difficulty has an impact on the math confidence score and since it varies within  cluster
# we will model it as a random effect too
full_model = lmer(math_conf_new~difficulty_new+(difficulty_new|school), data=df1 )
allFit(full_model) ## check other optimizers that can work
control =lmerControl(optimizer = "nmkbw")
full_model = lmer(math_conf_new~difficulty_new+(difficulty_new|school), data=df1,control = control)
tab_model(full_model)
summary(full_model)
icc(full_model)
report(full_model)

visualize(full_model,plot="model", formula=math_conf_new~difficulty_new|school)+ 
  xlab("Difficulty") + 
  ylab("Math Confidence")+ 
  scale_color_manual(values = rainbow(10))

visualize(full_model,plot="model", formula=math_conf_new~difficulty_new+school, sample=10, alpha=0.8,lwd = 2)+ 
  xlab("Difficulty") + 
  ylab("Math Confidence")+theme(axis.title.x = element_text(size = 16,face = "bold"),
                                axis.title.y = element_text(size = 16,face = "bold"),
                                axis.text.x = element_text(size = 16,face = "bold"),
                                axis.text.y = element_text(size = 16,face = "bold"),
                                legend.title = element_text(size = 16,face = "bold"),
                                legend.text = element_text(size = 16,face = "bold"),
                                plot.title = element_text(size = 16, face = "bold"))



#here we want to check if it random effect was neccessary
reduced_model = lmer(math_conf_new~difficulty_new+(1|school), data=df1,control = control )
summary(reduced_model)
icc(reduced_model)
tab_model(reduced_model)
report(reduced_model)

#comparing models
model.comparison(full_model,null_model)
anova(full_model,null_model)
# full model is better

model.comparison(full_model,reduced_model)
##based on the bic and bayes the reduced model seems better
anova(full_model, reduced_model)
# based on the aic the full_model is better
#we choose the full model or use the two 

##lets create the groupmean of the difficulty scores for each cluster
df1$difficulty_newMean <- group_mean(df1$difficulty_new,df1$school) 
# since this is fixed for each cluster then we don't add it as a random effect
full_model1= lmer(math_conf_new~difficulty_new+difficulty_newMean+(1|school), data=df1,control = control )
# comparing with the reduced model we have that
model.comparison(full_model1,reduced_model)
#based on the aic, bic and bayes factor the reduced model is better
anova(full_model1, reduced_model)
# reduced model is better based on AIC and BIC
model.comparison(full_model1,full_model)
#full model is better based on aic , bic and bayes factor

anova(full_model,full_model1, reduced_model)
#the full_model is better hence we keep the full_model

visualize(full_model,plot="model", formula=math_conf_new~difficulty_new|school)
visualize(full_model,plot="model", formula=math_conf_new~difficulty_new+school, sample=10, alpha=0.08,lwd = 2)
icc(full_model)
summary(full_model)

########random effect test####
anova(full_model, reduced_model)
anova(full_model,reduced_model,refit=FALSE)
mean(pchisq(7.979, df = c(1, 2), lower.tail = FALSE))


#################################################################################################################################
# now that we have control for difficulty we want to see how do math_conf scores differ in based on schooltype  
# since dayschool is a factored variable and it varies within group we only add it as a fixed effect 
#since we have a singular  problem if we add as a random  effect 

full_model2 = lmer(math_conf_new~difficulty_new+dayschool+(dayschool|school), data=df1 )
summary(full_model2)
icc(full_model2)
tab_model(full_model2)
visualize(full_model2,plot="model", formula=math_conf_new~difficulty_new|dayschool+school)
visualize(full_model2,plot="model", formula=math_conf_new~difficulty_new+school|dayschool, sample =10, alpha=0.08,lwd = 2)

full_model2a = lmer(math_conf_new~difficulty_new+dayschool+(difficulty_new|school), data=df1,control = control )
summary(full_model2a)
icc(full_model2a)
tab_model(full_model2a)
visualize(full_model2a,plot="model", formula=math_conf_new~difficulty_new|dayschool+school)
visualize(full_model2a,plot="model", formula=math_conf_new~difficulty_new+school|dayschool, sample =10, alpha=0.5,lwd = 2)+
  xlab("Difficulty") + 
  ylab("Math Confidence")+theme(axis.title.x = element_text(size = 16,face = "bold"),
                                axis.title.y = element_text(size = 16,face = "bold"),
                                axis.text.x = element_text(size = 16,face = "bold"),
                                axis.text.y = element_text(size = 16,face = "bold"),
                                legend.title = element_text(size = 16,face = "bold"),
                                legend.text = element_text(size = 16,face = "bold"),
                                plot.title = element_text(size = 16, face = "bold"),
                                strip.text = element_text(size = 16, face = "bold"))


interact_plot(full_model2,pred=difficulty_new, modx = dayschool)
reduced_model2 = lmer(math_conf_new~difficulty_new+dayschool+(1|school), data=df1,control = control )
summary(reduced_model2)
icc(reduced_model2)
tab_model(reduced_model2) 
# we compare full_model2 with the full model4a
model.comparison(full_model2,full_model2a)
#full_model2a is best

# we compare full_model2 with the reduced_model2 
model.comparison(reduced_model2 ,full_model2a)
##reduced_model2a is best based on bic and bayes factor
## full_modle4a is best based on aic



# we compare full_model2a with the full model
model.comparison(full_model2a,full_model)


# we compare reduced_model2 with the full model
model.comparison(reduced_model2,full_model)

# we compare full_model2a and reduced_model2 with with the full model
anova(full_model2a,reduced_model2,full_model)

########LRT  for random effect#######
anova(full_model2,reduced_model2,refit=FALSE)
mean(pchisq(0.4337, df = c(1, 2), lower.tail = FALSE))


###########################################################################################################################################
# now that we have control for difficulty we want to see how do math_conf scores differ in based on knowledge score
# since knowledege is a continouis variable and it varies within group we can add it as a random effect
full_model3 = lmer(math_conf_new~difficulty_new+knowledge_new+(difficulty_new+knowledge_new|school), data=df1, control=control)
summary(full_model3)
tab_model(full_model3)
icc(full_model3)
visualize(full_model3,plot="model", formula=math_conf_new~knowledge_new|school)
visualize(full_model3,plot="model", formula=math_conf_new~knowledge_new+school, sample =10, alpha=0.5,lwd = 2)+
  xlab("Knowledge") + 
  ylab("Math Confidence")+theme(axis.title.x = element_text(size = 16,face = "bold"),
                                axis.title.y = element_text(size = 16,face = "bold"),
                                axis.text.x = element_text(size = 16,face = "bold"),
                                axis.text.y = element_text(size = 16,face = "bold"),
                                legend.title = element_text(size = 16,face = "bold"),
                                legend.text = element_text(size = 16,face = "bold"),
                                plot.title = element_text(size = 16, face = "bold"))


full_model3a = lmer(math_conf_new~difficulty_new+knowledge_new+(difficulty_new|school), data=df1, control=control )

summary(full_model3a)
tab_model(full_model3a)
icc(full_model3a)  
visualize(full_model3a,plot="model", formula=math_conf_new~knowledge_new|school)
visualize(full_model3a,plot="model", formula=math_conf_new~knowledge_new+school, sample =10, alpha=0.1,lwd = 2)+
  xlab("Difficulty") + 
  ylab("Math Confidence")

full_model3b = lmer(math_conf_new~difficulty_new+knowledge_new+(knowledge_new|school), data=df1,control=control,REML=FALSE)
#allFit(full_model3b)
summary(full_model3b)
visualize(full_model3b,plot="model", formula=math_conf_new~knowledge_new|school)
visualize(full_model3b,plot="model", formula=math_conf_new~knowledge_new+school, sample =10, alpha=0.1,lwd = 2)+
  xlab("Knowledge") + 
  ylab("Math Confidence")

reduced_model3 = lmer(math_conf_new~difficulty_new+knowledge_new+(1|school), control=control,data=df1 )
summary(reduced_model3)
tab_model(reduced_model3)
icc(reduced_model3) 
visualize(reduced_model3,plot="model", formula=math_conf_new~knowledge_new+school,sample=10)
#so we we fit a model with only knowledge as explanatory variables to check
reduced_model3a = lmer(math_conf_new~knowledge_new+(1|school), control=control,data=df1 )

#model comparisons
model.comparison(full_model3,full_model3a)
#full_model3a is the best

model.comparison(full_model3a,full_model3b)
#full_model3a is the best

model.comparison(reduced_model3,reduced_model3a)
#reduced_model3 is the best

#comparing the models
model.comparison(full_model3a,reduced_model3)
#full_model3a is the best based on aic 
#but reduced_model3 is the best based on bic and bayes factor

#comparing with full_model
model.comparison(full_model,reduced_model3)
#reduced model9 is best

model.comparison(full_model,full_model3a)
#full_model3a is best

anova(full_model,full_model3a,reduced_model3)

########LRT  for random effect#######
anova(full_model3,reduced_model3,refit=FALSE)
mean(pchisq(9.9752, df = c(1, 5), lower.tail = FALSE))


anova(full_model3a,reduced_model3,refit=FALSE)
mean(pchisq(6.3403, df = c(1, 2), lower.tail = FALSE))
#################################################################################################################################
# now that we have control for difficulty we want to see how do math_conf scores differ in based on school ownership
# since ownership is a binary factored variable and it is fixed within group so we model it as a fixed effect only

full_model4 = lmer(math_conf_new~difficulty_new+ownership+(difficulty_new|school), data=df1)
summary(full_model4)
tab_model(full_model4)
icc(full_model4)
visualize(full_model4,plot="model", formula=math_conf_new~difficulty_new|ownership+school)
visualize(full_model4,plot="model", formula=math_conf_new~difficulty_new+school|ownership, sample =60, alpha=0.1,lwd = 2)

full_model4a = lmer(math_conf_new~difficulty_new+ownership+knowledge_new+(1|school), data=df1 )
summary(full_model4a)
tab_model(full_model4a)
icc(full_model4a)

visualize(full_model4a,plot="model", formula=math_conf_new~knowledge_new|ownership+school)
visualize(full_model4a,plot="model", formula=math_conf_new~knowledge_new+school|ownership, sample =10, alpha=0.3,lwd = 2)+ 
          xlab("Knowledge") + 
          ylab("Math Confidence")+theme(axis.title.x = element_text(size = 16,face = "bold"),
                                        axis.title.y = element_text(size = 16,face = "bold"),
                                        axis.text.x = element_text(size = 16,face = "bold"),
                                        axis.text.y = element_text(size = 16,face = "bold"),
                                        legend.title = element_text(size = 16,face = "bold"),
                                        legend.text = element_text(size = 16,face = "bold"),
                                        plot.title = element_text(size = 16, face = "bold"),
                                        strip.text = element_text(size = 16, face = "bold"))
interact_plot(full_model4a,pred=knowledge_new, modx = ownership)

#model comparison

model.comparison(full_model4,full_model4a)
#full_model4a is better
anova(full_model4,full_model4a,full_model3a)
#full_model3a is better and full_model4a is better

############Model Comparisons for the fixed effects################################
anova(null_model,reduced_model,full_model,reduced_model2,full_model2,full_model2a,reduced_model3,full_model3a,full_model3,full_model4a,full_model4)

############Model Comparisons for the Random effects################################
anova(null_model,full_model,reduced_model,full_model2,full_model2a,reduced_model2,full_model3,full_model3a,reduced_model3,full_model4,full_model4a, refit=FALSE)

anova(reduced_model,full_model) ##RQ1
anova(reduced_model2,full_model2, full_model2a) ##RQ2
anova(reduced_model3,full_model3a, full_model3) ##RQ3
anova( full_model4,full_model4a) ##RQ4

########LRT  for random effect#######
anova(reduced_model,full_model, refit=FALSE) ##RQ1
mean(pchisq(7.979, df = c(1, 2), lower.tail = FALSE))

anova(reduced_model2,full_model2, full_model2a, refit=FALSE) ##RQ2
mean(pchisq(0.4337, df = c(1, 2), lower.tail = FALSE))

anova(reduced_model3,full_model3a, full_model3, refit= FALSE) ##RQ3
mean(pchisq(6.3403, df = c(1, 2), lower.tail = FALSE))
mean(pchisq(3.6349, df = c(1, 3), lower.tail = FALSE))

anova(full_model4a,full_model4,refit=FALSE) ##RQ4
mean(pchisq(0, df = c(1, 1), lower.tail = FALSE))














#####################################################Other models that were fitted##################################################################
# now that we have control for difficulty we want to see how do math_conf scores differ in age group in school
# since ageg is a factored variable and it varies within group we only add it as a fixed effect 
#since we have a singular  problem if we add as a random  effect

full_model5 = lmer(math_conf_new~difficulty_new+ageg+(difficulty_new|school), data=df1,control = control )
summary(full_model5)
visualize(full_model5,plot="model", formula=math_conf_new~difficulty_new|ageg+school)
visualize(full_model5,plot="model", formula=math_conf_new~difficulty_new+school|ageg, sample =10, alpha=0.08,lwd = 2)
print(full_model5)

#full model is still the best based on aic , bic 
anova(full_model5,full_model)
#full model is still the best based on aic , bic 
# hence we still stick with the full_model
reduced_model5 = lmer(math_conf_new~difficulty_new+ageg+(1|school), data=df1,control = control )
anova(full_model5,reduced_model5,full_model)
#full model is still the best based on aic , bic 
# hence we still stick with the full_model


########################################################################################################################################
# now that we have control for difficulty we want to see how do math_conf scores differ in level of caregiver education  in school
# since cgeduc is a factored variableand it varies within group so we can add as random effect  

full_model6 = lmer(math_conf_new~difficulty_new+cgeduc+(difficulty_new+cgeduc|school), data=df1,control = control )
summary(full_model6)
visualize(full_model6,plot="model", formula=math_conf_new~difficulty_new|cgeduc+school)
visualize(full_model6,plot="model", formula=math_conf_new~difficulty_new+school|cgeduc, sample =10, alpha=0.8,lwd = 2)
# lets try to see if the random effect was needed 
reduced_model6 = lmer(math_conf_new~difficulty_new+cgeduc+(difficulty_new|school), data=df1,control = control )

#comparing the full_model3 and reduced_model3 models

model.comparison(full_model6,reduced_model6)
# based on the aic, bic the reduced model is better
anova(full_model6,reduced_model6)
# based on the aic, bic the reduced model is better
# hence there was no need to add the cgeduc as a random effect
# so we choose the reduced_model6

#comparing the reduced_model3 with the full_model
model.comparison(full_model,reduced_model6)
#full model is still the best based on aic, bic 
anova(full_model,reduced_model6)
#full model is still the best based on aic, bic 
# so we stick with the full model



###################################################################################################################################
# now that we have control for difficulty  and school type we want to see how do math_conf scores differ in based on religion  
# since religion is a factored variable and it varies within group we only add it as a fixed effect 
#since we have a singular  problem if we add as a random  effect 
full_model7 = lmer(math_conf_new~difficulty_new+dayschool+religion+(difficulty_new|school), data=df1,control = control )
visualize(full_model7,plot="model", formula=math_conf_new~difficulty_new|religion+school,lwd=2)
visualize(full_model7,plot="model", formula=math_conf_new~difficulty_new+school|religion, sample =10, alpha=0.08,lwd = 2)

reduced_model7= lmer(math_conf_new~difficulty_new+religion+(difficulty_new|school), data=df1,control = control )
reduced2_model7 =lmer(math_conf_new~religion+(religion|school), data=df1 )

allFit(reduced2_model7)
control2 =lmerControl(optimizer = "Nelder_Mead")
reduced2_model7 =lmer(math_conf_new~religion+(religion|school), data=df1,control = control2 )
summary(reduced2_model7 )
#comparing between reduced2_model5 and reduced_model5
model.comparison(reduced2_model7,reduced_model7)
#reduced_model5 is best based on aic, bic and bayes factor

#comparing between full_model4 and reduced2_model7
model.comparison(full_model6,reduced2_model7)
#full_model6 is best based on aic, bic, and bayes factor

#comparing between full_model4 and reduced_model5
model.comparison(full_model6,reduced_model7)
anova(full_model6,reduced_model7)
# this shows that reduced_model5 is best based on bic and bayes factor 
# and full_model6 is best based on aic

# comparing with the full model and reduced_model7 we have that
model.comparison(full_model,reduced_model7)
#full model is better

# comparing with the full model and full_model6 we have that
model.comparison(full_model,full_model6)
#full model is better


anova(full_model,full_model6,reduced_model7)
#full model is better

############################################################################################################################
# now that we have control for difficulty we want to see how do math_conf scores differ in based on ethnicity  
# since Muganda is a factored variable and it varies within group we only add it as a fixed effect 
#since we have a singular  problem if we add as a random  effect 
full_model8 = lmer(math_conf_new~difficulty_new+Muganda+(Muganda|school), data=df1,control = control )

visualize(full_model8,plot="model", formula=math_conf_new~difficulty_new|Muganda+school)
visualize(full_model8,plot="model", formula=math_conf_new~difficulty_new+school|Muganda, sample =10, alpha=0.08,lwd = 2)

full_model8a = lmer(math_conf_new~difficulty_new+Muganda+(difficulty_new|school), data=df1,control = control )
visualize(full_model6a,plot="model", formula=math_conf_new~difficulty_new|Muganda+school)
visualize(full_model6a,plot="model", formula=math_conf_new~difficulty_new+school|Muganda, sample =10, alpha=0.08,lwd = 2)

reduced_model8 = lmer(math_conf_new~difficulty_new+Muganda+(1|school), data=df1,control = control )


# fit a reduced model without the difficulty variable
reduced_model8a = lmer(math_conf_new~Muganda+(1|school), data=df1,control = control )

# comparing the two models
model.comparison(full_model8,full_model8a)
#full_model68a is better

# comparing the two models
model.comparison(reduced_model8,reduced_model8a)
#reduced_model8 is better

# comparing the two models
model.comparison(full_model8a,reduced_model8)
# the full model8a is better in aic while reduced_model8 is btter in terms of bic and bayes factor


# we compare the full_model6 with the full_model
model.comparison(full_model8a,full_model)
# the full model is better
anova(full_model8a,reduced_model8,full_model)
# the full model is better


###################################################################################################################################
# now that we have control for difficulty we want to see how do math_conf scores differ in based on household size 
# since hhsize is a factored variable and it varies within group we only add it as a fixed effect 
#since we have a singular  problem if we add as a random  effect 
full_model9 = lmer(math_conf_new~difficulty_new+hhsize+(difficulty_new+hhsize|school), data=df1,control = control2 )

visualize(full_model9,plot="model", formula=math_conf_new~difficulty_new|hhsize+school)
visualize(full_model9,plot="model", formula=math_conf_new~difficulty_new+school|hhsize, sample =10, alpha=0.1,lwd = 2)

full_model9a = lmer(math_conf_new~difficulty_new+hhsize+(difficulty_new|school), data=df1,control = control )

visualize(full_model9a,plot="model", formula=math_conf_new~difficulty_new|hhsize+school)
visualize(full_model9a,plot="model", formula=math_conf_new~difficulty_new+school|hhsize, sample =10, alpha=0.1,lwd = 2)

full_model9b = lmer(math_conf_new~difficulty_new+hhsize+(hhsize|school), data=df1,control = control )
visualize(full_model9b,plot="model", formula=math_conf_new~difficulty_new|hhsize+school)
visualize(full_model9b,plot="model", formula=math_conf_new~difficulty_new+school|hhsize, sample =10, alpha=0.1,lwd = 2)

reduced_model9 = lmer(math_conf_new~difficulty_new+hhsize+(1|school), data=df1,control = control )

reduced_model9a = lmer(math_conf_new~hhsize+(1|school), data=df1,control = control )

#model comparisons
model.comparison(full_model9,full_model9a)
#full_model9a is better

model.comparison(full_model9a,full_model9b)
#full_model9a is better

model.comparison(reduced_model9,reduced_model9a)
#reduced_model9 is better

# comparing the two models
model.comparison(full_model9a,reduced_model9)
#full_model9 is better based on aic, which reduced_model9 is better in terms of bic and bayes factor

#comparing with the full_model
model.comparison(full_model9a,full_model)
#full_model is better based on aic, bic and bayes factor

model.comparison(reduced_model9,full_model)
#full_model is better based on aic, bic and bayes factor



####################################################################################################################################
# now that we have control for difficulty we want to see how do math_conf scores differ in based on painmgt
# since painmgt is a factored variable and it varies within group we can add it as  random effect
full_model10 = lmer(math_conf_new~difficulty_new+painmgt+(difficulty_new|school), data=df1,control = control )
summary(full_model8)
visualize(full_model10 ,plot="model", formula=math_conf_new~difficulty_new|painmgt+school)
visualize(full_model10 ,plot="model", formula=math_conf_new~difficulty_new+school|painmgt, sample =10, alpha=0.1,lwd = 2)

full_model10a = lmer(math_conf_new~difficulty_new+painmgt+(painmgt|school),control = control, data=df1 )
summary(full_model10a)
visualize(full_model10a,plot="model", formula=math_conf_new~difficulty_new|painmgt+school)
visualize(full_model10a,plot="model", formula=math_conf_new~difficulty_new+school|painmgt, sample =10, alpha=0.1,lwd = 2)

reduced_model10 = lmer(math_conf_new~difficulty_new+painmgt+(1|school), data=df1,control = control )
reduced_model10a = lmer(math_conf_new~painmgt+(1|school), data=df1,control = control )
#model comparisons
model.comparison(full_model10,full_model10a) 
#full_model10 is the best

# comparing  with reduced
model.comparison(reduced_model10,reduced_model10a) 
#reduced_model10 is the best


model.comparison(full_model10,reduced_model10) 
#reduced_model10 is the best based on bic and bayes factor while full_model8 is best based on aic


#full_model10 is  best
model.comparison(full_model10,full_model)
#full_model is better based on aic, bic and bayes factor

model.comparison(full_model,reduced_model10)
#full_model is better based on aic, bic and bayes factor





##########################################################################################################################################
# now that we have control for difficulty we want to see how do math_conf scores differ in based on science confidence score
# since science_conf is a continous variable and it varies within group we can add it as a random effect

full_model11 = lmer(math_conf_new~difficulty_new+knowledge_new+science_conf_new+(difficulty_new+science_conf_new|school), data=df1,control=control )
visualize(full_model11,plot="model", formula=math_conf_new~science_conf_new|school)
visualize(full_model11,plot="model", formula=math_conf_new~science_conf_new+school, sample =10, alpha=0.1,lwd = 2)

full_model11a = lmer(math_conf_new~difficulty_new+knowledge_new+science_conf_new+(difficulty_new|school), data=df1,control=control )
visualize(full_model11a,plot="model", formula=math_conf_new~science_conf_new|school)
visualize(full_model11a,plot="model", formula=math_conf_new~science_conf_new+school, sample =10, alpha=0.1,lwd = 2)

full_model11b = lmer(math_conf_new~difficulty_new+knowledge_new+science_conf_new+(science_conf_new|school), data=df1,control=control )
visualize(full_model11b,plot="model", formula=math_conf_new~science_conf_new|school)
visualize(full_model11b,plot="model", formula=math_conf_new~science_conf_new+school, sample =10, alpha=0.1,lwd = 2)

reduced_model11 = lmer(math_conf_new~difficulty_new+knowledge_new+science_conf_new+(1|school), data=df1,control=control )
reduced_model11a = lmer(math_conf_new~difficulty_new+science_conf_new+(1|school), data=df1,control=control )
reduced_model11b = lmer(math_conf_new~knowledge_new+science_conf_new+(1|school), data=df1,control=control )
reduced_model11c = lmer(math_conf_new~science_conf_new+(1|school), data=df1,control=control )

#model comparison
model.comparison(full_model11,full_model11a)
#full_model10a is best

model.comparison(full_model11a,full_model11b)
#full_model10a is best

model.comparison(reduced_model11,reduced_model11a)
#reduced_model10 is best

model.comparison(reduced_model11,reduced_model11b)
#reduced_model10 is best

model.comparison(reduced_model11,reduced_model11c)

#reduced_model10 is best

model.comparison(full_model11a,reduced_model11)

#model.comparison(full_model11a,full_model3a)

#model.comparison(full_model3a,reduced_model10)

#anova(full_model10a,reduced_model10,full_model3a,reduced_model3,full_model)
#full_model3a is best



######################################################################################################################################
# now that we have control for difficulty we want to see how do math_conf scores differ in based on UNEB strata score high/low
# since UNEB strata is a binary factored variable and it is fixed within group so we model it as a fixed effect only

full_model12 = lmer(math_conf_new~difficulty_new+uneb_strata+(difficulty_new|school), data=df1,control =control )
visualize(full_model12,plot="model", formula=math_conf_new~difficulty_new|uneb_strata+school)
visualize(full_model12,plot="model", formula=math_conf_new~difficulty_new+school|uneb_strata, sample =10, alpha=0.1,lwd = 2)

full_model12a = lmer(math_conf_new~difficulty_new+uneb_strata+knowledge_new+(knowledge_new|school), data=df1,control =control )

visualize(full_model12a,plot="model", formula=math_conf_new~knowledge_new|uneb_strata+school)
visualize(full_model12a,plot="model", formula=math_conf_new~knowledge_new+school|uneb_strata, sample =10, alpha=0.1,lwd = 2)

#model comparison

model.comparison(full_model12,full_model12a)
#anova(full_model12,full_model12a,full_model3a)
#full_model3a is better 
 

 
 

 
 ######################################################################################################################################
 # now that we have control for difficulty we want to see how do math_conf scores differ in based on district
 # since district is a binary factored variable and it is fixed within group so we model it as a fixed effect only
 
 full_model13 = lmer(math_conf_new~difficulty_new+district+(difficulty_new|school), data=df1,control =control )
 summary(full_model13)
 visualize(full_model13,plot="model", formula=math_conf_new~difficulty_new|district+school)
 visualize(full_model13,plot="model", formula=math_conf_new~difficulty_new+school|district, sample =10, alpha=0.1,lwd = 2)
 
 full_model13a = lmer(math_conf_new~difficulty_new+district+knowledge_new+(1|school), data=df1 )
 visualize(full_model13a,plot="model", formula=math_conf_new~knowledge_new|district+school)
 visualize(full_model13a,plot="model", formula=math_conf_new~knowledge_new+school|district, sample =10, alpha=0.1,lwd = 2)
 
 #model comparison
 
 model.comparison(full_model13, full_model13a)
 #full_model13a is better
 
 model.comparison(full_model4a, full_model13a)
 #full_model13a is better
 model.comparison(full_model13a, full_model3a)
 #full_model13a is better 
 anova(full_model13,full_model13a,full_model3a,full_model4a)
 #full_model13a is better 
 
 