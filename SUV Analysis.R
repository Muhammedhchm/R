###### Logistic Regression (categorical variables 'nominal ,ordinal ' )
### Predict , estimate categriocal data . 
attach(suv_data_Log)

df = suv_data_Log

# -------------------------------------------
# data cleaning 
# -- - - - - - - - -

is.na(df)
sum(is.na(df))

str(df)




#------------------------------------
############ Y must be ("0","1") 
############ X must be ("0", "1") 


gender = ifelse(df$Gender=="Female",0,1)

gender


table(df$Gender)



# new Data frame 
# --------------

new_df = data.frame(Age, Gender , EstimatedSalary , Purchased , gender )

is.na(new_df)
sum(is.na(new_df))
sum()#----Remove NA 
#----------------------------------------------------------------------------

#Remove rows with NA 
#-------------------

new_df = na.omit(new_df)
str(new_df)
str()#replace NA 

new_df

#----------


str(new_df)

table(new_df$gender) # to calculate frequency 

###################################################################

#calculate mean manually

mean(new_df$df.Loan_Amount_Term)
ggplot(table(new_df$gender) , col ="blue")
plot(new_df$gender , new_df$Purchased)

#====================================================================
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

find_mode(new_df$Age )

mode()########################## ##################################################
##Logistic Regression with one or more than variable 

model <- glm(new_df$Purchased ~ new_df$Age + new_df$EstimatedSalary +
                new_df$Gender  , family = "binomial"  )

summary(model)
round(model$coefficients , 5)

round(exp(model$coefficients) , 5 )
round(exp(confint(model)) , 4)
(exp(model$coefficients) /(exp(model$coefficients)+1))

model2 <- glm(Purchased ~ . ,data = new_df, family = "binomial"  )

summary(model2)
#--------------------------------------------------------
# prediction Probability
#------------------------
predicted2  = data.frame(probability ,  model$fitted.values)


probability  = predict(model , n/ewdata = new_df , type = "response")
probability

pred = new_df5[order(new_df5$probability , decreasing = FALSE)]
datarank = 1:nrow(predicted2)
datarank


new_df = data.frame(new_df ,datarank , probability)


Prob  = ifelse(new_df$probability > 0.5 , "high_prob" , "Low_prob")
Prob

new_df5 = data.frame(new_df , Prob) 
new_df5

high_prob = new_df5[new_df5$probability > 0.5 , ]

view(high_prob)

library(cowplot)
library(ggplot2)



ggplot(data = new_df5, mapping = aes(x = Age, y = probability)) +
  geom_point() +
  geom_hline(yintercept=0.5)+
  geom_point(
    mapping = aes(y = probability), data = new_df5,
    colour = 'red', size = 3 
      
  )


  log(0.731/ ( 1- (0.731)))
#-----------------aes()#--------------------------------------------------------------------------

# Test Accuracy 
#---------------



# create training and testing data 
train_indice =  sample(nrow(new_df), 0.7 * nrow(new_df))
test_indice =  setdiff(1:nrow(new_df), train_indice)

train_data = new_df[train_indice, ]
test_data = new_df[test_indice, ]

is.na(train_data)

glm.status = glm(train_data$status ~   train_data$df.ApplicantIncome + train_data$df.LoanAmount +
                   train_data$gender + train_data$edu + train_data$married 
                 , data = train_data ,family = "binomial" )

glm.status2 = glm(status ~   df.ApplicantIncome + df.LoanAmount +
                    gender + edu + married 
                  , data = train_data ,family = "binomial" )

summary(glm.status2)


model_predict_status_test   = predict(glm.status2  , test_data , type = "response") 

model_predict_status_test



#convert predicted values to True or false 

test_predict_status = ifelse(model_predict_status_test  >= 0.5 , "TRUE" , "False")
test_predict_status

test_predict_status = as.logical(test_predict_status)
test_st = ifelse(as.numeric(test_data$status)  >= 0.7 , "TRUE" , "False")


test_predict_status
test_st

# accuracy 
#----------3
accurcy = mean(test_predict_status == test_st)
accurcy


#--------------------------------------------------------
#odds Ratio 
#----------


pur = ifelse(df$Purchased=="0","not Buy","buy")


table1 <- table(gender = new_df$Gender , purchase = pur)
table1

# -----------
# Risk Ratio
#------------
epitools::riskratio(table1,rev='both',method = 'wald')

epitab(table1,method="riskratio")     ->  # to calculate the percentage
# To calculate the risk of exposure(1) (The right side) 
# the right calcuc = P(0) female  / P(0) male 
#risk ratio = p(1) male / p(1) female 

  # ----------------------#
# Risk ratio (Survival Ratio)
#-----------------------------

riskratio.wald(table1, rev="c")     # survival ratio  # 
# 1/ risk ratio = the reverse of the value  "survival r-atio"


#-----------------------------------------------------------------------
#Odds Ratio
#----------

epitools::oddsratio(table1,rev='both',method = 'wald')    -  #To calculate  

epitab(table1,method="oddsratio")  
  
  
  

log(model2$coefficients)

glm.status3 = glm(new_df$status ~  new_df$edu   ,family = "binomial" )
summary(glm.status3)
round(model1$coefficients , 4)
exp(glm.status3$coefficients)
exp(confint(model2))











anova(model1 , model2 , test = "LRT")


Test <- predict(model2  , type = "response")
summary(Test)



##  Calcumodel1##Calculate the Odds Ratio 
odds <- exp(coef(model1))
odds
#odds num * 100 to get the percentage 
1.5079134 *100

##Calculate the Confident Interval for the Odds Ratio
odd_interval <- exp(confint(model1))
odd_interval

##Calculate the compare between two ratios exp1/exp2
exp(7.656928504)/exp( 4.426268858 )

##Calculate the probability of the prediction and relations (100%) 
exp(coef(model1)) * 100

round(coef(model1) , 3)

round(model1$coefficients , 3)

############################

##ANOVA (Test which variable is important and which is not ,
#     add and remove variable )

model1 <- glm(y ~ x1+x2+x3+x4 , family = 'binomial')
model2 <- glm(y ~ x1+x2+x3 , family = 'binomial')
model3 <- glm(y ~ x1+x2+x4 , family = "binomial")
model4 <- glm(y ~ x1+x3+x4 , family = "binomial")
model5 <- glm(y ~ x2+x3+x4 , family = "binomial")
anova( model1 , model2 , test='LRT')   #LRT comparing 2 models :

#### comparing each model with the main model 'first one' and then 

### looking at p-value if ( p < 0.05 ) then removing this variable
# from the model will decrease the predictive power of the model 
#  (" this variable effect in the model ") .
### if ( p > 0.05 ) then removing this variable will not effect


####################################

### checking confounding 

## 1- first: see the mosaicolot 
# if there is many frequents in other variable that mean 
# that there is an association 

mosaicplot(table( LUNG_CANCER , `ALCOHOL CONSUMING`),
           xlab="cancer" ,
           ylab="allergy" ,
           col= c(2,3,4))

## 2- second : plot and barplot
ggplot(GENDER)
###############################
#### Test the association between 2 categorical variables 
#   which are independent variables in the model 

### 1- calculate PROP for every categorical in another categorical variable  .

table(SMOKING[ALLERGY=="1"]) 
table(SMOKING[ALLERGY=="2"]) 

####the number of smokers in the (allergic people ) ,
## "1" who don't have allergic , "2" who having allergic and that is the  
#number of smokers in this categorical .
table(SMOKING[ALLERGY=="1"])/length(SMOKING[ALLERGY=="1"]) #the percentage 
############## OR do this 
table(SMOKING,ALLERGY) / apply(table(ALLERGY) , FUN = sum , MARGIN =  1)

## 2- Test using chisq.test
chisq.test(table(SMOKING , ALLERGY ))

## 3- Regreesion using glm function 
summary(glm(smo ~ allr , family = "binomial"))

#here is 2 coef for 2 

## 4- calculate the Odds 
exp(-2.829e-15)
exp()
####################################################################