     
 ##### 1- enter variables 

x1<- (building_examples$Gender) 
x2<- (building_examples$`Type of property`)
x3<- as.numeric(building_examples$`Area (ft.)`)
x4<- (building_examples$`Year of sale`) 
x5 <- as.numeric(building_examples$`Age at time of purchase`) 0
y<- (building_examples$Price)

                    ##################################

  ### linear regression with plot and fitted varlues , and ggplot

model1 = lm( y ~ x )
summary(model1)
plot(building_examples , aes(x=`Area (ft.)`,y=price))
abline(model1 , col = 'blue')
fitted.values(model1)
coef(model1)


# ggplot with linear and nonlinear 
data<-data.frame(y,x3,x4,x1)
ggplot(data = car , aes(x = years , y = prices )) +
  geom_line()+
  geom_point()

##############################################################################

  ## Multiple Regression analysis for numeric data (more than 2 variables)

Model1 = lm(c ~ x1 + x2 + x3 + x4)

plot(y~x1)
abline(Model1 , col='blue')

plot(y~x2)
abline(Model2 , col='blue')

plot(y~x3)
abline(Model3 , col='blue')

                #########################################
### if there is a categorical variables in X (independent variables )
x <- factor(building_examples$Gender, 
            levels = c("M", "F"), 
            labels = c("0", "1"))
model2 <- lm(building_examples$Price ~ x)
summary(model2)
plot(model2)

### ordinal 
x <- factor(building_examples$rank , 
            levels = c("bad", "good" , "very good"), 
            labels = c("0", "1","0")) # if ian interested in 'good'

x <- factor(building_examples$rank , 
            levels = c("bad", "good" , "very good"), 
            labels = c("1", "0","0")) # if ian interested in 'bad'

x <- factor(building_examples$rank , 
            levels = c("bad", "good" , "very good"), 
            labels = c("0", "0","1")) # if ian interested in 'very good'

                #########################################

##### Non Linear Regression model for ALL

names(movie_revenues)
attach()
library (ggplot2)

ggplot(data = movie_revenues , aes(x = Production_Budget , y = Worldwide_Gross )) +
  geom_point()+
  labs(x="production(in million)", y= "gross(in  million)") +
  geom_smooth(method = "gam", formula = y~I(x^3) , col='red')+
  geom_smooth(method = "gam", formula = y~log(x) , col="black")+
  geom_smooth(method = "gam", formula = y~I(x^2) , col="yellow")+
  geom_smooth(method = "gam", formula = y~I(x^1) , col = " green")


geom_smooth(method = "lm") ## for linear Reression 
geom_smooth(method =  " gam")  ## for non linear regression 


##########################################################################
    ##### notes : 

xcars$weight <- as.numeric(gsub("\\.", "", cars$weight))
cars$horsepower <- as.numeric(gsub("\\.", "", cars$horsepower))



is.na(x1) #is it a null , NA or missing variables in data 

# change variables or lists
x1_new = x1[x1 == 'M','F'] 
x1_new[x1_new == "M"]<- '0' 
x1_new[x1_new =='F']<-'1'
x1_new <- as.factor( x1_new)
x1_new

str(x1_new)
model4<- model.matrix(~x1)
model3<- lm (y~x1_new) 
summary(model4)

xcars$weight <- as.numeric(gsub("\\.", "", cars$weight))
cars$horsepower <- as.numeric(gsub("\\.", "", cars$horsepower))