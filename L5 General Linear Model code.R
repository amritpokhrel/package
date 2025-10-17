library(ggplot2)

# check out the model (design) matrix for a normal linear regression
# notice that the matrix has an intercept plus one column for each predictor

x<-rnorm(20)
z<-rnorm(20)
y<-x+rnorm(20)
data<-data.frame(y, x, z)
model<-lm(y~x+z, data)
model.matrix(model)
summary(model)

# now lets examine a dataset comprised of two groups and a y response
# simulate a dataset with two groups that have different mean values
a<-rnorm(mean=10, n=10)
b<-rnorm(mean=12, n=10)
data<-data.frame(group=rep(c("a","b"), each=10), y=c(a,b))

# lets make a plot to visualize these groups, 
# you can change the size and base_size numbers if it looks crazy on your computer
ggplot(data, aes(x=group, y=y, col=group))+
  geom_point(size=2)+
  theme_classic(base_size = 10)

# lets make a linear model where we model y as a function of group
model<-lm(y~group, data)
summary(model)

# how does this compare to a t.test? We have to specify that the variance are equal since this is an assumption of linear models
t.test(y~group, data, var.equal = T)
#check out the design matrix. Two important things to note.
# there is not column for group "a" it is the "default" level and its mean is the intercept
# group "b" is 0 and 1. Our observed data in group "b" has a 1, while group "a" has a 0
model.matrix(model)

#for fun lets see how to change the "default" (i.e. intercept) group level
data$group<-factor(data$group, levels=c("b","a"))
levels(data$group)
model<-lm(y~group, data)
#note that the coefficient for groupa is the negative of the slope for groupb in the previous model (before we changed the levels of group)
summary(model)
model.matrix(model)


# Lets do the same for a dataset with 4 groups and y response variable
a<-rnorm(mean=10, n=10)
b<-rnorm(mean=12, n=10)
c<-rnorm(mean=14, n=10)
d<-rnorm(mean=8, n=10)
data<-data.frame(group=rep(c("a","b","c","d"), each=10),  y=c(a,b,c,d))
ggplot(data, aes(x=group, y=y, col=group))+
  geom_point(size=2)+
  theme_classic(base_size = 10)

model<-lm(y~group, data)
summary(model)

#how does this compare to an Analysis of Variance (ANOVA)?
anova(model)
#notice the design matrix. all rows have a 1 in the intercept which is the mean of group "a"
# observations in group "b" have a 1 in that column but not in group "c" and so forth
model.matrix(model)

# if we want to compare all groups against each other
TukeyHSD(aov(model))


# lets examine a dataset with both categorical varibles and linear variables
a<-rnorm(mean=10, sd=0.5, n=20)
b<-rnorm(mean=9, sd=0.5, n=20)
data<-data.frame(group=rep(c("a","b"), each=20), x=c(a,b))
data
data$is_b<-as.numeric(data$group=="b") #needed for simulation
# y is equal to x (plus 2 if it is in group b) , then some residuals (rnorm)
data$y<-(data$x*1+data$is_b*2)+rnorm(40, sd=0.5)

ggplot(data, aes(x=x, y=y, col=group))+
  geom_point(size=2)+
  theme_classic(base_size = 10)

#how strong is the relationship between x and y?
model<-lm(y~x, data)
summary(model)

#how strong is the relationship between group and y?
model<-lm(y~group, data)
summary(model)

#how does the model change when we add group?
model<-lm(y~group+x, data)
summary(model)
#notice the design matrix.
model.matrix(model)

# as a refresher, lets check that the r2 value is still works, even when we have categorical variables
(var(data$y)-var(model$residuals))/var(data$y)


## Lets simulate a data set with interactions between a categorical variable and a continuous variable

a<-rnorm(mean=1, sd=0.5, n=20)
b<-rnorm(mean=1, sd=0.5, n=20)
data<-data.frame(group=rep(c("a","b"), each=20), x=c(a,b))
data
data$is_b<-as.numeric(data$group=="b")
data$y<-(data$x*(1+data$is_b*-2)+data$is_b*2)+rnorm(40, sd=0.3)

ggplot(data, aes(x=x, y=y, col=group))+
  geom_point(size=2)+
  theme_classic(base_size = 10)

#how strong is the relationship between x and y?
model<-lm(y~x, data)
summary(model)

#how strong is the relationship between group and y?
model<-lm(y~group, data)
summary(model)

#how does the model change when we add group?
model<-lm(y~group*x, data)
summary(model)
#but the anova says there's no significant effect of group
anova(model)
#notice the design matrix.
model.matrix(model)

#example why interpreting data is difficult when there is an interaction term
# here we make the main "effect" (that is, intercept difference) of b 0 
data$y<-(data$x*(1+data$is_b*-2)+data$is_b*0)+rnorm(40, sd=0.3)
ggplot(data, aes(x=x, y=y, col=group))+
  geom_point(size=2)+
  theme_classic(base_size = 10)

#lm: no significant difference of groupb and groupa interecepts
model<-lm(y~group*x, data)
summary(model)
#but the anova says there's as a highly significant effect of group
anova(model)

### one more example where the main effect actually matters
# here we make the main "effect" of b -1 but we also give it a 0.5 slope (vs a slope of 2 for the a group)
data$y<-(data$x*(2+data$is_b*-1.5)+data$is_b*-1)+rnorm(40, sd=0.1)
ggplot(data, aes(x=x, y=y, col=group))+
  geom_point(size=2)+
  theme_classic(base_size = 10)

#lm shows significant difference in intercept of groupb
model<-lm(y~group*x, data)
summary(model)
#and the anova also says there's as a highly significant effect of group
anova(model)

# in model matrix with interaction groupb:x is created...
# is 0 when group == "a" and is equal to x when group == "b"
model.matrix(model)
