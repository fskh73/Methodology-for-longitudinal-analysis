library(mdhglm)
library(Matrix)
library(boot)
library(mvtnorm)
require(ggplot2)
library(ggplot2) 
library(gtable)
library(scales)
library(Rcpp)
library(munsell)
library(colorspace)
library(digest)
library(lazyeval)
library(tibble)
library(pillar)
library(crayon)
library(vctrs)
library(zeallot)
library(pkgconfig)
library(methods)     
library(labeling)
library(dplyr)
library(lattice)

#Dataset used in Section 2.4 : Example of orthodontic growth#
data(Orthodont)

# Figure2.3: Plot of orthodontic growth against age for 27 children and the observations for 
# each individual are connected over time. And the red solid lines indicate the growth trajectory 
#for male and blue solid lines for female.
oplot<- ggplot(data = Orthodont, aes(x = age, y = distance, group = Subject,colour=Sex))
oplot+geom_line()+geom_point()

# Install R package that used to fit linear mixed model#
install.packages("lme4",
                 repos=c("http://lme4.r-forge.r-project.org/repos",getOption("repos")[["CRAN"]]))


# In this example, we use "lmer" function in "lme4" package to fit linear mixed models.#
library(lme4)

#Section 2.4.1
# Fit the data with Model 1 and restricted maximum likelihood estimation is applied#
o1 <- lmer(distance ~ Sex + age :Sex+ (1|Subject), data = Orthodont,REML="TRUE") 
summary(o1)
----------------------------------------------------------------------------------
# Section 2.4.3
# Fit the data with Model 2 (with a addition random term for age effect)
# and resticted maximum likelihood estmation is applied
o2<-lmer(distance ~ Sex + age:Sex + (1+age|Subject) , data=Orthodont,REML="TRUE")
summary(o2)

#Model Comparison between model 1 and model 2
anova(o1,o2)
----------------------------------------------------------------------------------

#Section 2.4.4
# Fit the data with Model 3 and maximum likelihood estmation (MLE) is applied#
r1<- lmer(distance ~ age +Sex +(1|Subject), REML="FALSE", data = Orthodont) 

# Fit the data with Model 3 and restricted maximum likelihood estimation is applied#
r2<-lmer(distance ~ age +Sex + (1|Subject), REML="TRUE", data = Orthodont) 

summary(r1)
summary(r2)

----------------------------------------------------------------------------------

#Section 2.4.5
#Fit the data with Model 4 and maximum likelihood estimation is applied
o4<- lmer(distance ~ Sex +(1|Subject), REML="FALSE", data = Orthodont)
summary(o4)

#Fit the data with Model 4 and maximum likelihood estimation is applied
o5<- lmer(distance ~ age +age:Sex +(1|Subject), REML="FALSE", data = Orthodont)
summary(o5)

#Model comparison between model 4 and model 5
anova(o4,o5)

--------------------------------------------------------------------------------
#Section2.6
#Figure2.4 Plot of residuals against the fitted value of model 1
plot(fitted(o1),resid(o1),xlab="Fitted",ylab="Residuals")
abline(0,0)


#Figure2.5 QQ plot for sample quantiles agaainst theorestical quantiles
qqnorm(resid(o1),main="") 
abline(0,1)

#Figure2.6 : Plot of orthodontics growth against age for each of the sampling individual 
#with solid line indicate the trajectory of orthodontics growth over time
p2.6<-ggplot(data=Orthodont) + geom_line(aes(x=age,y=distance)) + facet_wrap(~Subject, ncol=5) 
p2.6

#Figure 2.7 : Plot of orthodontics growth aganist age for each of the sampling
#individual with solid line indicate the fitted regression line where the red point indicate the observations results
Orthodont$pred.final <- fitted(o1)
ggplot(data=Orthodont) + geom_point(aes(x=age,y=distance), color="red", size=3) + 
  geom_line(aes(x=age,y=pred.final)) + facet_wrap(~Subject, ncol=5) 



# 95% confident interval for regression parameters
conf(o1)

# Expected random efect for each child
ranef(o1) 

#Figure 2.8: Plot of random effects estimates against the identity of children.
#Blue points are used to indicate the expected random effect for each children 
#and the horizontal solid line indicate the 95% confident interval of random effect.
dc = dotplot(ranef(o1, condVar=TRUE), strip=FALSE)
print(dc[[1]])





