# 16
 
require(mva) 
library(mva)
## the variances of the variables in the
## USArrests data vary by orders of magnitude
data(USArrests)
(pc.cr <- princomp(USArrests))
princomp(USArrests, cor = TRUE)
princomp(scale(USArrests, scale = TRUE, center = TRUE), cor = FALSE)
summary(pc.cr <- princomp(USArrests))
loadings(pc.cr)
plot(pc.cr) # does a screeplot.
biplot(pc.cr)

# 17

ctl = c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt = c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels=c("Ctl","Trt"))
weight <- c(ctl, trt)
anova(lm.D9 <- lm(weight ~ group))
summary(lm.D90 <- lm(weight ~ group - 1))# omitting intercept
summary(resid(lm.D9) - resid(lm.D90)) #- residuals almost identical
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm.D9, las = 1) # Residuals, Fitted, ...
par(opar)
 
# 18
 
data(cars)
attach(cars)
plot(speed, dist, main = "data(cars) & smoothing splines")
cars.spl <- smooth.spline(speed, dist)
(cars.spl)
all(cars.spl $ w == table(speed)) # TRUE (weights = multiplicities)
lines(cars.spl, col = "blue")
lines(smooth.spline(speed, dist, df=10), lty=2, col = "red")
legend(5,120,c(paste("default [C.V.] => df =",round(cars.spl$df,1)),
               "s( * , df = 10)"), col = c("blue","red"), lty = 1:2, bg='bisque')
detach()
 
# 18
 
# hospital.csv is a broken link as of this submission
hosp = read.csv("C:/Users/austr/Documents/Data Science/Stat384/hospital.csv")
pairs(hosp[,8:12])
# The next step in analyzing this data set is doing transformations.
group = hosp$TH +hosp$TRAUMA*2 +hosp$REHAB*4
table(group)
boxplot( split(hosp$SALES12,group))
boxplot( split(log(1+hosp$SALES12),group))
hosp.lm = lm(SALES12 ~ TH + TRAUMA+REHAB,data=hosp)
anova(hosp.lm)
summary(hosp.lm)
hosp.lm1 = lm(SALES12 ~ factor(group),data=hosp)
anova(hosp.lm1)
summary(hosp.lm1)
 