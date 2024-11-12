# count models

library(highcharter)
library(pscl)

# data

fordham <- read_dta("/Users/dave/Documents/teaching/606J-mle/2022/slides/L11_count2/code/fordham98jcr.dta")

# aggregate data by force frequency 

force <- fordham %>% group_by(force) %>% summarise(n=n())

# highcharter aggregated frequency bars of force variable 

highchart() %>% hc_chart(type = "column") %>% hc_title(text = "Uses of Force") %>% hc_xAxis(categories = force$force) %>% hc_yAxis(title = list(text = "Frequency")) %>% hc_add_series(name = "Uses of Force", data = force$n) %>% hc_tooltip(valueDecimals = 0) %>% hc_plotOptions(column = list(colorByPoint = TRUE)) %>% hc_colors(c("#005A43")) 

# ggplot(fordham, aes(x=force)) + geom_histogram(binwidth=.5, fill="#005A43", color="black") + scale_x_continuous(breaks=seq(0,5,1)) + theme_minimal() + labs(title="Uses of Force", x="Quarterly Uses of Force", y="Frequency") 
# 

# fit nbreg model using MASS

mod1nb <- MASS::glm.nb(force ~ unemp + cpi + war + wecycle + pecycle + demunemp + demcpi, data=fordham)
summary(mod1nb)

# no evidence of extra poissonness, so fit poisson model using GLM

mod1p <- glm(force ~ unemp + cpi + war + wecycle + pecycle + demunemp + demcpi, data=fordham, family = poisson(link = "log"))
summary(mod1p)

# logit predicting zero, non-zero force 
fordham$biforce <- ifelse(fordham$force > 0, 1, 0)

logit <- glm(biforce ~ unemp + cpi + war + wecycle + pecycle + demunemp + demcpi, data=fordham, family = binomial(link = "logit"))
summary(logit)

# zero inflated poisson model using PSCL

mod1zip <- zeroinfl(force ~ unemp + cpi + war + wecycle + pecycle + demunemp + demcpi + approval  | unemp + cpi + war + wecycle + pecycle + demunemp +demcpi  , data=fordham, dist = "poisson")

summary(mod1zip)


library(glmmTMB)

# negative binomial using glmmTMB

mod1nbre <- glmmTMB(force ~ unemp + cpi + war + wecycle + pecycle + demunemp + demcpi , data=fordham, family = poisson(link = "log"))

summary(mod1nbre)

# zero inflated poisson model using glmmTMB

mod1zipre <- glmmTMB(force ~ unemp + cpi + war + wecycle + pecycle + demunemp + demcpi + approval, data=fordham, family = poisson(link = "log"), ziformula = ~ unemp + cpi + war + wecycle + pecycle + demunemp +demcpi )

summary(mod1zipre)


###############

# zero inflated poisson model using PSCL

mod1zip <- zeroinfl(force ~ unemp + cpi + war + wecycle + pecycle + demunemp + demcpi + approval  | unemp + cpi + war + wecycle + pecycle + demunemp +demcpi  , data=fordham, dist = "poisson")

summary(mod1zip)

# formtted table of results for mod1zip using modelsummary

library(modelsummary)

modelsummary(mod1zip, stars = TRUE,  output = "flextable")

# predictions using mod1zip over unemployment





# zip - pr(0)



The probability $y=0$ is of particular interest in this setting. 


```{r}
#| echo: true
#| code-fold: true
#| code-summary: "code"

# predict probability of y=0 from the zero inflated poisson model 

preds3 <- data.frame(demforce =predict(mod2, newdata=predictiondatadem, type="zero"), unemp=predictiondatadem$unemp)

preds3 <- data.frame(preds3, repforce=predict(mod2, newdata=predictiondatarep, type="zero"), unemp=predictiondatadem$unemp)

ggplot(preds3, aes(x=unemp, y=demforce)) + geom_line(color="blue") + geom_line(aes(x=unemp, y=repforce), color="red") + labs(title="Uses of Force", x="Unemployment", y="Probability of Zero Uses of Force") + theme_minimal() + theme(legend.position="none")

bcount <- as.matrix(mod2[["coefficients"]][["count"]])
bzero <- as.matrix(mod2[["coefficients"]][["zero"]])

X <- expand.grid( Intercept=1, unemp=seq(min(fordham$unemp), max(fordham$unemp), length.out=100), cpi=median(fordham$cpi), war=0, wecycle=0, pecycle=0, demunemp=0, demcpi=median(fordham$demcpi))

X$demunemp<- X$unemp
X <- as.matrix(X)


xbC <- as.matrix(X %*% bcount)
xbZ <- as.matrix(X %*% bzero)

psi <- 1/(1+exp(-xbZ))
lambda <- exp(xbC)

ggplot(data.frame(X, psi), aes(x=unemp, y=psi)) + geom_line(color="blue") + labs(title="Uses of Force", x="Unemployment", y="Probability of Zero Uses of Force") + theme_minimal() + theme(legend.position="none")

p0 <- psi + (1-psi)*(-lambda)

df <- data.frame(X, xbC, xbZ, psi, lambda, p0)

ggplot(data.frame(X, p0), aes(x=unemp, y=p0)) + geom_line(color="blue") + labs(title="Uses of Force", x="Unemployment", y="Probability of Zero Uses of Force") + theme_minimal() + theme(legend.position="none")


```





