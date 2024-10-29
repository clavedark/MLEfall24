# count models

library(highcharter)
library(pscl)

# data

fordham <- read_dta("/Users/dave/Documents/teaching/606J-mle/2022/slides/L11_count2/code/fordham98jcr.dta")

# aggregate data by force frequency 

force <- fordham %>% group_by(force) %>% summarise(n=n())

# highcharter aggregated frequency bars of force variable 

highchart() %>% hc_chart(type = "column") %>% hc_title(text = "Uses of Force") %>% hc_xAxis(categories = force$force) %>% hc_yAxis(title = list(text = "Frequency")) %>% hc_add_series(name = "Uses of Force", data = force$n) %>% hc_tooltip(valueDecimals = 0) %>% hc_plotOptions(column = list(colorByPoint = TRUE)) %>% hc_colors(c("#005A43")) 

# fit nbreg model

mod1nb <- glm.nb(force ~ unemp + cpi + war + wecycle + pecycle + demunemp + demcpi, data=fordham)
summary(mod1nb)

# no evidence of extra poissonness, so fit poisson model

mod1p <- glm(force ~ unemp + cpi + war + wecycle + pecycle + demunemp + demcpi, data=fordham, family = poisson(link = "log"))
summary(mod1p)

# logit predicting zero, non-zero force 
fordham$biforce <- ifelse(fordham$force > 0, 1, 0)

logit <- glm(biforce ~ unemp + cpi + war + wecycle + pecycle + demunemp + demcpi, data=fordham, family = binomial(link = "logit"))
summary(logit)

# zero inflated poisson model

mod1zip <- zeroinfl(force ~ unemp + cpi + war + wecycle + pecycle + demunemp + demcpi + approval  | unemp + cpi + war + wecycle + pecycle + demunemp +demcpi  , data=fordham, dist = "poisson")

summary(mod1zip)


library(glmmTMB)







