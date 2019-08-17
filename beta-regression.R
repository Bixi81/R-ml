# Beta-regression for rates and proportions via maximum likelihood
library("betareg")
data("GasolineYield", package = "betareg")
summary(GasolineYield$yield)

br = betareg(yield ~ batch + temp, data = GasolineYield)
preds = predict(br, newdata=GasolineYield)
summary(preds)
