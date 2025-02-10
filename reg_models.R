model2 <-(lm(log(product_data_for_modeling$Dollar.Sales) ~ product_data_for_modeling$Price.per.Unit + product_data_for_modeling$Price.per.Unit.No.Merch  + product_data_for_modeling$ACV.Weighted.Distribution + product_data_for_modeling$ACV.Weighted.Distribution.No.Merch + product_data_for_modeling$Price.per.Volume + product_data_for_modeling$Type.Of.Substitute, data = product_data_for_modeling))
summary(model2)

mse_value <- mse(predict(model2), log(product_data_for_modeling$Dollar.Sales))
mse_value

model3 <-(lm(log(product_data_for_modeling$Dollar.Sales) ~ product_data_for_modeling$Price.per.Unit + product_data_for_modeling$Price.per.Unit.No.Merch  + product_data_for_modeling$ACV.Weighted.Distribution + product_data_for_modeling$ACV.Weighted.Distribution.No.Merch + product_data_for_modeling$Price.per.Volume + product_data_for_modeling$Price.per.Volume + product_data_for_modeling$Type.Of.Meat.Substituted, data = product_data_for_modeling))
summary(model3)

# Assuming your data and model are stored as 'data' and 'model' respectively

mse_value3 <- mse(predict(model3), log(product_data_for_modeling$Dollar.Sales))
mse_value3

model4 <-(lm(log(product_data_for_modeling$Dollar.Sales) ~ product_data_for_modeling$Price.per.Unit + product_data_for_modeling$Price.per.Unit.No.Merch  + product_data_for_modeling$ACV.Weighted.Distribution + product_data_for_modeling$ACV.Weighted.Distribution.No.Merch + product_data_for_modeling$Price.per.Volume + product_data_for_modeling$Price.per.Volume + product_data_for_modeling$Flavor...Scent, data = product_data_for_modeling))
summary(model4)

mse_value4 <- mse(predict(model4), log(product_data_for_modeling$Dollar.Sales))
mse_value4

#No heteroskedasticity for model4
bp_test <- bptest(model4)
bp_test

#Package
model5 <-(lm(log(product_data_for_modeling$Dollar.Sales) ~ product_data_for_modeling$Price.per.Unit + product_data_for_modeling$Price.per.Unit.No.Merch  + product_data_for_modeling$ACV.Weighted.Distribution + product_data_for_modeling$ACV.Weighted.Distribution.No.Merch + product_data_for_modeling$Price.per.Volume + product_data_for_modeling$Price.per.Volume + product_data_for_modeling$Package, data = product_data_for_modeling))
summary(model5)

mse_value5 <- mse(predict(model5), log(product_data_for_modeling$Dollar.Sales))
mse_value5

bp_test <- bptest(model5)
bp_test

#Form
model6 <-(lm(log(product_data_for_modeling$Dollar.Sales) ~ product_data_for_modeling$Price.per.Unit + product_data_for_modeling$Price.per.Unit.No.Merch  + product_data_for_modeling$ACV.Weighted.Distribution + product_data_for_modeling$ACV.Weighted.Distribution.No.Merch + product_data_for_modeling$Price.per.Volume + product_data_for_modeling$Price.per.Volume + product_data_for_modeling$Form, data = product_data_for_modeling))
summary(model6)

mse_value6 <- mse(predict(model6), log(product_data_for_modeling$Dollar.Sales))
mse_value6

bp_test <- bptest(model6)
bp_test

#Producttype
model7 <-(lm(log(product_data_for_modeling$Dollar.Sales) ~ product_data_for_modeling$Price.per.Unit + product_data_for_modeling$Price.per.Unit.No.Merch  + product_data_for_modeling$ACV.Weighted.Distribution + product_data_for_modeling$ACV.Weighted.Distribution.No.Merch + product_data_for_modeling$Price.per.Volume + product_data_for_modeling$Price.per.Volume + product_data_for_modeling$Product.Type, data = product_data_for_modeling))
summary(model7)

mse_value7 <- mse(predict(model7), log(product_data_for_modeling$Dollar.Sales))
mse_value7

bp_test <- bptest(model7)
bp_test

#Sub-category
model8 <-(lm(log(product_data_for_modeling$Dollar.Sales) ~ product_data_for_modeling$Price.per.Unit + product_data_for_modeling$Price.per.Unit.No.Merch  + product_data_for_modeling$ACV.Weighted.Distribution + product_data_for_modeling$ACV.Weighted.Distribution.No.Merch + product_data_for_modeling$Price.per.Volume + product_data_for_modeling$Price.per.Volume + product_data_for_modeling, data = product_data_for_modeling))
summary(model8)

mse_value8 <- mse(predict(model8), log(product_data_for_modeling$Dollar.Sales))
mse_value8

bp_test <- bptest(model8)
bp_test

# Load the lmtest package
install.packages("lmtest")  # Install the package if you haven't already
library(lmtest)
#Heteroskedasticity test
bp_test <- bptest(model2)
bp_test

bp_test <- bptest(model3)
bp_test



library(ggplot2)

# Define a function to create diagnostic plots
create_diagnostic_plots <- function(model) {
  # Residual plot
  residuals <- resid(model)
  fitted <- fitted(model)
  ggplot() +
    geom_point(aes(x = fitted, y = residuals)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggtitle("Residuals vs Fitted") +
    xlab("Fitted values") +
    ylab("Residuals") +
    theme_minimal()
  
  # Q-Q plot
  qq_plot <- qqnorm(residuals)
  qq_line <- qqline(residuals)
  plot(qq_plot)
  abline(qq_line)
}

# Create diagnostic plots for each model
create_diagnostic_plots(model2)
create_diagnostic_plots(model3)
create_diagnostic_plots(model4)
create_diagnostic_plots(model5)
create_diagnostic_plots(model6)
create_diagnostic_plots(model7)
create_diagnostic_plots(model8)

data.info()
