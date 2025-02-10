#product data for modelling
product_data_for_modeling<- read.csv("product_data_for_modeling.csv")
head(product_data_for_modeling)
colnames(product_data_for_modeling)
dim(product_data_for_modeling)

colnames(product_data_for_modeling)

install.packages("corrplot")
install.packages("Hmisc")
library(corrplot) # For visualization of correlation matrix
library(Hmisc)

product_data_for_modeling1<- read.csv("product_data_for_modeling_.csv")
# Extract numeric variables
numeric_data <- product_data_for_modeling1[, sapply(product_data_for_modeling1, is.numeric)]

# Extract categorical variables
categorical_data <- product_data_for_modeling1[, !sapply(product_data_for_modeling1, is.numeric)]

numeric_data <- apply(numeric_data, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Calculate correlation matrix for numeric variables using Pearson correlation
cor_numeric <- cor(numeric_data, use = "pairwise")

# Calculate correlation matrix for categorical variables using Spearman correlation
cor_categorical <- cor(categorical_data, method = "spearman", use = "pairwise", exact = TRUE)

cor(numeric_data)
# For demonstration, let's concatenate them side by side
cor_combined <- cbind(cor_numeric, cor_categorical$r)

# Step 4: Visualize the correlation matrix
corrplot(cor_numeric, method = "color")

plot.new()
dev.off()
corrplot(cor_numeric, method = "color", tl.cex = 0.4)

# creating the factor variable type of substitute
product_data_for_modeling$Type.Of.Substitute <- factor(product_data_for_modeling$Type.Of.Substitute)
is.factor(product_data_for_modeling$Type.Of.Substitute)

product_data_for_modeling$Type.Of.Substitute[1:15]

model2 <-(lm(log(product_data_for_modeling$Dollar.Sales) ~ product_data_for_modeling$Price.per.Unit + product_data_for_modeling$Price.per.Unit.No.Merch  + product_data_for_modeling$ACV.Weighted.Distribution + product_data_for_modeling$ACV.Weighted.Distribution.No.Merch + product_data_for_modeling$Price.per.Volume + product_data_for_modeling$Price.per.Volume + product_data_for_modeling$Type.Of.Substitute, data = product_data_for_modeling))

model3 <-(lm(log(product_data_for_modeling$Dollar.Sales) ~ product_data_for_modeling$Price.per.Unit + product_data_for_modeling$Price.per.Unit.No.Merch  + product_data_for_modeling$ACV.Weighted.Distribution + product_data_for_modeling$ACV.Weighted.Distribution.No.Merch + product_data_for_modeling$Price.per.Volume + product_data_for_modeling$Price.per.Volume + product_data_for_modeling$Type.Of.Meat.Substituted, data = product_data_for_modeling))
summary(model3)

model1 <- summary(lm(log(product_data_for_modeling$Dollar.Sales) ~ product_data_for_modeling$Price.per.Unit + product_data_for_modeling$Price.per.Unit.No.Merch + product_data_for_modeling$Price.per.Volume + product_data_for_modeling$Price.per.Volume.No.Merch, data = product_data_for_modeling))
summary(model1)

install.packages("Metrics")  # Install the package if you haven't already
library(Metrics)

mse_value <- mse(predict(model2), log(product_data_for_modeling$Dollar.Sales))
mse_value
# Assuming your data and model are stored as 'data' and 'model' respectively

mse_value3 <- mse(predict(model3), log(product_data_for_modeling$Dollar.Sales))
mse_value3

# Generate residual plot
plot(model1$residuals ~ fitted(model1))

# Residual plot
plot(model1, which = 1)  # Plot against fitted values
plot(model1, which = 2)  # Plot against each predictor variable

# Breusch-Pagan test
library(lmtest)
bptest(model1)

# White test
bptest(model1, ~ I(fitted(model1)^2))

# White test
bptest(model1, ~ fitted(model1) + I(fitted(model1)^2))
white_test(model1)

#load lmtest library
library(lmtest)

#perform White's test
bptest(model1, ~ product_data_for_modeling$Price.per.Unit*product_data_for_modeling$Price.per.Unit.No.Merch + I(product_data_for_modeling$Price.per.Unit) + I(product_data_for_modeling$Price.per.Unit.No.Merch^2), data = product_data_for_modeling)

