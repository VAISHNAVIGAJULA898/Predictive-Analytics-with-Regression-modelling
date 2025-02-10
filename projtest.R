#Clear the environment
rm(list = ls())

#Set the working Directory
setwd("C:/Users/gajul/Downloads/SAS Unit1/Data/Data")

library(readxl)
library(dplyr)

data20 <- read_excel("Fz_Rfg Substitute Meat_POS_2020.xlsx")
data20

data21<- read_excel("Fz_Rfg Substitute Meat_POS_2021.xlsx")

data22<- read_excel("Fz_Rfg Substitute Meat_POS_2022.xlsx")

data23<- read_excel("Fz_Rfg Substitute Meat_POS_2023.xlsx")

data24<- read_excel("Fz_Rfg Substitute Meat_POS_2024.xlsx")

attributes<-read_excel("Product Attributes.xlsx")

#converted each of above data file to csv and then using it for analysis
Fz_Substitute_POS_2020<- read.csv("Fz_Substitute_POS_2020.csv")
Fz_Substitute_POS_2021<- read.csv("Fz_Substitute_POS_2021.csv")
Fz_Substitute_POS_2022<- read.csv("Fz_Substitute_POS_2022.csv")
Fz_Substitute_POS_2023<- read.csv("Fz_Substitute_POS_2023.csv")
Fz_Substitute_POS_2024<- read.csv("Fz_Substitute_POS_2024.csv")
product_attributes<- read.csv("Product Attributes.csv")

Fz_Substitute_POS_2022$Time

install.packages("lubridate")
library(lubridate)
# Remove "Week Ending" substring
Fz_Substitute_POS_2020$Time <- substring(Fz_Substitute_POS_2020$Time, first = 13)
Fz_Substitute_POS_2021$Time <- substring(Fz_Substitute_POS_2021$Time, first = 13)
Fz_Substitute_POS_2022$Time <- substring(Fz_Substitute_POS_2022$Time, first = 13)
Fz_Substitute_POS_2023$Time <- substring(Fz_Substitute_POS_2023$Time, first = 13)
Fz_Substitute_POS_2024$Time <- substring(Fz_Substitute_POS_2024$Time, first = 13)

#Convert "Time" column from string to datetime format
Fz_Substitute_POS_2020$new_Time <- mdy(Fz_Substitute_POS_2020$Time)
Fz_Substitute_POS_2021$new_Time <- mdy(Fz_Substitute_POS_2021$Time)
Fz_Substitute_POS_2022$new_Time <- mdy(Fz_Substitute_POS_2022$Time)
Fz_Substitute_POS_2023$new_Time <- mdy(Fz_Substitute_POS_2023$Time)
Fz_Substitute_POS_2024$new_Time <- mdy(Fz_Substitute_POS_2024$Time)

# Merge data frames based on the "ID" column
# "Avoid this set of code"
data <- merge(Fz_Substitute_POS_2020, Fz_Substitute_POS_2021, by = "new_Time", all = TRUE)
data <- merge(data, Fz_Substitute_POS_2022, by = "new_Time", all = TRUE)
data <- merge(data, Fz_Substitute_POS_2023, by = "new_Time", all = TRUE)
data <- merge(data, Fz_Substitute_POS_2024, by = "new_Time", all = TRUE)
datanew <- full_join(Fz_Substitute_POS_2020, Fz_Substitute_POS_2021, by='new_Time')
datanew <- full_join(datanew, Fz_Substitute_POS_2022, by='new_Time')
datanew <- full_join(datanew, Fz_Substitute_POS_2023, by='new_Time')
datanew <- full_join(datanew, Fz_Substitute_POS_2024, by='new_Time')

##Concatenated data based on new_Time
combined_data <- rbind(Fz_Substitute_POS_2020, Fz_Substitute_POS_2021)
combined_data <- rbind(combined_data, Fz_Substitute_POS_2022)
combined_data <- rbind(combined_data, Fz_Substitute_POS_2023)
combined_data <- rbind(combined_data, Fz_Substitute_POS_2024)

datafinal<-combined_data

#Merging product attributes to combined data
combined_data <- left_join(combined_data, product_attributes, by = "UPC.13.digit")
combined_data$Product.Type
combined_data$Package

dim(combined_data)
names(combined_data)
class(combined_data)
str(combined_data)
str(product_data_for_modeling)

#combined_data <- data[data$`Product Type` != "MEAT SUBSTITUTE", ]

head(combined_data)


#New dataframe with "Sales" & "Packageinfo"
USales <- combined_data$Unit.Sales
Vsales <- combined_data$Volume.Sales
Packaging <- combined_data$Package

USales<- as.numeric(USales)
Vsales<- as.numeric(Vsales)

# Creating a new dataframe PackageData
PackageData <- data.frame(USales = USales, Vsales = Vsales, Packaging = Packaging)

#Dowloading new dataset created
write.table(PackageData, file = "Package_data.csv", sep=",")

Packagedata1<-read.csv("Package_data1.csv")

library(dplyr)

PackageData2 <- Packagedata1 %>%
  group_by(Packaging) %>%
  summarise(total_USales = sum(Usales),
            total_Vsales = sum(Vsales))

library(ggplot2)

# Assuming your dataset is called PackageData2 with columns Packaging, total_USales, and total_Vsales

# Plotting the graph for packaging
ggplot(PackageData2, aes(x = Packaging)) +
  geom_bar(aes(y = total_USales), fill = "blue", stat = "identity") +
  geom_bar(aes(y = total_Vsales), fill = "green", stat = "identity") +
  labs(x = "Packaging", y = "Total Sales", title = "Total Unit Sales and Volume Sales by Packaging") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#New dataset for flavor
Flavor_or_scent = combined_data$Flavor...Scent
FlavorData <- data.frame(Flavor_or_scent = Flavor_or_scent, Usales = combined_data$Unit.Sales, Vsales = combined_data$Volume.Sales)

write.table(FlavorData, file = "flavor_data.csv", sep=",")

Flavordata2 <-read.csv("flavor_data.csv")

Flavordata3 <- Flavordata2 %>%
  group_by(Flavordata2$Flavor_or_scent) %>%
  summarise(average_UnitSales = mean(Flavordata2$Usales),
            average_Volumesales = mean(Flavordata2$Vsales))

# Create the bar chart
ggplot(Flavordata3, aes(x = Flavordata3$flavor_or_type, y = average_UnitSales, fill = "Unit Sales")) +
  geom_bar(stat = "identity", position = "dodge") +  # Stacked bars
  geom_bar(aes(x = Flavordata3$flavor_or_type, y = average_Volumesales, fill = "Volume Sales"), stat = "identity", position = "dodge") +
  labs(title = "Average Unit & Volume Sales by Flavor/Type",
       x = "Flavor/Type",
       y = "Average Sales") +
  theme_minimal()  # Optional: adjust plot aesthetics


Franchise <- data.frame(FranchiseName = combined_data$Brand.Franchise.Name, Unitsales = combined_data$Unit.Sales, Volumesales = combined_data$Volume.Sales)
write.table(Franchise, file = "franchise_data.csv", sep=",")

Franchisedata <- read.csv("franchise_data.csv")
Franchisedata2 <- Franchisedata %>%
  group_by(Franchisedata$FranchiseName) %>%
  summarise(average_UnitSales = mean(UnitSales),
            average_Volumesales = mean(VolumeSales))

descending_franchise <- Franchisedata2[order(- Franchisedata2$average_Volumesales), ]


top_10_franchises <- Franchisedata2 %>%
  arrange(desc(Franchisedata2$average_Volumesales), desc(Franchisedata2$average_UnitSales)) %>%
  head(10)

# Assuming ggplot2 is installed (if not, install.packages("ggplot2"))
library(ggplot2)

# Create the plot using geom_bar
ggplot(top_10_franchises, aes(x = top_10_franchises$`Franchisedata$FranchiseName`)) +
  geom_bar(aes(y = top_10_franchises$total_UnitSales), stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_bar(aes(y = top_10_franchises$total_Volumesales), stat = "identity", fill = "orange", alpha = 0.7) +
  labs(title = "Average Unit Sales and Volume Sales by Franchise",
       x = "Franchise Name",
       y = "Average Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Filtering Cacique and Gardein data
filtered_franchise <- combined_data %>%
  filter(combined_data$Brand.Franchise.Name %in% c("IMPOSSIBLE", "GARDEIN"))

# Filter the combined_data dataset for CACIQUE and GARDEIN franchises
filtered_franchise <- combined_data %>%
  filter(Brand.Franchise.Name %in% c("IMPOSSIBLE", "GARDEIN"))

# Select the desired fields for the new dataframe
new_dataframe <- filtered_franchise %>%
  select(filtered_franchise$Unit.Sales, filtered_franchise$Unit.Sales.No.Merch, filtered_franchise$Unit.Sales.Any.Merch, 
         filtered_franchise$Unit.Sales.Price.Reductions.Only, filtered_franchise$Unit.Sales.Feature.Only, 
         filtered_franchise$Unit.Sales.Feature.Only, filtered_franchise$Unit.Sales.Special.Pack.Only, 
         filtered_franchise$Unit.Sales.Feature.and.Display, filtered_franchise$Volume.Sales, filtered_franchise$Volume.Sales.No.Merch, 
         filtered_franchise$Volume.Sales.Any.Merch, filtered_franchise$Volume.Sales.Price.Reductions.Only,
         filtered_franchise$Volume.Sales.Price.Reductions.Only, filtered_franchise$Volume.Sales.Display.Only,
         filtered_franchise$Volume.Sales.Feature.Only, filtered_franchise$Volume.Sales.Special.Pack.Only, 
         filtered_franchise$Volume.Sales.Feature.and.Display, filtered_franchise$Dollar.Sales, 
         filtered_franchise$Dollar.Sales.No.Merch, filtered_franchise$Dollar.Sales.Any.Merch, 
         filtered_franchise$Dollar.Sales.Feature.Only,filtered_franchise$Dollar.Sales.Display.Only, 
         filtered_franchise$Dollar.Sales.Special.Pack.Only, filtered_franchise$Dollar.Sales.Feature.and.Display, 
         filtered_franchise$Dollar.Sales.Price.Reductions.Only,filtered_franchise$Price.per.Unit, 
         filtered_franchise$Price.per.Unit.No.Merch, filtered_franchise$Price.per.Unit.Any.Merch, 
         filtered_franchise$Price.per.Unit.Price.Reductions.Only,filtered_franchise$Price.per.Unit.Feature.Only, 
         filtered_franchise$Price.per.Unit.Feature.Only, filtered_franchise$Price.per.Unit.Special.Pack.Only, 
         filtered_franchise$Price.per.Unit.Feature.and.Display,filtered_franchise$Price.per.Volume, 
         filtered_franchise$Price.per.Volume.No.Merch, filtered_franchise$Price.per.Volume.Feature.Only, 
         filtered_franchise$Price.per.Volume.Feature.and.Display,filtered_franchise$Price.per.Volume.Any.Merch, 
         filtered_franchise$Price.per.Volume.Price.Reductions.Only, filtered_franchise$Price.per.Volume.Special.Pack.Only, 
         filtered_franchise$Price.per.Volume.Display.Only, filtered_franchise$Base.Unit.Sales, 
         filtered_franchise$Base.Volume.Sales, filtered_franchise$Base.Dollar.Sales,
         filtered_franchise$Total.Count, filtered_franchise$Total.Ounces, filtered_franchise$Form, 
         filtered_franchise$Total.Count, filtered_franchise$Meat.Source, filtered_franchise$Product.Type, 
         filtered_franchise$Type.Of.Meat.Substituted, filtered_franchise$Type.Of.Substitute, 
         filtered_franchise$Cooked.Info, filtered_franchise$Geography)

new_dataframe <- data.frame(filtered_franchise$Brand.Franchise.Name,filtered_franchise$Package, filtered_franchise$Category.Name,  
                            filtered_franchise$Unit.Sales, filtered_franchise$Unit.Sales.No.Merch, filtered_franchise$Unit.Sales.Any.Merch, 
                            filtered_franchise$Sub.Category.Name, filtered_franchise$Brand.Name, filtered_franchise$Form, filtered_franchise$Flavor...Scent,
                            filtered_franchise$Meat.Source, filtered_franchise$Product.Type, filtered_franchise$Type.Of.Meat.Substituted,
                            filtered_franchise$Type.Of.Substitute, filtered_franchise$Cooked.Info,filtered_franchise$Unit.Sales.Price.Reductions.Only, filtered_franchise$Unit.Sales.Feature.Only, 
                             filtered_franchise$Unit.Sales.Feature.Only, filtered_franchise$Unit.Sales.Special.Pack.Only, 
                             filtered_franchise$Unit.Sales.Feature.and.Display, filtered_franchise$Volume.Sales, filtered_franchise$Volume.Sales.No.Merch, 
                             filtered_franchise$Volume.Sales.Any.Merch, filtered_franchise$Volume.Sales.Price.Reductions.Only,
                             filtered_franchise$Volume.Sales.Price.Reductions.Only, filtered_franchise$Volume.Sales.Display.Only,
                             filtered_franchise$Volume.Sales.Feature.Only, filtered_franchise$Volume.Sales.Special.Pack.Only, 
                             filtered_franchise$Volume.Sales.Feature.and.Display, filtered_franchise$Dollar.Sales, 
                             filtered_franchise$Dollar.Sales.No.Merch, filtered_franchise$Dollar.Sales.Any.Merch, 
                             filtered_franchise$Dollar.Sales.Feature.Only,filtered_franchise$Dollar.Sales.Display.Only, 
                             filtered_franchise$Dollar.Sales.Special.Pack.Only, filtered_franchise$Dollar.Sales.Feature.and.Display, 
                             filtered_franchise$Dollar.Sales.Price.Reductions.Only,filtered_franchise$Price.per.Unit, 
                             filtered_franchise$Price.per.Unit.No.Merch, filtered_franchise$Price.per.Unit.Any.Merch, 
                             filtered_franchise$Price.per.Unit.Price.Reductions.Only,filtered_franchise$Price.per.Unit.Feature.Only, 
                             filtered_franchise$Price.per.Unit.Feature.Only, filtered_franchise$Price.per.Unit.Special.Pack.Only, 
                             filtered_franchise$Price.per.Unit.Feature.and.Display,filtered_franchise$Price.per.Volume, 
                             filtered_franchise$Price.per.Volume.No.Merch, filtered_franchise$Price.per.Volume.Feature.Only, 
                             filtered_franchise$Price.per.Volume.Feature.and.Display,filtered_franchise$Price.per.Volume.Any.Merch, 
                             filtered_franchise$Price.per.Volume.Price.Reductions.Only, filtered_franchise$Price.per.Volume.Special.Pack.Only, 
                             filtered_franchise$Price.per.Volume.Display.Only, filtered_franchise$Base.Unit.Sales, 
                             filtered_franchise$Base.Volume.Sales, filtered_franchise$Base.Dollar.Sales,
                             filtered_franchise$Total.Count, filtered_franchise$Total.Ounces, filtered_franchise$Form, 
                             filtered_franchise$Total.Count, filtered_franchise$Meat.Source, filtered_franchise$Product.Type, 
                             filtered_franchise$Type.Of.Meat.Substituted, filtered_franchise$Type.Of.Substitute, 
                             filtered_franchise$Cooked.Info, filtered_franchise$Geography)
write.table(new_dataframe, file = "new_datafram.csv", sep=",")
Franchise_impossible <-read.csv("new_dataframe.csv")

# View the new dataframe
head(new_dataframe)

Franchisedata22 <- Franchise_impossible %>%
  group_by(Brand.Franchise.Name, Package, Product.Type, Meat.Source, Flavor...Scent) %>% 
  summarise(dollarsales_nomerch = sum(Dollar.Sales.No.Merch),sum(Dollar.Sales.Any.Merch), sum(Dollar.Sales.Feature.Only),
                                  sum(Dollar.Sales.Display.Only), sum(Dollar.Sales.Special.Pack.Only),
                                  sum(Dollar.Sales.Price.Reductions.Only), sum(Dollar.Sales.Special.Pack.Only), sum(Dollar.Sales.Feature.and.Display),
                                  sum(Price.per.Unit), sum(Price.per.Volume), sum(Price.per.Volume.No.Merch),
                                  sum(Price.per.Volume.Feature.Only), sum(Price.per.Volume.Feature.and.Display),
                                  sum(Price.per.Volume.Any.Merch), sum(Price.per.Volume.Price.Reductions.Only),
                                  sum(Price.per.Volume.Special.Pack.Only), sum(Price.per.Volume.Display.Only),
                                  mean(Total.Count), mean(Total.Ounces))

franchise_price_per_unit <- data.frame(FranchiseName = filtered_franchise$Brand.Franchise.Name, filtered_franchise$Price.per.Unit)
write.table(franchise_price_per_unit, file = "franchise_price_per_unit.csv", sep=",")

franchise_price_per_unit1 <- read.csv("franchise_price_per_unit.csv")

filtereddata_new <- data.frame(Franchisename = filtered_franchise$Brand.Franchise.Name, producttype = filtered_franchise$Product.Type, filtered_franchise$Unit.Sales, filtered_franchise$Volume.Sales )
write.table(filtereddata_new, file = "filtereddata_new.csv", sep=",")

filtered_data_use <- read.csv("filtereddata_new.csv")
Franchisedata21 <- filtered_data_use %>%
  group_by(Franchisename, producttype) %>% 
  summarise(unitsales = sum(filtered_franchise.Unit.Sales), volumesales = sum(filtered_franchise.Volume.Sales))

franchisedata22 <- data.frame(franchisename = filtered_franchise$Brand.Franchise.Name, producttype = filtered_franchise$Product.Type, package = filtered_franchise$Package, unitsales =filtered_franchise$Unit.Sales, volumesales = filtered_franchise$Volume.Sales)
write.table(franchisedata22, file = "franchisedata22.csv", sep=",")

Franchisedata23 <- read.csv("franchisedata22.csv")

filtered_data_use <- read.csv("filtereddata_new.csv")
Franchisedata25 <- Franchisedata23 %>%
  group_by(franchisename, producttype, package) %>% 
  summarise(unitsales = sum(unitsales), volumesales = sum(volumesales))

product_data_for_modeling<- read.csv("product_data_for_modeling.csv")
head(product_data_for_modeling)
colnames(product_data_for_modeling)
dim(product_data_for_modeling)

#Regression model creation
model1<- lm(Unit.Sales ~ Price.per.Unit + Price.per.Unit.No.Merch + Price.per.Volume + Price.per.Volume.No.Merch + ACV.Weighted.Distribution + ACV.Weighted.Distribution.No.Merch + Total.Count + Total.Ounces, data = product_data_for_modeling)
summary(model1)
model2<- lm(product_data_for_modeling$Unit.Sales ~ product_data_for_modeling$Price.per.Unit)

install.packages("car")
library(car)
vif_model <- lm(Unit.Sales ~ Price.per.Unit + Price.per.Unit.No.Merch + Price.per.Volume + Price.per.Volume.No.Merch + ACV.Weighted.Distribution + ACV.Weighted.Distribution.No.Merch + Total.Count + Total.Ounces, data = product_data_for_modeling)
vif_values <- vif(vif_model)
print(vif_values)

product_data_for_modeling_cont_var <- data.frame(product_data_for_modeling$Unit.Sales,  product_data_for_modeling$Price.per.Unit + product_data_for_modeling$Price.per.Unit.No.Merch + product_data_for_modeling$Price.per.Volume + product_data_for_modeling$Price.per.Volume.No.Merch + product_data_for_modeling$ACV.Weighted.Distribution + product_data_for_modeling$ACV.Weighted.Distribution.No.Merch + product_data_for_modeling$Total.Count + product_data_for_modeling$Total.Ounces)

#correlation matrix
# Assuming 'data' is your dataset
correlation_matrix <- cor(product_data_for_modeling_cont_var)

# Print the correlation matrix
print(correlation_matrix)

eigen_values <- eigen(correlation_matrix)$values
print(eigen_values)

#
grouped_franchise <- product_data_for_modeling %>%
  group_by(Manufacturer.Name, Brand.Name, Product.Type, Package, Form, Flavor...Scent, Type.Of.Meat.Substituted, Type.Of.Substitute) %>% 
  summarise(sum(Unit.Sales), sum(Unit.Sales.No.Merch), sum(Volume.Sales), sum(Volume.Sales.No.Merch), sum(Dollar.Sales), sum(Dollar.Sales.No.Merch), sum(Price.per.Unit), sum(Price.per.Unit.No.Merch), sum(Price.per.Volume), sum(Price.per.Volume.No.Merch), sum(ACV.Weighted.Distribution), sum(ACV.Weighted.Distribution.No.Merch), sum(Total.Count), sum(Total.Ounces))

write.table(grouped_franchise, file = "summarized_data.csv", sep=",")
write.table(sorted_dollarsales, file = "summarized_data.csv", sep=",")

sorted_dollarsales<- grouped_franchise[order(-grouped_franchise$`sum(Dollar.Sales)`), ]
# Assuming 'data' is your dataset and 'Dollar_Sales' is the variable containing dollar sales data
hist(product_data_for_modeling$Dollar.Sales, breaks = 20, col = "skyblue", main = "Histogram of Dollar Sales")

# Apply logarithmic transformation to dollar sales data
transformed_sales <- log(product_data_for_modeling$Dollar.Sales)

# Plot histogram of transformed sales data
hist(transformed_sales, breaks = 20, col = "skyblue", main = "Histogram of Log(Dollar Sales)")

# Fit a regression model
model <- lm(transformed_sales ~ product_data_for_modeling$Price.per.Unit, data = product_data_for_modeling)
summary(model)

# Predict values
predicted_values <- predict(model2, newdata = product_data_for_modeling)

# Calculate residuals
residuals <- model - predicted_values

# Calculate MSE
mse <- mean(residuals^2)

# Print MSE
print(mse)

ggdensity(product_data_for_modeling$Dollar.Sales, main = "")
