

#Skeleton code for Data Analysis Project: EC:6062

#Student Name: Samaksh Baijal
#Student ID:23011289

# The project focuses on multiple regression analysis, using firm level data.
# To complete the project, you need to have the following packages installed.


#Step 1 Install packages 
# Please note, the installations are commented out with # (take this out and the codes will run)

install.packages("tidyverse")
install.packages("magrittr")
install.packages("stargazer")
install.packages ("dplyr")

install.packages("writexl")

install.packages("car")
install.packages("lmtest")
install.packages("RCurl")
install.packages("readxl")
install.packages("expss")
install.packages("maditr")
install.packages("broom") 
install.packages("mosaic")



# Step 2: Loading packages (library command)
library(tidyverse)
library(magrittr)
library(stargazer)
library(dplyr)
library(writexl)
library(lmtest)
library (car)
library(readxl)
library(expss)
library(maditr)
library(broom) 
library(mosaic)
                
# import the function to calculate Robust Standard Erro from the Repo.

url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
   envir=.GlobalEnv)

#don't worry if you get an error message. If you have an object names url_robust, you are okay!


# Step 3: set your directory and load the data (Ensure to save the data file in this folder)

setwd("C:/Users/Samaksh/Downloads")

list.files()  #Do you see the data file?

data <- read_excel("Firm-profits_Data.xlsx")


# Apply the following labels to the data with package expss

data = apply_labels(data, 
                    ID = "Unique Identifier",
                    log_profits = "Firms profits in 2024 in Natural Logs",
                    log_training = "Total investments in Training between 2022 and 2023 in Logs",
                    log_equipment = "Total investments in Capital Equipment between 2022 and 2023 in Logs",
                    Enterprise_Group = "Does the firm belong to an Enterprise Group, 1 = Yes",
                    Firm_Age = "Year of the firm since registration to 2024",
                    Export_yes_no = "The firm is an exporting firm, 1= Yes",
                    Small_Firm = "The firm has fewer than 50 employees",
                    Industrial_sector = "Industrial Sector codes",
                    innovation_yes = "The firm introduced new products or services to market during 2022 ans 2023",
                    Employees_log = "Total number of employees in logs in 2024",
                    R_D_yes = "The firm invested in RD during 2022 and 2023, 1 = yes"
)

#Your a now set.Let's begin by populating the field for each of the points in the Instructions

#POINT 2:  Explore your data by running some descriptive statistics. 
#Do you sport anything that makes no sense? For example, can a firm have no profits? Are there any extreme values in our data?
#Tip if you want to exclude some observations, use the subset function.  Please ensure to justify why you are excluding them.


summary(data)
missing_values <- colSums(is.na(data))
print(missing_values)

data <- na.omit(data)


z_scores <- scale(data)

outliers <- apply(abs(z_scores) > 3, 1, any)

outlier_indices <- which(outliers)


data_clean <- data[!outliers, ]

summary(data_clean)



boxplot(data)

#outliers <- data.frame(which(apply(data, 2, function(x) any(abs(scale(x, center = TRUE))))))
#print(outliers)


#POINT 3: Now that you have sorted your data, you can check for associations between the dependent and independent variables
#Do you see anything strange happening? Please keep in mind that a firm may decide not to invest in training. 

# Scatterplot for Profits vs Training
ggplot(data, aes(x = log_training, y = log_profits)) +
  geom_point() +
  labs(x = "Training", y = "Profits") +
  ggtitle("Scatterplot of Profits vs Training")


# Scatterplot for Profits vs Equipment
ggplot(data, aes(x = log_equipment, y = log_profits)) +
  geom_point() +
  labs(x = "Equipment", y = "Profits") +
  ggtitle("Scatterplot of Profits vs Equipment")



#POINT 4: Great, your data is ready. Time time to run some analysis.

#Y= Profits and X = training

model_training <- lm(log_profits ~ log_training, data = data)
summary(model_training)


#Y= Profits and X= Equipment

model_equipment <- lm(log_profits ~ log_equipment, data = data)
summary(model_equipment)

#Point 5: Y= Profits and X1 = training and X2 = Equipment.

model_multiple <- lm(log_profits ~ log_training + log_equipment , data = data)
summary(model_multiple)

#Point 6: Do point 5 with all other (suitable) control variables
model_multiple <- lm(log_profits ~ log_training + log_equipment + Firm_Age + Export_yes_no + Small_Firm + Industrial_sector 
            + innovation_yes + Employees_log + R_D_yes, data = data)
summary(model_multiple)


#Point 7: For model in Point 6, check for Heteroskedasticity.

bp_test <- bptest(model_multiple)
print(bp_test)
summary(bp_test)

white_test <- bptest (model_multiple, studentize = FALSE)
print(white_test)
summary(white_test)

white_test <- bptest(model_multiple, ~ fitted(model_multiple)^2)
print(white_test)
summary(white_test)

#Finally, produce a regression table with all outputs from Points 4 to 6, and ensure to use Robust Standard Errors.
#tip, to obtain robust standard errors, use the "summary(model_name, robust=T). this will use the URl_robust function to compute them.

small_firms <- subset(data, Small_Firm = 1)
non_small_firms <- subset(data, Small_Firm = 0)

model_small <- lm(log_profits ~ log_training + log_equipment, data = small_firms)
summary(model_small)

model_non_small <- lm(log_profits ~ log_training + log_equipment, data = non_small_firms)
summary(non_small_firms)


# Assuming model_small and model_non_small are the fitted regression models for small-sized and non-small-sized firms, respectively

# Extract coefficients for small-sized firms
coeff_small <- coef(model_small)

# Extract coefficients for non-small-sized firms
coeff_non_small <- coef(model_non_small)

# Compare coefficients
coefficients_comparison <- data.frame(
  Coefficient = names(coeff_small),  # Assuming coefficients have names
  Small_Firms = coeff_small,
  Non_Small_Firms = coeff_non_small,
  Difference = coeff_small - coeff_non_small
)
print(coefficients_comparison)


#Well done, you are done.  Ensure to upload this script with your Report

