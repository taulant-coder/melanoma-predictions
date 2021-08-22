###############################################################################
##############################################################################
############################################################################
### Project Title: Machine Learning for Cancer Cells Predictions
### Researcher: Taulant Elshani
### Machine Learning Algorithm: K- Nearest Algorithm kNN
### Place: Prishtine, Kosove
### Date: 9/8/2021
################################################################################
###############################################################################
#############################################################################

### Step 1 - Collecting Data

# Downloading data from Kaggle

### Step 1.1 - Data Vizualisation

hist(wbcd$radius_mean, 
     
     main = "Radius Mean", 
     
     col = "pink")

### Step 2

# Importing data to R environment

wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

# Exploring the dataset

str(wbcd)

wbcd <- wbcd[-1]

wbcd

table(wbcd$diagnosis)

# Coding the target feature as a factor

wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), 
                         
                         labels = c("Benign", "Malignant"))


round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

## Transformation - normalizing numeric data

# Creating a function

normalize <- function(x) {
  
  return((x - min(x)) / (max(x) - min(x)))
}

normalize(c(1, 2, 3, 4, 5))

normalize(c(10, 20, 30, 40, 50, 60, 70))

# Using lapply function

wbcd_n <-  as.data.frame(lapply(wbcd[2:31], normalize))

# Testing normalization from the function

summary(wbcd_n$area_mean)

## Data preparation - creating training and test datasets

wbcd_train <- wbcd_n[1:469, ]

wbcd_test <- wbcd_n[470:569, ]

wbcd_train_labels <- wbcd[1:469, 1]

wbcd_test_labels <- wbcd[470:569, 1]


### Step 3 training a model on the data

# Installing "Class" package

install.packages("class")

library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      
                      cl = wbcd_train_labels, k = 21)

### Step 4 - evaluating model performance

# Installing "gmodels" package

install.packages("gmodels")

library(gmodels)

CrossTable(x =wbcd_test_labels, y = wbcd_test_pred, 
           
           prop.chisq = FALSE)
