# Import dataset
loan_data <-read.csv("E:/All Semester/10th semester/Data Science/Project/test_Y3wMUE5_7gLdaTN.csv",header = TRUE,sep=",")
head(loan_data)


# View structure of dataset
str(loan_data)

summary(loan_data)

dim(loan_data)


# Check missing values in dataset
missing_values <- colSums(is.na(loan_data) | loan_data == "")
missing_values



# Install package (run once)
install.packages("ggplot2")

library(ggplot2)

# Convert missing_values vector to data frame
missing_df <- data.frame(
  Variable = names(missing_values),
  Missing_Count = as.numeric(missing_values)
)

# Plot
ggplot(missing_df, aes(x = Variable, y = Missing_Count)) +
  geom_bar(stat = "identity") +
  labs(title = "Missing Values per Column",
       x = "Column Names",
       y = "Missing Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Function to calculate mode
get_mode <- function(x) {
  ux <- unique(x[!is.na(x) & x != ""])
  ux[which.max(tabulate(match(x, ux)))]
}

# Loop through each column
for (col in names(loan_data)) {
  
  # If column is numeric → replace NA with mean
  if (is.numeric(loan_data[[col]])) {
    mean_value <- mean(loan_data[[col]], na.rm = TRUE)
    loan_data[[col]][is.na(loan_data[[col]])] <- mean_value
  }
  
  # If column is categorical → replace NA or "" with mode
  else if (is.character(loan_data[[col]]) || is.factor(loan_data[[col]])) {
    mode_value <- get_mode(loan_data[[col]])
    loan_data[[col]][is.na(loan_data[[col]]) | loan_data[[col]] == ""] <- mode_value
  }
}

# Check missing values again
missing_values <- sapply(loan_data, function(x) sum(is.na(x) | x == ""))
missing_values




# Boxplot to detect outliers
boxplot(loan_data$LoanAmount,
        main = "Boxplot for Loan Amount",
        ylab = "Loan Amount",
        col = "lightblue")



# Calculate mean of valid LoanAmount values (within reasonable range)
mean_value <- mean(loan_data$LoanAmount[
  loan_data$LoanAmount > 0 & loan_data$LoanAmount < 500
], na.rm = TRUE)

# Replace outliers (<=0 or very large values) with mean
loan_data$LoanAmount[
  loan_data$LoanAmount <= 0 | loan_data$LoanAmount >= 500
] <- mean_value

# Boxplot after handling outliers
boxplot(loan_data$LoanAmount,
        main = "Loan Amount after Outlier Replacement",
        ylab = "Loan Amount",
        col = "lightgreen")




# Check unique values in categorical columns
unique(loan_data$Gender)
unique(loan_data$Married)
unique(loan_data$Education)
unique(loan_data$Self_Employed)
unique(loan_data$Property_Area)



# Fix Gender column
loan_data$Gender[loan_data$Gender == "male"] <- "Male"
loan_data$Gender[loan_data$Gender == "MALE"] <- "Male"

loan_data$Gender[loan_data$Gender == "female"] <- "Female"
loan_data$Gender[loan_data$Gender == "FEMALE"] <- "Female"

# Fix Married column
loan_data$Married[loan_data$Married == "yes"] <- "Yes"
loan_data$Married[loan_data$Married == "YES"] <- "Yes"

loan_data$Married[loan_data$Married == "no"] <- "No"
loan_data$Married[loan_data$Married == "NO"] <- "No"

# Fix Self_Employed column
loan_data$Self_Employed[loan_data$Self_Employed == "yes"] <- "Yes"
loan_data$Self_Employed[loan_data$Self_Employed == "YES"] <- "Yes"

loan_data$Self_Employed[loan_data$Self_Employed == "no"] <- "No"
loan_data$Self_Employed[loan_data$Self_Employed == "NO"] <- "No"

# Fix Education column
loan_data$Education[loan_data$Education == "graduate"] <- "Yes"
loan_data$Education[loan_data$Education == "GRADUATE"] <- "Yes"

loan_data$Education[loan_data$Education == "not Graduate"] <- "No"
loan_data$Education[loan_data$Education == "NOT GRADUATE"] <- "No"

# Fix Property_Area column (standardize values)
loan_data$Property_Area[loan_data$Property_Area == "urban"] <- "Urban"
loan_data$Property_Area[loan_data$Property_Area == "URBAN"] <- "Urban"

loan_data$Property_Area[loan_data$Property_Area == "semiurban"] <- "Semiurban"
loan_data$Property_Area[loan_data$Property_Area == "SEMIURBAN"] <- "Semiurban"

loan_data$Property_Area[loan_data$Property_Area == "rural"] <- "Rural"
loan_data$Property_Area[loan_data$Property_Area == "RURAL"] <- "Rural"

# Recheck values
unique(loan_data$Gender)
unique(loan_data$Married)
unique(loan_data$Self_Employed)
unique(loan_data$Education)
unique(loan_data$Property_Area)




# Convert Gender (Male/Female → 1/0)
loan_data$Gender[loan_data$Gender == "Male"] <- 1
loan_data$Gender[loan_data$Gender == "Female"] <- 0

# Convert Married (Yes/No → 1/0)
loan_data$Married[loan_data$Married == "Yes"] <- 1
loan_data$Married[loan_data$Married == "No"] <- 0

# Convert Self_Employed (Y/N → 1/0)
loan_data$Self_Employed[loan_data$Self_Employed == "Yes"] <- 1
loan_data$Self_Employed[loan_data$Self_Employed == "No"] <- 0

# Convert Education (Y/N → 1/0)
loan_data$Education[loan_data$Education == "Graduate"] <- 1
loan_data$Education[loan_data$Education == "Not Graduate"] <- 0

# Convert Property_Area (Urban/Semiurban/Rural → 2/1/0)
loan_data$Property_Area[loan_data$Property_Area == "Urban"] <- 2
loan_data$Property_Area[loan_data$Property_Area == "Semiurban"] <- 1
loan_data$Property_Area[loan_data$Property_Area == "Rural"] <- 0



# Convert to numeric type
loan_data$Gender <- as.numeric(loan_data$Gender)
loan_data$Married <- as.numeric(loan_data$Married)
loan_data$Self_Employed <- as.numeric(loan_data$Self_Employed)
loan_data$Education <- as.numeric(loan_data$Education)
loan_data$Property_Area <- as.numeric(loan_data$Property_Area)


# Check results
unique(loan_data$Gender)
unique(loan_data$Married)
unique(loan_data$Self_Employed)
unique(loan_data$Education)
unique(loan_data$Property_Area)




# Check dataset size before removing duplicates
dim(loan_data)

# Find duplicate rows
duplicates <- loan_data[duplicated(loan_data), ]
duplicates   # View duplicate rows

# Remove duplicate rows
loan_data <- loan_data[!duplicated(loan_data), ]

# Check dataset size after removing duplicates
dim(loan_data)





# Number of columns and rows
numberOfCol <- ncol(loan_data)
numberOfRow <- nrow(loan_data)

cat("Number of Columns:", numberOfCol, "\n")
cat("Number of Rows:", numberOfRow, "\n")

# Structure of dataset
str(loan_data)

# Summary statistics
summary(loan_data)





library(dplyr)

# 1. Applicants with good credit history (likely approved)
filter(loan_data, Credit_History == 1)

# 2. Applicants with high income (greater than 5000)
filter(loan_data, ApplicantIncome > 5000)

# 3. Married applicants with good credit history
filter(loan_data, Married == 1 & Credit_History == 1)

# 4. Female applicants with low credit history
filter(loan_data, Gender == 0 & Credit_History == 0)

library(dplyr)

# 1. Applicants with high income AND good credit history
filter(loan_data, ApplicantIncome > 5000 & Credit_History == 1)

# 2. Graduates with good credit history and high income
filter(loan_data, Education == "Graduate" & Credit_History == 1 & ApplicantIncome > 5000)

# 3. Married applicants living in Urban areas
filter(loan_data, Married == 1 & Property_Area == 2)

# 4. Self-employed applicants with high loan amount
filter(loan_data, Self_Employed == "Yes" & LoanAmount > 200)

# 5. Applicants with long loan term and good credit history
filter(loan_data, Loan_Amount_Term > 300 & Credit_History == 1)

# 6. Male graduates with high income and good credit history
filter(loan_data, Gender == 1 & Education == "Graduate" & ApplicantIncome > 5000 & Credit_History == 1)




# Min-max normalization of ApplicantIncome
loan_data$ApplicantIncome <- 
  (loan_data$ApplicantIncome - min(loan_data$ApplicantIncome)) / 
  (max(loan_data$ApplicantIncome) - min(loan_data$ApplicantIncome))

# Check the result
summary(loan_data$ApplicantIncome)




library(dplyr)

# Descriptive statistics of ApplicantIncome by Credit History
loan_data %>%
  group_by(Credit_History) %>%
  summarise(
    Count = n(),
    Mean_Income = mean(ApplicantIncome, na.rm = TRUE),
    Median_Income = median(ApplicantIncome, na.rm = TRUE),
    SD_Income = sd(ApplicantIncome, na.rm = TRUE)
  )




library(dplyr)

loan_data %>%
  group_by(Credit_History) %>%
  summarise(
    Average_Income = mean(ApplicantIncome, na.rm = TRUE)
  )

loan_data %>%
  group_by(Gender) %>%
  summarise(
    Average_Income = mean(ApplicantIncome, na.rm = TRUE)
  )


loan_data %>%
  group_by(Property_Area) %>%
  summarise(
    Average_Income = mean(ApplicantIncome, na.rm = TRUE)
  )



# Compare variation (standard deviation) of ApplicantIncome by Loan
library(dplyr)

loan_data %>%
  group_by(Property_Area) %>%
  summarise(
    Count = n(),
    Variance = var(ApplicantIncome, na.rm = TRUE),
    Standard_Deviation = sd(ApplicantIncome, na.rm = TRUE)
  )

loan_data %>%
  group_by(Credit_History) %>%
  summarise(
    Count = n(),
    Variance = var(ApplicantIncome, na.rm = TRUE),
    Standard_Deviation = sd(ApplicantIncome, na.rm = TRUE)
  )

loan_data %>%
  group_by(Gender) %>%
  summarise(
    Count = n(),
    Variance = var(ApplicantIncome, na.rm = TRUE),
    Standard_Deviation = sd(ApplicantIncome, na.rm = TRUE)
  )





hist(loan_data$ApplicantIncome,
     main = "Histogram of Applicant Income",
     xlab = "Applicant Income",
     ylab = "Frequency")


boxplot(loan_data$ApplicantIncome,
        main = "Boxplot of Applicant Income",
        ylab = "Applicant Income")


property_area_counts <- table(loan_data$Property_Area)
barplot(property_area_counts,
        main = "Distribution of Property Area",
        xlab = "Property Area",
        ylab = "Frequency")


gender_counts <- table(loan_data$Gender)
barplot(gender_counts,
        main = "Gender Distribution",
        xlab = "Gender (0 = Female, 1 = Male)",
        ylab = "Frequency")


credit_counts <- table(loan_data$Credit_History)
barplot(credit_counts,
        main = "Credit History Distribution",
        xlab = "Credit History (0 = Bad, 1 = Good)",
        ylab = "Frequency")




set.seed(123)

# Define sample size for training (70% of the data)
sample_size <- floor(0.7 * nrow(loan_data))

# Create training indices
train_indices <- sample(seq_len(nrow(loan_data)), size = sample_size)

# Split the dataset
train_data <- loan_data[train_indices, ]
test_data <- loan_data[-train_indices, ]

# Check dimensions
dim(train_data)
dim(test_data)



































