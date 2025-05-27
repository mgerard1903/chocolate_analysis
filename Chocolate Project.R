
######## LIBRARIES########
library(ggplot2)
library(reshape2)
library(dplyr)
library(psych)
library(countrycode)
library(car)
library(cluster)
library(factoextra)
library(randomForest)
library(rpart)
library(rpart.plot)
library(gbm)
library(MASS)   
library(caret) 
library(DescTools)
library(pROC)
library(forcats)
library(viridis)
library(randomForest)
library(gbm)
######## PART 1: DATA CLEANING ########
data = read_csv("Dataset 4 — Chocolate bar ratings.csv")

#Converting Columns to a better format more suited for analysis 
names(data) = gsub("[[:space:]]+", " ", names(data))  
names(data) = gsub("[^a-zA-Z0-9]", "_", names(data)) 
names(data) = gsub("_+", "_", names(data))           
names(data) = trimws(names(data))       

#Handeling Missing Values 
missing_summary = sapply(data, function(x) sum(is.na(x)))
print(missing_summary)
total_missing = sum(is.na(data))
print(paste("Total missing values:", total_missing))
data = na.omit(data)

#Converting Cocoa_percent in a numerical Variable
data$Cocoa_Percent = gsub("%", "", data$Cocoa_Percent)  
data$Cocoa_Percent = as.numeric(data$Cocoa_Percent)    

# Check the conversion
str(data$Cocoa_Percent)

#Removing the "REF" variable as it is an identifier 
data = data[, !names(data) %in% "REF"]
print(names(data))

# Renaming the "Company _(Maker-if_known)" to a simpler name 
names(data)[names(data) == "Company_Maker_if_known_"] = "Company"
print(names(data))

#Renameing the "Specific_Bean_Origin_or_Bar_Name" to a simpler name 
names(data)[names(data) == "Specific_Bean_Origin_or_Bar_Name"] = "Specific_Bean_Origin"
print(names(data))

#Removing duplicating rows 
data = data[!duplicated(data), ]


######## PART 2: EXPLORING VARIABLES (EDA) ########

######## PARRT 2a: Numerical VARIABLES########
# Plotting
ggplot(data, aes(x = Cocoa_Percent, y = Rating)) +
  geom_point(color = "violet") +
  labs(x = "Cocoa Percent", y = "Rating", title = "Rating & Cocoa Percent") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "black"))

ggplot(data, aes(y = Cocoa_Percent)) +
  geom_boxplot(fill = "purple") +
  labs(y = "Cocoa Percent", title = "Cocoa Percent Boxplot") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "black"))

ggplot(data, aes(y = Rating)) +
  geom_boxplot(fill = "blueviolet") +
  labs(y = "Rating", title = "Rating Boxplot") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "black"))

ggplot(data, aes(y = Review_Date)) +
  geom_boxplot(fill = "orchid") +
  labs(y = "Year", title = "Review Date Boxplot") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "black"))

ggplot(data, aes(x = Cocoa_Percent)) +
  geom_histogram(bins = 50, fill = "blueviolet") +
  labs(x = "Cocoa Percent", y = "Frequency", title = "Cocoa Percent Histogram") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "black"))

ggplot(data, aes(x = Rating)) +
  geom_histogram(bins = 50, fill = "mediumpurple") +
  labs(x = "Rating", y = "Frequency", title = "Rating Histogram") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "black"))

ggplot(data, aes(x = Review_Date)) +
  geom_histogram(bins = 50, fill = "darkorchid") +
  labs(x = "Year", y = "Frequency", title = "Review Date Histogram") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "black"))

# Correlation Heatmap
cor_matrix_melt = reshape2::melt(cor_matrix)
ggplot(cor_matrix_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) + # Add correlation values
  scale_fill_gradient2(low = "blue", mid = "white", high = "violet", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Heatmap",
       x = "",
       y = "",
       fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold", color = "black"))

#Statistical Analysis 
attach(data)
mean(Cocoa_Percent)
quantile(Cocoa_Percent)
mean(Rating)
quantile(Rating)
quantile(Review_Date)

#Examining collinearity 
data_subset = data[, c("Rating", "Review_Date", "Cocoa_Percent")]
cor_matrix = cor(data_subset, use = "complete.obs")
cor_matrix_melt = reshape2::melt(cor_matrix)
ggplot(cor_matrix_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) + # Add correlation values
  scale_fill_gradient2(low = "blue", mid = "white", high = "violet", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Heatmap",
       x = "",
       y = "",
       fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold", color = "black"))

#Checking for Multicolinearity 
quantvars=data[,c(3,4,6)]  
corr_matrix=cor(quantvars)
round(corr_matrix,2)

#Checking for non-linearity
reg=lm(Rating~Review_Date+Cocoa_Percent,data)
residualPlots(reg)

######## PARRT 2b: Categorical VARIABLES########

#Checking Frequncy 
table(data$Company)
table(data$Specific_Bean_Origin)
table(data$Company_Location)
table(data$Bean_Type)
table(data$Broad_Bean_Origin)

# Checking Relative frequencies
prop.table(table(data$Company))
prop.table(table(data$Specific_Bean_Origin))
prop.table(table(data$Company_Location))
prop.table(table(data$Bean_Type))
prop.table(table(data$Broad_Bean_Origin))

#Using Visualization 
ggplot(data, aes(x = Company)) +
  geom_bar(fill = "purple") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Frequency of Companies", 
    x = "Company", 
    y = "Count"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "black")
  )

ggplot(data, aes(x = Company_Location)) +
  geom_bar(fill = "blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Frequency of Company Locations", 
    x = "Company Location", 
    y = "Count"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "black")
  )

#Checking Relationships between variables 
table(data$Company, data$Company_Location)
prop.table(table(data$Company, data$Company_Location), margin = 1)  


#Checking Impact on Ratings 
aggregate(Rating ~ Company, data = data, FUN = mean)
aggregate(Rating ~ Company_Location, data = data, FUN = mean)
aggregate(Rating ~ Specific_Bean_Origin, data = data, FUN = mean)
aggregate(Rating ~ Bean_Type, data = data, FUN = mean)
aggregate(Rating ~ Broad_Bean_Origin, data = data, FUN = mean)

# Using Visualization 
ggplot(data, aes(x = Company, y = Rating)) +
  geom_boxplot(fill = "violet") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Ratings by Company", x = "Company", y = "Rating") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "black"))

ggplot(data, aes(x = Company_Location, y = Rating)) +
  geom_boxplot(fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Ratings by Company Location", x = "Company Location", y = "Rating") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "black"))

ggplot(data, aes(x = Company, fill = Company_Location)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Company by Location", x = "Company", y = "Proportion") +
  scale_fill_viridis_d(option = "C") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "black"))

#Chisquare-test 
chisq.test(table(data$Company, data$Company_Location))

#Grouping variables to analyse further 

#Grouping per continent 
data$Continent = countrycode(data$Company_Location, origin = "country.name", destination = "continent")
table(data$Continent)

rating_by_continent = aggregate(Rating ~ Continent, data = data, FUN = mean)
print(rating_by_continent)

ggplot(rating_by_continent, aes(x = Continent, y = Rating, fill = Continent)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("purple", "blue", "magenta", "violet", "lightblue")) +
  labs(title = "Average Rating by Continent", x = "Continent", y = "Average Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5, face = 'bold',color = "black"))


ggplot(data, aes(x = Continent, y = Rating, fill = Continent)) +
  geom_boxplot() +
  theme_minimal() +
  scale_fill_manual(values = c("purple", "blue", "magenta", "violet", "lightblue")) +
  labs(title = "Distribution of Ratings by Continent", x = "Continent", y = "Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5,face = 'bold', color = "black"))


#Using Simple Linear Regression to analyse the predictor
reg1=lm(Rating~Company,data)
summary(reg1)

reg2=lm(Rating~Specific_Bean_Origin,data)
summary(reg2)

reg3=lm(Rating~Review_Date,data)
summary(reg3)

reg4=lm(Rating~Cocoa_Percent,data)
summary(reg4)

reg5=lm(Rating~Company_Location,data)
summary(reg5)

reg6=lm(Rating~Bean_Type,data)
summary(reg6)

reg7=lm(Rating~Broad_Bean_Origin,data)
summary(reg7)

# First image: 3 residual plots
png("Residual_Plots_1.png", width = 1200, height = 600) # Save first image
par(mfrow = c(1, 3)) # Layout for 3 plots in one row

residualPlot(reg1, quadratic = FALSE, main = "Residual Plot for Company")
residualPlot(reg2, quadratic = FALSE, main = "Residual Plot for Specific Bean Origin")
residualPlot(reg3, quadratic = FALSE, main = "Residual Plot for Review Date")

dev.off() # Close the graphics device

# Second image: 4 residual plots
png("Residual_Plots_2.png", width = 1600, height = 600) # Save second image
par(mfrow = c(2, 2)) # Layout for 2x2 grid

residualPlot(reg4, quadratic = FALSE, main = "Residual Plot for Cocoa Percent")
residualPlot(reg5, quadratic = FALSE, main = "Residual Plot for Company Location")
residualPlot(reg6, quadratic = FALSE, main = "Residual Plot for Bean Type")
residualPlot(reg7, quadratic = FALSE, main = "Residual Plot for Broad Bean Origin")

dev.off() # Close the graphics device
#Analysis heterodescadicity 
residualPlot(reg1,quadratic=FALSE) 
residualPlot(reg2,quadratic=FALSE) 
residualPlot(reg3,quadratic=FALSE) 
residualPlot(reg4,quadratic=FALSE) 
residualPlot(reg5,quadratic=FALSE) 
residualPlot(reg6,quadratic=FALSE) 
residualPlot(reg7,quadratic=FALSE) 

ncvTest(reg1)
ncvTest(reg2) 
ncvTest(reg3) 
ncvTest(reg4) 
ncvTest(reg5)
ncvTest(reg6) 
ncvTest(reg7) 

#Performing Anova Test on Comapanie related features 
anova=aov(Rating~Company+Company_Location, data)
summary(anova)

#Visualizing Rating against Categorical Variables 

rating_by_bean_type = aggregate(Rating ~ Bean_Type, data = data, FUN = mean)
ggplot(rating_by_bean_type, aes(x = Bean_Type, y = Rating, fill = Bean_Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_viridis_d(option = "C") + 
  labs(title = "Average Rating by Bean Type", x = "Bean Type", y = "Average Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5, color = "black"))

# Grouping by broad bean type 
table(data$Simplified_Bean_Type)
data$Simplified_Bean_Type = sub(" .*", "", data$Bean_Type)    
data$Simplified_Bean_Type = gsub("[,]", "", data$Simplified_Bean_Type)  
data$Simplified_Bean_Type = trimws(data$Simplified_Bean_Type) 
unique_simplified_bean_types = unique(data$Simplified_Bean_Type)
print(unique_simplified_bean_types)

rating_by_simplified_bean_type = aggregate(Rating ~ Simplified_Bean_Type, data = data, FUN = mean)
ggplot(rating_by_simplified_bean_type, aes(x = Simplified_Bean_Type, y = Rating, fill = Simplified_Bean_Type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_viridis_d(option = "C") +  
  labs(title = "Average Rating by Bean Type",
       x = " Bean Type",
       y = "Average Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5, face = 'bold', color = "black"))

ggplot(data, aes(x = Simplified_Bean_Type, y = Rating, fill = Simplified_Bean_Type)) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "C") +  
  theme_minimal() +
  labs(title = "Distribution of Ratings by  Bean Type",
       x = " Bean Type",
       y = "Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5, face = 'bold',color = "black"))

top_companies_by_freq = data %>%
  count(Company) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)  

data_top_freq = data %>% filter(Company %in% top_companies_by_freq$Company)

ggplot(data_top_freq, aes(x = Company, y = Rating, fill = Company)) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "C") +  
  theme_minimal() +
  labs(
    title = "Distribution of Ratings for Companies with Most Reviews",
    x = "Company",
    y = "Rating"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.2, face = "bold", color = "black", size = 16),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(color = "black")
  )

avg_rating_by_company = data %>%
  group_by(Company) %>%
  summarise(avg_rating = mean(Rating, na.rm = TRUE)) %>%
  arrange(desc(avg_rating))

top_companies = avg_rating_by_company %>%
  arrange(desc(avg_rating)) %>%
  slice_head(n = 10)

ggplot(top_companies, aes(x = reorder(Company, avg_rating), y = avg_rating, fill = Company)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "C") + 
  theme_minimal() +
  labs(title = "Average Ratings for Top 10 Companies",
       x = "Company",
       y = "Average Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5,face = 'bold', color = "black")) 

#Detecting Outliers 
data$Outlier_Cocoa = abs(scale(data$Cocoa_Percent)) > 3
data$Outlier_Rating = abs(scale(data$Rating)) > 3
table(data$Outlier_Cocoa, data$Outlier_Rating)

data$Cocoa_Percent = Winsorize(data$Cocoa_Percent, probs = c(0.05, 0.95))
data$Rating = Winsorize(data$Rating, probs = c(0.05, 0.95))
data = data[!data$Outlier_Cocoa & !data$Outlier_Rating, ]

ggplot(data, aes(x = Cocoa_Percent)) +
  geom_histogram(bins = 50, fill = "purple") +
  labs(title = "Cocoa Percent Distribution After Outlier Treatment",
       x = "Cocoa Percent",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', color = "black"))

# Rating Distribution After Outlier Treatment
data$Outlier_Cocoa = abs(scale(data$Cocoa_Percent)) > 3
data$Outlier_Rating = abs(scale(data$Rating)) > 3
table(data$Outlier_Cocoa, data$Outlier_Rating)

ggplot(data, aes(x = Rating)) +
  geom_histogram(bins = 50, fill = "blue") +
  labs(title = "Rating Distribution After Outlier Treatment",
       x = "Rating",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,face = 'bold', color = "black"))

#Analysing trend overtime 
ggplot(data, aes(x = Review_Date, y = Rating)) +
  geom_line(stat = "summary", fun = mean, color = "blue") +
  labs(title = "Average Rating Over Time", x = "Year", y = "Average Rating") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,face = 'bold', color = "black"))

ggplot(data, aes(x = Review_Date, y = Cocoa_Percent)) +
  geom_line(stat = "summary", fun = mean, color = "purple") +
  labs(title = "Average Cocoa Percent Over Time", x = "Year", y = "Average Cocoa Percent") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', color = "black"))

#Analysing rating per percentage group 
data$Cocoa_Group = cut(data$Cocoa_Percent, breaks = c(0, 50, 70, 85, 100), 
                       labels = c("0-50%", "50-70%", "70-85%", "85-100%"))

ggplot(data, aes(x = Cocoa_Group, y = Rating, fill = Cocoa_Group)) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "C") +  # Using viridis palette
  labs(title = "Ratings by Cocoa Percentage Group",
       x = "Cocoa Percentage Group",
       y = "Rating") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face ='bold', color = "black"))

######## PART 3: MODELING ########

######## Objective 1: Identify Cocoa Bean Origins Less Prone to Volatility ########

#Calculating Rating Volatility for Each Broad Bean Origin
volatility_data = data %>%
  group_by(Broad_Bean_Origin) %>%
  summarize(
    Mean_Rating = mean(Rating, na.rm = TRUE),
    Rating_SD = sd(Rating, na.rm = TRUE),   
    Count = n()
  ) %>%
  filter(Count > 5)  

# Preparing Data for Clustering
clustering_data = volatility_data[, c("Mean_Rating", "Rating_SD")]
clustering_data_scaled = scale(clustering_data)
# Applying K-means Clustering
set.seed(123)  
kmeans_result = kmeans(clustering_data_scaled, centers = 3, nstart = 25)  
volatility_data$Cluster = kmeans_result$cluster

#  Visualizing Clustering Results
fviz_cluster(kmeans_result, data = clustering_data_scaled,
             geom = "point", ellipse.type = "euclid", 
             ggtheme = theme_minimal(),
             main = "Clustering of Broad Bean Origins Based on Rating Volatility")

fviz_cluster(kmeans_result, 
             data = clustering_data_scaled, # Ensure data is passed
             geom = "point", 
             ellipse.type = "euclid", 
             ggtheme = theme_minimal(),
             palette = c("#619CFF", "#C77CFF", "#F8766D"), # Custom colors: blue, purple, pink
             main = "Clustering of Broad Bean Origins Based on Rating Volatility") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold") # Center and bold title
  )

# Analyzing Clusters
cluster_summary = volatility_data %>%
  group_by(Cluster) %>%
  summarize(
    Avg_Mean_Rating = mean(Mean_Rating),
    Avg_Rating_SD = mean(Rating_SD),
    Count = n()
  )
print(cluster_summary)

ggplot(volatility_data, aes(x = Mean_Rating, y = Rating_SD, color = factor(Cluster))) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "Clustering of Broad Bean Origins",
    x = "Mean Rating",
    y = "Rating Volatility (SD)",
    color = "Cluster"
  ) +
  scale_color_manual(values = c("blue", "purple", "pink")) +  # Set cluster colors
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Bold and centered title
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  )

#Printing final Summary 
print(cluster_summary)

clustered_origins = volatility_data[, c("Broad_Bean_Origin", "Cluster", "Mean_Rating", "Rating_SD")]
cluster_1 = clustered_origins[clustered_origins$Cluster == 1, ]
cluster_2 = clustered_origins[clustered_origins$Cluster == 2, ]
cluster_3 = clustered_origins[clustered_origins$Cluster == 3, ]

cat("Cluster 1 (High Rating, Low Volatility):\n")
print(cluster_1)

cat("\nCluster 2 (Moderate Rating, Higher Volatility):\n")
print(cluster_2)

cat("\nCluster 3 (Low Rating, High Volatility):\n")
print(cluster_3)

######## Objective 2: Regional Taste Preferences with LDA ########
######## Objective 2a: CONTINENT ANALYSIS ########

# Inspecting Continent Mapping and Rating Aggregation (Already Done in EDA)
cat("Continent Distribution:\n")
print(table(data$Continent))
cat("\nAverage Rating by Continent:\n")
rating_by_continent = aggregate(Rating ~ Continent, data = data, FUN = mean)
print(rating_by_continent)

# Preparing LDA Data
lda_data = data %>%
  dplyr::select(Continent, Company_Location, Cocoa_Percent, Rating) %>% 
  filter(Continent != "Other") 
lda_data = na.omit(lda_data)
# Standardizing Features
lda_data = lda_data %>%
  mutate(
    Cocoa_Percent = scale(Cocoa_Percent),
    Rating = scale(Rating)
  )
cat("Prepared LDA Data:\n")
print(head(lda_data))

#Train-Test Split
set.seed(123) 
train_indices = sample(1:nrow(lda_data), 0.7 * nrow(lda_data))
train_data = lda_data[train_indices, ]
test_data = lda_data[-train_indices, ]

lda_model = lda(Continent ~ Cocoa_Percent + Rating, data = train_data)
cat("\nLDA Model Summary:\n")
print(lda_model)

#Evaluating on Test Data
lda_pred = predict(lda_model, test_data)

confusion_matrix = table(Predicted = lda_pred$class, Actual = test_data$Continent)
cat("\nConfusion Matrix:\n")
print(confusion_matrix)

accuracy = sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("\nModel Accuracy:", accuracy, "\n")

# Visualization
lda_coefficients = as.data.frame(lda_model$scaling)
lda_coefficients$Feature = rownames(lda_coefficients)

ggplot(lda_coefficients, aes(x = reorder(Feature, LD1), y = LD1)) +
  geom_bar(stat = "identity", fill = "blueviolet") +
  coord_flip() +
  labs(
    title = "Feature Contributions to LD1",
    x = "Feature",
    y = "Coefficient"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  )

test_data$Predicted = lda_pred$class
ggplot(test_data, aes(x = Cocoa_Percent, y = Rating, color = Predicted)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("blue", "purple", "magenta", "violet", "lightblue")) +
  labs(
    title = "LDA Predictions by Continent",
    x = "Cocoa Percent (Standardized)",
    y = "Rating (Standardized)",
    color = "Predicted Continent"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  )
######## Objective 2b: RE_RUNNING WITH SOUTH AMERICA ########
# Define a list of South American countries
south_america_countries = c(
  "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", 
  "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela"
)

#Filtering to south amaerica 
south_america_data = data %>%
  filter(Company_Location %in% south_america_countries) %>%
  dplyr::select(Company_Location, Cocoa_Percent, Rating) %>%
  mutate(
    Company_Location = as.factor(Company_Location), # Convert to factor for classification
    Cocoa_Percent = scale(Cocoa_Percent),
    Rating = scale(Rating)
  )

cat("Country Distribution in South America:\n")
print(table(south_america_data$Company_Location))

set.seed(123) 
train_indices = sample(1:nrow(south_america_data), 0.7 * nrow(south_america_data))
train_data = south_america_data[train_indices, ]
test_data = south_america_data[-train_indices, ]

set.seed(123)
rf_model_sa = randomForest(
  Company_Location ~ Cocoa_Percent + Rating,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

cat("\nRandom Forest Model Summary:\n")
print(rf_model_sa)

rf_pred_sa = predict(rf_model_sa, test_data)

confusion_matrix_rf_sa = table(Predicted = rf_pred_sa, Actual = test_data$Company_Location)
cat("\nConfusion Matrix (South America - Random Forest):\n")
print(confusion_matrix_rf_sa)


accuracy_rf_sa = sum(diag(confusion_matrix_rf_sa)) / sum(confusion_matrix_rf_sa)
cat("\nModel Accuracy (South America - Random Forest):", accuracy_rf_sa, "\n")

test_data$Predicted = rf_pred_sa
ggplot(test_data, aes(x = Cocoa_Percent, y = Rating, color = Predicted)) +
  geom_point() +
  labs(
    title = "Random Forest Predictions by Country (South America)",
    x = "Cocoa Percent (Standardized)",
    y = "Rating (Standardized)"
  ) +
  theme_minimal()

importance_data = as.data.frame(importance(rf_model_sa))
importance_data$Feature = rownames(importance_data)
ggplot(importance_data, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Feature Importance in Random Forest (South America)",
    x = "Feature",
    y = "Mean Decrease in Gini"
  )

#### This predictited very very low accuracy thus it is better to use another Model able to metigate the class impblance such as hirarchical clustering 
######## Objective 2c: HIERARCHICAL CLUSTERING SOUTH AMERICA ########
#Filtering south america
south_america_data = data %>%
  filter(Company_Location %in% c(
    "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", 
    "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", 
    "Uruguay", "Venezuela"
  )) %>%
  dplyr::select(Company_Location, Cocoa_Percent, Rating) 

# Standardizing numerical features (Cocoa_Percent and Rating)
south_america_data_scaled = south_america_data %>%
  mutate(
    Cocoa_Percent = scale(Cocoa_Percent),
    Rating = scale(Rating)
  )
distance_matrix = dist(south_america_data_scaled %>% dplyr::select(Cocoa_Percent, Rating)) 
hclust_model = hclust(distance_matrix, method = "ward.D")

# Ploting the dendrogram
plot(
  hclust_model, 
  labels = south_america_data$Company_Location, 
  main = "Hierarchical Clustering Dendrogram",
  xlab = "Countries", 
  sub = "",
  cex = 0.8
)

# Truncate labels to the first few characters
short_labels <- substr(south_america_data$Company_Location, 1, 3)

# Plot with shortened labels
plot(
  hclust_model, 
  labels = short_labels, 
  main = "Hierarchical Clustering Dendrogram",
  xlab = "Countries", 
  sub = "",
  cex = 0.8
)


plot(
  hclust_model, 
  labels = south_america_data$Company_Location, 
  main = "Hierarchical Clustering Dendrogram",
  xlab = "Countries", 
  sub = "",
  cex = 0.5  # Reduce label size
)

dev.off()

# Adding cluster labels to the data
south_america_data_scaled$Cluster = clusters

# Visualizing the clusters
ggplot(south_america_data_scaled, aes(x = Cocoa_Percent, y = Rating, color = factor(Cluster))) +
  geom_point(size = 3) +
  scale_color_manual(values = c("blue", "purple", "magenta", "violet")) +
  labs(
    title = "Hierarchical Clustering of South America (Cacoa Percent vs Ratings)",
    x = "Cocoa Percent (Standardized)",
    y = "Rating (Standardized)",
    color = "Cluster"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "black"), # Center and bold title
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  )

# Printing the cluster assignments
cat("Cluster Assignments:\n")
print(table(south_america_data_scaled$Cluster, south_america_data_scaled$Company_Location))

# Summarizing the composition of each cluster
cluster_summary = south_america_data_scaled %>%
  group_by(Cluster) %>%
  summarise(
    Countries = paste(unique(Company_Location), collapse = ", "),
    Avg_Cocoa_Percent = mean(Cocoa_Percent, na.rm = TRUE),
    Avg_Rating = mean(Rating, na.rm = TRUE),
    Count = n()
  )

cat("Cluster Composition Summary:\n")
print(cluster_summary)

ggplot(south_america_data_scaled, aes(x = factor(Cluster), fill = factor(Cluster))) +
  geom_bar() +
  scale_fill_manual(values = c("blue", "purple", "magenta", "violet")) +
  labs(
    title = "Cluster Composition",
    x = "Cluster",
    y = "Number of Countries",
    fill = "Cluster"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  )

cluster_details = south_america_data_scaled %>%
  dplyr::select(Company_Location, Cocoa_Percent, Rating, Cluster) %>%
  arrange(Cluster)

cat("Detailed Cluster Data:\n")
print(cluster_details)

######## Objective 3: Cocoa Percentage Correlation with Chocolate Ratings ########
# Preparing the data
data_regression = data %>%
  dplyr::select(Cocoa_Percent, Rating) %>%
  na.omit() 

print(names(data_regression))
# Train-Test Split
set.seed(123) 
train_indices = sample(1:nrow(data_regression), 0.7 * nrow(data_regression))
train_data = data_regression[train_indices, ]
test_data = data_regression[-train_indices, ]

# Fitting a Regression Tree
reg_tree_model = rpart(
  Rating ~ Cocoa_Percent, 
  data = train_data,
  method = "anova",
  control = rpart.control(cp = 0.01) 
)

# Printing Model Summary
cat("\nRegression Tree Model Summary:\n")
printcp(reg_tree_model)
# Printing Model Summary
cat("\nRegression Tree Model Summary:\n")
printcp(reg_tree_model)

# Visualizing the Regression Tree
rpart.plot(
  reg_tree_model,
  main = "Regression Tree: Cocoa Percent vs Rating",
  type = 3, 
  box.palette = c("pink", "magenta"), # Updated color palette
  fallen.leaves = TRUE,
  tweak = 1.2 # Adjusts text size for clarity
)

#Evaluating Model Performance
predictions = predict(reg_tree_model, test_data)
# Calculating RMSE (Root Mean Squared Error)
rmse = sqrt(mean((predictions - test_data$Rating)^2))
cat("\nRMSE on Test Data:", rmse, "\n")

# Visualizing Predictions vs Actual Ratings
ggplot(test_data, aes(x = Rating, y = predictions)) +
  geom_point(color = "purple", alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "magenta") +
  labs(
    title = "Predicted vs Actual Ratings",
    x = "Actual Rating",
    y = "Predicted Rating"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  )

# Analyzing Variable Importance
importance = as.data.frame(varImp(reg_tree_model))
cat("\nVariable Importance:\n")
print(importance)

# Extracting Key Insights from the Tree
cat("\nRegression Tree Splits:\n")
print(reg_tree_model$frame)

ggplot(data_regression, aes(x = Cocoa_Percent, y = Rating)) +
  geom_point(alpha = 0.6, color = "blueviolet", size = 2) +
  stat_smooth(method = "loess", color = "red") +
  labs(
    title = "Cocoa Percentage vs Chocolate Ratings",
    x = "Cocoa Percent",
    y = "Rating"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "black"), # Bold and center the title
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  )

######## Objective 4: Predict Chocolate Bar Ratings Using Logistic Regression ########
# Data Preparation
data_logistic = data %>%
  mutate(
    High_Rating = ifelse(Rating > 3.5, 1, 0),
    Cocoa_Percent = scale(Cocoa_Percent),    
    Broad_Bean_Origin = as.factor(Broad_Bean_Origin), 
    Company_Location = as.factor(Company_Location)
  ) %>%
  na.omit() 

# Putting into rare category 
data_logistic$Broad_Bean_Origin = fct_lump(data_logistic$Broad_Bean_Origin, n = 10)
data_logistic$Company_Location = fct_lump(data_logistic$Company_Location, n = 10)

# Train-Test Split
set.seed(123)
train_indices = sample(1:nrow(data_logistic), 0.7 * nrow(data_logistic))
train_data = data_logistic[train_indices, ]
test_data = data_logistic[-train_indices, ]

# Ensuring test data has the same factor levels as training data
test_data$Broad_Bean_Origin = factor(test_data$Broad_Bean_Origin, levels = levels(train_data$Broad_Bean_Origin))
test_data$Company_Location = factor(test_data$Company_Location, levels = levels(train_data$Company_Location))

# Training Logistic Regression Model
logistic_model = glm(
  High_Rating ~ Cocoa_Percent + Broad_Bean_Origin + Company_Location,
  data = train_data,
  family = binomial
)
cat("\nLogistic Regression Model Summary:\n")
summary(logistic_model)

# Predicting and Performance Evaluation
test_data$Predicted_Prob = predict(logistic_model, newdata = test_data, type = "response")
test_data$Predicted = ifelse(test_data$Predicted_Prob > 0.5, 1, 0)

confusion_matrix = table(Predicted = test_data$Predicted, Actual = test_data$High_Rating)
cat("\nConfusion Matrix:\n")
print(confusion_matrix)

accuracy = sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision = ifelse(sum(confusion_matrix[2, ]) == 0, NA, confusion_matrix[2, 2] / sum(confusion_matrix[2, ]))
recall = ifelse(sum(confusion_matrix[, 2]) == 0, NA, confusion_matrix[2, 2] / sum(confusion_matrix[, 2]))
cat("\nAccuracy:", accuracy)
cat("\nPrecision:", precision)
cat("\nRecall:", recall)

roc_obj = roc(test_data$High_Rating, test_data$Predicted_Prob, quiet = TRUE)
auc_value = auc(roc_obj)
cat("\nAUC:", auc_value)

#Visualization 
plot(roc_obj, col = "blueviolet", main = "ROC Curve for Logistic Regression")
abline(a = 0, b = 1, lty = 2, col = "magenta")

ggplot(test_data, aes(x = Cocoa_Percent, y = Predicted_Prob, color = as.factor(High_Rating))) +
  geom_point(alpha = 0.6, size = 2) +
  stat_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("violet", "blue")) + # Updated color palette
  labs(
    title = "Cocoa Percent vs Predicted Probability",
    x = "Cocoa Percent (Standardized)",
    y = "Predicted Probability of High Rating",
    color = "High Rating"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold', color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  )

######## Objective 4b: Predict Chocolate Bar Ratings Using Random Forest and Gradient Boosting ########

#  Testing Random Forest Model for comparaison 
train_data$High_Rating = as.factor(train_data$High_Rating)
test_data$High_Rating = as.factor(test_data$High_Rating)

set.seed(123)
rf_model = randomForest(
  High_Rating ~ Cocoa_Percent + Broad_Bean_Origin + Company_Location,
  data = train_data,
  ntree = 500,
  mtry = 2, 
  importance = TRUE
)

cat("\nRandom Forest Model Summary:\n")
print(rf_model)


test_data$RF_Predicted_Prob = predict(rf_model, newdata = test_data, type = "prob")[, 2]
test_data$RF_Predicted = ifelse(test_data$RF_Predicted_Prob > 0.5, 1, 0)

rf_confusion_matrix = table(Predicted = test_data$RF_Predicted, Actual = test_data$High_Rating)
cat("\nRandom Forest Confusion Matrix:\n")
print(rf_confusion_matrix)

rf_accuracy = sum(diag(rf_confusion_matrix)) / sum(rf_confusion_matrix)
rf_precision = ifelse(sum(rf_confusion_matrix[2, ]) == 0, NA, rf_confusion_matrix[2, 2] / sum(rf_confusion_matrix[2, ]))
rf_recall = ifelse(sum(rf_confusion_matrix[, 2]) == 0, NA, rf_confusion_matrix[2, 2] / sum(rf_confusion_matrix[, 2]))
cat("\nRandom Forest Accuracy:", rf_accuracy)
cat("\nRandom Forest Precision:", rf_precision)
cat("\nRandom Forest Recall:", rf_recall)

rf_roc_obj = roc(as.numeric(test_data$High_Rating) - 1, test_data$RF_Predicted_Prob, quiet = TRUE)
rf_auc_value = auc(rf_roc_obj)
cat("\nRandom Forest AUC:", rf_auc_value)

plot(
  roc_obj, 
  col = "blueviolet", 
  main = "ROC Curves for Models", 
  lwd = 2, 
  print.auc = TRUE, 
  print.auc.y = 0.4
)

plot(
  rf_roc_obj, 
  col = "blue", 
  lwd = 2, 
  print.auc = TRUE, 
  print.auc.y = 0.3, 
  add = TRUE
)
legend(
  "bottomright", 
  legend = c("Logistic Regression", "Random Forest"), 
  col = c("blueviolet", "blue"), 
  lwd = 2
)

######## Objective 4c: Predict Chocolate Bar Ratings Using Gradient Boosting ########

#  Testing Gradient Boosting for comparaison
train_data$High_Rating = ifelse(train_data$High_Rating == 1, 1, 0)
test_data$High_Rating = ifelse(test_data$High_Rating == 1, 1, 0)
set.seed(123)
gbm_model = gbm(
  High_Rating ~ Cocoa_Percent + Broad_Bean_Origin + Company_Location,
  data = train_data,
  distribution = "bernoulli", 
  n.trees = 1000, 
  interaction.depth = 3, 
  shrinkage = 0.01, 
  cv.folds = 5,
  verbose = TRUE
)
cat("\nGradient Boosting Model Summary:\n")
summary(gbm_model)

test_data$GBM_Predicted_Prob = predict(
  gbm_model, 
  newdata = test_data, 
  n.trees = gbm.perf(gbm_model, method = "cv"), 
  type = "response"
)
test_data$GBM_Predicted = ifelse(test_data$GBM_Predicted_Prob > 0.5, 1, 0)

gbm_confusion_matrix = table(Predicted = test_data$GBM_Predicted, Actual = test_data$High_Rating)
cat("\nGradient Boosting Confusion Matrix:\n")
print(gbm_confusion_matrix)

gbm_accuracy = sum(diag(gbm_confusion_matrix)) / sum(gbm_confusion_matrix)
gbm_precision = ifelse(sum(gbm_confusion_matrix[2, ]) == 0, NA, gbm_confusion_matrix[2, 2] / sum(gbm_confusion_matrix[2, ]))
gbm_recall = ifelse(sum(gbm_confusion_matrix[, 2]) == 0, NA, gbm_confusion_matrix[2, 2] / sum(gbm_confusion_matrix[, 2]))
cat("\nGradient Boosting Accuracy:", gbm_accuracy)
cat("\nGradient Boosting Precision:", gbm_precision)
cat("\nGradient Boosting Recall:", gbm_recall)
gbm_roc_obj = roc(test_data$High_Rating, test_data$GBM_Predicted_Prob, quiet = TRUE)
gbm_auc_value = auc(gbm_roc_obj)
cat("\nGradient Boosting AUC:", gbm_auc_value)


# Visualization: 
plot(roc_obj, col = "blueviolet", main = "ROC Curves with AUC for Models", lwd = 2)
lines(rf_roc_obj, col = "blue", lwd = 2)
lines(gbm_roc_obj, col = "magenta", lwd = 2)
legend(
  "bottomright", 
  legend = c(
    paste("Logistic Regression"),
    paste("Random Forest"),
    paste("Gradient Boosting")
  ),
  col = c("blueviolet", "blue", "magenta"),
  lwd = 2,
  cex = 0.9,
  bg = "white"
) 
abline(a = 0, b = 1, lty = 2, col = "gray")

text(0.1, 0.6, paste("Logistic AUC:", round(auc(roc_obj), 3)), col = "blueviolet", cex = 0.9)
text(0.1, 0.5, paste("Random Forest AUC:", round(auc(rf_roc_obj), 3)), col = "blue", cex = 0.9)
text(0.1, 0.4, paste("Gradient Boosting AUC:", round(auc(gbm_roc_obj), 3)), col = "magenta", cex = 0.9)


# Training  Gradient Boosting as it outperform logitic model 
gbm_model = gbm(
  High_Rating ~ Cocoa_Percent + Broad_Bean_Origin + Company_Location,
  data = train_data,
  distribution = "bernoulli", 
  n.trees = 1000,
  interaction.depth = 3,
  shrinkage = 0.01,
  cv.folds = 5,
  verbose = TRUE
)

feature_importance = summary(gbm_model)
print(feature_importance)

test_data$Predicted_Prob = predict(
  gbm_model, 
  newdata = test_data, 
  n.trees = gbm.perf(gbm_model, method = "cv"), 
  type = "response"
)

test_data$Category = paste(test_data$Broad_Bean_Origin, test_data$Company_Location, sep = "-")
test_data = test_data %>%
  arrange(desc(Predicted_Prob))

category_performance = test_data %>%
  group_by(Category) %>%
  summarise(
    Average_Prob = mean(Predicted_Prob),
    Count = n()
  ) %>%
  arrange(desc(Average_Prob))
print(category_performance)

top_products = test_data %>%
  filter(Predicted_Prob > 0.8) 
print(top_products)

top_10_categories = category_performance %>%
  slice_max(order_by = Average_Prob, n = 10)
ggplot(top_10_categories, aes(x = reorder(Category, -Average_Prob), y = Average_Prob)) +
  geom_bar(stat = "identity", fill = ifelse(top_10_categories$Average_Prob > 0.4, "purple", "blue")) +
  coord_flip() +
  labs(
    title = "Top 10 Categories by Predicted Probabilities",
    x = "Category",
    y = "Average Predicted Probability"
  ) +
  theme_minimal() +
  geom_hline(yintercept = 0.4, linetype = "dashed", color = "darkgrey") +
  theme(
    axis.text.y = element_text(size = 10, color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


