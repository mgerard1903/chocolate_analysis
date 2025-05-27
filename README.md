# Chocolate Bar Ratings Analysis and Modeling

_A comprehensive R-based analysis of chocolate bar ratings to uncover consumer preferences, regional patterns, and predictive models._

## Table of Contents

1. [Description](#description)  
2. [Key Features](#key-features)  
3. [Tech Stack](#tech-stack)  
4. [Installation](#installation)  
5. [Datasets](#datasets)  
6. [Usage](#usage)  
7. [Project Structure](#project-structure)  
8. [Configuration](#configuration)

---

## Description

This project explores a dataset of chocolate bar reviews to analyze key drivers of consumer ratings. It includes data cleaning, exploratory data analysis, clustering, discriminant analysis, regression trees, and classification models to derive actionable insights for the chocolate industry.

## Key Features

- **Data Cleaning**: Handles missing values, converts cocoa percentage, removes duplicates and outliers.  
- **Exploratory Analysis**: Visualizes distributions, correlations, and relationships across variables.  
- **Categorical Analysis**: Frequencies and boxplots for companies, bean types, and locations.  
- **Clustering**: K-means and hierarchical clustering of bean origins based on rating volatility and regional preferences.  
- **Discriminant Analysis**: Linear Discriminant Analysis to assess continental taste differences.  
- **Predictive Modeling**: Regression trees for rating prediction; logistic regression, random forest, and gradient boosting to classify high-rated bars.  

## Tech Stack

- **Language:** R  
- **Script:** `Chocolate Project.R`  
- **Libraries:** tidyverse (ggplot2, dplyr), psych, countrycode, cluster, factoextra, randomForest, rpart, gbm, MASS, caret, DescTools, pROC, forcats, viridis  

## Installation

```bash
git clone https://github.com/your-username/chocolate-ratings-analysis.git
cd chocolate-ratings-analysis
# Ensure R and required packages are installed:
Rscript -e "install.packages(c('ggplot2','dplyr','psych','countrycode','cluster','factoextra','randomForest','rpart','gbm','MASS','caret','DescTools','pROC','forcats','viridis'))"
```

## Datasets

- **`Dataset 4 — Chocolate bar ratings.csv`**: Main dataset of 1,759 reviews with attributes like cocoa percentage, company, origin, ratings, and review date.  
- Place the CSV in the working directory before running the analysis.

## Usage

1. **Run the analysis script**:  
   ```bash
   Rscript "Chocolate Project.R"
   ```  
2. **Review outputs**:  
   - Cleaned dataset summary and missing value report.  
   - Plots (histograms, boxplots, heatmaps) saved to PNG files.  
   - Cluster visualizations (`fviz_cluster`) and dendrograms.  
   - Model summaries and diagnostic plots (regression tree, ROC curves).  

## Project Structure

```
chocolate-ratings-analysis/
├── README_CHOCOLATE.md        # This file
├── Chocolate Project.R        # R analysis script
├── Dataset 4 — Chocolate bar ratings.csv  # Raw data
├── outputs/                   # Generated plots and model outputs
│   ├── Rating_Histogram.png
│   ├── Correlation_Heatmap.png
│   ├── Cluster_Plots/
│   └── Model_Results/
└── final_anonymous.pdf        # Project report
```

## Configuration

- **Outlier thresholds**: Adjust z-score limits in the script for Cocoa_Percent and Rating.  
- **Clustering parameters**: Change `centers` in `kmeans()` and `method` in `hclust()`.  
- **Model tunings**: Modify hyperparameters for `rpart`, `randomForest`, and `gbm` within the script.
