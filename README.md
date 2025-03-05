# Project Title

This project analyzes collective agreements data using various statistical methods, including clustering and regression analysis.

## Data Description

The dataset contains the following columns:

- **antalanstallda_UC**: Number of Employees
- **Omsättning_SEK**: Revenue (in SEK)
- **Total_lön**: Total Salary
- **Genomsnittlig_lön_Inkl..Pension_TSEK**: Average Salary Including Pension (in TSEK)
- **Resultat**: Result
- **Cluster**: Cluster assignment from K-means clustering

## Libraries Used

- `caTools`: For data splitting
- `randomForest`: For building random forest models
- `cluster`: For clustering algorithms
- `ggplot2`: For data visualization
- `factoextra`: For visualizing clustering results
- `knitr`: For dynamic report generation
- `scales`: For scaling data in plots

## Analysis Steps

1. **Data Preparation**: The data is cleaned by removing missing values and outliers.
2. **Clustering**: K-means clustering is performed to group similar data points.
3. **Modeling**: Linear regression and random forest models are built to predict the results based on selected features.
4. **Results Comparison**: The performance of different models is compared using metrics like MAE, MSE, and RMSE.

## Generated Plots

The code generates various plots during the analysis, which are saved in the `data/graphs` directory for easy access and organization.

## How to Run the Code

1. Ensure you have R and the required libraries installed.
2. Set the working directory to the location of the dataset.
3. Run the script to perform the analysis and generate plots. 