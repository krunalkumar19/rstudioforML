# Insurance Claims Prediction using Machine Learning
## Overview
This repository contains an RStudio-based machine learning project focused on predicting insurance claims using a dataset from Snowflake. 
The code demonstrates how to load, clean, transform, and analyze data to build machine learning models for predicting insurance claims.

## Project Structure
- Data Preprocessing: The dataset is loaded from Snowflake and combined into a single dataframe. 
- The data is then cleaned and transformed to ensure it is ready for modeling.
- Machine Learning Models: Various machine learning models, including Random Forest and Generalized Linear Models (GLM), are applied to predict insurance claims.
- Model Evaluation: The performance of the models is evaluated using common metrics, such as accuracy, precision, recall, and F1 score.

## Key Features
- Data Integration: The project connects multiple Snowflake-based datasets into one dataframe, ensuring that all relevant information is available for analysis.
- Data Cleaning: Missing values and irrelevant columns are handled, ensuring the dataset is prepared for accurate predictions.
- Model Building: The code applies several machine learning algorithms, including Random Forest and GLM, to predict insurance claims.
- Evaluation Metrics: The models' performance is evaluated using common classification metrics.

## Outcomes
- The project provides valuable insights into the ability of different machine learning models to predict insurance claims. 
- By utilizing data from Snowflake and transforming it into a suitable format, the models generate accurate predictions, which insurance companies can use to improve their claim management processes. 
- The results of the machine learning models can help reduce claim processing times, optimize resource allocation, and improve customer satisfaction.

## Value
- This project demonstrates the application of machine learning techniques in the insurance industry. 
- It highlights the importance of data cleaning, transformation, and model evaluation when working with real-world datasets. 
- The models can be used as a basis for automating insurance claim predictions, enabling better decision-making and operational efficiency.

## How to Run the Code
- Install Rstudio
- Install the necessary R packages (e.g., dplyr, randomForest, caret).
- Find the dataset in the repository and use it.
- **There are 2 CSV files, one is raw data and the second one is scrubbed for the script's use.**
- Set up a connection to the dataset (Locally) and ensure the dataset is accessible.
- Run the R script to load, clean, and analyze the data.
- Train the machine learning models and evaluate their performance.

## Technologies Used
- RStudio: For data analysis and machine learning.
- Random Forest & GLM: Machine learning algorithms used for model training.
dplyr, caret: R packages for data manipulation and machine learning.
