# Mercari-Price-Suggestion
         Project Report on Mercari Price Suggestion

Abstract
Mercari, Japan’s biggest community-powered shopping app, knows this problem deeply. They would like to offer pricing suggestions to sellers, but this is tough because their sellers are enabled to put just about anything, or any bundle of things, on Mercari's marketplace.
Mercari Price Suggestion challenge is to build an algorithm that automatically suggests the right product prices with a model of least RMS value for accurate pricings.


Introduction
Product pricing gets even harder at scale, considering just how many products are sold online. Clothing has strong seasonal pricing trends and is heavily influenced by brand names, while electronics have fluctuating prices based on product specs.
Mercari Price Suggestion challenge is to build an algorithm that automatically suggests the right product prices. You’ll be provided user-inputted text descriptions of their products, including details like product category name, brand name, and item condition and the data scientist has to build a model which suggests a right price for a right product on considering all the aspects of the product .

Drive link for dataset: https://drive.google.com/file/d/12p4cp7XUvVenmZQA6Z4zK2AmcsgDmYWz/view?usp=sharing
 
Overview
The project folder consists of mainly 3 sub – folders.

1. datasets.

2. output.

3. source codes.

4. model.

1.Datasets.
The folder containing the test and train datasets which is required for the project.

2.Output.
It contains the sub – folder namely.

• Plots
Contains the various wordcloud of the extracted words, various brand – stats. 

• Stastistics
Contains all analysed out files such as the price prediction,average brand prices,word_count which are in .csv file format.

3. Source code.
Contains source_code.r and word_count.r file.

4. Model
A RF model(random_forest_model.rda) with parameters mtry=7 and ntree=7 .

Conclusion
On the above datasets, the required data pre-processing is done and a random forest model was built on top of it as it fits good than other models for the given test and train datasets giving an optimal RMS value of “0.639456” with a value for mtry=7 and ntree=7 and an approximate computation time of around 50 minutes to build a model.



