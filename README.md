Inspiration
To anticipate the demand for medical supplies we are proposing a solution where we analyze the population for the diseases and then keep the stock of the medical supplies required to treat the disease in the nearest pharmacy/ warehouse.

What it does
Based on the results obtained from the model we calculated the difference between the forecasted values of Collin County and Tarrant County and determined the place with the highest forecasted incidence of dengue cases

Based on the incidence of the dengue cases we determined which county’s warehouse can stock up the resources.

How we built it
For making the prototype of the project we are currently taking the data of the Dengue Disease Incidence from two Counties of Texas - Collin, and Tarrant

Each of the data is taken from a CSV file and preprocessed to get the count of dengue cases for each County per month from - 2016 to 2018

From the data, we created a time series and fitted the data using an ARIMA model

Challenges we ran into
Since the data was not properly formatted we had to do a lot of preprocessing on the data.

Accomplishments that we're proud of
The forecasting model will accurately determine where to stock supplies at.

What we learned
The new thing we learned was forecasting time series data.

What's next for Forecasting future demand of Medical Supplies
We can scale this project to include populations from multiple regions

Currently, the prediction is done only based on Dengue Disease, but this can be scaled to include wider set of diseases.

With the results of the Regression and Prediction model, we can plan of starting a new distribution system that can cater to the upcoming needs which cannot be met by our current 37 distribution centers.
