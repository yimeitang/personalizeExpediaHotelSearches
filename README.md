# personalizeExpediaHotelSearches

The original data set is obtained from Kaggle.com and consists of a representative sample of 99,917,530
hotels. Also, 52 features are included, such as: price, user history, competitors, etc.

1. The booking rate is 2.74%.
2. The click-through-rate is 4.44%
3. The conversion rate is 61.6 %


# Step 1: Analyze and clean the data(it is a sparse dataset with 10M rows and 52 features)
![booking_vs_location](https://user-images.githubusercontent.com/27776652/32145958-a929601a-bc9e-11e7-9902-2c277cbc35d6.PNG)
![booking_vs_review_score](https://user-images.githubusercontent.com/27776652/32145959-a938c65e-bc9e-11e7-8e57-013553bccb7f.PNG)
![missing_data](https://user-images.githubusercontent.com/27776652/32145964-a98bfb4e-bc9e-11e7-98fc-d81e82aba258.png)
![mice_imputation](https://user-images.githubusercontent.com/27776652/32145963-a97d1066-bc9e-11e7-9532-f008e8c10679.PNG)

# Step 2: Conduct PCA ,CFA and other feature selection methods to choose most relevant features
![pca](https://user-images.githubusercontent.com/27776652/32145966-a9aedbbe-bc9e-11e7-9761-f47a34ff854f.PNG)
![extratreeclassifier](https://user-images.githubusercontent.com/27776652/32145961-a957bdfc-bc9e-11e7-966d-8a184b289374.PNG)
![feature_selection_results](https://user-images.githubusercontent.com/27776652/32145962-a96a1024-bc9e-11e7-8d99-15fa9f0acabf.PNG)

# Step 3: Build different models in CV to achieve better accuracy
![decision_tree](https://user-images.githubusercontent.com/27776652/32146012-419df158-bc9f-11e7-867e-145e9fcd6966.PNG)
![random_forest](https://user-images.githubusercontent.com/27776652/32146013-41c17f42-bc9f-11e7-96db-8741f802b9e4.PNG)

# Step 4: Compare model performance.
![model performance comparison](https://user-images.githubusercontent.com/27776652/32145965-a99e873c-bc9e-11e7-9c54-8868cbbd68ec.PNG)

