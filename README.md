# NetflixMovieRatingPredictionandRecommendationAnalysis

## Introduction

The streaming industry is a significant player in the global market, with companies constantly vying for the attention of their audiences. A crucial aspect of this competition is the ability to recommend engaging content to users. However, predicting user ratings for movies and series is a complex task with various factors at play. This project aimed to predict user review ratings based on historical user review data, initially focusing on movie metadata.

## Executive Summary

In our research, we explored the challenges and insights related to predicting user review ratings on streaming platforms. We made several key observations:

- The assumption that users watch each film entirely may not always hold, affecting their review ratings.
- Not all viewers leave a review, making it challenging to gather comprehensive user feedback.
- Predictive modeling for average movie ratings based on historical data had limited effectiveness.
- Personalized recommendations based on customer metadata, such as age, location, and favorite genre, may offer better results.
- Movie ratings are highly subjective, and additional research into the effects of customer descriptors on movie ratings could be beneficial.

## Data Description and Exploratory Analysis

We used a dataset comprising user ratings, movie details, and IMDb information to conduct our analysis. We performed data cleaning and filtering to focus on movies with a sufficient number of ratings. The data revealed insights into user rating distributions and movie attributes.

## Modeling Approach

We initially used OLS regression as a baseline model to predict user review ratings. SVM was also considered for potentially more accurate results. However, both models faced challenges in accurately predicting user ratings, given the complexity of the data and its non-linear nature.

## Results

The OLS and SVM models showed limited success in predicting user review scores. Some users had highly skewed rating behavior, making predictions challenging. The limitations of hardware power and processing time also affected the results.

## Conclusion

While our research showed potential, there is no definitive evidence that predictive modeling based solely on movie metadata effectively predicts user review scores. Future research may benefit from more advanced modeling techniques, larger datasets, and the consideration of user-related factors. Additionally, addressing skewed user behavior and bot accounts could improve modeling accuracy.

## Future Directions

Future research in this area could explore:

- Advanced modeling techniques for better predictive accuracy.
- Removal of skewed reviewers and bot accounts for cleaner data.
- Incorporating user-related factors for more personalized recommendations.

We hope that our work provides valuable insights into the challenges of predicting user review ratings in the streaming industry and serves as a foundation for further research in this field.

For more details, refer to the full project report.
