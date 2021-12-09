# File Overview:
* the code - FinalProject_final.R
* the raw data - RecipeData.csv
* pairs_4.txt
* random_beer_prediction.txt


# beer-clustering project outline
### Approach 1 - FinalProject.R (DONE)
* Cleaning
* LDA on full data
* Subset to high distinct beers (using all variable columns - "OG","FG",...,"BoilTime","Efficiency", etc)
* Classification of subsetted data using combinations of "Color","ABV", "OG", "FG"  (recommended by LDA?)
* Best Model Results: 75% accuracy on 6 groups with 80% missing data, Used: "LDA1","LDA2"

