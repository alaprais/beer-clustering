# beer-clustering project outline

### Approach 1 - FinalProject.R
* Cleaning
* LDA on full data
* Subset to high distinct beers (using all columns)
* Classification of subsetted data (using full_data LDA - recommended variables: "FG","OG")
* Results: 73% accuracy on 6 groups with 80% missing data, Used: Color, ABV

### Approach 2 - FinalProject_Updated.R
* Cleaning
* Subset to high distinct beers (using all columns)
* LDA on subsetted data
* Classification of subsetted data (using subsetted data LDA linear combo variables - "LDA1","LDA2")
* Results:          , easier to visualize
