# beer-clustering project outline

### Approach 1 - FinalProject.R (DONE)
* Cleaning
* LDA on full data
* Subset to high distinct beers (using all variable columns - "OG","FG",...,"BoilTime","Efficiency", etc)
* Classification of subsetted data using combinations of "Color","ABV", "OG", "FG"  (recommended by LDA?)
* Best Model Results: 73% accuracy on 6 groups with 80% missing data, Used: "Color", "ABV"

### Approach 2 - FinalProject_Updated.R (TODO)
* Cleaning
* Subset to high distinct beers (using all columns)
* LDA on subsetted data
* Classification of subsetted data (using subsetted data LDA linear combo variables - "LDA1","LDA2")
* Results:          , easier to visualize
