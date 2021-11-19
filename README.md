# beer-clustering project outline

### Approach 1 - FinalProject.R
* Cleaning
* LDA on full data
* Subset to high distinct beers (using full_data LDA variables)
* Classification of subsetted data (using full_data LDA - recommended variables: "FG","ABV","Color")
* Results: 40% accuracy, but hard to visualize

### Approach 2 - FinalProject_Updated.R
* Cleaning
* Subset to high distinct beers (using all columns)
* LDA on subsetted data
* Classifcation of subsetted data (using subsetted data LDA linear combo variables - "LDA1","LDA2")
* Results:          , easier to visualize
