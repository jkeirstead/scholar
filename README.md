# scholar

The scholar R package provides functions to extract citation data from [Google Scholar](http://scholar.google.com).  In addition to retrieving basic information about a single scholar, the package also allows you to compare multiple scholars and predict future h-index values.

*Development of the scholar package has resumed and a new maintainer should be confirmed shortly. Please continue to file issues and make pull requests against https://github.com/jkeirstead/scholar going forwards.*

## Basic features

Individual scholars are referenced by a unique character string, which can be found by searching for an author and inspecting the resulting scholar homepage.  For example, the profile of physicist Richard Feynman is located at http://scholar.google.com/citations?user=B7vSqZsAAAAJ and so his unique id is `B7vSqZsAAAAJ`.

Basic information on a scholar can be retrieved as follows:

```
# Define the id for Richard Feynman
id <- 'B7vSqZsAAAAJ'

# Get his profile and print his name
l <- get_profile(id)
l$name 

# Get his citation history, i.e. citations to his work in a given year 
get_citation_history(id)

# Get his publications (a large data frame)
get_publications(id)
```

Additional functions allow the user to query the publications list, e.g. `get_num_articles`, `get_num_distinct_journals`, `get_oldest_article`, `get_num_top_journals`.  Note that Google doesn't explicit categorize publications as journal articles, book chapters, etc, and so *journal* or *article* in these function names is just a generic term for a publication.

## Comparing scholars

You can also compare multiple scholars, as shown below.  Note that these two particular scholars are rather profilic and these queries will take a very long time to run.

```
# Compare Feynman and Stephen Hawking
ids <- c('B7vSqZsAAAAJ', 'qj74uXkAAAAJ')

# Get a data frame comparing the number of citations to their work in
# a given year 
compare_scholars(ids)

# Compare their career trajectories, based on year of first citation
compare_scholar_careers(ids)
```

## Predicting future h-index values

Finally users can predict the future [h-index](http://en.wikipedia.org/wiki/H-index) of a scholar, based on the method of [Acuna et al.](http://www.nature.com/nature/journal/v489/n7415/full/489201a.html).  Since the method was originally calibrated on data from neuroscientists, it goes without saying that, if the scholar is from another discipline, then the results should be taken with a large pinch of salt.  A more general critique of the original paper is available   [here](http://simplystatistics.org/2012/10/10/whats-wrong-with-the-predicting-h-index-paper/).  Still, it's a bit of fun.  

```
## Predict h-index of original method author, Daniel Acuna
id <- 'GAi23ssAAAAJ'
predict_h_index(id)
```
