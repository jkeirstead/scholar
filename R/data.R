##
## REMOVE
##
##
## #' Impact Factor (2017)
## #'
## #' Downloaded from https://www.researchgate.net/post/New_Impact_factors_2017_for_Journals_are_released_now
## #'
## #' # library(readxl)
## #' # library(tidyverse)
## #' #
## #' # impactfactor <- read_excel("JournalImpactfactor_2017.xls", skip=2) %>%
## #' #   dplyr::select(Journal = `Full Journal Title`,
## #' #                 Cites = `Total Cites`,
## #' #                 ImpactFactor = `Journal Impact Factor`,
## #' #                 Eigenfactor = `Eigenfactor Score`) %>%
## #' #   dplyr::mutate(Cites = as.numeric(stringr::str_remove_all(Cites, ",")),
## #' #                 ImpactFactor = as.numeric(stringr::str_remove_all(ImpactFactor, ",")),
## #' #                 Eigenfactor = as.numeric(stringr::str_remove_all(Eigenfactor, ",")))
## #'
## #' # save(impactfactor, file="impactfactor.rda")
## #'
####### save(impactfactor, file = "~/github/scholar/R/sysdata.rda", compress="xz")
####
## #' @format A data frame with journal metrics.
## #' \describe{
## #'   \item{Journal}{Journal title}
## #'   \item{Cites}{Total cites}
## #'   \item{ImpactFactor}{Impact factor}
## #'   \item{Eigenfactor}{Eigenfactor}
## #' }
## "impactfactor"


# Journal metrics (2021)
# 
# Downloaded from https://www.scimagojr.com/journalrank.php
# 
# Rename (remove whitespaces) to scimagojr2021.csv
# 
# library(tidyverse)
# journalrankings <- read.csv2("scimagojr2021.csv") %>%
#    dplyr::rename(Journal = Title)
# # 
# save(journalrankings, file="R/sysdata.rda", compress='xz')
