# Installing packages used in Empirical Industrial Organization

# First install R and RStudio and then open this file

# Run the following commands from RStudio

#Installing useful packages from CRAN
install.packages("mlogit")
install.packages("googleVis")
install.packages("AER")
install.packages("lubridate")
install.packages("R.utils")
install.packages("readr")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("randtoolbox")
install.packages("devtools")
install.packages("stringr")
install.packages('installr')
install.packages("caret")
install.packages("skimr")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("ranger")
install.packages("glmnet")

# Use my own R repository
repos = c("https://skranz-repo.github.io/drat/",getOption("repos"))
install.packages("BLPestimatoR", repos=repos)
install.packages("RTutor",repos=repos)

# Installing sktools from github
devtools::install_github(repo = "skranz/sktools")

