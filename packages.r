install.packages("MASS", dependencies = TRUE)
install.packages("Matrix", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("DataExplorer", dependencies = TRUE)
install.packages("e1071", dependencies = TRUE)
install.packages("corrplot", dependencies = TRUE)
install.packages("caret", dependencies = TRUE)
install.packages("devtools", dependencies = TRUE)

# conda install -c conda-forge r-MASS
# conda install -c conda-forge r-Matrix
# conda install -c conda-forge r-ggplot2
# conda install -c conda-forge r-DataExplorer
# conda install -c conda-forge r-e1071
# conda install -c conda-forge r-corrplot
# conda install -c conda-forge r-caret
# conda install -c conda-forge r-klar

library("devtools")

devtools::install_github("pohlio/tidyinftheo")
devtools::install_github("mdscheuerell/muti")
devtools::install_github("bhklab/mrmre")
