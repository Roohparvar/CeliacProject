if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("scRepertoire")
suppressMessages(library(scRepertoire))

library(dplyr)
library(tidyr)
library(ggplot2)

full_metadata <- full_metadata[which(full_metadata$imm_receptor_Esmaeil == "Aberrant ab"), ]
full_metadata <- full_metadata[full_metadata$Diagnosis == "ACD", ]
full_metadata <- full_metadata[full_metadata$cluster %in% c("IEL GZMK+", "Prolif. IEL", "Trm IEL", "Cyt. IEL", "nIEL", "IEL CCL4+"), ]

# Zero