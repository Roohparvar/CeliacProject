setwd("C:/Esmaeil/CeliacProject/CeliacProject/genes associated with large clones")

full_metadata = full_metadata[full_metadata$clone_size_ab >= 26, ]


HealthyCD4 <- full_metadata[
  full_metadata$Diagnosis == "Healthy" &
    full_metadata$cluster %in% c("Th1 Mem", "Th17", "Th2/Tfh", "Tregs"),
]


ACDCD4 = full_metadata[
  full_metadata$Diagnosis == "ACD" &
                         full_metadata$cluster %in% c("Th1 Mem", "Th17", "Th2/Tfh", "Tregs"),
]

RCD1CD4 = full_metadata[full_metadata$Diagnosis == "RCD-I" &
                          full_metadata$cluster %in% c("Th1 Mem", "Th17", "Th2/Tfh", "Tregs"), ]
RCD2CD4 = full_metadata[full_metadata$Diagnosis == "RCD-II" &
                          full_metadata$cluster %in% c("Th1 Mem", "Th17", "Th2/Tfh", "Tregs"), ]



HealthyCD8 = full_metadata[full_metadata$Diagnosis == "Healthy" &
                             full_metadata$cluster %in% c("nIEL", "Tγδ CD8+", "IEL GZMK+", "CD8 Mem", "CD8 Cyt."), ]
ACDCD8 = full_metadata[full_metadata$Diagnosis == "ACD" &
                         full_metadata$cluster %in% c("nIEL", "Tγδ CD8+", "IEL GZMK+", "CD8 Mem", "CD8 Cyt."), ]
RCD1CD8 = full_metadata[full_metadata$Diagnosis == "RCD-I" &
                          full_metadata$cluster %in% c("nIEL", "Tγδ CD8+", "IEL GZMK+", "CD8 Mem", "CD8 Cyt."), ]
RCD2CD8 = full_metadata[full_metadata$Diagnosis == "RCD-II" &
                          full_metadata$cluster %in% c("nIEL", "Tγδ CD8+", "IEL GZMK+", "CD8 Mem", "CD8 Cyt."), ]


