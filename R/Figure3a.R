library(dplyr)

dfData <- read.csv('consolidated_maximum_test_acc.tsv',header=FALSE,sep="\t")
dfData <- dfData[order(dfData[[1]]), ]
dfData$Mean <- rowMeans(dfData[, 2:11])

optimal <- c(
  "species_RESNET18_benA_4mer_32,64,256,7",
  "species_RESNET18_common_4mer_32,64,256,7"
)

dfFiltered <- subset(dfData, dfData[[1]] %in% optimal)
dfFiltered <- dfFiltered[, 1:11]

dfFiltered <- dfFiltered %>%
  slice(match(c(optimal), dfFiltered$V1))

dfFiltered <- rbind(dfFiltered)
dfFiltered[,1] <- c('benA', 'ITS')

dfFiltered <- as.data.frame(dfFiltered)
dfFiltered$V1 <- factor(dfFiltered$V1, levels = c('benA', 'ITS'))
dfFiltered[, 2:11] <- lapply(dfFiltered[, 2:11], as.numeric)

plot_data <- dfFiltered %>%
  gather(key = "Metric", value = "Value", -V1)

p <- ggplot(plot_data, aes(x = V1, y = Value, fill=V1)) +
  geom_boxplot(aes(group = V1)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none") +
  xlab("") +
  ylab("Test accuracy") +
  ggtitle("")

print(p)
ggsave("Figure3a.png", plot = p, width = 10, height = 6)
