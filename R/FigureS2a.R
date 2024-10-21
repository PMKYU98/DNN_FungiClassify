dfData <- read.csv('consolidated_maximum_test_acc.tsv',header=FALSE,sep="\t")
dfData <- dfData[order(dfData[[1]]), ]

dfFiltered <- dfFiltered <- subset(dfData, grepl("^genus_(LENET|RESNET8|RESNET10)", dfData[[1]]))
dfFiltered[[1]] <- gsub("ITS_4mer", "", dfFiltered[[1]])
dfFiltered[[1]] <- gsub("genus_", "", dfFiltered[[1]])

# Calculate the mean, max, and min for the 10 columns
means <- rowMeans(dfFiltered[, 2:11])
max_vals <- apply(dfFiltered[, 2:11], 1, max)
min_vals <- apply(dfFiltered[, 2:11], 1, min)

# Create a data frame for plotting
plot_data <- data.frame(
  X = dfFiltered[[1]],
  Mean = means,
  Max = max_vals,
  Min = min_vals
)

# Create the bar plot with error bars
p <- ggplot(plot_data, aes(x = X, y = Mean)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  geom_errorbar(aes(ymin = Min, ymax = Max), width = 0.2, color = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("Test accuracy") +
  ggtitle("Average test accuracy of 10-fold cross-validation with Min and Max accuracy") +
  coord_cartesian(ylim = c(0.8, 1.0))

# Print the plot
print(p)

# Save the plot to a file
ggsave("genus_figures2.png", plot = p, width = 10, height = 6)
