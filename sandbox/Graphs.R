# Graphing functions for BondLab



OAS.Dist <- function(Run.Name = "character", Run.Metric = "character", interval = numeric()){

Name <- toString(Run.Name)
Metric <- as.character(Run.Metric)


graph.data <- slot(Name, Metric)
graph.data <- data.frame(cbind(graph.data, seq(1: length(call))))

colnames(graph.data) <- c("value", "count")
lower <- as.integer(min(graph.data[,1]))-1
upper <- as.integer(max(graph.data[,1])) + 1
interval = (upper - lower)/interval

ggplot(graph.data, aes(x = value )) +
  geom_histogram(colour = "black", fill = "lightgrey") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(lower, upper, interval)) +
  ylab("Frequency")+
  xlab("Path Price") +
  theme(panel.grid.major = element_line(size = .25, color = "grey")) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title = element_text(size = 20)) + 
  theme(legend.position = "none")
}
