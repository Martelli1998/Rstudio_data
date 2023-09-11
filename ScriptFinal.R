options(scipen = 999) #evitamos la notacion cientifica para una mejor lectura

# load libraries
library(magrittr)
library(dplyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(psych)
library(gplots)
library(vcd)
library(graphics)
library(yardstick)
library(caret)
library(patchwork)



Airlines_2008 <- read.csv(file.choose()) #cargamos el archivo de 2008
Airlines_2002 <- read.csv(file.choose()) #cargamos el archivo de 2002
Airlines_2007 <- read.csv(file.choose()) #cargamos el archivo de 2007
plane_data <- read.csv(file.choose()) #cargamos plane_data

Airlines <- rbind(Airlines_2002, Airlines_2008, Airlines_2007)

Airlines$flight_delay <- ifelse(Airlines$DepDelay < 15, 'On_time', 'Late_departure')


# Calculate percentages and create the pie chart data
flight_delay_table <- table(Airlines$flight_delay)
percentages <- round(prop.table(flight_delay_table) * 100)
labels <- paste(percentages, "%", sep = "")
pie_data <- data.frame(labels = labels, values = as.numeric(flight_delay_table))

# Create the pie chart using ggplot2
ggplot(pie_data, aes(x = "", y = values, fill = labels, label = labels)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(position = position_stack(vjust = 0.5), size = 4) +
  labs(fill = "Categories", title = "Flights_Departure") +
  theme_void()




#creamos boxplot
Airlines_2 <- Airlines
# Calculate the number of rows to retain (20% of the original dataset)
rows_to_keep <- round(0.2 * nrow(Airlines_2))
# Randomly sample 20% of the rows from the dataset
reduced_Airlines_2 <- Airlines_2[sample(nrow(Airlines_2), rows_to_keep), ]
# Verify the new size of the reduced dataset
nrow(reduced_Airlines_2)

reduced_Airlines_2$Month <- factor(reduced_Airlines_2$Month)

ggplot(reduced_Airlines_2, aes(x = Month, y = DepDelay)) +
  geom_boxplot() +
  xlab("Month") +
  ylab("Departure Delay") +
  ggtitle("Departure Delay by Month - Boxplot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plot 1: Boxplot
plot_boxplot <- ggplot(reduced_Airlines_2, aes(x = Month, y = DepDelay)) +
  geom_boxplot() +
  xlab("Month") +
  ylab("Departure Delay") +
  ggtitle("Departure Delay by Month - Boxplot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Pie Chart
plot_pie <- ggplot(pie_data, aes(x = "", y = values, fill = labels, label = labels)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(position = position_stack(vjust = 0.5), size = 4) +
  labs(fill = "Categories", title = "Flights_Departure") +
  theme(
    plot.title = element_text(hjust = 0)  # Adjusts the title alignment to the left
  )

# Combine the two plots
combined_plot <- plot_boxplot + plot_pie

# Display the combined plot
combined_plot



# Convert float_time to integer hour
convert_flat_time <- function(float_time) {
  return(as.integer(float_time %/% 100))
}

# Add DepHour and AvgDelayTime columns to the original dataframe
Airlines$DepHour <- sapply(Airlines$CRSDepTime, convert_flat_time)
Airlines$AvgDelayTime <- (Airlines$ArrDelay + Airlines$DepDelay)/2


TailNum <- plane_data$tailnum
manufacturer <- plane_data$manufacturer
plane_year <- plane_data$year
Plane_model <- plane_data$model


planes <- data.frame(TailNum,manufacturer, plane_year,Plane_model)
airlines_merged <- merge(Airlines, planes, by = "TailNum")


# Create Q1_df with selected columns from airlines
Q1_df <- Airlines[c('Year', 'Month', 'DayofMonth', 'DayOfWeek', 'DepTime', 'CRSDepTime', 'ArrTime',
                    'CRSArrTime', 'ArrDelay', 'DepDelay', 'Origin', 'Dest', 'DepHour', 'AvgDelayTime')]

# Drop rows with missing values
Q1_df <- na.omit(Q1_df)

# Print the length of Q1_df
print(length(Q1_df))


# Grouping and aggregating the data
Q1_temp_df <- Q1_df %>%
  group_by(DepHour) %>%
  summarize(AvgDelayTime = mean(AvgDelayTime))

ggplot(data = Q1_temp_df, aes(x = DepHour, y = AvgDelayTime)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f", AvgDelayTime)),
            vjust = -0.5, color = "black", size = 3) +
  scale_x_continuous(breaks = Q1_temp_df$DepHour, labels = Q1_temp_df$DepHour) +
  labs(title = "Overall Delay Time of Each Hour",
       x = "departure hour",
       y = "average delay time (minute)") +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank())


# Count the frequency of values in the 'Name' column
Hour_freq <- table(Airlines$DepHour)
print(Hour_freq)
barplot(Hour_freq, main = "Number of Flights Per Hour", xlab = "Time", ylab = "Frequency")



Hour_freq <- table(Airlines$DepHour)


Manufacturer <- airlines_merged[c('AvgDelayTime', 'manufacturer')]
Manufacturer_2 <- subset(Manufacturer, manufacturer != '')
str(manufacturer)
#Manufacturer_2 <- as.factor(Manufacturer_2$manufacturer)

Make <- levels(Manufacturer_2)
print(Make)
str(Manufacturer_2)
mean_delay <- aggregate(AvgDelayTime ~ manufacturer, Manufacturer_2, mean)

#aviones mas grande sufren mas retrasos?
Delay_by_model <- airlines_merged[c('AvgDelayTime', 'Plane_model')]
Delay_by_model <- subset(Delay_by_model, Plane_model != '')
mean_plane_model <- aggregate(AvgDelayTime ~ Plane_model, Delay_by_model, mean)
#premisa verdadera


airlines_merged <- na.omit(airlines_merged, cols = 'DepDelay')
airlines_merged_2 <- airlines_merged[airlines_merged$DepDelay >= 0,]

airlines_merged$flight_delay <- ifelse(airlines_merged$DepDelay < 15, 'flight_on_time', 'Flight_delayed')


#Machine learning Model. Vamos a predecir si un vuelo va sufrir retrasos 
airlines_merged_2$flight_delay <- ifelse(airlines_merged_2$DepDelay < 15, 0, 1)


# use a train/test split
rows <- sample(nrow(airlines_merged_2))
airlines_merged_2 <- airlines_merged_2[rows, ]
# train:test split is 70%:30%
split <- round(nrow(airlines_merged_2) * 0.70)
train <- airlines_merged_2[1:split, ]
test <- airlines_merged_2[(split + 1):nrow(airlines_merged_2), ]
nrow(train) / nrow(airlines_merged_2)


fit.boa <- glm(flight_delay ~ Month + DepTime + plane_year + UniqueCarrier + Distance + DayofMonth + DepHour + Plane_model + Plane_model,
               data = train, family = binomial())


summary(fit.boa)

pred.test <- predict(fit.boa, newdata = test, type = "response")
summary(pred.test)

actual_response <- test$flight_delay
table(test$flight_delay)
predicted_response <- ifelse(pred.test > 0.5, 1, 0)
table(predicted_response)
outcomes <- table(predicted_response, actual_response)
outcomes


confusion <- conf_mat(outcomes)
confusion
plot <- autoplot(confusion) + ggtitle("Confusion Matrix")
plot


# model performance metrics
summary(confusion,event_level = "second")



#creamos K-mean clusters para los aeropuertos
features <- airlines_merged_2[c('DepDelay','Origin')]
avg_delay <- aggregate(DepDelay ~ Origin, features, mean)


k <- 2  # Number of clusters
kmeans_result <- kmeans(avg_delay$DepDelay, centers = k)
avg_delay$Cluster <- kmeans_result$cluster

# Print the results
print(avg_delay)
plot(avg_delay$DepDelay, col = avg_delay$Cluster, pch = 19, xlab = "Airport", ylab = "Average Delay", main = "Airport Delay Cluster")
legend("topright", legend = paste("Cluster", 1:k), col = 1:k, pch = 19)



scatter_plot <- ggplot(avg_delay, aes(x = Origin, y = DepDelay)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  xlab("Airports") +
  ylab("Departure_Delays") +
  ggtitle("Scatter Plot Mean Delay_by_Airport")

# Display the scatter plot
scatter_plot


Airline_cluster <- airlines_merged_2[c('DepDelay','UniqueCarrier')]

#creamos K-mean clusters para los carriers
avg_Carrier_delay <- aggregate(DepDelay ~ UniqueCarrier, Airline_cluster, mean)
k <- 3  # Number of clusters
kmeans_result <- kmeans(avg_Carrier_delay$DepDelay, centers = k)
avg_Carrier_delay$Cluster <- kmeans_result$cluster

# Print the results
plot(avg_Carrier_delay$DepDelay, col = avg_Carrier_delay$Cluster, pch = 19, xlab = "Airline", ylab = "Average Delay", main = "Carrier Delay Cluster")
legend("topright", legend = paste("Cluster", 1:k), col = 1:k, pch = 19)


# Plot 1: Boxplot
plot_boxplot_1 <- ggplot(avg_Carrier_delay, aes(x = as.factor(Cluster), y = DepDelay)) +
  geom_boxplot() +
  xlab("Cluster") +
  ylab("average Departure Delay") +
  ggtitle("Departure Delay by Carrier - Boxplot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_boxplot_1


plot_boxplot_1 <- ggplot(avg_Carrier_delay, aes(x = as.factor(Cluster), y = DepDelay, fill = as.factor(Cluster))) +
  geom_boxplot() +
  xlab("Cluster") +
  ylab("Average Departure Delay") +
  ggtitle("Departure Delay by Carrier - Boxplot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

