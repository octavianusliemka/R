#Import data
cuisines <- read.csv('data/chefmozcuisine.csv', stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
places <- read.csv('data/geoplaces2.csv', stringsAsFactors = F)
ratings <- read.csv('data/rating_final.csv', stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')

#Join data
cuisines_places <- merge(cuisines, places, by = 'placeID')
ratings_places <- merge(ratings, places, by = 'placeID')

#V1
cuisine_distribution <- table(cuisines_places$Rcuisine)
cuisine_distribution <- cuisine_distribution[cuisine_distribution > 5]
#percentages <- cuisine_distribution / sum(cuisine_distribution) * 100
#percentages <- round(percentages, 3)
#percentages_labels <- paste(names(cuisine_distribution), '\n', percentages, '%', sep = '')
pie(cuisine_distribution, main = 'Cuisine Distribution', labels = percentages_labels, col = rainbow(length((cuisine_distribution))))

#V2
restaurant_cuisine_count <- table(cuisines_places$placeID)
hist(restaurant_cuisine_count, main = 'Cuisines Count Frequency based on Restaurant', xlab = 'Cuisine Count')
#Kalo mau pake density, tambahin di akhir code, freq = F

#V3
ratings_places$average_rating <- rowMeans(ratings_places[, c('rating', 'service_rating', 'food_rating')])
above1 <- ratings_places[ratings_places$average_rating > 1.2,]

above1[above1$state %in% c('s.l.p.', 'S.L.P.', 'san luis potosi', 'slp', 'SLP', 'San Luis Potosi', 'san luis potos'), ]$state <- 'slp'

above1[above1$state == 'Tamaulipas', ]$state <- 'tamaulipas'

above1[above1$state == 'Morelos', ]$state <- 'morelos'

rating_state_distribution <- table(above1$average_rating, above1$state)

barplot_colors = c('red', 'blue', 'pink')

barplot(rating_state_distribution, beside = T, main = 'Average Rating Distributions\n based on the State of the Restaurants', xlab = 'State', col = barplot_colors)
legend('top', rownames(rating_state_distribution), fill = barplot_colors, cex = 0.5)

#Frequent Pattern Analysis
#Data Preprocessing
datasets <- cuisines_places
datasets <- datasets[datasets$franchise == 'f', ]
datasets <- datasets[datasets$other_services == 'none', ]
datasets <- datasets[datasets$country != '?', ]
datasets$Rcuisine <- gsub('_', ' ', datasets$Rcuisine)

nrow(datasets)

#Data Transformation

#install.packages('arules')
library(arules)

itemsets <- split(datasets$Rcuisine, datasets$placeID)
itemsets <- as(itemsets, 'transactions')

#Data Mining
frequent_itemsets <- apriori(itemsets, parameter = list(support = 0.008, target = 'frequent itemsets'))
inspect(frequent_itemsets)

rules <- ruleInduction(frequent_itemsets, confidence = 0.8)
inspect(rules)