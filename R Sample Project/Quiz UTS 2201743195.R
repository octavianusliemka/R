orders <- read.csv('orders.csv', stringsAsFactors = F)
products <- read.csv('products.csv', stringsAsFactors = F)

orders_products <- merge(orders, products, by = 'product_id')

#1a
boxplot(product_price ~ department, data = products, main = 'Product Price for All Product Department', xlab = 'Department', ylab = 'Product Price', las = 3, col = rainbow(21))

#1b
top_dept <- table(products$department)
top5 <- c(top_dept)
top5 <- tail(sort(top5), 5)
percentages <- top5 / sum(top5) * 100
percentages <- round(percentages, 2)
percentages_labels <- paste(names(top5), ' (', percentages, '%', ')', sep = '')
pie(top5, main = 'Top 5 Department (Based on Product Count)', labels = percentages_labels ,col = rainbow(5))

#1c
frozen_dept <- products
frozen_dept <- frozen_dept[frozen_dept$department == 'frozen', ]
frozen_dept <- table(frozen_dept$aisle)
low3 <- c(frozen_dept)
low3 <- head(sort(low3), 3)
barplot(low3, main = 'Lowest 3 Aisle in frozen Department (Based on Product Count)', xlab = 'aisle', col = rainbow(3))

#2a
dataprep <- orders_products
dataprep <- dataprep[dataprep$department == 'alcohol', ]
dataprep <- dataprep[dataprep$aisle != 'specialty wines champagnes', ]
dataprep <- dataprep[!duplicated(dataprep), ]

#2b
#install.packages('arules')
library(arules)
itemsets <- split(dataprep$product_name, dataprep$order_id)

#2c
frequent_product <- apriori(itemsets, parameter = list(support = 0.04, target = 'frequent itemsets'))
inspect(frequent_product)

rules <- ruleInduction(frequent_product, confidence = 0.5)
inspect(rules)