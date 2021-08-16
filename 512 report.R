#Import CSV
library(readr)
customer_data <- read_csv("customer_data.csv")
View(customer_data)

#install tidyverse
library(tidyverse)

#install ggplot
library(ggplot2)

#install dplyr
library(dplyr)

#Check data vectors and zero values
summary(customer_data)
sum(is.na(customer_data))

#Create subset
customer_data.subset <- customer_data

#Check character values of subset
summary(customer_data.subset)

# Convert late/no to a factor and reassign new names
customer_data.subset$Reached.on.Time_Y.N <- recode_factor(customer_data.subset$Reached.on.Time_Y.N, '0' = 'On time', '1' = 'Late')

#Basic analysis

#gender 
Gender1 <- customer_data.subset %>% count(Gender)

ggplot(weeds.summarise2, aes(x=soil, y=mean, fill=species)) +
  geom_bar(stat="identity")
ggplot(Gender1, aes(x=Gender1$Gender, y=Gender1$n)) + geom_bar(stat="identity", color="blue", fill="blue")+ geom_text(aes(label=n), vjust=-0.3, size=3.5) +labs(title= "Gender of customers",y= "Number of people", x = "Gender")

#shipment mode
shipment <- customer_data.subset %>% count(Mode_of_Shipment)
ggplot(shipment, aes(x=shipment$Mode_of_Shipment, y=shipment$n)) + geom_bar(stat="identity", color="blue", fill="blue")+ geom_text(aes(label=n), vjust=-0.3, size=3.5)+labs(title= "Shipment Method", y= "Number of items", x = "Shipment method")

#customer ranking
customerrank <- customer_data.subset %>% count(Customer_rating)
ggplot(customerrank, aes(x=customerrank$Customer_rating, y=customerrank$n)) + geom_bar(stat="identity", color="blue", fill="blue")+ geom_text(aes(label=n), vjust=-0.3, size=3.5)+labs(title= "Customer rank", y= "Number of customers", x = "Customer Rank (1 lowest - 5 highest")

#warehouseblock
warehouse <- customer_data.subset %>% count(Warehouse_block)
ggplot(warehouse, aes(x=warehouse$Warehouse_block, y=warehouse$n)) + geom_bar(stat="identity",color="blue", fill="blue")+ geom_text(aes(label=n), vjust=-0.3, size=3.5)+labs(title= "Warehouse block", y= "Number of items", x = "Warehouse Block")

#purchase history
purchasehistory <- customer_data.subset %>% count(Prior_purchases)

ggplot(purchasehistory, aes(x=purchasehistory$Prior_purchases, y=purchasehistory$n)) + geom_bar(stat="identity",color="blue", fill="blue")+ geom_text(aes(label=n), vjust=-0.3, size=3.5)+labs(title= "Prior purchase history", y= "Number of customers", x = "Total number of orders" ) + xlim(1,12)

#on time or not
arrivalontime <- customer_data.subset %>% count(Reached.on.Time_Y.N)
ggplot(arrivalontime, aes(x=arrivalontime$Reached.on.Time_Y.N, y=arrivalontime$n)) + geom_bar(stat="identity", color="blue", fill="blue")+ geom_text(aes(label=n), vjust=-0.3, size=3.5)+labs(title= "Items that arrived on time", y= "Number of items", x = "Arrived on time " )

#Product importance
productimportance <- customer_data.subset %>% count(Product_importance)
ggplot(productimportance, aes(x=productimportance$Product_importance, y=productimportance$n)) + geom_bar(stat="identity", color="blue", fill="blue")+ geom_text(aes(label=n), vjust=-0.3, size=3.5)+labs(title= "Importance of products", y= "Number of items", x = "Importance of products sent" )

#customer care calls 
customercarecalls <- customer_data.subset %>% count(Customer_care_calls)
ggplot(customercarecalls, aes(x=customercarecalls$Customer_care_calls, y=customercarecalls$n)) + geom_bar(stat="identity", color="blue", fill="blue")+ geom_text(aes(label=n), vjust=-0.3, size=3.5)+labs(title= "Number of customer care calls per order", y= "Number of items", x = "total number of calls" )

#Average purchase
mean(customer_data.subset$Cost_of_the_Product, trim = 0, na.rm = FALSE,)
ggplot(customercarecalls, aes(x=customercarecalls$Customer_care_calls, y=customercarecalls$n)) + geom_bar(stat="identity", color="blue", fill="blue")+ geom_text(aes(label=n), vjust=-0.3, size=3.5)+labs(title= "Number of customer care calls per order", y= "Number of items", x = "total number of calls" )

#priority
Productpriority<- customer_data.subset %>% count(Product_importance)
ggplot(Productpriority, aes(x=Productpriority$Product_importance, y=Productpriority$n)) + geom_bar(stat="identity", color="blue", fill="blue")+ geom_text(aes(label=n), vjust=-0.3, size=3.5) + labs(title= "Product priority of order", y= "Number of items", x = "Product Priority" )

#Cost to of average order
ggplot(customer_data.subset, aes(x = Cost_of_the_Product)) +
  geom_histogram()

#Shipping analysis analysis

#Customer rating/shipping delay 
g1 <- ggplot(customer_data.subset, aes(customer_data.subset$Customer_rating))
g1 + geom_bar((aes(fill = customer_data.subset$Reached.on.Time_Y.N))) + labs(title= "Customer rating vs Delivery status", y= "Number of customers", x = "Customer rating" ) + scale_fill_discrete(name = "Delivery status") 

#Customer calls/shipping delay 
g2 <- ggplot(customer_data.subset, aes(customer_data.subset$Customer_care_calls))
g2 + geom_bar((aes(fill = customer_data.subset$Reached.on.Time_Y.N))) + labs(title= "Customer call inquiries vs Delivery status", y= "Number of customers", x = "Customer calls per order" ) + scale_fill_discrete(name = "Delivery status")

#prior purchases/shipping delay 
g3 <- ggplot(customer_data.subset, aes(customer_data.subset$Prior_purchases))
g3 + geom_bar((aes(fill = customer_data.subset$Reached.on.Time_Y.N)))+ labs(title= "Delivery status vs Order history ", y= "Number of customers", x = "Number of Orders" ) + scale_fill_discrete(name = "Delivery status")

#shipping mode/shipping delay 
g4 <- ggplot(customer_data.subset, aes(customer_data.subset$Mode_of_Shipment))
g4 + geom_bar((aes(fill = customer_data.subset$Reached.on.Time_Y.N))) + 
  labs(title= "Delivery status vs Shipping status", y= "Number of customers", x = "Shipping method" ) + scale_fill_discrete(name = "Shipping Method")

#product/shipping delay 
g5 <- ggplot(customer_data.subset, aes(customer_data.subset$Cost_of_the_Product))
g5 + geom_histogram((aes(fill = customer_data.subset$Reached.on.Time_Y.N))) +
  labs(title= "Cost of product vs Deliver status", y= "Total orders", x = "Cost of product" ) + scale_fill_discrete(name = "Delivery status")

#priority/shipping delay 
g6 <- ggplot(customer_data.subset, aes(customer_data.subset$Product_importance))
g6 + geom_bar((aes(fill = customer_data.subset$Reached.on.Time_Y.N))) +
  labs(title= "Priority vs Deliver status", y= "Total orders", x = "Priority" ) + scale_fill_discrete(name = "Delivery status")

ggplot(customer_data.subset, aes(x = Weight_in_gms )) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(Reached.on.Time_Y.N ~ .)

#Customer shipments delays by priority
g7 <- ggplot(customer_data.subset, aes(customer_data.subset$Reached.on.Time_Y.N))
g7 + geom_bar((aes(fill = customer_data.subset$Mode_of_Shipment)))+ labs(title= "On time/Delayed vs shipping method", y= "Number of items", x = " Shipping method" ) + scale_fill_discrete(name = "Delivery status")

#shipping method to priority
g8<- ggplot(customer_data.subset, aes(customer_data.subset$Reached.on.Time_Y.N))
g8 + geom_bar((aes(fill = customer_data.subset$))

#shipping method by warehouse
g9<- ggplot(customer_data.subset, aes(customer_data.subset$Warehouse_block))
g9 + geom_bar((aes(fill = customer_data.subset$Reached.on.Time_Y.N))) + labs(title= "On time/Delayed vs Warehouse block", y= "Number of shipments", x = "Warehouse Block" ) + scale_fill_discrete(name = "Delivery status")

#priority/shipping delay 
g10 <- ggplot(customer_data.subset, aes(customer_data.subset$Prior_purchases))
g10 + geom_bar((aes(fill = customer_data.subset$Reached.on.Time_Y.N))) 
  labs(title= "Priority vs Deliver status", y= "Total orders", x = "Priority" ) + scale_fill_discrete(name = "Delivery status")

#Make seperate data frames for late and on time parcels 
customer_data.subsetonetime <- customer_data.subset[which(customer_data.subset$Reached.on.Time_Y.N == "On time"),]
customer_data.subsetlate <- customer_data.subset[which(customer_data.subset$Reached.on.Time_Y.N== "Late"),]

#histogram of discount
ggplot(customer_data.subset, aes(x=Discount_offered, color=Reached.on.Time_Y.N)) + geom_histogram() + labs(title= "Weight of products to delivery status", y= "Count", x = "Weight of products" ) + scale_fill_discrete(name = "Delivery status")

#histogram of weight
ggplot(customer_data.subset, aes(x=Weight_in_gms, color=Reached.on.Time_Y.N)) + geom_histogram() + labs(title= "Weight of products by warehouse", y= "Count", x = "Weight of products" ) + scale_fill_discrete(name = "Delivery status")

#histogram of warehouseblock/discount for late orders
ggplot(customer_data.subsetlate, aes(x=Discount_offered, color=Warehouse_block)) + geom_histogram() + labs(title= "Discount of products by warehouse (late orders)", y= "Count", x = "Given discount" ) + geom_vline(aes(xintercept=mean(Discount_offered)),color="blue", linetype="dashed", size=1)

#histogram of weight/warehouseblock for late orders
ggplot(customer_data.subsetlate, aes(x=Weight_in_gms, color=Warehouse_block)) + geom_histogram() + labs(title= "Weight of products by warehouse (late orders)", y= "Count", x = "Weight of products" ) + geom_vline(aes(xintercept=mean(Weight_in_gms)),color="blue", linetype="dashed", size=1)

#Order cost to discount by shipping method
customer_data.subsetshipping<- customer_data.subset[which(customer_data.subset$Mode_of_Shipment == "Ship"),]

ggplot(customer_data.subsetshipping, aes(x=Discount_offered, y=Cost_of_the_Product,color=Reached.on.Time_Y.N)) + geom_point()+
  geom_point(size=.5, shape=23) + labs(title= "Order costs to discount given (shipping)", y=
                                         "Total order cost", x = "Discount given" ) + scale_fill_discrete(name = "Order status")

#Weight of goods to product cost of shipping
ggplot(customer_data.subsetshipping, aes(x=Cost_of_the_Product, y=Weight_in_gms, color=Reached.on.Time_Y.N)) + geom_point()+
  geom_point(size=.5, shape=23) + labs(title= "Weight of goods to product cost of late deliveries (shipping)", y= "Total weight grams", x = "cost of products" )

#Order cost to discount by land method
customer_data.subsetsland<- customer_data.subset[which(customer_data.subset$Mode_of_Shipment == "Road"),]

ggplot(customer_data.subsetsland, aes(x=Discount_offered, y=Cost_of_the_Product,color=Reached.on.Time_Y.N)) + geom_point()+
  geom_point(size=.5, shape=23) + labs(title= "Order costs to discount given (Road)", y= "Order cost", x = "Discount given" )

#Weight of goods to product cost of Land
ggplot(customer_data.subsetsland, aes(x=Discount_offered, y=Weight_in_gms, color=Reached.on.Time_Y.N)) + geom_point()+
  geom_point(size=.5, shape=23) + labs(title= "Weight of goods to product cost of late deliveries", y= "Total weight grams", x = "cost of products" )

#Order cost to discount by Flight method
customer_data.subsetflight<- customer_data.subset[which(customer_data.subset$Mode_of_Shipment == "Flight"),]
ggplot(customer_data.subsetflight, aes(x=Discount_offered, y=Cost_of_the_Product,color=Reached.on.Time_Y.N)) + geom_point()+
  geom_point(size=.5, shape=23) + labs(title= "Order costs to discount given (Flight)", y= "Total order cost", x = "Discount given" ) + scale_fill_discrete(name = "Order status")

#discount cost by order weight by Flight method
ggplot(customer_data.subsetflight, aes(x=Discount_offered, y=Weight_in_gms,color=Reached.on.Time_Y.N)) + geom_point()+
  geom_point(size=.5, shape=23) + labs(title= "Discount given to package weight (Flight)", y= "Total weight of order", x = "Discount given" ) + scale_fill_discrete(name = "Order status")

#discount cost by order weight by Ship method
ggplot(customer_data.subsetshipping, aes(x=Discount_offered, y=Weight_in_gms,color=Reached.on.Time_Y.N)) + geom_point()+
  geom_point(size=.5, shape=23) + labs(title= "Discount given to package weight (Shipping)", y= "Total weight of order", x = "Discount given" ) + scale_fill_discrete(name = "Order status")

#discount cost by order weight by Land method
ggplot(customer_data.subsetsland, aes(x=Discount_offered, y=Weight_in_gms,color=Reached.on.Time_Y.N)) + geom_point()+
  geom_point(size=.5, shape=23) + labs(title= "Discount given to package weight (Land)", y= "Total weight of order", x = "Discount given" ) + scale_fill_discrete(name = "Order status")

