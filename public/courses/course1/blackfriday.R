# QTM2000 Final Project
# Professor Mathaisel
# Black Friday
# Exploratory Data Analysis and Association Rules

# References:
# https://github.com/shwetachandel/Black-Friday-Dataset
# https://www.kaggle.com/dessinhaprado/black-friday
# https://www.kaggle.com/dabate/black-friday-examined-eda-apriori

#################################################################
# Installing Packages: Only need to install once on your hard disk
install.packages("tidyverse")
install.packages("scales")
install.packages("arules")
install.packages("gridExtra")
install.packages("arulesViz")

library(tidyverse)
library(scales)
library(gridExtra)
library(arules)
library(arulesViz) # Visualization of the arules results

#################################################################
# Loading the datset and checking its structure.
dataset = read.csv("C:/Docs/Babson/QTM2000/Data/BlackFriday.csv")
str(dataset)

#################################################################
# Exploratory Data Analysis (EDA)

# Gender of shoppers.
# Since each row represents a transaction made by an individual, an individual might make
# multiple transactions. So, we must first group the data by User_ID to remove duplicate IDs.

dataset_gender = dataset %>%
  select(User_ID, Gender) %>%
  group_by(User_ID) %>%
  distinct()
head(dataset_gender) # To give you an idea of the User_ID and Gender for the first few observations.

# Now that we have the dataframe necessary to see each User_IDs, corresponding gender, 
# and their total counts, lets plot the distribution of gender across our dataset.
options(scipen=10000)   # To remove scientific numbering
genderDist  = ggplot(data = dataset_gender) +
  geom_bar(mapping = aes(x = Gender, y = ..count.., fill = Gender)) +
  labs(title = 'Gender of Customers') + 
  scale_fill_brewer(palette = 'Dark2')
print(genderDist)

# This gender split metric is helpful to retailers because some might want to modify their store layout, 
# product selection, and other attributes differently depending on the gender proportion of their shoppers.
# A study published in the Clothing and Textiles Research Journal writes:
# (Abstract):
# "Involvement, variety seeking, and physical environment of stores were selected as antecedents of 
# shopping experience satisfaction. The structural model for female subjects confirmed the 
# existence of the mediating role of hedonic shopping value in shopping satisfaction, 
# whereas the model for male respondents did not." Chang, E., Burns, L. D., & Francis, S. K. (2004)
# Although this does not give direct insight into recommended actions for retail stores, 
# it does display a difference in the value derived from shopping and its relationship to gender, 
# which should be taken into account by retailers.

# To investigate further, lets compute the average spending amount for Gender. 
# For easy interpretation and traceback we will create separate tables and then join them together.
total_purchase_user = dataset %>%
  select(User_ID, Gender, Purchase) %>%
  group_by(User_ID) %>%
  arrange(User_ID) %>%
  summarise(Total_Purchase = sum(Purchase))
user_gender = dataset %>%
  select(User_ID, Gender) %>%
  group_by(User_ID) %>%
  arrange(User_ID) %>%
  distinct()
user_purchase_gender = full_join(total_purchase_user, user_gender, by = "User_ID")
head(user_purchase_gender) # To give you an idea of the first few observations.

average_spending_gender = user_purchase_gender %>%
  group_by(Gender) %>%
  summarize(Purchase = sum(as.numeric(Total_Purchase)), 
            Count = n(), 
            Average = Purchase/Count)
head(average_spending_gender)

# Visualize the results.
genderAverage  = ggplot(data = average_spending_gender) +
  geom_bar(mapping = aes(x = Gender, y = Average, fill = Gender), stat = 'identity') +
  labs(title = 'Average Spending by Gender') +
  scale_fill_brewer(palette = 'Accent')
print(genderAverage)

#################################################################
# Top Sellers
# Now lets switch gears and examine our top selling products. 
# In this situation, we won't group by product ID since we want to see duplicates, 
# just in case people are buying 2 or more quantities of the same product.
top_sellers = dataset %>%
  count(Product_ID, sort = TRUE)
top_5 = head(top_sellers, 5)
top_5

# Now that we have Identified our top 5 best selling products, lets examine the best selling product.
best_seller = dataset[dataset$Product_ID == 'P00265242', ]
head(best_seller)

# We can see that this product fits into Product_Category_1 = 5 and Product_Category_2 = 8. 
# It would be useful to have a key to reference the item name in order to determine what it is. 
# But, we don't have that item name in our dataset or data dictionary.

# Another interesting finding is that even though people are purchasing the same product, 
# they are paying different prices. This could be due to various Black Friday promotions, 
# discounts, or coupon codes. Otherwise, investigation would need to be done regarding the reason 
# for different purchase prices of the same product between customers.

# Lets continue to analyze our best seller to see if any relationship exits to Gender.
genderDist_bs  = ggplot(data = best_seller) +
  geom_bar(mapping = aes(x = Gender, y = ..count.., fill = Gender)) +
  labs(title = 'Gender of Customers (Best Seller)') +
  scale_fill_brewer(palette = 'Spectral')
print(genderDist_bs)

# Is there a similar distribution between gender and best seller and gender and all (total) purchases?
genderDist_bs_prop = ggplot(data = best_seller) + 
  geom_bar(fill = 'lightblue', mapping = aes(x = Gender, y = ..prop.., group = 1, fill = Gender)) +
  labs(title = 'Gender of Customers (Best Seller - Proportion)') +
  theme(plot.title = element_text(size=9.5))
genderDist_prop = ggplot(data = dataset_gender) + 
  geom_bar(fill = "lightblue4", mapping = aes(x = Gender, y = ..prop.., group = 1)) +
  labs(title = 'Gender of Customers (Total Proportion)') +
  theme(plot.title = element_text(size=9.5)) 
grid.arrange(genderDist_prop, genderDist_bs_prop, ncol=2)

#################################################################
# Age distribution of shoppers.
customers_age = dataset %>%
  select(User_ID, Age) %>%
  distinct() %>%
  count(Age)
customers_age_vis = ggplot(data = customers_age) + 
  geom_bar(color = 'black', stat = 'identity', mapping = aes(x = Age, y = n, fill = Age)) +
  labs(title = 'Age of Customers') +
  theme(axis.text.x = element_text(size = 10)) +
  scale_fill_brewer(palette = 'Blues') +
  theme(legend.position="none")
print(customers_age_vis)

# We can also generate a similar chart depicting the distribution of age within our "best seller" category. 
# This will show us if there is a specific age category that purchased the best selling 
# product more than other shoppers.
ageDist_bs  = ggplot(data = best_seller) +
  geom_bar(color = 'black', mapping = aes(x = Age, y = ..count.., fill = Age)) +
  labs(title = 'Age of Customers (Best Seller)') +
  theme(axis.text.x = element_text(size = 10)) +
  scale_fill_brewer(palette = 'GnBu') + 
  theme(legend.position="none")
print(ageDist_bs)

# Compare this discovery about customers to the overall purchases side-by-side.
grid.arrange(customers_age_vis, ageDist_bs, ncol=2)

#################################################################
# City (location) of our customers. Remember that there was no key given for city category.
# Let's create a table of each User_ID and their corresponding City_Category.
customers_location =  dataset %>%
  select(User_ID, City_Category) %>%
  distinct()
customers_location_vis = ggplot(data = customers_location) +
  geom_bar(color = 'white', mapping = aes(x = City_Category, y = ..count.., fill = City_Category)) +
  labs(title = 'Location of Customers') + 
  scale_fill_brewer(palette = "Dark2") + 
  theme(legend.position="none")
print(customers_location_vis)

# Compute the total purchase amount by City to see the which city's customers spent the most at our store.
purchases_city = dataset %>%
  group_by(City_Category) %>%
  summarise(Purchases = sum(Purchase))
purchases_city_1000s = purchases_city %>%
  mutate(purchasesThousands = purchases_city$Purchases / 1000)
# Visualize the results.
purchaseCity_vis = ggplot(data = purchases_city_1000s, aes(x = City_Category, y = purchasesThousands, fill = City_Category)) +
  geom_bar(color = 'white', stat = 'identity') +
  labs(title = 'Total Customer Purchase Amount (by City)', y = '($000s)', x = 'City Category') +
  scale_fill_brewer(palette = "Dark2") + 
  theme(legend.position="none", plot.title = element_text(size = 9))
print(purchaseCity_vis)
grid.arrange(customers_location_vis, purchaseCity_vis, ncol=2)

# Let's continue to investigate and try to determine the reason for the above discovery.
# Lets find how many purchases were made by customers from each city. 
# First, we will get the total number of purchases for each corresponding User_ID.
customers = dataset %>%
  group_by(User_ID) %>%
  count(User_ID)
head(customers)
# This tells us how many times a certain user made a purchase. 
# To dive deeper lets compute the total purchase amount for each user, then join it with the other table.
customers_City =  dataset %>%
  select(User_ID, City_Category) %>%
  group_by(User_ID) %>%
  distinct() %>%
  ungroup() %>%
  left_join(customers, customers_City, by = 'User_ID') 
head(customers_City)

city_purchases_count = customers_City %>%
  select(City_Category, n) %>%
  group_by(City_Category) %>%
  summarise(CountOfPurchases = sum(n))
city_purchases_count

city_count_purchases_vis = ggplot(data = city_purchases_count, aes(x = City_Category, y = CountOfPurchases, fill = City_Category)) +
  geom_bar(color = 'white', stat = 'identity') +
  labs(title = 'Total Purchase Count (by City)', y = 'Count', x = 'City Category') +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position="none", plot.title = element_text(size = 9))
print(city_count_purchases_vis)
grid.arrange(purchaseCity_vis, city_count_purchases_vis, ncol = 2)

# Now, since we have identified that the purchase counts across City_Category follow a similar distribution 
# to total purchase amount, lets examine the distribution of our best selling product (P00265242) 
# within each City_Category.
head(best_seller)
best_seller_city = best_seller %>%
  select(User_ID, City_Category) %>%
  distinct() %>%
  count(City_Category)
best_seller_city
best_seller_city_vis = ggplot(data = best_seller_city, aes(x = City_Category, y = n, fill = City_Category)) +
  geom_bar(color = 'white', stat = 'identity') +
  labs(title = 'Best Seller Purchase Count (by City)', y = 'Count', x = 'City Category') +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position="none", plot.title = element_text(size = 9))
grid.arrange(city_count_purchases_vis,best_seller_city_vis, ncol = 2)

# Stay in Current City:
# Lets now examine the distribution of customers who have lived in their city the longest.
customers_stay = dataset %>%
  select(User_ID, City_Category, Stay_In_Current_City_Years) %>%
  group_by(User_ID) %>%
  distinct()
# Lets see where most of our customers are living.
residence = customers_stay %>%
  group_by(City_Category) %>%
  tally()
head(residence)

# Lets investigate further.
customers_stay_vis = ggplot(data = customers_stay, aes(x = Stay_In_Current_City_Years, y = ..count.., fill = Stay_In_Current_City_Years)) +
  geom_bar(stat = 'count') +
  scale_fill_brewer(palette = 15) +
  labs(title = 'Customers Stay in Current City', y = 'Count', x = 'Stay in Current City', fill = 'Number of Years in Current City')
print(customers_stay_vis)

# In order to see a better distribution, lets make a stacked bar chart according to each City_Category.
stay_cities = customers_stay %>%
  group_by(City_Category, Stay_In_Current_City_Years) %>%
  tally() %>%
  mutate(Percentage = (n/sum(n))*100)
ggplot(data = stay_cities, aes(x = City_Category, y = n, fill = Stay_In_Current_City_Years)) + 
  geom_bar(stat = "identity", color = 'white') + 
  scale_fill_brewer(palette = 2) + 
  labs(title = "City Category + Stay in Current City", 
       y = "Total Count (Years)", 
       x = "City", 
       fill = "Stay Years")

#################################################################
# Purchases made by our customers.
# Now lets do some investigation regarding store customers and their purchases. 
# We will start by computing the total purchase amount by user ID.
customers_total_purchase_amount = dataset %>%
  group_by(User_ID) %>%
  summarise(Purchase_Amount = sum(Purchase))
head(customers_total_purchase_amount)
# Now that we have grouped our purchases and grouped by User ID, we will sort and find our top spenders.
customers_total_purchase_amount = arrange(customers_total_purchase_amount, desc((Purchase_Amount)))
head(customers_total_purchase_amount)

# Visualize the distribution of purchase amounts to see if purchases are normally distributed or contain some skewness. 
# A density plot will show us where the highest number of similar purchase amounts rests 
# in accordance to the entire customer base.
# In this density chart, the blue dashed line is the median and the red is the mean.
# Which way is it skewed?
ggplot(customers_total_purchase_amount, aes(Purchase_Amount)) +
  geom_density(adjust = 1) +
  geom_vline(aes(xintercept=median(Purchase_Amount)),
             color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(Purchase_Amount)),
             color="red", linetype="dashed", size=1) +
  geom_text(aes(x=mean(Purchase_Amount), label=round(mean(Purchase_Amount)), y=1.2e-06), color = 'red', angle=360,
            size=4, vjust=3, hjust=-.1) +
  geom_text(aes(x=median(Purchase_Amount), label=round(median(Purchase_Amount)), y=1.2e-06), color = 'blue', angle=360,
            size=4, vjust=0, hjust=-.1) +
  scale_x_continuous(name="Purchase Amount", limits=c(0, 7500000), breaks = seq(0,7500000, by = 1000000), expand = c(0,0)) +
  scale_y_continuous(name="Density", limits=c(0, .00000125), labels = scientific, expand = c(0,0))

#################################################################
# Marital Status of our customers.
dataset_maritalStatus = dataset %>%
  select(User_ID, Marital_Status) %>%
  group_by(User_ID) %>%
  distinct()
# We need to change Marital_Status from a numeric variable to a categorical type.
dataset_maritalStatus$Marital_Status = as.character(dataset_maritalStatus$Marital_Status)
typeof(dataset_maritalStatus$Marital_Status)

# If we look back at the variable descriptions of the dataset, we don't have a clear guide for marital status. 
# In other cases, it would be best to reach out to the provider of the data to be completely 
# sure of what the values in a column represent; but, in this case, we will assume that 1 = married and 0 = single.
marital_vis = ggplot(data = dataset_maritalStatus) +
  geom_bar(mapping = aes(x = Marital_Status, y = ..count.., fill = Marital_Status)) +
  labs(title = 'Marital Status') +
  scale_fill_brewer(palette = 'Paired')
print(marital_vis)

# Similar to our investigation of age groups, we can look at the makeup of Marital_Status by each City_Category.
dataset_maritalStatus = dataset_maritalStatus %>%
  full_join(customers_stay, by = 'User_ID') 
maritalStatus_cities = dataset_maritalStatus %>%
  group_by(City_Category, Marital_Status) %>%
  tally()
ggplot(data = maritalStatus_cities, aes(x = City_Category, y = n, fill = Marital_Status)) + 
  geom_bar(stat = "identity", color = 'black') + 
  scale_fill_brewer(palette = 2) + 
  labs(title = "City + Marital Status", 
       y = "Total Count (Shoppers)", 
       x = "City", 
       fill = "Marital Status")

#################################################################
# Now, investigate the Stay_in_Current_City distribution within each City_Category.
Users_Age = dataset %>%
  select(User_ID, Age) %>%
  distinct()
dataset_maritalStatus = dataset_maritalStatus %>%
  full_join(Users_Age, by = 'User_ID')
City_A = dataset_maritalStatus %>%
  filter(City_Category == 'A')
City_B = dataset_maritalStatus %>%
  filter(City_Category == 'B')
City_C = dataset_maritalStatus %>%
  filter(City_Category == 'C')
City_A_stay_vis = ggplot(data = City_A, aes(x = Age, y = ..count.., fill = Age)) + 
  geom_bar(stat = 'count') +
  scale_fill_brewer(palette = 8) +
  theme(legend.position="none", axis.text = element_text(size = 6)) +
  labs(title = 'City A', y = 'Count', x = 'Age', fill = 'Age')
City_B_stay_vis = ggplot(data = City_B, aes(x = Age, y = ..count.., fill = Age)) +
  geom_bar(stat = 'count') +
  scale_fill_brewer(palette = 9) +
  theme(legend.position="none", axis.text = element_text(size = 6)) +
  labs(title = 'City B', y = 'Count', x = 'Age', fill = 'Age')
City_C_stay_vis = ggplot(data = City_C, aes(x = Age, y = ..count.., fill = Age)) +
  geom_bar(stat = 'count') +
  scale_fill_brewer(palette = 11) +
  theme(legend.position="none", axis.text = element_text(size = 6)) +
  labs(title = 'City C', y = 'Count', x = 'Age', fill = 'Age')
grid.arrange(City_A_stay_vis, City_B_stay_vis, City_C_stay_vis, ncol = 3)

#################################################################
# Who are our "Top Shoppers" on Black Friday?
top_shoppers = dataset %>%
  count(User_ID, sort = TRUE)
head(top_shoppers)

# We can join together this top shoppers data with our total customer purchases data to see them combined.
top_shoppers =  top_shoppers %>%
  select(User_ID, n) %>%
  left_join(customers_total_purchase_amount, Purchase_Amount, by = 'User_ID')
head(top_shoppers)
# Now that we have joined the two tables together, who has the highest number of total purchases?
# And, who has the highest Purchase_Amount 
# From here, we can also compute the average Purchase_Amount for each user.
top_shoppers = mutate(top_shoppers,
                      Average_Purchase_Amount = Purchase_Amount/n)
head(top_shoppers)

# We can sort according to Average_Purchase_Amount to see which customers, on average, are spending the most.
top_shoppers_averagePurchase = top_shoppers %>%
  arrange(desc(Average_Purchase_Amount))
head(top_shoppers_averagePurchase)

#################################################################
# Occupation of our shoppers.
customers_Occupation =  dataset %>%
  select(User_ID, Occupation) %>%
  group_by(User_ID) %>%
  distinct() %>%
  left_join(customers_total_purchase_amount, Occupation, by = 'User_ID')
head(customers_Occupation)

# Group together the total Purchase_Amount for each Occupation identifier. 
# Then convert Occupation to a character data type.
totalPurchases_Occupation = customers_Occupation %>%
  group_by(Occupation) %>%
  summarise(Purchase_Amount = sum(Purchase_Amount)) %>%
  arrange(desc(Purchase_Amount))
totalPurchases_Occupation$Occupation = as.character(totalPurchases_Occupation$Occupation)
typeof(totalPurchases_Occupation$Occupation)
head(totalPurchases_Occupation)

# Now, plot each occupation and their total Purchase_Amount.
occupation = ggplot(data = totalPurchases_Occupation) +
  geom_bar(mapping = aes(x = reorder(Occupation, -Purchase_Amount), y = Purchase_Amount, fill = Occupation), stat = 'identity') +
  scale_x_discrete(name="Occupation", breaks = seq(0,20, by = 1), expand = c(0,0)) +
  scale_y_continuous(name="Purchase Amount ($)", expand = c(0,0), limits = c(0, 750000000)) +
  labs(title = 'Total Purchase Amount by Occupation') + 
  theme(legend.position="none")
print(occupation)

# If a key to occupation was provided in the data dictionary, 
# we could use that information to classify or cluster our shoppers accordingly.
# But, we don't have that key!

#################################################################
# Association Rules Modelling.
# Now lets use the Apriori machine learning algorithim to make some association rules 
# regarding customer purchases. We will be using the arules package.
# The arules package was developed specifically to deal with Association Rules and Frequent Itemset mining. 
# In order to begin our analysis, we must retrieve the necessary data from the original dataset 
# and then apply the correct formatting.

# Getting the dataset into the correct format
customers_products = dataset %>%
  select(User_ID, Product_ID) %>%   # Selecting the columns we will need
  group_by(User_ID) %>%             # Grouping by "User_ID"          
  arrange(User_ID) %>%              # Arranging by "User_ID" 
  mutate(id = row_number()) %>%     # Defining a key column for each "Product_ID" and its corresponding "User_ID" (Must do this for spread() to work properly)
  spread(User_ID, Product_ID) %>%   # Converting our dataset from tall to wide format, and grouping "Product_IDs" to their corresponding "User_ID"
  t()                               # Transposing the dataset from columns of "User_ID" to rows of "User_ID"
# Remove the Id row created earlier since we no longer need it.
customers_products = customers_products[-1,]

# In order for the Apriori algorithm to work correctly, we need to convert the customers_products table 
# into a sparce matrix. Unfortunately, Apriori doesn't take strings or text as input, but rather 1 and 0 (Binary Format) 
# This means that we must allocate a column for each individual product, and then if a User_ID contains that product, 
# it will be marked as a 1. On the other hand, if the User_ID does not contain that Product_ID, 
# it wil be marked with a 0.
# In order to do so, we need to import the table as a csv file. 
# From there, we can use the arules function, "read.transactions()" to get our sparse matrix.

# So, write out the customers_products data to the C:drive, then read it back in as transactions.
# This may take a while.
write.csv(customers_products, file = 'C:/Docs/Babson/QTM2000/Data/customers_products.csv')
customersProducts = read.transactions('C:/Docs/Babson/QTM2000/Data/customers_products.csv', sep = ',', rm.duplicates = TRUE) # remove duplicates with rm.duplicates
# Take a quick look at our newly created sparse matrix.
summary(customersProducts)
# We can see that there are 5892 rows (elements/itemsets/transactions) and 10539 columns (items) 
# in our sparse matrix. With this summary function, we get a density of 0.008768598 in our matrix. 
# The density tells us that we have 0.9% non-zero values (1) in our sparse matrix and 99.1% zero (0) values.
# Also, as we discovered in our Exploratory Data Analysis, the summary() function also gives us 
# the most frequent items that customers purchased. This will be useful in our association rules findings.

# Lets continue to examine our sparse matrix.
# The "element (itemset/transaction) length distribution" gives us a distribution of the number of items 
# in a customers (User) basket and underneath it we can see more information including the quartile 
# and mean information. What does that say about how many items each customer purchased, on average?
# We are aware that a few customers purchased over ~1000 items. 
# It may be useful to use the median value of items purchased instead of the mean,
# since the mean can be heavily affected by outlier values.

# To get a clearer picture of the items, create an item frequency plot, which is included in the arules package.
# topN is limiting the plot to the top 25 products (by choice).
itemFrequencyPlot(customersProducts, topN = 25)    

# Begin training the association rule (arules) model.
# Our first step will be to set our parameters. The first parameters we will set are the support and confidence. 
# The support value is derived from the frequency of a specific item within the dataset. 
# When we set our support value, we are setting a minimum number of transactions necessary for our rules to take effect.

# Support: Our support value will be the minimum number of transactions necessary divided by 
# the total number of transactions.
# As described by summary(customersProducts), we have a total number of unique customer transactions of 5892.
# From our dataset, lets assume that we want to choose a product which was purchased 
# by at least 50 different customers.
# With these two values established, we can compute the support value with simple division. (50/5892) = .008486083

# The second parameter we will take into consideration will be the confidence. 
# The confidence value determines how often a rule is to be found true. It is the conditional
# probability of buying the Consequent given that the antecedent was first purchased.
# In other words, the minimum strength of any rule is a limit we place when setting our minimum confidence value.
# The default confidence value in the apriori() function is 0.80 or 80%, 
# so we can begin with that number, and then adjust the parameters to applicable results.

# Confidence: We can determine our confidence value by first starting with the default value and adjusting accordingly.
# With more domain knowledge, and with Product_IDs referencing items with recognizable names, 
# the Confidence value can be easily changed to see different, and more relevant, results.
# In our case, we will start with a value and then lower the confidence to see different rules.

# The parameter, maxtime = 0, will allow the apriori algorithim to run until completion with no time limit.
# This takes a while!
rules = apriori(data = customersProducts,
parameter = list(support = 0.008, confidence = 0.80, maxtime = 0)) 

# It looks like apriori has created 7 rules in accordance to our specified parameters.
# Let's examine our results to get a better idea of how our algoritm worked.
# We will sort the rules by lift.
inspect(sort(rules, by = 'lift'))

# Here we see the association rules created by our apriori algorithm. Let's take a look at rule number 1.
# We see a few values listed and we will go through them individually.
# The first value, lhs, corresponds to a grouping of items which the algorithm has pulled from the dataset.
# The second value, rhs, corresponds to the value predicted by apriori to be purchased with items in the "lhs" category.
# The third value, support is the number of transactions including that specific set of items 
# divided by the total number of transactions.
# The fourth value, confidence is the % chance in which a rule will be upheld.
# The fifth value, lift gives us the independence/dependence of a rule. 
# It takes the confidence value and its relationship to the entire dataset into account.
# The sixth and final value, count is the number of times a rule occurred during the 
# implementation of Apriori for our data.

# We can visualize these rules using the arulesViz package.
# It will be hard to see the arrows (associations), so Export the image and paste it into MSWord.
plot(rules, method = 'graph')
# Here we can see a visualization of our association rules. 
# Arrows pointing from items to rule vertices indicate LHS (Grouped) items and arrows 
# from rules to items indicates the RHS (Rule Item).
# The size of the bubbles indicate the support, with larger bubbles representing a higher support value. 
# Fill color represents the lift values, with darker colors representing higher lifts.

#################################################################
# Modifying some of the parameters for the Apriori algotrithm to observe the result. 
# This process would prove to be more intuitive if given a key for each corresponding Product_ID, which we don't have,
# so we will only implement the algorithm once more.
# This time, we will decrease our confidence value to 75% and keep our support value the same (0.008).
rules = apriori(data = customersProducts,
                parameter = list(support = 0.008, confidence = 0.75, maxtime = 0))

# Now that we have decreased the minimum confidence value to 75%, we have a total of 171 rules.
# This is a much higher number of rules compared to our previous rule list, which only contained 7. 
# This should now give us more interesting rules to examine.

# limiting (using the head function) to just the top 6 rules.
inspect(head(sort(rules, by = 'lift'))) 
# We can now see that we now have a new set of rules and the rule with the highest lift value has also changed.
# Rule number 1 shows that Customers who bought items P00221142 and P00249642 will also purchase 
# item P00103042 ~76% of the time, given a support of 0.008.

# Visualize this iteration of arules.
plot(rules, method = 'graph', max = 25)
# Now that we have more that 7 rules, this visualization becomes alot more difficult to interpret. 
# Frankly, I would not attempt to explain this plot to an executive in a summary, 
# especially since there are too many product keys.
# Instead, we can create a matrix and have a similar plot and clearer interpretation.
# To do this, we will use the "grouped" method.

# In the following visualization, we have our LHS on top; and, 
# on the right hand side the corresponding RHS. 
# The size of the bubbles represents the support value of the rule,
# and the fill/color represents the lift.
plot(rules, method = 'grouped', max = 25)

#################################################################
#################################################################