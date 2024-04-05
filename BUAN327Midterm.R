
library(dplyr)
library(ggplot2)

# Exploratory analysis

hospitals = read.csv("hospitals.csv")

dim(hospitals)
colnames(hospitals)
str(hospitals)

anyNA(hospitals)
colSums(is.na(hospitals))


# Which hospital has the lowest number of beds?
hospitals %>% filter( Beds == min(Beds))
# Answer- Hospital Number 1064 and 1751 with 3 beds 

# Which hospital has the lowest expense?
hospitals %>% filter( Total.Expense == min(Total.Expense))
# Answer- Hospital Number 826

# How many hospitals deliver babies?
sum(hospitals$Births.or.Not == 1)
# Answer- 1112

# Scatterplot of Beds vs Total Expense 
ggplot(hospitals, aes(x = Beds, y = Total.Expense)) +
  geom_point() + 
  xlab("Beds") +  
  ylab("Total Expense") + 
  ggtitle("Scatterplot of Beds vs Total Expense") 


# Scatterplot of Admissions vs Total Expense
ggplot(hospitals, aes(x = Admissions, y = Total.Expense)) +
  geom_point() + 
  xlab("Admissions") +  
  ylab("Total Expense") + 
  ggtitle("Scatterplot of Admissions vs Total Expense") 


# Beds vs Total Expense of hospitals that deliver babies
delivering_hospitals <- filter(hospitals, Births.or.Not == 1)
ggplot(delivering_hospitals, aes(x = Beds, y = Total.Expense)) +
  geom_point() + 
  xlab("Beds") +  
  ylab("Total Expense") + 
  ggtitle("Scatterplot of Beds vs Total Expense") 

# Scatterplot of Outpatient Visits vs Total Expense
ggplot(hospitals, aes(x = Outpatient.Visits, y = Total.Expense)) +
  geom_point() + 
  xlab("Outpatient Visits") +  
  ylab("Total Expense") + 
  ggtitle("Scatterplot of Admissions vs Total Expense") 

# Pie Chart 
total_admissions <- sum(hospitals$Admissions)
total_visits <- sum(hospitals$Outpatient.Visits)

pie_data <- data.frame(Category = c("Admissions", "Outpatient Visits"),
                       Value = c(total_admissions, total_visits))

 ggplot(pie_data, aes(x = "", y = Value, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Distribution of Admissions and Outpatient Visits")

 # Bar chart 
 filtered_hospitals <- hospitals %>% 
   filter(Admissions < 10000)

 ggplot(filtered_hospitals, aes(x = Hospital.Number)) +
   geom_col(aes(y = Admissions, fill = "Admissions"), position = "dodge") +
   geom_col(aes(y = Beds, fill = "Beds"), position = "dodge") +  
   scale_fill_manual(values = c("Admissions" = "blue", "Beds" = "green")) +  
   ggtitle("Comparison of Admissions and Beds by Hospital #") +  
   theme_minimal()

 # Line chart
 
 ggplot(hospital_data, aes(x = factor(1), y = Admissions, fill = "Admissions")) +
   geom_bar(stat = "identity") +
   geom_bar(aes(y = -`Total Expense`, fill = "Total Expense"), stat = "identity") +
   labs(y = "Value", x = NULL) +
   scale_y_continuous(labels = abs) +
   scale_fill_manual(values = c("Admissions" = "blue", "Total Expense" = "darkred")) +
   theme(legend.title = element_blank())
 
 
 
 # Simple Regression Analysis
 
 reg_model <- summary(lm(Total.Expense ~ Beds, hospitals))
 
 #### R squared = 0.6043 shows that 60% of the change in total expense 
 #### can be explained by the amount of beds in the hospital
 ## One P value = 2.2e-16 because we are testing one dependent variable
 ## Small Pvalue shows that our sample is a good predictor of what the relationship btwn
 ## the population of all hospitals and number of beds looks like. We reject the
 ## Null hypothesis that there is no relationship between expense and beds. 
 ## based on my calculations, It looks like 100 beds would lead to an expense range 
 ## between $55-$75 million
 
 # Multiple regression analysis
 
 multi_model <- lm(Total.Expense ~ Admissions + Beds, data = hospitals)
 summary(multi_model)
 ### R squared= 0.7398 shows that 73% of the change in total expense can be explained 
 ## by both admissions and beds together. There are 3 pvalues. One that predicts just
 # beds on expense, one that predicts admissions and expense, and an overall P value
 # that predicts the extremeness of the null hypothesis on a population rather than 
 # the sample. Since the pvalue is extremely low, we reject the null hypothesis and conclude
 # that together, beds and admissions affect total expense. 
 
 
 ## Regression results showed a relationship between Beds and Admissions on the Total
 # Expense. We can use the equation of the regression line to figure out how many beds 
 # we should have in our hospital in a medium-sized town
 
 # According to my calculations, it would be best to build a small hospital with 90-100 beds
 # to meet the expense range of $55-$75 million. Option B would not work because the expense
 # would be too high.
 
 
 