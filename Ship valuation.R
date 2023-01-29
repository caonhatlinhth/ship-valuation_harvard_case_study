
library(corrplot)
library(ggplot2)
library(reshape2)
library(GGally)
library(dplyr)
file <- "RegressionData.csv"
ship_info <- read.csv(file) #Upload the data
head(ship_info,5) #View the ship_info
names(ship_info)

summary(ship_info) # Summary Statistics 

ship_info$Vessel <- NULL # Removing character for our analysis
ship_info$SaleDate <- NULL # Removing character for our analysis

# Mean and Standard Deviation for the 5 numeric variables
summary_table = data.frame( Variable = c("Price","YearBuilt","Age.at.Sale","DWT","Capesize"),
                            Mean = c(mean(ship_info$Price, na.rm = TRUE),mean(ship_info$YearBuilt, na.rm = TRUE),
                                     mean(ship_info$Age.at.Sale, na.rm = TRUE),mean(ship_info$DWT, na.rm = TRUE),
                                     mean(ship_info$Capesize, na.rm = TRUE) ),
                            
                            Std = c( sd(ship_info$Price, na.rm = TRUE),
                                    sd(ship_info$YearBuilt, na.rm = TRUE), sd(ship_info$Age.at.Sale, na.rm = TRUE), 
                                    sd(ship_info$DWT, na.rm = TRUE), sd(ship_info$Capesize, na.rm = TRUE))
)


# 5 variable correlation matrix
ship_info_corr <- cor(ship_info[c("YearBuilt", "Age.at.Sale", "Price", "DWT", "Capesize")] )


# Correlation matrix
correlation_matrix <- round(cor(ship_info_corr),1)

# assign to ship_corr_mat correlation matrix
ship_corr_mat <- melt(correlation_matrix)

# plotting of Ship Price
hist(ship_info$Price,
     main="Histogram for Ship Prices",
     xlab="Price",
     ylab="# of Ship",
     xlim=c(20,160),
     breaks = 10
)



# plotting the correlation heatmap
ggplot(data = ship_corr_mat, aes(x=Var1, y=Var2,fill=value)) +
  
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "value", title="Correlation between the ship variables") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size = 4) +
  theme_classic() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  theme(text=element_text(family="Arial"))


#output mean and standard deviation of five variables
print(summary_table)
#output Correlation matrix
print(correlation_matrix)


# Filter the rows based on the condition
# Based on Comparable transactions (Benchmark)
ship_info$YearBuilt <- NULL # Removing character for our analysis
subset(ship_info, ship_info$Price > 100 & ship_info$DWT >=170)



###########################Q1  ######################################################
print(ship_info)
print("Finding manhattan distance to bet performer")
bet_performer= data.frame(Age.at.Sale = c(11) ,DWT =c(172), Capesize=c(12479))   #refer to the case study sheet[bet performer]
m_distance_betperformer <-  (ship_info$Age.at.Sale - bet_performer$Age.at.Sale) + (ship_info$DWT - bet_performer$DWT) + (ship_info$Capesize - bet_performer$Capesize)
print(abs(m_distance_betperformer))

summary(abs(m_distance_betperformer))

###################################Q1##Price###########################################
normalize <- function(x){
  return( trunc( (x - min(x)) / (max(x) - min(x)),0) )
}
print("Bet Performer comparable ship price and best transaction reference is:")
normalize(m_distance_betperformer)
print("Bet Performer comparable ships price are:")
bet_comparable_ship <-  filter(ship_info, Capesize == 12479 & Age.at.Sale == 15)   
head(bet_comparable_ship)


################### Regression Section - Q2  Ans 1start here #####################################
################### Model-2 Overall ship price which associated to others (without Economic index) #######################
lm_price_model <- lm(Price~Age.at.Sale + DWT , data = ship_info, method ="qr")  #linear regression decomposition method.
summary(lm_price_model)
#plot(lm_price_model)

# Significant p-values
# age.at.sale = 0.00000000537
# dwt  =0.0310
# p-value = 0.00000000003311
hist(ship_info$Price,
     main="Ship Price Distribution",
     xlab="Price",
     xlim=c(20,160),
     col="steelblue",
     breaks = 15
)

################## checking ####################################################################
#coef(lm_price_model)
#confint(lm_price_model, level=0.95)
#plot( ship_info$Price , predict(lm_price_model))
r2 <- summary(lm_price_model)$r.squared 
print(r2)
###################################Q2 Ans (1)#############################################
#Regression Plots relationship btw Price and other ship features 
#pch = 16 creates solid dots, while cex = 1.3 dots bigger
plot(ship_info$Age.at.Sale, ship_info$Price, pch = 17, cex = 1.0,col = "steelblue" , main = "Age at Sale and Price" , xlab = "Age.at.Sale (In Year)" , ylab = " Price (In Mils)")
abline(lm(Price~Age.at.Sale, data = ship_info))
lm(Price~Age.at.Sale, data = ship_info)


plot(ship_info$DWT, ship_info$Price, pch = 17, cex = 1.0,col = "steelblue", main = "DWT and Price" , xlab = "DWT (In Tonnes)" , ylab = " Price (In Mils)")
abline(lm(Price~DWT, data = ship_info))
lm(Price~DWT, data = ship_info)


plot(ship_info$Capesize, ship_info$Price, pch = 17, cex = 1.0, col = "steelblue", main = "Capesize and Price" , xlab = "Capesize (Index)" , ylab = " Price (In Mils)")
abline(lm(Price~Capesize, data = ship_info))
lm(Price~Capesize, data = ship_info)


##############################################################
#pairs(ship_info, pch = 18, col = "steelblue")


# Predict Price fit in 95% confidence Interval 
# bet performer
predict_newprice <- data.frame(Age.at.Sale = c(11) ,DWT =c(172), Capesize=c(12479))  #independent varaible refer from case study
predict(lm_price_model,newdata = predict_newprice)
print("Predicted of bet performer price :")
predict(lm_price_model,newdata = predict_newprice , interval = "confidence")


##############################Q2 - Ans 2 ##################################################
print(("Which single feature is best predictor of ship price:"))
#cor(ship_info)

ggpairs(ship_info)


##############################Q2 - Ans 5 ##################################################
2008 - 5 
print("5 years younger ship =")
2009 - 2003
print("20k lighter DWT =")
172 - 20
print("30% lower charter rates/capesize  =")
12479 - (12479 * 0.3)

# Predict Price fit in 95% confidence Interval 
# Predict price for 5 year younger
predict_newprice <- data.frame(Age.at.Sale = c(6) ,DWT =c(172), Capesize=c(12479))  
predict(lm_price_model,newdata = predict_newprice)
print("If the ship is 5 year younger, the predicted price will be:")
predict(lm_price_model,newdata = predict_newprice , interval = "confidence")


# Predict 20k lighter DWT 
predict_newprice <- data.frame(Age.at.Sale = c(11) ,DWT =c(152), Capesize=c(12479))  
predict(lm_price_model,newdata = predict_newprice)
print("If the DWT is 20k lighter, the predicted price will be:")
predict(lm_price_model,newdata = predict_newprice , interval = "confidence")

# Predict 30% lower charter rates
predict_newprice <- data.frame(Age.at.Sale = c(11) ,DWT =c(172), Capesize=c(8735))  
predict(lm_price_model,newdata = predict_newprice)
print("If the charter rates is 30% lower, the predicted price will be:")
predict(lm_price_model,newdata = predict_newprice , interval = "confidence")

