library(tidyverse)

# Import and read file as a dataframe
mecha_mpg_data <- read.csv('MechaCar_mpg.csv')
head(mecha_mpg_data)

#Perform linear regression using the lm() function. 
#In the lm() function, pass in all six variables (i.e., columns), and 
#add the dataframe you created in Step 4 as the data parameter.

mechaLM <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mecha_mpg_data)

#Using the summary() function, determine the p-value and the r-squared value for the linear regression model.
summary(mechaLM)

# import and read in the Suspension_Coil.csv file as a table
suspension_table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

#total_summary dataframe using the summarize() function to get 
#the mean, median, variance, and standard deviation of the suspension coil's PSI column
total_summary <- suspension_table %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep')

#creates a lot_summary dataframe using the group_by() and the summarize() functions 
#to group each manufacturing lot by the mean, median, variance, and standard deviation 
#of the suspension coil's PSI column
lot_summary <- suspension_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep')
lot_summary2 <- suspension_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI),Min=min(PSI),Max=max(PSI), .groups = 'keep')

#using the t.test() function to determine if the PSI across all 
#manufacturing lots is statistically different from the 
#population mean of 1,500 pounds per square inch.
t.test((suspension_table$PSI),mu=1500) 

t.test(subset(suspension_table,Manufacturing_Lot=="Lot1")$PSI, mu = 1500)

t.test(subset(suspension_table,Manufacturing_Lot=="Lot2")$PSI, mu = 1500)

t.test(subset(suspension_table,Manufacturing_Lot=="Lot3")$PSI, mu = 1500)






