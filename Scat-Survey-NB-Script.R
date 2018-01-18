
#Call Packages: 

#Tidyverse: for
library(tidyverse)

#ggplot2: for creating plots
library(ggplot2)

#gridExtra: for visualizing multiple plots at the same time 
library(gridExtra)

#corrplot: for checking correlation between variables 
library(corrplot)

#MASS: contains glm.nb
library(MASS)

#MuMIn: contains dredge function
library(MuMIn)

#pander: For creating more readable AICc tables 
library(pander)

# Read Data: 
Trail_Segment_Variables<- read.csv("~/Documents/Scat_Survey/Trail Segment Buffer Variables/Trail Segment Variables.csv")

#Create dataframe that does not contain unnecessary fields:
Analysis_Variables <- Trail_Segment_Variables[c(11:16, 20:22, 24, 26)]

# Create a histogram to see the number of trail segments with each scat count 
Scat_Histogram <- ggplot(Analysis_Variables, aes(Scat_Count)) + 
  geom_histogram(binwidth = 0.2) 

Scat_Histogram

#Create plots of Scat count vs each analysis variable:
Agricultural <- ggplot(Analysis_Variables, aes(y = Scat_Count, x = Percent_Agricultural)) + 
  geom_point() + 
  geom_smooth(method = glm.nb)

Greenspace <- ggplot(Analysis_Variables, aes(y = Scat_Count, x = Percent_Greenspace)) + 
  geom_point() + 
  geom_smooth(method = glm.nb)

Open <- ggplot(Analysis_Variables, aes(y = Scat_Count, x = Percent_Developed_Open_Space)) + 
  geom_point() + 
  geom_smooth(method = glm.nb)

Low <- ggplot(Analysis_Variables, aes(y = Scat_Count, x = Percent_Developed_Low)) + 
  geom_point() + 
  geom_smooth(method = glm.nb)

Medium <- ggplot(Analysis_Variables, aes(y = Scat_Count, x = Percent_Developed_Medium)) +
  geom_point() + 
  geom_smooth(method = glm.nb)

High <- ggplot(Analysis_Variables, aes(y = Scat_Count, x = Percent_Developed_High)) + 
  geom_point() + 
  geom_smooth(method = glm.nb)

Road <- ggplot(Analysis_Variables, aes(y = Scat_Count, x = Road_Density)) + 
  geom_point() + 
  geom_smooth(method = glm.nb)

Evenness <- ggplot(Analysis_Variables, aes(y = Scat_Count, x = Shannon_Evenness)) + 
  geom_point() + 
  geom_smooth(method = glm.nb)

#Look at all plots at once
grid.arrange(Agricultural, Greenspace, Open, Low, Medium, High, Road, Evenness)

#Check correlation between variables:
Correlation_Matrix <- cor(Trail_Segment_Variables[c(11:16,20:21)])

corrplot(Correlation_Matrix, method = "circle")

#Create global model with remainin variables and check summary: 
Global_Model <- glm.nb(Scat_Count ~ Percent_Greenspace + Percent_Developed_Open_Space + Percent_Developed_Low + 
                       Percent_Developed_Medium + Percent_Developed_High + Shannon_Evenness, 
                       data = Analysis_Variables, na.action = "na.fail") 

summary(Global_Model)

#Use dredge to creat all possible models: 
models <- dredge(global.model = Global_Model)

model.sel(models)

#Create top 5 models and look at summaries: 
Greenspace_Model <- glm.nb(Scat_Count ~ Percent_Greenspace, data = Analysis_Variables)
summary(Greenspace_Model)

Greenspace_Shannon_Evenness <-  glm.nb(Scat_Count ~ Percent_Greenspace + Shannon_Evenness, data = Analysis_Variables)
summary(Greenspace_Shannon_Evenness)

Greenspace_Percent_Developed_Medium <-  glm.nb(Scat_Count ~ Percent_Greenspace + Percent_Developed_Medium, data = Analysis_Variables)
summary(Greenspace_Percent_Developed_Medium)

Greenspace_Percent_Developed_Open <-  glm.nb(Scat_Count ~ Percent_Greenspace + Percent_Developed_Open_Space, data = Analysis_Variables)
summary(Greenspace_Percent_Developed_Open)

Greenspace_Percent_Developed_Low <-  glm.nb(Scat_Count ~ Percent_Greenspace + Percent_Developed_Low, data = Analysis_Variables)
summary(Greenspace)

#Create AICc Table with Top 5 Models:  
Top_Models <- c(Greenspace_Model, Greenspace_Percent_Developed_Low, Greenspace_Percent_Developed_Medium,
                Greenspace_Percent_Developed_Open, Greenspace_Shannon_Evenness)

summ.table2 <- do.call(rbind, lapply(list(Greenspace_Model, Greenspace_Percent_Developed_Low, 
                                          Greenspace_Percent_Developed_Medium, Greenspace_Percent_Developed_Open, 
                                          Greenspace_Shannon_Evenness), broom::glance))
summ.table2

table.cols <- c("AIC")
reported.table2 <- summ.table2[table.cols]

reported.table2[['AICc']] <- with(reported.table2, c(167.9986, 169.718, 170.148, 170.1559, 170.1616))
reported.table2[['dAICc']] <-  with(reported.table2, AICc - min(AICc))
reported.table2[['weight']] <- with(reported.table2, exp(- 0.5 * dAICc) / sum(exp(- 0.5 * dAICc)))
reported.table2 <- reported.table2[c(2:4)]

row.names(reported.table2) <- c("Greenspace","Greenspace + Shannon Evenness","Greenspace + Percent Developed Medium","Greenspace + Percent Developed Open", "Greenspace + Percent Developed Low")

reported.table2

#Use Pander to get a cleaner looking AIC table:
pander(reported.table2)
