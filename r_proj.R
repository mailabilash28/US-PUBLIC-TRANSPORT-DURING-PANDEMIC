#Hypothesis test for the pollution difference mean is eaual/ not equal to sample pollution diff. mean 
library("readxl")
poll.us <- read_excel("hypdata.xlsx")
poll_sample <- {set.seed(1); sample( poll.us$Poll_diff,30 )}
head(poll_sample)
head(poll.us$Poll_diff)
mean(poll_sample)
mean(poll.us$Poll_diff)
# X = R.V. of pollution difference
# H0: pollution_Mean = 0.8674429
# H1: pollution_mean != 0.8674429

# Step 1: Create a z-test function manually.

# Create the function: two tail sample z test
z.test <- function(sample, pop){ #This function has two input arguments (sample, pop)
  sample_mean = mean(sample) 
  pop_mean = mean(pop)
  n = length(sample) 
  var = var(pop)
  z = (sample_mean - pop_mean) / sqrt((var/(n))) 
  df<-data.frame("Z_calc"=z,"P_value"=pnorm(z))
  return(df)
}

#Step 2: Using samples created earlier

sample     <-poll_sample
population <-poll.us$Poll_diff
options(scipen = 100)
# Step 3: Running the two-sample z-test 
z.test(sample, population)

# Step 4: Conclusion from the Test using the p-value approach

----------------------------------------------------------------------------------------

# finding the Confidence Intervel for the NO2 pollutant
library("readxl")
popul.us <- read_excel("fdata.xlsx",
                       col_names = TRUE)
attach(popul.us)
options(scipen = 100)
sample.mean <- mean(popul.us$NO2_18)
print(sample.mean)
sample.n <- length(popul.us$NO2_18)
sample.sd <- sd(popul.us$NO2_18)
sample.se <- sample.sd/ sqrt(sample.n)
print(sample.se)
alpha = 0.05
degrees.freedom = sample.n - 1
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
print(t.score)
margin.error <- t.score * sample.se
lower.bound <- sample.mean - margin.error
upper.bound <- sample.mean + margin.error
print(c(lower.bound,upper.bound))

--------------------------------------------------------------------------------
# finding the goodness of fitness test
library(fitdistrplus)
library(ggplot2)
library("readxl")
popul.us <- read_excel("fdata.xlsx")
names(popul.us)
#2018
popul.us <- read_excel("fdata.xlsx")
ggplot(popul.us, aes(tot_pollution18)) +
  geom_histogram(bins = 50, color = 'Black', fill = 'steelblue')
descdist(popul.us$tot_pollution18)

# fitness 2018
fit_ln <- fitdist(popul.us$tot_pollution18, "lnorm")
summary(fit_ln)
fit_nor <- fitdist(popul.us$tot_pollution18, "norm")

summary(fit_nor)
fit_exp <- fitdist(popul.us$tot_pollution18, "exp")
summary(fit_exp)
fit_gama <- fitdist(popul.us$tot_pollution18,"gamma", lower=c(0,0), start=list(scale=1,shape=1))
summary(fit_gama)
par(mfrow=c(2,2))
plot.legend <- c("lnorm")
denscomp(list(fit_ln), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
cdfcomp (list(fit_ln), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
qqcomp  (list(fit_ln), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
ppcomp  (list(fit_ln), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
par(mfrow=c(2,2))
plot.legend <- c("norm")
denscomp(list(fit_nor), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
cdfcomp (list(fit_nor), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
qqcomp  (list(fit_nor), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
ppcomp  (list(fit_nor), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
par(mfrow=c(2,2))
plot.legend <- c("exp")
denscomp(list(fit_exp), legendtext = plot.legend, xlab = 'Relative Humidity (RH)', xlegend = 'topleft')
cdfcomp (list(fit_exp), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
qqcomp  (list(fit_exp), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
ppcomp  (list(fit_exp), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
par(mfrow=c(2,2))
plot.legend <- c("gama")
denscomp(list(fit_gama), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
cdfcomp (list(fit_gama), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
qqcomp  (list(fit_gama), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
ppcomp  (list(fit_ln), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')

------------------------------------------------------------------------------------
# Showing the heatmap for the pollution of 2018,2019,2020
  library(fitdistrplus)
library(ggplot2)
library("readxl")
popul.us <- read_excel("fdata.xlsx")
names(popul.us)
#2018
popul.us <- read_excel("fdata.xlsx")
ggplot(popul.us, aes(tot_pollution18)) +
  geom_histogram(bins = 50, color = 'Black', fill = 'steelblue')
descdist(popul.us$tot_pollution18)

# fitness 2018
fit_ln <- fitdist(popul.us$tot_pollution18, "lnorm")
summary(fit_ln)
fit_nor <- fitdist(popul.us$tot_pollution18, "norm")
summary(fit_exp)
fit_gama <- fitdist(popul.us$tot_pollution18,"gamma", lower=c(0,0), start=list(scale=1,shape=1))
summary(fit_gama)
par(mfrow=c(2,2))
plot.legend <- c("lnorm")
denscomp(list(fit_ln), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
cdfcomp (list(fit_ln), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
qqcomp  (list(fit_ln), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
ppcomp  (list(fit_ln), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
par(mfrow=c(2,2))
plot.legend <- c("norm")
denscomp(list(fit_nor), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
cdfcomp (list(fit_nor), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
qqcomp  (list(fit_nor), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
ppcomp  (list(fit_nor), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
par(mfrow=c(2,2))

plot.legend <- c("gama")
denscomp(list(fit_gama), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
cdfcomp (list(fit_gama), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
qqcomp  (list(fit_gama), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
ppcomp  (list(fit_ln), legendtext = plot.legend, xlab = 'Relative Humidity (RH)')
----------------------------------------------------------------------------------
# Mutiple line graph for Total Vehicle Reg.2018,2019,2020
library("ggplot2")
library("readxl")
popul.us <- read_excel("fdata.xlsx")
#head(popul.us)
#names(popul.us)
#dim(popul.us)
#
options(scipen = 100)
#ggplot(data=popul.us)+
# geom_line(aes(x=state,y=trans18, group=1, colour="red"))
ggplot(data=popul.us)+
  geom_line(aes(x=state,y=trans18),group=1, colour="red")+
  geom_line(aes(x=state,y=trans19),group=1, colour="blue")+
  geom_line(aes(x=state,y=trans20),group=1, colour="green")+
  xlab("State")+
  ylab("Total Vehicle Reg.2018,2019,2020")+
  ggtitle("Total Veh. Reg 2018 - 2020")  

---------------------------------------------------------------------------------
#line graph of Total pollution 2018-2020
  library("readxl")
popul.us <- read_excel("fdata.xlsx")
head(popul.us)
names(popul.us)
dim(popul.us)
options(scipen = 100)
ggplot(data=popul.us,aes(x=state) )+
  geom_line(aes(y=tot_pollution18,group=1), colour="red")+
  geom_line(aes(y=tot_pollution19,group=1), colour="blue")+
  geom_line(aes(y=tot_pollution20,group=1), colour="green")+
  ggtitle("Total Pollution 2018 - 2020") +
  xlab("State")+
  ylab("Total pollution µg/m³-2018,2019,2020")
  library("ggplot2")
library("readxl")
popul.us <- read_excel("fdata.xlsx")
head(popul.us)
names(popul.us)
dim(popul.us)
options(scipen = 100)
ggplot(data=popul.us,aes(x=state) )+
  geom_line(aes(y=tot_pollution18,group=1), colour="red")+
  geom_line(aes(y=tot_pollution19,group=1), colour="blue")+
  geom_line(aes(y=tot_pollution20,group=1), colour="green")+
  ggtitle("Total Pollution 2018 - 2020") +
  xlab("State")+
  ylab("Total pollution µg/m³-2018,2019,2020")
----------------------------------------------------------------
#finding the largest pollution state
library("readxl")
getwd()
poll.us <- read_excel("fdata.xlsx")
head(poll.us)
names(poll.us)
state<- poll.us$state
poll_18<- poll.us$tot_pollution18
poll_19<- poll.us$tot_pollution19
poll_20<- poll.us$tot_pollution20
state
library(ggplot2)
#largest pollution causing state 2018
# for ordered
ggplot()+
  geom_bar(aes(x= reorder(state,+poll_18), 
               y= poll_18),
           stat = "identity" )+
  ggtitle("Largest pollution state 2018")+
  xlab("")+ylab("pollution (µg/m³) in 2018")+
  coord_flip()
# largest pollution causing state 2019
# for ordered
ggplot()+
  geom_bar(aes(x= reorder(state,+poll_19), 
               y= poll_19),
           stat = "identity" ) +
  ggtitle("Largest pollution state 2019")+
  xlab("")+ylab("pollution (µg/m³)in 2019")+
  coord_flip()
# largest pollution causing state 2020
# for ordered
ggplot()+
  geom_bar(aes(x= reorder(state,+poll_20), 
               y= poll_20),
           stat = "identity" ) +
  ggtitle("Largest pollution state 2020")+
  xlab("")+ylab("pollution (µg/m³) in 2020")+
  coord_flip()
-----------------------------------------------------------------
  
# correlation veh vs pollution 18
install.packages("ggpubr")
library("ggpubr")
library("readxl")
popul.us <- read_excel("fdata.xlsx")
names(popul.us)
# veh vs pollution 18
ggscatter(popul.us, x = "trans18", y = "tot_pollution18", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "veh. reg 18", ylab = "Total pollution 18")
# veh vs pollution 19
ggscatter(popul.us, x = "trans19", y = "tot_pollution19", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "veh. reg 19", ylab = "Total pollution 19")
# veh vs pollution 20
ggscatter(popul.us, x = "trans20", y = "tot_pollution20", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Veh. reg 2020", ylab = "Total pollution 2020 µg/m³")+
  ggtitle("Correlation b/t Veh. reg 2020 and Pollution of 2020")
-------------------------------------------------------------------
# correlation
# population vs pollution
ggscatter(popul.us, x = "pop_18", y = "tot_pollution18", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "population 2018", ylab = "pollution 2018")

ggscatter(popul.us, x = "pop_19", y = "tot_pollution19", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Population 2019", ylab = "Pollution 19")
ggscatter(popul.us, x = "pop_20", y = "tot_pollution20", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Population 2020", ylab = "Pollution 2020 µg/m³")+
  ggtitle("Correlation b/t Population and Pollution of 2020")
--------------------------------------------------------------
# correlation bt area and pollution
library("ggpubr")
library("readxl")
popul.us <- read_excel("fdata.xlsx")
names(popul.us)
options(scipen = 100)
#ggplot(data=popul.us)+
# geom_line(aes(x=state,y=trans18, group=1, colour="red"))
ggplot(data=popul.us,aes(x=tot_pollution20) )+
  geom_line(aes(y=tot_pollution18,group=1), colour="red")+
  geom_line(aes(y=tot_pollution19,group=1), colour="blue")+
  geom_line(aes(y=tot_pollution20,group=1), colour="green")
ggscatter(popul.us, x = "area", y = "tot_pollution18", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "area", ylab = "pollution18")
ggscatter(popul.us, x = "area", y = "tot_pollution19", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Area", ylab = "Pollution 2019 µg/m³")+
  ggtitle("Correlation b/t Area and Pollution of 2019")
ggscatter(popul.us, x = "area", y = "tot_pollution20", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "area", ylab = "pollution20")
-----------------------------------------------------------------------
# pie 2018
  
  library(readxl)
library(dplyr)
pollutants18<-read_excel("pie.xlsx")
pollutants<-pollutants18$pollutants2018
percentage<-pollutants18$percentage2018
library(ggplot2)
head(pollutants18)
names(pollutants18)
p = ggplot() + theme_bw() + 
  geom_bar(aes(x = "", y = percentage, fill = pollutants), stat = "identity", color = "white") + 
  coord_polar("y", start = 0) + 
  ggtitle("Pollutants percentage in 2018") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        panel.border = element_blank())

print(p)

pollutants = pollutants[order(percentage)]
percentage = sort(percentage)
pollutants = factor(pollutants, levels = as.character(pollutants))

p0 = ggplot() + theme_bw() + 
  geom_bar(aes(x = "", y = percentage, fill = pollutants), 
           stat = "identity", color = "white") + 
  coord_polar("y", start = 0) + 
  ggtitle("Pollutants percentage in 2018") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        panel.border = element_blank())



ypos = cumsum(percentage) - 0.5 * percentage
ypos = 100 - ypos 

p = p0 + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme(legend.text=element_text(size=8),
        legend.title = element_text(hjust = 0.5, size=14),
        legend.key.size = unit(1,"cm")) 

print(p)
-------------------------------------------------------------------------------
# pie 2019
  
  library(readxl)
library(dplyr)
pollutants19<-read_excel("pie.xlsx")
pollutants<-pollutants19$pollutants2019
percentage<-pollutants19$percentage2019
library(ggplot2)
head(pollutants19)
names(pollutants19)
p = ggplot() + theme_bw() + 
  geom_bar(aes(x = "", y = percentage, fill = pollutants), stat = "identity", color = "white") + 
  coord_polar("y", start = 0) + 
  ggtitle("Pollutants percentage in 2019") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        panel.border = element_blank())

print(p)

pollutants = pollutants[order(percentage)]
percentage = sort(percentage)
pollutants = factor(pollutants, levels = as.character(pollutants))

p0 = ggplot() + theme_bw() + 
  geom_bar(aes(x = "", y = percentage, fill = pollutants), 
           stat = "identity", color = "white") + 
  coord_polar("y", start = 0) + 
  ggtitle("Pollutants percentage in 2019") + 
  theme(plot.title = element_text(hjust = 0.5, size = ),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid= element_blank(),
        panel.border = element_blank())



ypos = cumsum(percentage) - 0.5 * percentage
ypos = 100 - ypos 

p = p0 + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme(legend.text=element_text(size=8),
        legend.title = element_text(hjust = 0.5, size=14),
        legend.key.size = unit(1,"cm")) 

print(p)


-----------------------------------------------------------------
  #pie 2020
  
  library(readxl)
library(dplyr)
pollutants19<-read_excel("pie.xlsx")
pollutants<-pollutants19$pollutants2019
percentage<-pollutants19$percentage2019
library(ggplot2)
head(pollutants19)
names(pollutants19)
p = ggplot() + theme_bw() + 
  geom_bar(aes(x = "", y = percentage, fill = pollutants), stat = "identity", color = "white") + 
  coord_polar("y", start = 0) + 
  ggtitle("Pollutants percentage in 2019") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        panel.border = element_blank())

print(p)

pollutants = pollutants[order(percentage)]
percentage = sort(percentage)
pollutants = factor(pollutants, levels = as.character(pollutants))

p0 = ggplot() + theme_bw() + 
  geom_bar(aes(x = "", y = percentage, fill = pollutants), 
           stat = "identity", color = "white") + 
  coord_polar("y", start = 0) + 
  ggtitle("Pollutants percentage in 2019") + 
  theme(plot.title = element_text(hjust = 0.5, size = ),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid= element_blank(),
        panel.border = element_blank())



ypos = cumsum(percentage) - 0.5 * percentage
ypos = 100 - ypos 

p = p0 + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme(legend.text=element_text(size=8),
        legend.title = element_text(hjust = 0.5, size=14),
        legend.key.size = unit(1,"cm")) 

print(p)


#pollution difference 

library(readxl)
library(dplyr)
pollutants19<-read_excel("pie.xlsx")
pollutants<-pollutants19$pollutants2019
percentage<-pollutants19$percentage2019
library(ggplot2)
head(pollutants19)
names(pollutants19)
p = ggplot() + theme_bw() + 
  geom_bar(aes(x = "", y = percentage, fill = pollutants), stat = "identity", color = "white") + 
  coord_polar("y", start = 0) + 
  ggtitle("Pollutants percentage in 2019") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        panel.border = element_blank())

print(p)

pollutants = pollutants[order(percentage)]
percentage = sort(percentage)
pollutants = factor(pollutants, levels = as.character(pollutants))

p0 = ggplot() + theme_bw() + 
  geom_bar(aes(x = "", y = percentage, fill = pollutants), 
           stat = "identity", color = "white") + 
  coord_polar("y", start = 0) + 
  ggtitle("Pollutants percentage in 2019") + 
  theme(plot.title = element_text(hjust = 0.5, size = ),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid= element_blank(),
        panel.border = element_blank())



ypos = cumsum(percentage) - 0.5 * percentage
ypos = 100 - ypos 

p = p0 + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme(legend.text=element_text(size=8),
        legend.title = element_text(hjust = 0.5, size=14),
        legend.key.size = unit(1,"cm")) 

print(p)

-----------------------------------------------------------------------
# veh reg difference
  
  library(readxl)
library(dplyr)
D<-read_excel("pie.xlsx")
state<-D$state
diffrence<-D$Vehicle_Registrations_19_18
library(ggplot2)
color <- ifelse(D$Vehicle_Registrations_19_18 < 0, "pink", "lightgreen")
ggplot(D, aes(x = state, y = Vehicle_Registrations_19_18)) +
  geom_bar(stat = "identity",
           show.legend = FALSE,
           fill=color)+
  geom_text(aes(label = round(Vehicle_Registrations_19_18, 1),
                hjust = 0.5,
                vjust = ifelse(Vehicle_Registrations_19_18 < 0, 1.5, -1)),
            size = 2.5)

xlab("state") +
  ylab("Vehicle_Registrations_19_18")






  



