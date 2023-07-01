#########################################################
## ANOVA and Analysis for Stranded CoRiv Inverts Sample
## by Angelika Kurthen and Connor Eck
#########################################################

install.packages("ggplot2")
install.packages("ggpubr")


install.packages("ggsignif")
library(ggplot2)
library(ggpubr)
library(ggsignif)


# import data set
URSA <- as.data.frame(read.csv("~/Desktop/URSA.csv"))
View(URSA)

URSA <- URSA[1:15,]

# make sure the Sites and Samples are of class factors
URSA$Site <- as.factor(URSA$Site[1:15])
URSA$Subsample <- as.factor(URSA$Subsample)


#######################################################
# Calculating Mean and CI
#######################################################

# convert abundances to abundance per m^2
# the sample taken was 0.072967596 m^2
# if easier for you, this can be done in excel
URSA$Dreissena.bugensis <- URSA$Dreissena.bugensis/0.072967596
URSA$Potamopyrgus.antipodarym <- URSA$Potamopyrgus.antipodarym/0.072967596
URSA$Gammarus.lacustris <-URSA$Gammarus.lacustris/0.072967596
URSA$Chironomidae <- URSA$Chironomidae/0.072967596
URSA$Hydropsyche.oslari <-URSA$Hydropsyche.oslari/0.072967596

#Quagga Mussels per Square Meter
URSA$Dreissena.bugensis <- URSA$Dreissena.bugensis/0.072967596
mean(subset(URSA$Dreissena.bugensis, URSA$Site == "RM-15"))
mean(subset(URSA$Dreissena.bugensis, URSA$Site == "RM-4.5"))
mean(subset(URSA$Dreissena.bugensis, URSA$Site == "RM226"))

#Snails Per square meter
URSA$Potamopyrgus.antipodarym <- URSA$Potamopyrgus.antipodarym/0.072967596
mean(subset(URSA$Potamopyrgus.antipodarym, URSA$Site == "RM-15"))
mean(subset(URSA$Potamopyrgus.antipodarym, URSA$Site == "RM-4.5"))
mean(subset(URSA$Potamopyrgus.antipodarym, URSA$Site == "RM226"))

#Gammarus per square meter
URSA$Gammarus.lacustris <-URSA$Gammarus.lacustris/0.072967596
mean(subset(URSA$Gammarus.lacustris, URSA$Site == "RM-15"))
mean(subset(URSA$Gammarus.lacustris, URSA$Site == "RM-4.5"))
mean(subset(URSA$Gammarus.lacustris, URSA$Site == "RM226"))

#Chironomidae per square meter
URSA$Chironomidae <- URSA$Chironomidae/0.072967596
mean(subset(URSA$Chironomidae, URSA$Site == "RM-15"))
mean(subset(URSA$Chironomidae, URSA$Site == "RM-4.5"))
mean(subset(URSA$Chironomidae, URSA$Site == "RM226"))

#Hydropsyche per square meter
URSA$Hydropsyche.oslari <-URSA$Hydropsyche.oslari/0.072967596
mean(subset(insect$Hydropsyche.oslari, URSA$Site == "RM-15"))
mean(subset(URSA$Hydropsyche.oslari, URSA$Site == "RM-4.5"))
mean(subset(URSA$Hydropsyche.oslari, URSA$Site == "RM226"))

#Quagga Mussels per Square Meter Confidence Interval

# calculate 95% CI for each site-speices combo using SE = 1.96 * standard dev / sqrt(N)
#x-bar (sample mean) +- (t* whihc is the qt value)*SE)
1.96*sd(subset(ExampleData$Quagga.Mussels, ExampleData$Site == "1"))/sqrt(5) # you will want to change N to 5 

#Confidence Interval for Quagga Mussels at RM-15
1.96*sd(subset(URSA$Dreissena.bugensis, URSA$Site == "RM-15"))/sqrt(5)
#RM -15 Mean:55031.02
qt(0.95,4)
#Upper Bound
55031.02+(2.131847*7123.967)
#Lower Bound
55031.02-(2.131847*7123.967)

#Confidence Interval for Quagga Mussels at RM-4.5
1.96*sd(subset(URSA$Dreissena.bugensis, URSA$Site == "RM-4.5"))/sqrt(5)
#RM -15 Mean:1540.117
#RM-15 SE:586.6956
qt(0.95,4)
#Upper Bound
1540.117+(2.131847*586.6956)
#Lower Bound
1540.117-(2.131847*586.6956)


#Confidence Interval for Quagga Mussels at RM226
#RM226 Mean:0
#RM226 SE: 0
1.96*sd(subset(URSA$Dreissena.bugensis, URSA$Site == "RM226"))/sqrt(5)
#Upper Bound
0+(2.131847*0)
#Lower Bound
0-(2.131847*0)
#Snails per Square Meter Confidence Interval
#Confidence Interval for Snails at RM-15
1.96*sd(subset(URSA$Potamopyrgus.antipodarym, URSA$Site == "RM-15"))/sqrt(5)
#RM -15 Mean:1112.823
qt(0.95,4)
#Upper Bound
1112.823+(2.131847*22.29064)
#Lower Bound
1112.823-(2.131847*22.29064)

#Confidence Interval for Snails at RM-4.5
1.96*sd(subset(URSA$Potamopyrgus.antipodarym, URSA$Site == "RM-4.5"))/sqrt(5)
#RM -15 Mean:3226.09
qt(0.95,4)
#Upper Bound
3226.09+(2.131847*22.29064)
#Lower Bound
3226.09-(2.131847*22.29064)

#Confidence Interval for Snails at RM226
1.96*sd(subset(URSA$Potamopyrgus.antipodarym, URSA$Site == "RM226"))/sqrt(5)
#RM -15 Mean:104.1558
qt(0.95,4)
#Upper Bound
104.1558+(2.131847*22.29064)
#Lower Bound
104.1558-(2.131847*22.29064)

#Gamarus per Square Meter Confidence Interval
#Confidence Interval for Gammarus at RM-15
1.96*sd(subset(URSA$Gammarus.lacustris, URSA$Site == "RM-15"))/sqrt(5)
#RM -15 Mean:10.96377
qt(0.95,4)
#Upper Bound
10.96377+(2.131847*15.66266)
#Lower Bound
10.96377-(2.131847*15.66266)

#Confidence Interval for Gammarus at RM-4.5
1.96*sd(subset(URSA$Gammarus.lacustris, URSA$Site == "RM-4.5"))/sqrt(5)
#RM -15 Mean:150.7518
qt(0.95,4)
#Upper Bound
150.7518+(2.131847*112.3686)
#Lower Bound
150.7518-(2.131847*10.7445)

#Confidence Interval for Gammarus at RM226
1.96*sd(subset(URSA$Gammarus.lacustris, URSA$Site == "RM226"))/sqrt(5)
#RM -15 Mean:10.96377
qt(0.95,4)
#Upper Bound
8.222828+(2.131847*10.7445)
#Lower Bound
8.222828-(2.131847*10.7445)

#Chironomidae per Square Meter Confidence Interval
#Confidence Interval for Chironomidae at RM-15
1.96*sd(subset(URSA$Chironomidae, URSA$Site == "RM-15"))/sqrt(5)
#RM -15 Mean:1014.224
qt(0.95,4)
#Upper Bound
1014.224+(2.131847*643.9568)
#Lower Bound
1014.224-(2.131847*643.9568)

#Confidence Interval for Chironomidae at RM-4.5
1.96*sd(subset(URSA$Chironomidae, URSA$Site == "RM-4.5"))/sqrt(5)
#RM -15 Mean:3305.617
qt(0.95,4)
#Upper Bound
3305.617+(2.131847*2748.89)
#Lower Bound
3305.617-(2.131847*2748.89)

#Confidence Interval for Chironomidae at RM226
1.96*sd(subset(URSA$Chironomidae, URSA$Site == "RM226"))/sqrt(5)
#RM -15 Mean:17392.06
qt(0.95,4)
#Upper Bound
17392.06+(2.131847* 9521.296)
#Lower Bound
17392.06-(2.131847* 9521.296)

#Hydropsyche Oslari per Square Meter Confidence Interval

#Confidence Interval for Hydropsyche Oslari at RM-15
1.96*sd(subset(URSA$Chironomidae, URSA$Site == "RM-15"))/sqrt(5)
#RM -15 Mean:0
qt(0.95,4)
#Upper Bound
0+(2.131847* 0)
#Lower Bound
0-(2.131847* 0)

#Confidence Interval for Hydropsyche Oslari at RM-4.5
1.96*sd(subset(URSA$Chironomidae, URSA$Site == "4.5"))/sqrt(5)
#RM -15 Mean:0
qt(0.95,4)
#Upper Bound
0+(2.131847* 0)
#Lower Bound
0-(2.131847* 0)

#Confidence Interval for Hydropsyche Oslari at RM226
1.96*sd(subset(URSA$Chironomidae, URSA$Site == "RM226"))/sqrt(5)
#RM -15 Mean:14574.77
qt(0.95,4)
#Upper Bound
14574.77+(2.131847* 9521.296)
#Lower Bound
14574.77-(2.131847* 9521.296)
#####################################################
# Testing for a Normal Distribution
#####################################################

# we can test normality for each site and species with a shaprio test 
# if p-value is great thatn 0.05, data is considered normal and we can conduct an ANOVA
# if no, we have to conduct a Kruskal-Wallis test
shapiro.test(subset(URSA$Quagga.Mussels, ExampleData$Site == "1"))

#Normality Test for Quagga Mussels
shapiro.test(subset(URSA$Dreissena.bugensis, URSA$Site == "RM-15"))
#W = 0.90808, p-value = 0.4561
shapiro.test(subset(URSA$Dreissena.bugensis, URSA$Site == "RM-4.5"))
#W = 0.83782, p-value = 0.159
shapiro.test(subset(URSA$Dreissena.bugensis, URSA$Site == "RM226"))
#Identical x values
#Normality Test for Snails
shapiro.test(subset(URSA$Potamopyrgus.antipodarym, URSA$Site == "RM-15"))
W = 0.84066, p-value = 0.1668
shapiro.test(subset(URSA$Potamopyrgus.antipodarym, URSA$Site == "RM-4.5"))
W = 0.97464, p-value = 0.9041
shapiro.test(subset(URSA$Potamopyrgus.antipodarym, URSA$Site == "RM226"))
W = 0.91251, p-value = 0.4828

#Normality Test for Gammarus
shapiro.test(subset(URSA$Gammarus.lacustris, URSA$Site == "RM-15"))
W = 0.73477, p-value = 0.02138 #KRUSKAL
shapiro.test(subset(URSA$Gammarus.lacustris, URSA$Site == "RM-4.5"))
W = 0.94464, p-value = 0.6989
shapiro.test(subset(URSA$Gammarus.lacustris, URSA$Site == "RM226"))
W = 0.77091, p-value = 0.04595 #KRUSKAL

#Normality Test for Chironomidae
shapiro.test(subset(URSA$Chironomidae, URSA$Site == "RM-15"))
W = 0.932, p-value = 0.6101
shapiro.test(subset(URSA$Chironomidae, URSA$Site == "RM-4.5"))
W = 0.83778, p-value = 0.1589
shapiro.test(subset(URSA$Chironomidae, URSA$Site == "RM226"))
W = 0.88968, p-value = 0.3555

#Normaility Test for Hydropsyche Oslari
shapiro.test(subset(URSA$Hydropsyche.oslari, URSA$Site == "RM-15"))
#identical x-values
shapiro.test(subset(URSA$Hydropsyche.oslari, URSA$Site == "RM-4.5"))
#identical x-values
shapiro.test(subset(URSA$Hydropsyche.oslari, URSA$Site == "RM226"))
W = 0.91218, p-value = 0.4808
#####################################################
# Conducting an ANOVA (or Kruskal-Wallis) Test
#####################################################
# Analysis of Variance (anova)
aov1 <- aov(ExampleData$Quagga.Mussels~ExampleData$Site)
summary(aov1)
TukeyHSD(aov1)

# Analysis of Variance (anova) for Quagga Mussels
aov1 <- aov(URSA$Dreissena.bugensis~URSA$Site)
summary(aov1)
TukeyHSD(aov1)
# Analysis of Variance (anova) for Snails
aov1 <- aov(URSA$Potamopyrgus.antipodarym~URSA$Site)
summary(aov1)
TukeyHSD(aov1)

# Analysis of Variance (anova) for Chironomidae
aov1 <- aov(URSA$Chironomidae~URSA$Site)
summary(aov1)
TukeyHSD(aov1)

# Analysis of Variance (anova) for Hydropsyche Oslari
aov1 <- aov(URSA$Hydropsyche.oslari~URSA$Site)
summary(aov1)
TukeyHSD(aov1)

# Kruskal-Wallis Test
kruskal.test(ExampleData$Quagga.Mussels~ExampleData$Site)

# Kruskal-Wallis Test for Gammarus
kruskal.test(URSA$Gammarus.lacustris~URSA$Site)

#####################################################
# Graphically Representing Data 
#####################################################
# Visualize: 
ggplot(ExampleData, aes(x=Site, y=Quagga.Mussels, color = Site)) + # specify the x and y you want
  geom_boxplot() +
  stat_compare_means(method = "anova")+ # specify the test, either "anova" or "kruskal"
  ggtitle("Quagga Mussels") + # add a title
  xlab("Site") + ylab("Abundance per m^2")

#Graphs of Data
# Graph of ANOVA test for Quagga Mussels
ggplot(URSA, aes(x=Site, y=Dreissena.bugensis, color = Site)) + # specify the x and y you want
  geom_boxplot() +
  stat_compare_means(method = "anova")+ # specify the test, either "anova" or "kruskal"
  ggtitle("Abundance of Dreissena bugensis") + # add a title
  xlab("Site") + ylab("Abundance per m^2")


# Graph of KRUSKAL test for Gammarus
ggplot(URSA, aes(x=Site, y=Gammarus.lacustris, color = Site)) + # specify the x and y you want
  geom_boxplot() +
  stat_compare_means(method = "kruskal")+ # specify the test, either "anova" or "kruskal"
  ggtitle("Abundance of Gammarus lacustris") + # add a title
  xlab("Site") + ylab("Abundance per m^2")

# Graph of ANOVA test for Chironomidae
ggplot(URSA, aes(x=Site, y=Chironomidae, color = Site)) + # specify the x and y you want
  geom_boxplot() +
  stat_compare_means(method = "anova")+ # specify the test, either "anova" or "kruskal"
  ggtitle("Abundance of Chironomidae") + # add a title
  xlab("Site") + ylab("Abundance per m^2")

# Graph of ANOVA test for Hydropsyche Oslari
ggplot(URSA, aes(x=Site, y=Hydropsyche.oslari, color = Site)) + # specify the x and y you want
  geom_boxplot() +
  stat_compare_means(method = "anova")+ # specify the test, either "anova" or "kruskal"
  ggtitle("Abundance of Hydropsyche oslari") + # add a title
  xlab("Site") + ylab("Abundance per m^2")


