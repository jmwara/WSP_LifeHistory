#load in libraries
library(ggplot2)
library(car)
library(emmeans)
library(readxl)
library(dplyr)
library(data.table)
library(lme4)
library(MuMIn)
library(vegan)
library(tidyr)
library(ade4)
library(gridExtra)
library(MuMIn)
library(ggmap)
library(grid)

setwd("/Volumes/jmwara_NDSU/WhiteSandsPupfish_LifeHistory/")
setwd("YOUR_DIRECTORY_HERE") #fill in this spot with the folder where you stored files from the repository

#Import data 
Rogowski_PhD_data2 <- read_excel("Rogowski_PhD_data2.xls", range = "A1:AX828") #read in excel sheet, ignoring any extraneous notes
Rogowski_PhD_data2$`egg DW` <- ifelse(is.na(Rogowski_PhD_data2$`egg DW`)==T, 0, Rogowski_PhD_data2$`egg DW`) #replace missing values in the "egg dry weight" column with 0 (most missing values here are male fish)
Rogowski_PhD_data2$`egg DW` <- as.numeric(Rogowski_PhD_data2$`egg DW`) #convert the "egg dry weight" column to numeric class
Rogowski_PhD_data2$`dwt rep` <- as.numeric(Rogowski_PhD_data2$`dwt rep`) #convert the "reproductive dry weight column to numeric class
WSP_data <- data.frame(ID=Rogowski_PhD_data2$ID, Site=Rogowski_PhD_data2$Site...5, System=Rogowski_PhD_data2$System, Sex=Rogowski_PhD_data2$sex, Age=Rogowski_PhD_data2$age, Length=Rogowski_PhD_data2$length, Wet_Wt=as.numeric(Rogowski_PhD_data2$`wet weight`), Dry_Wt=as.numeric(Rogowski_PhD_data2$`dry weight`), Fat=as.numeric(Rogowski_PhD_data2$`fat gr`), Water=as.numeric(Rogowski_PhD_data2$water), Rep_Alloc=rowSums(Rogowski_PhD_data2[,c(38,44)])) #make a data frame with body composition variables
#pruning fish with data errors/missing data
WSP_data <- WSP_data[-375,] #one fish missing wet weight datum
WSP_data <- WSP_data[-386,] #one fish missing dry weight datum
WSP_data <- WSP_data[-c(12, 357, 394, 622, 634, 689, 701),] #remove seven fish with missing or negative fat content values
WSP_data <- WSP_data[-c(141, 230, 497),] #removing fish with missing or incorrectly entered sex data

#Age Distributions 
age_by_sex <- ggplot(data=WSP_data, aes(x=Age, fill=Sex, alpha=0.5))+
  geom_density(bw=0.5)
age_by_sex #plot of age split by the sexes
#Most males Age-0, age-1 is plurality of females
age_by_site <- ggplot(data=WSP_data, aes(x=Age, fill=System, alpha=0.5))+
  geom_density(bw=0.5)
age_by_site #plot of ages split by systems
#Lost River has youngest fish (mostly age-0), other pops mostly age-1, most older fish found in Malpais system

sex_by_site <- ggplot(data=WSP_data, aes(x=System, y=Sex))+
  geom_count()
sex_by_site #count of number of individuals of each sex sampled per system
#sampled sex ratios consistent across systems

WSP_data %>% count(Sex, System) #count the number of fish assigned to each sex for each system separately

length_at_age <- ggplot(data=WSP_data, aes(x=Age, y=Length, color=Sex))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm")+
  theme_bw()
length_at_age #plot the length at age for each sex across all systems

#Length-Weight Relationships
WSP_data_females <- WSP_data[WSP_data$Sex=="f",] #pull out all females samples from dataset
length_wetwt_eq.lm <- lm(log10(Wet_Wt) ~ log10(Length), data=WSP_data_females) #calculate length-weight power function for all fish
length_wetwt_eq.lm #print results of the length-weight power function

length_wetwt_eq_by_sys_test.lm <- lm(log10(Wet_Wt)~(log10(Length))*System, data=WSP_data_females) #anova to see how system affects slope of length-weight relationship
Anova(length_wetwt_eq_by_sys_test.lm, type="III") #test for significant differences in system
length_wetwt_eq_by_sys_ancova_results <- emtrends(length_wetwt_eq_by_sys_test.lm, "System", var="log10(Length)") #Get slope estimates for each system in previous ANCOVA model 
pairs(length_wetwt_eq_by_sys_ancova_results) #pairwise post hoc comparisons of slope in length-weight power functions for each system, Lost River significantly lower slope than other systems

length_wetwt_by_sys_plot <- ggplot(data=WSP_data_females, aes(x=log(Length), y=log(Wet_Wt), fill=System, color=System))+ #ggplot to show log length vs. log wet wt, colored by system
  geom_point(alpha=0.5)+ #add semi-transparent points to plot
  scale_color_hue(labels=c("Malpais", "Lost River", "Mound", "Salt Creek"))+ #edit labels for colors
  scale_fill_hue(labels=c("Malpais", "Lost River", "Mound", "Salt Creek"))+ #edit labels for colors (fill)
  geom_smooth(method="lm", se=F)+ #add lines of best fit from linear models 
  theme_classic() #change plot theme
length_wetwt_by_sys_plot #plot showing slopes of linearized length-weight power function for each system

#Temp Variables
pf_temp <- read_excel("Rogowski_PhD_data2.xls", sheet=7) #read in data from temperature loggers
pf_temp$Date.Time <- as.POSIXct(paste(pf_temp$Date...1, pf_temp$Time), format="%Y-%m-%d %H:%M:%S ") #create a data column with both date and time
pf_temp <- pf_temp[1:2425,] #remove bottom row of data which contains totals 
pf_temp$Logs <- rowSums(!is.na(pf_temp[,4:12])) #create column with number of data records for a given data & time (columns 4-12 are temperatures recorded in each site)
pf_temp_cleaned <- pf_temp[pf_temp$Logs==9, ] #select dates and times where all sites have a recorded data point
#make sure all temperature columns are in the correct data format
pf_temp_cleaned$scup <- as.numeric(pf_temp_cleaned$scup) #Salt Creek Upper
pf_temp_cleaned$SCm <- as.numeric(pf_temp_cleaned$SCm) #Salt Creek Middle
pf_temp_cleaned$Sclow <- as.numeric(pf_temp_cleaned$Sclow) #Salt Creek Lower
pf_temp_cleaned$Lrup <- as.numeric(pf_temp_cleaned$Lrup) #Lost River Upper
pf_temp_cleaned$lrmid <- as.numeric(pf_temp_cleaned$lrmid) #Lost River Middle
pf_temp_cleaned$Lrlow <- as.numeric(pf_temp_cleaned$Lrlow) #Lost River Lower
pf_temp_cleaned$`mal spring` <- as.numeric(pf_temp_cleaned$`mal spring`) #Malpais Spring
pf_temp_cleaned$`mal marsh` <- as.numeric(pf_temp_cleaned$`mal marsh`) #Malpais Marsh
pf_temp_cleaned$Moundlow <- as.numeric(pf_temp_cleaned$Moundlow) #Mound Lower

pf_temp_daily <- data.frame(Date=unique(pf_temp_cleaned$Date...1)) #create new data frame with one row for each date 
#add maximum temperature recorded for each date to the daily data frame (done separately for each site)
pf_temp_daily$SCup.Max <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(max=max(scup)))$max)
pf_temp_daily$SCmid.Max <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(max=max(SCm)))$max)
pf_temp_daily$SClow.Max <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(max=max(Sclow)))$max)
pf_temp_daily$LRup.Max <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(max=max(Lrup)))$max)
pf_temp_daily$LRmid.Max <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(max=max(lrmid)))$max)
pf_temp_daily$LRlow.Max <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(max=max(Lrlow)))$max)
pf_temp_daily$MalSpring.Max <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(max=max(`mal spring`)))$max)
pf_temp_daily$MalMarsh.Max <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(max=max(`mal marsh`)))$max)
pf_temp_daily$Mound.Max <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(max=max(Moundlow)))$max)

#add minimum temperature recorded for each date to the daily data frame (done separately for each site)
pf_temp_daily$SCup.Min <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(min=min(scup)))$min)
pf_temp_daily$SCmid.Min <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(min=min(SCm)))$min)
pf_temp_daily$SClow.Min <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(min=min(Sclow)))$min)
pf_temp_daily$LRup.Min <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(min=min(Lrup)))$min)
pf_temp_daily$LRmid.Min <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(min=min(lrmid)))$min)
pf_temp_daily$LRlow.Min <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(min=min(Lrlow)))$min)
pf_temp_daily$MalSpring.Min <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(min=min(`mal spring`)))$min)
pf_temp_daily$MalMarsh.Min <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(min=min(`mal marsh`)))$min)
pf_temp_daily$Mound.Min <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(min=min(Moundlow)))$min)

#add mean temperature recorded for each date to the daily data frame (done separately for each site)
pf_temp_daily$SCup.Mean <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(mean=mean(scup)))$mean)
pf_temp_daily$SCmid.Mean <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(mean=mean(SCm)))$mean)
pf_temp_daily$SClow.Mean <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(mean=mean(Sclow)))$mean)
pf_temp_daily$LRup.Mean <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(mean=mean(Lrup)))$mean)
pf_temp_daily$LRmid.Mean <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(mean=mean(lrmid)))$mean)
pf_temp_daily$LRlow.Mean <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(mean=mean(Lrlow)))$mean)
pf_temp_daily$MalSpring.Mean <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(mean=mean(`mal spring`)))$mean)
pf_temp_daily$MalMarsh.Mean <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(mean=mean(`mal marsh`)))$mean)
pf_temp_daily$Mound.Mean <- as.numeric((pf_temp_cleaned %>% group_by(Date...1) %>% summarise(mean=mean(Moundlow)))$mean)

#add columns to the daily data frame with the range of temperatures recorded each day (maximum-minimum temperatures, separately for each site)
pf_temp_daily$SCup.Var <- pf_temp_daily$SCup.Max - pf_temp_daily$SCup.Min
pf_temp_daily$SCmid.Var <- pf_temp_daily$SCmid.Max - pf_temp_daily$SCmid.Min
pf_temp_daily$SClow.Var <- pf_temp_daily$SClow.Max - pf_temp_daily$SClow.Min
pf_temp_daily$LRup.Var <- pf_temp_daily$LRup.Max - pf_temp_daily$LRup.Min
pf_temp_daily$LRmid.Var <- pf_temp_daily$LRmid.Max - pf_temp_daily$LRmid.Min
pf_temp_daily$LRlow.Var <- pf_temp_daily$LRlow.Max - pf_temp_daily$LRlow.Min
pf_temp_daily$MalSpring.Var <- pf_temp_daily$MalSpring.Max - pf_temp_daily$MalSpring.Min
pf_temp_daily$MalMarsh.Var <- pf_temp_daily$MalMarsh.Max - pf_temp_daily$MalMarsh.Min
pf_temp_daily$Mound.Var <- pf_temp_daily$Mound.Max - pf_temp_daily$Mound.Min

#plots of temperature data 
pf_temp_daily.Max <- reshape2::melt(pf_temp_daily, id.vars="Date", measure.vars=2:10, variable.name="Site", value.name="Daily.Max.Temp") #create a reorganized temperature data frame in long format to plot maximum daily temperatures in each site
dailytempMax.plot <- ggplot(data=pf_temp_daily.Max, aes(x=Date, y=Daily.Max.Temp, color=Site, linetype=Site))+ #plot lines of max temperature in each study site
  geom_smooth()+ #draw smoothed splines connecting daily temperature points
  scale_color_manual(values=c("blue", "blue", "blue", "red", "red", "red", "green", "green", "purple"), labels=c("Salt Creek - Upper", "Salt Creek - Middle", "Salt Creek - Lower", "Lost River - Upper", "Lost River - Middle", "Lost River - Lower", "Malpais Spring - Springhead", "Maplais Spring - Marsh", "Mound Spring"))+ #color each study site based on the study system
  scale_linetype_manual(values=c(1,2,3,1,2,3,1,2,1), labels=c("Salt Creek - Upper", "Salt Creek - Middle", "Salt Creek - Lower", "Lost River - Upper", "Lost River - Middle", "Lost River - Lower", "Malpais Spring - Springhead", "Maplais Spring - Marsh", "Mound Spring"))+ #edit linetypes so each site within a system has a different style
  geom_hline(yintercept = 0)+ #add a line along the x-axis to make it more prominent
  labs(y="Maximum Daily Temperature (ºC)")+
  theme_bw() #change the plot theme

#create a function that will extract a legend graphic that can be added as a grob to a multiplot
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
dailytemp.legend <- get_legend(dailytempMax.plot) #copy the legend from the maximum temperature plot, all the plots will use the same legend, this makes it easier to plot as a grob

dailytempMax.plot <- ggplot(data=pf_temp_daily.Max, aes(x=Date, y=Daily.Max.Temp, color=Site, linetype=Site))+ #plot lines of max temperature in each study site
  geom_smooth()+ #draw smoothed splines connecting daily temperature points
  scale_color_manual(values=c("blue", "blue", "blue", "red", "red", "red", "green", "green", "purple"), labels=c("Salt Creek - Upper", "Salt Creek - Middle", "Salt Creek - Lower", "Lost River - Upper", "Lost River - Middle", "Lost River - Lower", "Malpais Spring - Springhead", "Maplais Spring - Marsh", "Mound Spring"))+ #color each study site based on the study system
  scale_linetype_manual(values=c(1,2,3,1,2,3,1,2,1), labels=c("Salt Creek - Upper", "Salt Creek - Middle", "Salt Creek - Lower", "Lost River - Upper", "Lost River - Middle", "Lost River - Lower", "Malpais Spring - Springhead", "Maplais Spring - Marsh", "Mound Spring"))+ #edit linetypes so each site within a system has a different style
  geom_hline(yintercept = 0)+ #add a line along the x-axis to make it more prominent
  labs(y="Maximum Daily Temperature (ºC)")+ #add y-axis label
  theme_bw()+ #change the plot theme
  theme(legend.position = "none") #remove legend

pf_temp_daily.Min <- reshape2::melt(pf_temp_daily, id.vars="Date", measure.vars=11:19, variable.name="Site", value.name="Daily.Min.Temp") #create a reorganized temperature data frame in long format to plot minimum daily temperatures in each site
dailytempMin.plot <- ggplot(data=pf_temp_daily.Min, aes(x=Date, y=Daily.Min.Temp, color=Site, linetype=Site))+ #plot lines of min temperature in each study site
  geom_smooth()+ #draw smoothed splines connecting daily temperature points
  scale_color_manual(values=c("blue", "blue", "blue", "red", "red", "red", "green", "green", "purple"), labels=c("Salt Creek - Upper", "Salt Creek - Middle", "Salt Creek - Lower", "Lost River - Upper", "Lost River - Middle", "Lost River - Lower", "Malpais Spring - Springhead", "Maplais Spring - Marsh", "Mound Spring"))+ #color each study site based on the study system
  scale_linetype_manual(values=c(1,2,3,1,2,3,1,2,1), labels=c("Salt Creek - Upper", "Salt Creek - Middle", "Salt Creek - Lower", "Lost River - Upper", "Lost River - Middle", "Lost River - Lower", "Malpais Spring - Springhead", "Maplais Spring - Marsh", "Mound Spring"))+ #edit linetypes so each site within a system has a different style
  geom_hline(yintercept = 0)+ #add a line along the x-axis to make it more prominent
  labs(y="Minimum Daily Temperature (ºC)")+ #add y-axis label
  theme_bw()+ #change the plot theme
  theme(legend.position = "none") #remove legend

pf_temp_daily.Mean <- reshape2::melt(pf_temp_daily, id.vars="Date", measure.vars=20:28, variable.name="Site", value.name="Daily.Mean.Temp") #create a reorganized temperature data frame in long format to plot average daily temperature in each sample site
dailytempMean.plot <- ggplot(data=pf_temp_daily.Mean, aes(x=Date, y=Daily.Mean.Temp, color=Site, linetype=Site))+ #plot lines of average temperature in each sample site
  geom_smooth()+ #draw smoothed splines connecting daily temperature points
  scale_color_manual(values=c("blue", "blue", "blue", "red", "red", "red", "green", "green", "purple"), labels=c("Salt Creek - Upper", "Salt Creek - Middle", "Salt Creek - Lower", "Lost River - Upper", "Lost River - Middle", "Lost River - Lower", "Malpais Spring - Springhead", "Maplais Spring - Marsh", "Mound Spring"))+ #color each study site based on the study system
  scale_linetype_manual(values=c(1,2,3,1,2,3,1,2,1), labels=c("Salt Creek - Upper", "Salt Creek - Middle", "Salt Creek - Lower", "Lost River - Upper", "Lost River - Middle", "Lost River - Lower", "Malpais Spring - Springhead", "Maplais Spring - Marsh", "Mound Spring"))+ #edit linetypes so each site within a system has a different style
  geom_hline(yintercept = 0)+ #add a line along the x-axis to make it more prominent
  labs(y="Mean Daily Temperature (ºC)")+ #add y-axis label
  theme_bw()+ #change the plot theme
  theme(legend.position = "none") #remove legend

pf_temp_daily.Var <- reshape2::melt(pf_temp_daily, id.vars="Date", measure.vars=29:37, variable.name="Site", value.name="Variance in Daily Temperature")
dailytempVar.plot <- ggplot(data=pf_temp_daily.Var, aes(x=Date, y=`Variance in Daily Temperature`, color=Site, linetype=Site))+ #plot lines of daily temperature range in each sample site
  geom_smooth()+ #draw smoothed splines connecting daily temperature points
  scale_color_manual(values=c("blue", "blue", "blue", "red", "red", "red", "green", "green", "purple"), labels=c("Salt Creek - Upper", "Salt Creek - Middle", "Salt Creek - Lower", "Lost River - Upper", "Lost River - Middle", "Lost River - Lower", "Malpais Spring - Springhead", "Maplais Spring - Marsh", "Mound Spring"))+ #color each study site based on the study system
  scale_linetype_manual(values=c(1,2,3,1,2,3,1,2,1), labels=c("Salt Creek - Upper", "Salt Creek - Middle", "Salt Creek - Lower", "Lost River - Upper", "Lost River - Middle", "Lost River - Lower", "Malpais Spring - Springhead", "Maplais Spring - Marsh", "Mound Spring"))+ #edit linetypes so each site within a system has a different style
  geom_hline(yintercept = 0)+ #add a line along the x-axis to make it more prominent
  labs(y="Range in Daily Temperature (ºC)")+ #add a y-axis label
  theme_bw()+ #change the plot theme
  theme(legend.position = "none") #remove legend

DailyTemp_plots <- grid.arrange(dailytempMax.plot, dailytempMin.plot, dailytemp.legend, dailytempMean.plot, dailytempVar.plot, ncol=3, layout_matrix=cbind(c(1,4), c(2,5), c(3,3)), widths=c(1.8, 1.8, 1)) #plot multiple plots together, with a shared legend
ggsave(filename="Figure_S1.tiff", plot=DailyTemp_plots, path="~/Desktop/WhiteSandsPupfish_LifeHistory/WSPupfish_LifeHistory/", device="tiff", width=9, height=6, units="in", dpi=300) #save a copy of this plot (Suppl Fig 1)

#Parasite analyses
WSP_data <- data.frame(ID=Rogowski_PhD_data2$ID, Site=Rogowski_PhD_data2$Site...5, System=Rogowski_PhD_data2$System, Sex=Rogowski_PhD_data2$sex, Age=Rogowski_PhD_data2$age, Length=Rogowski_PhD_data2$length, Wet_Wt=as.numeric(Rogowski_PhD_data2$`wet weight`), Dry_Wt=as.numeric(Rogowski_PhD_data2$`dry weight`), Fat=as.numeric(Rogowski_PhD_data2$`fat gr`), Water=as.numeric(Rogowski_PhD_data2$water), Rep_Alloc=rowSums(Rogowski_PhD_data2[,c(38,44)], na.rm = T), Eye_Parasites=rowSums(cbind(as.numeric(Rogowski_PhD_data2$`l eye`), as.numeric(Rogowski_PhD_data2$`r eye`))), Gill_Parasites=rowSums(cbind(as.numeric(Rogowski_PhD_data2$`right gill 1`), as.numeric(Rogowski_PhD_data2$`rg 2`), as.numeric(Rogowski_PhD_data2$`rg 3`), as.numeric(Rogowski_PhD_data2$`rg 4`), as.numeric(Rogowski_PhD_data2$`lg 1`), as.numeric(Rogowski_PhD_data2$`lg 2`), as.numeric(Rogowski_PhD_data2$`lg 3`), as.numeric(Rogowski_PhD_data2$`lg 4`))), Liver_Parasites=as.numeric(Rogowski_PhD_data2$liver), Mesen_Parasites=as.numeric(Rogowski_PhD_data2$`body cavity`)) #make a data frame with body composition variables and parasite counts
#pruning fish with data errors/missing data
WSP_data <- WSP_data[-375,] #one fish missing wet weight datum
WSP_data <- WSP_data[-386,] #one fish missing dry weight datum
WSP_data <- WSP_data[-c(12, 357, 394, 622, 634, 689, 701),] #remove three fish with missing or negative fat content values
WSP_data <- WSP_data[-c(141, 230, 497),] #removing fish with missing or incorrectly entered sex data

#Check distributions of parasite variables, all are highly zero-inflated
hist(WSP_data$Eye_Parasites) 
hist(WSP_data$Gill_Parasites)
hist(WSP_data$Liver_Parasites)
hist(WSP_data$Mesen_Parasites)

WSP_data$Fat_Prop <- as.numeric(WSP_data$Fat/WSP_data$Dry_Wt) #calculate fat as a proportion of total dry weight
WSP_data$Dry_Wt_Prop <- (WSP_data$Dry_Wt/WSP_data$Wet_Wt) #calculate dry weight as a proportion of wet weight

#RDA
WSP_fishdata <- data.frame(Sex=WSP_data$Sex, System=WSP_data$System, Site=WSP_data$Site, Age=WSP_data$Age, Length=WSP_data$Length, Wet_Wt=WSP_data$Wet_Wt, Dry_Wt_Prop=WSP_data$Dry_Wt/WSP_data$Wet_Wt, Fat=WSP_data$Fat/WSP_data$Dry_Wt, GSI=WSP_data$Rep_Alloc/WSP_data$Wet_Wt, Cond.Factor=(100*WSP_data$Wet_Wt)/((WSP_data$Length/10)^3)) #create a data frame with all of the life history traits to be used in redundancy analysis
WSP_parasitedata <- data.frame(Eye_Parasites=WSP_data$Eye_Parasites, Gill_Parasites=WSP_data$Gill_Parasites, Liver_Parasites=WSP_data$Liver_Parasites, Mesen_Parasites=WSP_data$Mesen_Parasites) #create a data frame with the parasite counts to be used in redundancy analysis
WSP_parasitedata <- WSP_parasitedata[WSP_data$Sex=="f", ] #select only female samples for the parasite data frame
WSP_fishdata <- WSP_fishdata[WSP_data$Sex=="f", ] #select only female samples for the life history trait data frame
WSP_parasitedata <- WSP_parasitedata[is.na(WSP_fishdata$GSI)==F, ] #remove fish with NAs for GSI from the parasite data frame
WSP_fishdata <- WSP_fishdata[is.na(WSP_fishdata$GSI)==F, ] #remove fish with NAs for GSI from the life history trait data frame
WSP_parasitedata$Liver_Parasites <- replace_na(WSP_parasitedata$Liver_Parasites, 0) #replace missing values in the parasite counts with 0
WSP_parasitedata$Eye_Parasites <- replace_na(WSP_parasitedata$Eye_Parasites, 0) #replace missing values in the parasite counts with 0

WSP_fishdata.s <- data.frame(System=WSP_fishdata$System, Site=WSP_fishdata$Site, Age=stdize(WSP_fishdata$Age), Length=stdize(WSP_fishdata$Length), Wet_Wt=stdize(WSP_fishdata$Wet_Wt), Dry_Wt=stdize(WSP_fishdata$Dry_Wt_Prop), Fat=stdize(WSP_fishdata$Fat), GSI=stdize(WSP_fishdata$GSI), Cond.Factor=stdize(WSP_fishdata$Cond.Factor)) #standardize all the numeric variables to have a mean=0 and standard deviation=1
WSP_parasitedata.s <- data.frame(System=WSP_fishdata$System, Site=WSP_fishdata$Site, Eye_Parasites=stdize(WSP_parasitedata$Eye_Parasites), Gill_Parasites=stdize(WSP_parasitedata$Gill_Parasites), Liver_Parasites=stdize(WSP_parasitedata$Liver_Parasites), Mesen_Parasites=stdize(WSP_parasitedata$Mesen_Parasites)) #standardize all the numeric variables to have a mean=0 and standard deviation=1

full.rda <- rda(WSP_fishdata.s[,3:9] ~ Eye_Parasites + Gill_Parasites + Liver_Parasites + Mesen_Parasites + Condition(Site), data=WSP_parasitedata.s) #run an RDA with site as a conditional variable (this variation is partitioned out first), and all four types of parasites as constrained variables
anova(full.rda, by="term", model="full") #test the significance of RDA terms in explaining life history trait variation
rda.sum <- summary(full.rda) #print the summary of the RDA (this is where to get variance explained)
rda.sum

eye.rda <- rda(WSP_fishdata.s[,3:9] ~ Eye_Parasites + Condition(Gill_Parasites) + Condition(Liver_Parasites) + Condition(Mesen_Parasites) + Condition(Site), data=WSP_parasitedata.s) #run an RDA to calculate the amount of variance explained by eye parasite counts alone
summary(eye.rda) #print the summary of the eye parasite RDA

gill.rda <- rda(WSP_fishdata.s[,3:9] ~ Condition(Eye_Parasites) + Gill_Parasites + Condition(Liver_Parasites) + Condition(Mesen_Parasites) + Condition(Site), data=WSP_parasitedata.s) #run an RDA to calculate the amount of variance explained by gill parasite counts alone
summary(gill.rda) #print the summary of the gill parasite RDA

liver.rda <- rda(WSP_fishdata.s[,3:9] ~ Condition(Eye_Parasites) + Condition(Gill_Parasites) + Liver_Parasites + Condition(Mesen_Parasites) + Condition(Site), data=WSP_parasitedata.s) #run an RDA to calculate the amount of variance explained by liver parasite counts alone
summary(liver.rda) #print the summary of the liver parasite RDA

mesen.rda <- rda(WSP_fishdata.s[,3:9] ~ Condition(Eye_Parasites) + Condition(Gill_Parasites) + Condition(Liver_Parasites) + Mesen_Parasites + Condition(Site), data=WSP_parasitedata.s) #run an RDA to calculate the amount of variance explained by mesentary parasite counts alone
summary(mesen.rda) #print the summary of mesentary parasites RDA
#to calculate the confounded variation among the four types of parasites, subtract the amount of variation explained by each parasite type alone from the constrained variation explained in the full RDA

rdadf.fish <- as.data.frame(rda.sum$sites) #extract the RDA points for samples
rdadf.parasites <- as.data.frame(rda.sum$biplot) #extract the RDA vectors for the parasite counts
rdadf.traits <- as.data.frame(rda.sum$species) #extract the RDA vectors for the life history traits
rda1.plot <- ggplot(data=rdadf.fish, aes(x=RDA1, y=RDA2, color=WSP_fishdata.s$System, fill=WSP_fishdata.s$System))+ #plot the first two RDA axes and color the results by system
  stat_ellipse(geom="polygon", alpha=0.25, color=NA)+ #add 80% confidence interval ellipses for the estimated distribution of individual RDA scores for each system 
  geom_point(alpha=0.9)+ #add slightly transparent points for the individual coordinates 
  geom_hline(yintercept = 0)+ #add line along x-axis to make it more prominent
  geom_vline(xintercept = 0)+ #add line along y=axis to make it more prominent
  geom_segment(data=rdadf.parasites, aes(x=0, xend=RDA1*3, y=0, yend=RDA2*3, fill=NA), color="red", arrow=arrow(length = unit(0.01, "npc")))+ #add arrows showing the magnitude and direction for the parasite vectors (multiplied by 3 to make them more visible)
  geom_text(data=rdadf.parasites, aes(x=RDA1*3, y=RDA2*3, label=rownames(rdadf.parasites), fill=NA), hjust=0.5*(1-sign(rdadf.parasites$RDA1)), vjust=0.5*(1-sign(rdadf.parasites$RDA2)), color="red", size=4)+ #add and adjust positioning of the labels of the parasite vectors
  geom_text(data=rdadf.traits, aes(x=RDA1*3, y=RDA2*3, label=c("Age", "Length", "Wet Weight", "Dry Weight(%)", "Lipids", "GSI", "Cond.Factor"), fill=NA), color="black", size=4, position = position_jitter(height=0.5, seed=100))+ #add labels for the life history traits showing the direction and magnitude of the life history trait vectors along RDA axes 1 & 2
  theme_classic()+ #change the plot theme
  scale_color_manual(labels=c("Malpais Spring", "Lost River", "Mound Spring", "Salt Creek"), values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"))+ #edit the colors and associated labels to be displayed in the legend
  scale_fill_manual(labels=c("Malpais Spring", "Lost River", "Mound Spring", "Salt Creek"), values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"), guide="none")+ #edit fill colors and make sure they match with the outline colors
  labs(color="Population", fill="Population", x="RDA Axis 1 (6.41%)", y="RDA Axis 2 (0.71%)") #add legend and axis titles
ggsave("Figure_3.tiff", rda1.plot, device="tiff", width=12, height=9, units="in", dpi=300) #save version of this plot as a tiff for publication (Figure 3)

#Principal Components Analysis
Fish.pca <- dudi.pca(WSP_fishdata.s[,3:9], scannf=F, nf=4) #run principal components analysis on standardized life history data, run non-interactively and retain first 4 axes

pca1.plot <- ggplot(data=as.data.frame(Fish.pca$li), aes(x=Axis1*-1, y=Axis2, color=WSP_fishdata.s$System, fill=WSP_fishdata.s$System))+ #plot first two principal components (with sign flipped on the first PC so it is facing the same direction as RDA1) and color the plot by system
  stat_ellipse(geom="polygon", color=NA, alpha=0.25)+ #add 80% confidence interval ellipses for the estimated distribution of individual PC scores for each system 
  geom_point(alpha=0.9)+ #add slightly transparent points for the individual coordinates
  geom_hline(yintercept = 0)+ #add line along x-axis to make it more prominent
  geom_vline(xintercept = 0)+ #add line along y=axis to make it more prominent
  geom_segment(data=Fish.pca$c1, aes(x=0, xend=CS1*-5, y=0, yend=CS2*5, fill=NA), color="black", arrow=arrow(length = unit(0.01, "npc")))+ #add arrows for the life history traits showing the direction and magnitude of the life history trait loadings along PC axes 1 & 2
  geom_text(data=Fish.pca$c1, aes(x=CS1*-5, y=CS2*5, label=c("Age", "Length", "Wet Weight", "Dry Weight (%)", "Lipids", "GSI", "Condition Factor"), fill=NA), hjust=0.2*(sign(Fish.pca$c1$CS1)), color="black", size=6, position = position_jitter(height=0.25, seed=4243))+ #add labels for the life history traits showing the direction and magnitude of the life history trait loadings along PC axes 1 & 2
  theme_classic()+ #change the plot theme
  labs(color="System", fill="System", x="PC1 (42.0%)", y="PC2 (26.1%)")+ #add legend and axis titles
  scale_color_hue(labels=c("Malpais Spring", "Lost River", "Mound Spring", "Salt Creek"))+ #edit the colors and associated labels to be displayed in the legend
  scale_fill_hue(labels=c("Malpais Spring", "Lost River", "Mound Spring", "Salt Creek"), guide="none") #edit fill colors and make sure they match with the outline colors
ggsave("Figure_2.tiff", pca1.plot, device="tiff", width=12, height=9, units="in", dpi=300)

#Residual Analysis of Wet Wt vs. Dry Weight (Edema mixed model analysis)
WSP_data <- WSP_data[!is.na(WSP_data$Eye_Parasites), ] #remove fish with missing values for eye parasite counts
WSP_data <- WSP_data[!is.na(WSP_data$Liver_Parasites),] #remove fish with missing values for liver parasite counts
wetvsdry.lm <- lm(log(Wet_Wt) ~ log(Dry_Wt), data=WSP_data) #run linear model to estimate relationship between log dry weight and log wet weight
plot(log(WSP_data$Wet_Wt), log(WSP_data$Dry_Wt)) # plot log wet weight versus log dry weight
plot(wetvsdry.lm$fitted.values, wetvsdry.lm$residuals) #plot fitted values of linear model against the residuals to check for heteroskedacity and other model biases
hist(wetvsdry.lm$residuals) #plot the histogram of linear model residuals to check if they are approximately normally distributed

#all parasite model - predict log wet weight with log dry weight, and square root transformed counts of all parasite types and a random effect of sample site
#dry weight and site random effects are kept constant in all subsequent model iterations
full_parasite.lme <- lmer(log(WSP_data$Wet_Wt) ~ log(Dry_Wt) + sqrt(Liver_Parasites) + sqrt(Mesen_Parasites) + sqrt(Eye_Parasites) + sqrt(Gill_Parasites) + (1|Site), data=WSP_data) #run full edema mixed model
summary(full_parasite.lme) #print full model summary
AIC(full_parasite.lme) #get AIC value

#Liver+Mesen+Eye Parasites
LME.lme <- lmer(log(Wet_Wt) ~ log(Dry_Wt) + sqrt(Liver_Parasites) + sqrt(Mesen_Parasites) + sqrt(Eye_Parasites) + (1|Site), data=WSP_data)
summary(LME.lme) #print model summary
AIC(LME.lme) #print AIC

#Liver+Mesen+Gills
LMG.lme <- lmer(log(Wet_Wt) ~ log(Dry_Wt) + sqrt(Liver_Parasites) + sqrt(Mesen_Parasites) + sqrt(Gill_Parasites) + (1|Site), data=WSP_data)
summary(LMG.lme) #print model summary
AIC(LMG.lme) #print AIC

#Liver+Eyes+Gills
LEG.lme <- lmer(log(Wet_Wt) ~ log(Dry_Wt) + sqrt(Liver_Parasites) + sqrt(Eye_Parasites) + sqrt(Gill_Parasites) + (1|Site), data=WSP_data)
summary(LEG.lme) #print model summary
AIC(LEG.lme) #print AIC

#Mesen+Eyes+Gills
MEG.lme <- lmer(log(Wet_Wt) ~ log(Dry_Wt) + sqrt(Mesen_Parasites) + sqrt(Eye_Parasites) + sqrt(Gill_Parasites) + (1|Site), data=WSP_data)
summary(MEG.lme) #print model summary
AIC(MEG.lme) #print AIC

#Liver+Mesen
LM.lme <- lmer(log(Wet_Wt) ~ log(Dry_Wt) + sqrt(Mesen_Parasites) + sqrt(Liver_Parasites)  + (1|Site), data=WSP_data)
summary(LM.lme) #print model summary
AIC(LM.lme) #print AIC

#Liver+Eyes
LE.lme <- lmer(log(Wet_Wt) ~ log(Dry_Wt) + sqrt(Eye_Parasites) + sqrt(Liver_Parasites)  + (1|Site), data=WSP_data)
summary(LE.lme) #print model summary
AIC(LE.lme) #print AIC

#Liver+Gills
LG.lme <- lmer(log(Wet_Wt) ~ log(Dry_Wt) + sqrt(Gill_Parasites) + sqrt(Liver_Parasites)  + (1|Site), data=WSP_data)
summary(LG.lme) #print model summary
AIC(LG.lme) #print AIC

#Mesen+Eyes
ME.lme <- lmer(log(Wet_Wt) ~ log(Dry_Wt) + sqrt(Eye_Parasites) + sqrt(Mesen_Parasites)  + (1|Site), data=WSP_data)
summary(ME.lme) #print model summary
AIC(ME.lme) #print AIC

#Mesen+Gills
MG.lme <- lmer(log(Wet_Wt) ~ log(Dry_Wt) + sqrt(Gill_Parasites) + sqrt(Mesen_Parasites)  + (1|Site), data=WSP_data)
summary(MG.lme) #print model summary
AIC(MG.lme) #print AIC

#Eye+Gills
EG.lme <- lmer(log(Wet_Wt) ~ log(Dry_Wt) + sqrt(Gill_Parasites) + sqrt(Eye_Parasites)  + (1|Site), data=WSP_data)
summary(EG.lme) #print model summary
AIC(EG.lme) #print AIC

#Liver
L.lme <- lmer(log(Wet_Wt) ~ log(Dry_Wt) + sqrt(Liver_Parasites) + (1|Site), data=WSP_data)
summary(L.lme) #print model summary
AIC(L.lme) #print AIC

#Mesen
M.lme <- lmer(log(Wet_Wt) ~ log(Dry_Wt) + sqrt(Mesen_Parasites) + (1|Site), data=WSP_data)
summary(M.lme) #print summary
AIC(M.lme) #print AIC

#Gills
G.lme <- lmer(log(Wet_Wt) ~ log(Dry_Wt) + sqrt(Gill_Parasites) + (1|Site), data= WSP_data)
summary(G.lme) #print summary
AIC(G.lme) #print AIC

#Eyes
E.lme <- lmer(log(Wet_Wt) ~ log(Dry_Wt) + sqrt(Eye_Parasites) + (1|Site), data=WSP_data)
summary(E.lme) #print summary
AIC(E.lme) #print AIC

#Null
null.lme <- lmer(log(Wet_Wt) ~ log(Dry_Wt) + (1|Site), data=WSP_data)
summary(null.lme)
AIC(null.lme)

DryWetWeight_Analysis <- data.frame(Residuals=wetvsdry.lm$residuals, Eye_Parasites=WSP_data$Eye_Parasites, Mesen_Parasites=WSP_data$Mesen_Parasites, System=WSP_data$System) #create a data frame with the residuals from the null model and parasite counts for plotting
eyes_x_weight.plot <- ggplot()+ #create new ggplot object
  geom_point(data=DryWetWeight_Analysis[DryWetWeight_Analysis$System=="a" | DryWetWeight_Analysis$System=="m",], aes(x=Eye_Parasites, y=Residuals, color=System), alpha=0.9)+ #plot points of null model residuals along y axis and eye parasite counts along x axis colored by system (only systems where eye parasites were present)
  geom_smooth(method="lm", formula="y~sqrt(x)", alpha=0.2, color="black", linetype=2, data=DryWetWeight_Analysis[DryWetWeight_Analysis$System=="a" | DryWetWeight_Analysis$System=="m",], aes(x=Eye_Parasites, y=Residuals))+ #plot generalized line of best fit through the displayed points
  scale_color_manual(values=c("#F8766D", "#C77CFF"), labels=c("Malpais Spring", "Mound Spring"))+ #set colors and labels for the legend
  theme_classic()+ #change plot theme
  labs(x="Eye Parasite Count", y="Deviance in Wet Weight from Expected", title = "B") #add axis titles and plot letter

mesen_x_weight.plot <- ggplot()+ #create new ggplot object
  geom_point(data=DryWetWeight_Analysis[DryWetWeight_Analysis$System != "l",], aes(x=Mesen_Parasites, y=Residuals, color=System), alpha=0.9)+ #plot points of null model residuals along y axis and mesentery parasite counts along x axis colored by system (only systems where mesentery parasites were present)
  geom_smooth(method="lm", formula="y~sqrt(x)", alpha=0.2, color="black", linetype=2, data=DryWetWeight_Analysis, aes(x=Mesen_Parasites, y=Residuals))+ #plot generalized line of best fit through the displayed points
  scale_color_manual(values=c("#F8766D", "#C77CFF", "#00BFC4"), labels=c("Malpais Spring", "Mound Spring", "Salt Creek"))+ #set colors and labels for the legend
  theme_classic()+ #change plot theme
  labs(x="Mesentery Parasite Count", y="Deviance in Wet Weight from Expected", title = "A") #add axis titles and plot letter

parasite_x_weight_plots <- grid.arrange(mesen_x_weight.plot, eyes_x_weight.plot, ncol=1) #stack the previous two plots on top of each other
ggsave(filename="Figure_4.tiff", plot=parasite_x_weight_plots, device="tiff", width=8, height=12, units="in", dpi=300) #save a tiff of the two plots as Figure 4

#Lipid Trade-offs models
WSP_data <- data.frame(Sex=Rogowski_PhD_data2$sex, System=Rogowski_PhD_data2$System, Site=Rogowski_PhD_data2$Site...5, Age=as.numeric(Rogowski_PhD_data2$age), Length=as.numeric(Rogowski_PhD_data2$length), Wet_Wt=as.numeric(Rogowski_PhD_data2$`wet weight`), Dry_Wt=as.numeric(Rogowski_PhD_data2$`dry weight`), Fat=as.numeric(Rogowski_PhD_data2$`fat gr`), Rep_Alloc=as.numeric(rowSums(Rogowski_PhD_data2[,c(38,44)])), yolked_eggs=Rogowski_PhD_data2$`yolked eggs`, Eye_Parasites=rowSums(cbind(as.numeric(Rogowski_PhD_data2$`l eye`), as.numeric(Rogowski_PhD_data2$`r eye`))), Gill_Parasites=rowSums(cbind(as.numeric(Rogowski_PhD_data2$`right gill 1`), as.numeric(Rogowski_PhD_data2$`rg 2`), as.numeric(Rogowski_PhD_data2$`rg 3`), as.numeric(Rogowski_PhD_data2$`rg 4`), as.numeric(Rogowski_PhD_data2$`lg 1`), as.numeric(Rogowski_PhD_data2$`lg 2`), as.numeric(Rogowski_PhD_data2$`lg 3`), as.numeric(Rogowski_PhD_data2$`lg 4`))), Liver_Parasites=as.numeric(Rogowski_PhD_data2$liver), Mesen_Parasites=as.numeric(Rogowski_PhD_data2$`body cavity`)) #make a data frame with life history traits and parasite counts
WSP_data <- WSP_data[WSP_data$Sex=="f", ] #remove male fish from the dataset
WSP_data <- WSP_data[is.na(WSP_data$Age)==F, ] #remove fish with missing age data 
WSP_data <- WSP_data[is.na(WSP_data$Length)==F, ] #remove fish with missing length data
WSP_data <- WSP_data[is.na(WSP_data$Wet_Wt)==F, ] #remove fish with missing wet weight data
WSP_data <- WSP_data[is.na(WSP_data$Dry_Wt)==F, ] #remove fish with missing dry weight data
WSP_data <- WSP_data[is.na(WSP_data$Fat)==F, ] #remove fish with missing lipid content data
WSP_data <- WSP_data[is.na(WSP_data$Rep_Alloc)==F, ] #remove fish with missing egg + ovary mass data
WSP_data <- WSP_data[is.na(WSP_data$Gill_Parasites)==F, ] #remove fish with missing gill parasite count data
WSP_data <- WSP_data[is.na(WSP_data$Eye_Parasites)==F, ] #remove fish with missing eye parasite count data
WSP_data <- WSP_data[is.na(WSP_data$Liver_Parasites)==F, ] #remove fish with missing liver parasite count data
WSP_data <- WSP_data[is.na(WSP_data$Mesen_Parasites)==F, ] #remove fish with missing mesentery parasite count data
WSP_data <- WSP_data[WSP_data$Rep_Alloc/WSP_data$Dry_Wt < 0.4, ] #remove fish with high outlier egg + ovary mass compared to body mass (N=2)
WSP_data <- WSP_data[WSP_data$Fat > 0, ] #remove fish with 0 values for lipid content (N=2)
WSP_fishdata <- data.frame(Length=as.numeric(WSP_data$Length), Wet_Wt=as.numeric(WSP_data$Wet_Wt), Dry_Wt_Prop=as.numeric(WSP_data$Dry_Wt)/as.numeric(WSP_data$Wet_Wt), Fat_Prop=as.numeric(WSP_data$Fat)/as.numeric(WSP_data$Dry_Wt), GSI=as.numeric(WSP_data$Rep_Alloc)/as.numeric(WSP_data$Dry_Wt), CF=(100*as.numeric(WSP_data$Wet_Wt))/as.numeric(WSP_data$Length)^3) #ensure all quantitative life history variables are classed as numeric
WSP_fishdata2 <- cbind(WSP_fishdata, yolked_eggs=WSP_data$yolked_eggs, Eye_Parasites=WSP_data$Eye_Parasites, Mesen_Parasites=WSP_data$Mesen_Parasites, Liver_Parasites=WSP_data$Liver_Parasites, Gill_Parasites=WSP_data$Gill_Parasites) #add categorical variables and parasite counts to numeric life history trait data frame
WSP_fishdata2$Parasitized <- as.factor((WSP_fishdata2$Eye_Parasites + WSP_fishdata2$Mesen_Parasites + WSP_fishdata2$Gill_Parasites + WSP_fishdata2$Liver_Parasites)>0) #add a yes/no factor of whether or not a fish had any parasites
WSP_fishdata2$Parasite_Load <- WSP_fishdata2$Eye_Parasites + WSP_fishdata2$Mesen_Parasites + WSP_fishdata2$Gill_Parasites + WSP_fishdata2$Liver_Parasites #create a variable from the sums of all parasite types in each fish
WSP_fishdata2$Parasite_Density <- WSP_fishdata2$Parasite_Load/WSP_fishdata2$Wet_Wt #create a variable of the total number of parasites of all types per gram of wet weight in each fish
WSP_fishdata2 <- cbind(System=WSP_data$System, Site=WSP_data$Site, WSP_fishdata2) #add sample site data to the data frame
WSP_fishdata2.r <- stdize(WSP_fishdata2[,c(3:8, 10:13, 15:16)]) #standardize all the quantitative variables to have a mean of 0 and standard deviation of 1
WSP_fishdata2.r <- cbind(System=WSP_data$System, Site=WSP_data$Site, yolked_eggs=as.factor(WSP_fishdata2$yolked_eggs), Parasitized=WSP_fishdata2$Parasitized, WSP_fishdata2.r) #add all the categorical variables to the standardized quantitative variables data frame

fat.tradeoffs.full <- lmer(z.Fat_Prop ~ z.Length + z.Wet_Wt + z.GSI + z.CF + yolked_eggs + z.Parasite_Load + Parasitized + z.Parasite_Density + (1|Site), data=WSP_fishdata2.r) #run full lipids model with all explanatory variables
vif(fat.tradeoffs.full) #check variance inflation factors to see if any variables are too colinear - length and wet weight have high VIFs, but everything else looks good

fat.tradeoffs.full <- lmer(Fat_Prop ~ Length + GSI + CF + yolked_eggs + Parasite_Load + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r) #re-run full lipids model with wet weight variable removed
vif(fat.tradeoffs.full) #re-check VIFs to check variable collinearity, looks good now
AIC(fat.tradeoffs.full) #print AIC for full model

#Run all variable combinations - dredge can't handle random effects so it's all written out
#Random effect site variable is kept constant through all models

#single variable lipid tradeoffs models
fat.tradeoffs.len <- lmer(Fat_Prop ~ Length + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.len) #print AIC
fat.tradeoffs.GSI <- lmer(Fat_Prop ~ GSI + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI) #print AIC
fat.tradeoffs.ye <- lmer(Fat_Prop ~ yolked_eggs + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.ye) #print AIC
fat.tradeoffs.PL <- lmer(Fat_Prop ~ Parasite_Load + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.PL) #print AIC
fat.tradeoffs.P <- lmer(Fat_Prop ~ Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.P) #print AIC
fat.tradeoffs.PD <- lmer(Fat_Prop ~ Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.PD) #print AIC
fat.tradeoffs.CF <- lmer(Fat_Prop ~ CF + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF) #print AIC

#two variable lipid tradeoff models
fat.tradeoffs.GSI.Len <- lmer(Fat_Prop ~ GSI + Length + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.Len) #print AIC
fat.tradeoffs.len.ye <- lmer(Fat_Prop ~ yolked_eggs + Length + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.len.ye) #print AIC
fat.tradeoffs.len.PL <- lmer(Fat_Prop ~ Length + Parasite_Load + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.len.PL) #print AIC
fat.tradeoffs.len.P <- lmer(Fat_Prop ~ Length + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.len.P) #print AIC
fat.tradeoffs.len.PD <- lmer(Fat_Prop ~ Length + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.len.PD) #print AIC
fat.tradeoffs.len.cf <- lmer(Fat_Prop ~ CF + Length + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.len.cf) #print AIC
fat.tradeoffs.cf.GSI <- lmer(Fat_Prop ~ CF + GSI + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.cf.GSI) #print AIC
fat.tradeoffs.cf.ye <- lmer(Fat_Prop ~ CF + yolked_eggs + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.cf.ye) #print AIC
fat.tradeoffs.cf.PL <- lmer(Fat_Prop ~ CF + Parasite_Load + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.cf.PL) #print AIC
fat.tradeoffs.cf.P <- lmer(Fat_Prop ~ CF + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.cf.P) #print AIC
fat.tradeoffs.cf.PD <- lmer(Fat_Prop ~ CF + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.cf.PD) #print AIC
fat.tradeoffs.GSI.ye <- lmer(Fat_Prop ~ GSI + yolked_eggs + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.ye) #print AIC
fat.tradeoffs.GSI.PL <- lmer(Fat_Prop ~ GSI + Parasite_Load + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.PL) #print AIC
fat.tradeoffs.GSI.P <- lmer(Fat_Prop ~ GSI + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.P) #print AIC
fat.tradeoffs.GSI.PD <- lmer(Fat_Prop ~ GSI + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.PD) #print AIC
fat.tradeoffs.ye.PL <- lmer(Fat_Prop ~ yolked_eggs + Parasite_Load + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.ye.PL) #print AIC
fat.tradeoffs.ye.P <- lmer(Fat_Prop ~ yolked_eggs + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.ye.P) #print AIC
fat.tradeoffs.ye.PD <- lmer(Fat_Prop ~ yolked_eggs + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.ye.PD) #print AIC
fat.tradeoffs.PL.P <- lmer(Fat_Prop ~ Parasite_Load + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.PL.P) #print AIC
fat.tradeoffs.PL.PD <- lmer(Fat_Prop ~ Parasite_Load + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.PL.PD) #print AIC
fat.tradeoffs.P.PD <- lmer(Fat_Prop ~ Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.P.PD) #print AIC

#three variable lipid tradeoff models
fat.tradeoffs.Len.CF.GSI <- lmer(Fat_Prop ~ Length + CF + GSI + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.GSI) #print AIC
fat.tradeoffs.Len.CF.ye <- lmer(Fat_Prop ~ Length + CF + yolked_eggs + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.ye) #print AIC
fat.tradeoffs.Len.CF.PL <- lmer(Fat_Prop ~ Length + CF + Parasite_Load + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.PL) #print AIC
fat.tradeoffs.Len.CF.P <- lmer(Fat_Prop ~ Length + CF + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.P) #print AIC
fat.tradeoffs.Len.CF.PD <- lmer(Fat_Prop ~ Length + CF + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.PD) #print AIC
fat.tradeoffs.GSI.Len.ye <- lmer(Fat_Prop ~ GSI + Length + yolked_eggs + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.Len.ye) #print AIC
fat.tradeoffs.GSI.Len.PL <- lmer(Fat_Prop ~ GSI + Length + Parasite_Load + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.Len.PL) #print AIC
fat.tradeoffs.GSI.Len.P <- lmer(Fat_Prop ~ GSI + Length + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.Len.P) #print AIC
fat.tradeoffs.GSI.Len.PD <- lmer(Fat_Prop ~ GSI + Length + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.Len.PD) #print AIC
fat.tradeoffs.Len.ye.PL <- lmer(Fat_Prop ~ Length + yolked_eggs + Parasite_Load + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.ye.PL) #print AIC
fat.tradeoffs.Len.ye.P <- lmer(Fat_Prop ~ Length + yolked_eggs + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.ye.P) #print AIC
fat.tradeoffs.Len.ye.PD <- lmer(Fat_Prop ~ Length + yolked_eggs + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.ye.PD) #print AIC
fat.tradeoffs.Len.PL.P <- lmer(Fat_Prop ~ Length + Parasite_Load + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.PL.P) #print AIC
fat.tradeoffs.Len.PL.PD <- lmer(Fat_Prop ~ Length + Parasite_Load + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.PL.PD) #print AIC
fat.tradeoffs.Len.P.PD <- lmer(Fat_Prop ~ Length + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.P.PD) #print AIC
fat.tradeoffs.CF.GSI.ye <- lmer(Fat_Prop ~ CF + GSI + yolked_eggs + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.GSI.ye) #print AIC
fat.tradeoffs.CF.GSI.PL <- lmer(Fat_Prop ~ CF + GSI + Parasite_Load + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.GSI.PL) #print AIC
fat.tradeoffs.CF.GSI.P <- lmer(Fat_Prop ~ CF + GSI + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.GSI.P) #print AIC
fat.tradeoffs.CF.GSI.PD <- lmer(Fat_Prop ~ CF + GSI + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.GSI.PD) #print AIC
fat.tradeoffs.CF.ye.PL <- lmer(Fat_Prop ~ CF + yolked_eggs + Parasite_Load + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.ye.PL) #print AIC
fat.tradeoffs.CF.ye.P <- lmer(Fat_Prop ~ CF + yolked_eggs + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.ye.P) #print AIC
fat.tradeoffs.CF.ye.PD <- lmer(Fat_Prop ~ CF + yolked_eggs + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.ye.PD) #print AIC
fat.tradeoffs.CF.PL.P <- lmer(Fat_Prop ~ CF + Parasite_Load + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.PL.P) #print AIC
fat.tradeoffs.CF.PL.PD <- lmer(Fat_Prop ~ CF + Parasite_Load + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.PL.PD) #print AIC
fat.tradeoffs.CF.P.PD <- lmer(Fat_Prop ~ CF + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.P.PD) #print AIC
fat.tradeoffs.GSI.ye.PL <- lmer(Fat_Prop ~ GSI + yolked_eggs + Parasite_Load + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.ye.PL) #print AIC
fat.tradeoffs.GSI.ye.P <- lmer(Fat_Prop ~ GSI + yolked_eggs + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.ye.P) #print AIC
fat.tradeoffs.GSI.ye.PD <- lmer(Fat_Prop ~ GSI + yolked_eggs + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.ye.PD) #print AIC
fat.tradeoffs.GSI.PL.P <- lmer(Fat_Prop ~ GSI +  Parasite_Load + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.PL.P) #print AIC
fat.tradeoffs.GSI.PL.PD <- lmer(Fat_Prop ~ GSI +  Parasite_Load + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.PL.PD) #print AIC
fat.tradeoffs.GSI.P.PD <- lmer(Fat_Prop ~ GSI +  Parasite_Density + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.P.PD) #print AIC
fat.tradeoffs.ye.PL.P <- lmer(Fat_Prop ~ yolked_eggs + Parasite_Load + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.ye.PL.P) #print AIC
fat.tradeoffs.ye.PL.PD <- lmer(Fat_Prop ~ yolked_eggs + Parasite_Load + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.ye.PL.PD) #print AIC
fat.tradeoffs.ye.P.PD <- lmer(Fat_Prop ~ yolked_eggs + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.ye.P.PD) #print AIC
fat.tradeoffs.PL.P.PD <- lmer(Fat_Prop ~ Parasite_Load + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.PL.P.PD) #print AIC

#four variable lipid tradeoff models
fat.tradeoffs.Len.CF.GSI.ye <- lmer(Fat_Prop ~ Length + CF + GSI + yolked_eggs + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.GSI.ye) #print AIC
fat.tradeoffs.Len.CF.GSI.PL <- lmer(Fat_Prop ~ Length + CF + GSI + Parasite_Load + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.GSI.PL) #print AIC
fat.tradeoffs.Len.CF.GSI.P <- lmer(Fat_Prop ~ Length + CF + GSI + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.GSI.P) #print AIC
fat.tradeoffs.Len.CF.GSI.PD <- lmer(Fat_Prop ~ Length + CF + GSI + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.GSI.PD) #print AIC
fat.tradeoffs.Len.CF.ye.PL <- lmer(Fat_Prop ~ Length + CF + yolked_eggs + Parasite_Load + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.ye.PL) #print AIC
fat.tradeoffs.Len.CF.ye.P <- lmer(Fat_Prop ~ Length + CF + yolked_eggs + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.ye.P) #print AIC
fat.tradeoffs.Len.CF.ye.PD <- lmer(Fat_Prop ~ Length + CF + yolked_eggs + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.ye.PD) #print AIC
fat.tradeoffs.Len.CF.PL.P <- lmer(Fat_Prop ~ Length + CF + Parasite_Load + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.PL.P) #print AIC
fat.tradeoffs.Len.CF.PL.PD <- lmer(Fat_Prop ~ Length + CF + Parasite_Load + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.PL.PD) #print AIC
fat.tradeoffs.Len.CF.P.PD <- lmer(Fat_Prop ~ Length + CF + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.P.PD) #print AIC
fat.tradeoffs.GSI.Len.ye.PL <- lmer(Fat_Prop ~ GSI + Length + yolked_eggs + Parasite_Load + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.Len.ye.PL) #print AIC
fat.tradeoffs.GSI.Len.ye.P <- lmer(Fat_Prop ~ GSI + Length + yolked_eggs + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.Len.ye.P) #print AIC
fat.tradeoffs.GSI.Len.ye.PD <- lmer(Fat_Prop ~ GSI + Length + yolked_eggs + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.Len.ye.PD) #print AIC
fat.tradeoffs.GSI.Len.PL.P <- lmer(Fat_Prop ~ GSI + Length + Parasite_Load + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.Len.PL.P) #print AIC
fat.tradeoffs.GSI.Len.PL.PD <- lmer(Fat_Prop ~ GSI + Length + Parasite_Load + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.Len.PL.PD) #print AIC
fat.tradeoffs.GSI.Len.P.PD <- lmer(Fat_Prop ~ GSI + Length + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.Len.P.PD) #print AIC
fat.tradeoffs.Len.ye.PL.P <- lmer(Fat_Prop ~ Length + yolked_eggs + Parasite_Load + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.ye.PL.P) #print AIC
fat.tradeoffs.Len.ye.PL.PD <- lmer(Fat_Prop ~ Length + yolked_eggs + Parasite_Load + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.ye.PL.PD) #print AIC
fat.tradeoffs.Len.ye.P.PD <- lmer(Fat_Prop ~ Length + yolked_eggs + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.ye.P.PD) #print AIC
fat.tradeoffs.Len.PL.P.PD <- lmer(Fat_Prop ~ Length + Parasite_Load + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.PL.P.PD) #print AIC
fat.tradeoffs.CF.GSI.ye.PL <- lmer(Fat_Prop ~ CF + GSI + yolked_eggs + Parasite_Load + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.GSI.ye.PL) #print AIC
fat.tradeoffs.CF.GSI.ye.P <- lmer(Fat_Prop ~ CF + GSI + yolked_eggs + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.GSI.ye.P) #print AIC
fat.tradeoffs.CF.GSI.ye.PD <- lmer(Fat_Prop ~ CF + GSI + yolked_eggs + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.GSI.ye.PD) #print AIC
fat.tradeoffs.CF.GSI.PL.P <- lmer(Fat_Prop ~ CF + GSI + Parasite_Load + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.GSI.PL.P) #print AIC
fat.tradeoffs.CF.GSI.PL.PD <- lmer(Fat_Prop ~ CF + GSI + Parasite_Load + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.GSI.PL.PD) #print AIC
fat.tradeoffs.CF.GSI.P.PD <- lmer(Fat_Prop ~ CF + GSI + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.GSI.P.PD) #print AIC
fat.tradeoffs.CF.ye.PL.P <- lmer(Fat_Prop ~ CF + yolked_eggs + Parasite_Load + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.ye.PL.P) #print AIC
fat.tradeoffs.CF.ye.PL.PD <- lmer(Fat_Prop ~ CF + yolked_eggs + Parasite_Load + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.ye.PL.PD) #print AIC
fat.tradeoffs.CF.ye.P.PD <- lmer(Fat_Prop ~ CF + yolked_eggs + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.ye.P.PD) #print AIC
fat.tradeoffs.CF.PL.P.PD <- lmer(Fat_Prop ~ CF + Parasite_Load + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.PL.P.PD) #print AIC
fat.tradeoffs.GSI.ye.PL.P <- lmer(Fat_Prop ~ GSI + yolked_eggs + Parasite_Load + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.ye.PL.P) #print AIC
fat.tradeoffs.GSI.ye.PL.PD <- lmer(Fat_Prop ~ GSI + yolked_eggs + Parasite_Load + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.ye.PL.PD) #print AIC
fat.tradeoffs.GSI.ye.P.PD <- lmer(Fat_Prop ~ GSI + yolked_eggs + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.ye.P.PD) #print AIC
fat.tradeoffs.GSI.PL.P.PD <- lmer(Fat_Prop ~ GSI + Parasite_Load + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.PL.P.PD) #print AIC
fat.tradeoffs.ye.PL.P.PD <- lmer(Fat_Prop ~ yolked_eggs + Parasite_Load + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.ye.PL.P.PD) #print AIC

#five variable lipid tradeoff models
fat.tradeoffs.Len.CF.GSI.ye.PL <- lmer(Fat_Prop ~ Length + CF + GSI + yolked_eggs + Parasite_Load + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.GSI.ye.PL) #print AIC
fat.tradeoffs.Len.CF.GSI.ye.P <- lmer(Fat_Prop ~ Length + CF + GSI + yolked_eggs + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.GSI.ye.P) #print AIC
fat.tradeoffs.Len.CF.GSI.ye.PD <- lmer(Fat_Prop ~ Length + CF + GSI + yolked_eggs + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.GSI.ye.PD) #print AIC
fat.tradeoffs.Len.CF.GSI.PL.P <- lmer(Fat_Prop ~ Length + CF + GSI + Parasite_Load + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.GSI.PL.P) #print AIC
fat.tradeoffs.Len.CF.GSI.PL.P <- lmer(Fat_Prop ~ Length + CF + GSI + Parasite_Load + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.GSI.PL.P) #print AIC
fat.tradeoffs.Len.CF.GSI.PL.PD <- lmer(Fat_Prop ~ Length + CF + GSI + Parasite_Load + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.GSI.PL.PD) #print AIC
fat.tradeoffs.Len.CF.GSI.P.PD <- lmer(Fat_Prop ~ Length + CF + GSI + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.GSI.P.PD) #print AIC
fat.tradeoffs.Len.CF.ye.PL.P <- lmer(Fat_Prop ~ Length + CF + yolked_eggs + Parasite_Load + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.ye.PL.P) #print AIC
fat.tradeoffs.Len.CF.ye.PL.PD <- lmer(Fat_Prop ~ Length + CF + yolked_eggs + Parasite_Load + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.ye.PL.PD) #print AIC
fat.tradeoffs.Len.CF.ye.P.PD <- lmer(Fat_Prop ~ Length + CF + yolked_eggs + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.ye.P.PD) #print AIC
fat.tradeoffs.Len.CF.PL.P.PD <- lmer(Fat_Prop ~ Length + CF + Parasite_Load + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.PL.P.PD) #print AIC
fat.tradeoffs.Len.GSI.ye.PL.P <- lmer(Fat_Prop ~ Length + GSI + yolked_eggs + Parasite_Load + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.GSI.ye.PL.P) #print AIC
fat.tradeoffs.Len.GSI.ye.PL.PD <- lmer(Fat_Prop ~ Length + GSI + yolked_eggs + Parasite_Load + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.GSI.ye.PL.PD) #print AIC
fat.tradeoffs.Len.GSI.ye.P.PD <- lmer(Fat_Prop ~ Length + GSI + yolked_eggs + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.GSI.ye.P.PD) #print AIC
fat.tradeoffs.Len.GSI.PL.P.PD <- lmer(Fat_Prop ~ Length + GSI + Parasite_Load + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.GSI.PL.P.PD) #print AIC
fat.tradeoffs.Len.ye.PL.P.PD <- lmer(Fat_Prop ~ Length + yolked_eggs + Parasite_Load + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.ye.PL.P.PD) #print AIC
fat.tradeoffs.CF.GSI.ye.PL.P <- lmer(Fat_Prop ~ CF + GSI + yolked_eggs + Parasite_Load + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.GSI.ye.PL.P) #print AIC
fat.tradeoffs.CF.GSI.ye.PL.PD <- lmer(Fat_Prop ~ CF + GSI + yolked_eggs + Parasite_Load + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.GSI.ye.PL.PD) #print AIC
fat.tradeoffs.CF.GSI.ye.P.PD <- lmer(Fat_Prop ~ CF + GSI + yolked_eggs + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.GSI.ye.P.PD) #print AIC
fat.tradeoffs.CF.GSI.PL.P.PD <- lmer(Fat_Prop ~ CF + GSI + Parasite_Load + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.GSI.PL.P.PD) #print AIC
fat.tradeoffs.CF.ye.PL.P.PD <- lmer(Fat_Prop ~ CF + yolked_eggs + Parasite_Load + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.ye.PL.P.PD) #print AIC
fat.tradeoffs.GSI.ye.PL.P.PD <- lmer(Fat_Prop ~ GSI + yolked_eggs + Parasite_Load + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.GSI.ye.PL.P.PD) #print AIC

#six variable lipid tradeoff models
fat.tradeoffs.Len.CF.GSI.ye.PL.P <- lmer(Fat_Prop ~ Length + CF + GSI + yolked_eggs + Parasite_Load + Parasitized + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.GSI.ye.PL.P) #print AIC
fat.tradeoffs.Len.CF.GSI.ye.PL.PD <- lmer(Fat_Prop ~ Length + CF + GSI + yolked_eggs + Parasite_Load + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.GSI.ye.PL.PD) #print AIC
fat.tradeoffs.Len.CF.GSI.ye.P.PD <- lmer(Fat_Prop ~ Length + CF + GSI + yolked_eggs + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.GSI.ye.P.PD) #print AIC
fat.tradeoffs.Len.CF.GSI.PL.P.PD <- lmer(Fat_Prop ~ Length + CF + GSI + Parasite_Load + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.GSI.PL.P.PD) #print AIC
fat.tradeoffs.Len.CF.ye.PL.P.PD <- lmer(Fat_Prop ~ Length + CF + yolked_eggs + Parasite_Load + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.CF.ye.PL.P.PD) #print AIC
fat.tradeoffs.Len.GSI.ye.PL.P.PD <- lmer(Fat_Prop ~ Length + GSI + yolked_eggs + Parasite_Load + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.Len.GSI.ye.PL.P.PD) #print AIC
fat.tradeoffs.CF.GSI.ye.PL.P.PD <- lmer(Fat_Prop ~ CF + GSI + yolked_eggs + Parasite_Load + Parasitized + Parasite_Density + (1|Site), data=WSP_fishdata2.r)
AIC(fat.tradeoffs.CF.GSI.ye.PL.P.PD) #print AIC

#Lipid tradeoff plots
yolked.boxplot <- ggplot(data=WSP_fishdata2, aes(y=Fat_Prop, x=as.factor(yolked_eggs), color=as.factor(yolked_eggs), fill=as.factor(yolked_eggs)))+ #create a ggplot with lipid mass as proportion of dry weight on y axis and categorical variable with the presence/absence of yolked eggs on y-axis
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.25, alpha=1)+ #add density dotplot layer, orient vertically centered on the two yolked egg categories
  geom_violin(alpha=0.5)+ #add transparent violin plots over dotplot
  geom_boxplot(width=0.1, fill="white", color="black")+ #add thin boxplots on top of the violin plots and recolor boxplots black & white
  theme_classic()+ #change plot theme
  labs(x="Presence of Yolked Eggs", y="Lipid Proportion of Dry Weight", title="A")+ #add axis titles and multiplot letter
  theme(legend.position = "none") #remove legend (last plot will include legend for all 3)

fatxGSI.plot <- ggplot(data=WSP_fishdata2, aes(x=GSI, y=Fat_Prop, color=as.factor(yolked_eggs)))+ #create a ggplot object with GSI along the x axis and lipid mass as proportion of dry weight on y axis, colored by presence/absence of yolked eggs
  geom_point(size=2, alpha=0.5)+ #add points, make transparent
  geom_smooth(method="lm")+ #add lines of best fit 
  theme_classic()+ #change plot theme
  ylim(0, 0.45)+ #set y-axis limits (geom_smooth includes 95% CI that dips under 0, but it's past most of the data points and causes the plot to be zoomed out farther than it needs to be)
  labs(x="GSI", y="Lipid Proportion of Dry Weight", title="B")+ #add axis titles and multiplot letter
  theme(legend.position = "none") #remove legend (last plot will include legend for all 3)

fatxpdensity.plot <- ggplot(data=WSP_fishdata2, aes(x=Parasite_Density, y=Fat_Prop, color=as.factor(yolked_eggs)))+ #create a ggplot object with parasite density along the x axis and lipid mass as proportion of dry weight on y axis, colored by presence/absence of yolked eggs
  geom_point(size=2, alpha=0.5)+ #add points, make transparent
  geom_smooth(method="lm")+ #add lines of best fit
  theme_classic()+ #change plot theme
  scale_color_discrete(labels=c("absent", "present"))+
  ylim(0, 0.45)+ #set y-axis limits (geom_smooth includes 95% CI that dips under 0, but it's past most of the data points and causes the plot to be zoomed out farther than it needs to be)
  labs(x="Parasite Density (Count per g of Wet Weight)", y="Lipid Proportion of Dry Weight", color="Presence of\nYolked Eggs", title="C") #add axis and legend titles, multiplot letter

fatplots <- grid.arrange(yolked.boxplot, fatxGSI.plot, fatxpdensity.plot, ncol=3, widths=c(1,1,1.3)) #arrange three lipid tradeoff plots in a single row, give the third plot a little extra room for the width because it has the legend for all three
ggsave("Figure_5.tiff", fatplots, width = 16, height=5, units = "in", device="tiff", dpi=300) #save multiplot as Figure 5 tiff

#Map
habitats <- data.frame(System=c("Malpais Spring", "Salt Creek", "Mound Spring", "Lost Creek"), long=c(-106.3098, -106.3889, -106.2849, -106.1159), lat=c(33.2878, 33.2953, 33.4257, 32.9007))

register_stadiamaps("f179183b-b95a-4b00-b32a-7f13310e90e8") #add Stadia Maps api key
NM_basemap <- get_map(location=c(left=-106.6, bottom=32.8, right=-106, top=33.5), maptype = "stamen_terrain", zoom=12, source="stadia") #download terrain map of White Sands Missile Range and surrounding area from stamen

NM_basemap.ggplot <- ggmap(NM_basemap)+ #plot the stadia basemap for White Sands
  geom_point(data=habitats, aes(x=long, y=lat))+ #add points for the four habitats
  geom_text(data=habitats[2:4,], aes(x=long, y=lat, label = System), nudge_x = -0.04, nudge_y = 0.015)+ #add text labels for the habitats above points (all except Malpais)
  geom_text(data=habitats[1,], aes(x=long, y=lat, label = System), nudge_x = 0.04, nudge_y = -0.015)+ #add text label for Malpais Spring under the point so it doesn't overlap with Salt Creek
  annotate("text", x=-106.21, y=33.05, label="paste(italic('White Sands Missile Range'))", parse=T)
  
inset.ggplot <- ggplot()+ #create an empty inset plot to show the extent of the study area shown in the White Sands basemap
  borders(database = "world", fill="grey80", colour="grey50", xlim=c(-115, -102), ylim=c(30, 38))+ #plot shapefile for world landmasses, fill with light grey and darker border lines
  borders(database = "state", fill="grey80", colour="grey50", xlim=c(-115, -102), ylim=c(30, 38))+ #plot shapefile for US state landmasses, fill with light grey and darker border lines
  geom_rect(aes(xmin=-106.6, xmax=-106, ymin=32.8, ymax=33.5), color="red", fill=NA)+ #add a bounding box showing the extent of the White Sands basemap
  theme(panel.background=element_rect(fill="grey60"), panel.grid = element_line(color="grey70"), axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0.2,0.2,0.2,0.2, "cm"), panel.border = element_rect(fill=NA, color="black"))+ #remove the axes and change the colors of the long/lat grid in the extent plot
  coord_sf(xlim=c(-111, -102), ylim=c(28, 38)) #crop the inset plot to focus on New Mexico

tiff("Figure1.tiff", width=8, height=10, units="in", res=300) #open an 8x10" 300dpi tiff to save the sample site plot
print(NM_basemap.ggplot) #add the plot with the sample site points over the White Sands basemap
print(inset.ggplot, vp=viewport(0.232, 0.88, width=0.25, height=0.25)) #add the extent plot in the upper right hand corner of the tiff
dev.off() #stop writing to the tiff

