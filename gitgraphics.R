
library(readxl)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(gridExtra)
library(grid)
library(scales)
library(reshape2)



View(table1)

str(table1)

#IBD vs IBS

table1<- read_excel("IBD vs IBS.xlsx")

table1$population<-factor(table1$population, levels  = c("All Subjects","Adult Subjects","Pediatric Subjects"), ordered =TRUE)
table1$metric<-factor(table1$metric, levels  = c("Sensitivity @ 80 µg/g","Specificity @ 80 µg/g", "PPV @ 80 µg/g","NPV @ 80 µg/g","Sensitivity @ 160 µg/g",
                                                 "Specificity @ 160 µg/g","PPV @ 160 µg/g","NPV @ 160 µg/g", "ROC AUC" ), 
                      ordered =TRUE)


table2<-table1 %>% 
  filter(metric != "ROC AUC" )

g1<-ggplot(table2,aes(x=(population), color=population)) +geom_point(aes(y=value),colour="red") +
  geom_errorbar(aes(ymin=`lower CI`,ymax=`Upper CI`)) +coord_flip() +facet_wrap(~metric)

table3<-table1 %>% 
  filter(metric == "ROC AUC" )

g2<-ggplot(table3,aes(x=population, color=population)) +geom_point(aes(y=value),colour="red") +
  geom_errorbar(aes(ymin=`lower CI`,ymax=`Upper CI`)) +coord_flip() 

#Remove grids


g1 + theme_bw() +theme(panel.border = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor = element_blank()) + 
  theme(legend.title=element_blank(),legend.position="none") +ylab("Test Characteristic (%)") +xlab("Population Evaluated")

g2  + theme_bw() +theme(panel.border = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor = element_blank()) + 
  theme(legend.title=element_blank(),legend.position="none")  +ylab("Test Characteristic (%)") +xlab("Population Evaluated")

#Combined table
combined_table <- read_excel("Combined.xlsx")

unique(combined_table$population)
combined_table$population<-factor(combined_table$population, levels  = c("All Subjects IBD vs. IBS","Adult Subjects IBD vs. IBS",
                                                                         "Pediatric Subjects IBD vs. IBS","All Subjects IBD vs. non-IBD", 
                                                                         "Adult Subjects IBS vs. non-IBD", "Pediatric Subjects IBD vs. non-IBD"), ordered =TRUE)     
combined_table$metric<-factor(combined_table$metric, levels  = c("Sensitivity @ 80 µg/g","Specificity @ 80 µg/g", "PPV @ 80 µg/g","NPV @ 80 µg/g",
                                                                 "Sensitivity @ 160 µg/g", "Specificity @ 160 µg/g","PPV @ 160 µg/g","NPV @ 160 µg/g", "ROC AUC" ),  ordered =TRUE)


str(combined_table)

#Filter out ROC for now as it has a diff scale

combined_table1<-combined_table %>% 
  filter(metric != "ROC AUC" )

combinedplot<-ggplot(combined_table1,aes(x=population, color=population)) +geom_point(aes(y=value),colour="red") +
  geom_errorbar(aes(ymin=`lower CI`,ymax=`Upper CI`)) +coord_flip() +facet_wrap(~metric)

combinedplot + theme_bw() +theme(panel.border = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor = element_blank()) + 
  theme(legend.title=element_blank(),legend.position="none") +ylab("Test Characteristic (%)") +xlab("Population Evaluated")

combined_table2<-combined_table %>% 
  filter(metric == "ROC AUC" )

g2<-ggplot(combined_table2,aes(x=population, color=population)) +geom_point(aes(y=value),colour="red") +
  geom_errorbar(aes(ymin=`lower CI`,ymax=`Upper CI`)) +coord_flip() 

g2 + theme_bw() +theme(panel.border = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor = element_blank()) + 
  theme(legend.title=element_blank(),legend.position="none")  +ylab("Test Characteristic (%)") +xlab("Population Evaluated")

#Samee Graph diff Scale

#Create Dummy Var

ddummy = read_excel("Combined_dummy.xlsx")

ddummy$population<-factor(ddummy$population, levels  = c("All Subjects IBD vs. IBS","Adult Subjects IBD vs. IBS",
                                                         "Pediatric Subjects IBD vs. IBS","All Subjects IBD vs. non-IBD", 
                                                         "Adult Subjects IBS vs. non-IBD", "Pediatric Subjects IBD vs. non-IBD"), ordered =TRUE)
ddummy$metric<-factor(ddummy$metric, levels  = c("Sensitivity @ 80 µg/g","Specificity @ 80 µg/g", "PPV @ 80 µg/g","NPV @ 80 µg/g","Sensitivity @ 160 µg/g",
                                                 "Specificity @ 160 µg/g","PPV @ 160 µg/g","NPV @ 160 µg/g", "ROC AUC" ), 
                      ordered =TRUE)
str(ddummy)
str(combined_table)
str(combined_table)

combined_plot_with_ROC<-ggplot(combined_table, aes(population), color=population) + geom_point(aes(y=value),colour="red") + geom_blank(data=ddummy, aes(population)) +
  geom_errorbar(aes(ymin=`lower CI`,ymax=`Upper CI`)) +coord_flip() + facet_wrap(~metric, scales="free")  

combined_plot_with_ROC + expand_limits(y = 0) + scale_y_continuous(expand = c(0, 0))+ theme_bw() +theme(panel.border = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor = element_blank()) + 
  theme(legend.title=element_blank(),legend.position="none")  +ylab("Test Characteristic (%)") +xlab("Population Evaluated")

#With rorder attempt

combined_plot_with_ROC<-ggplot(combined_table, aes(reorder(population, c("All Subjects IBD vs. IBS","Adult Subjects IBD vs. IBS",
                                                                         "Pediatric Subjects IBD vs. IBS","All Subjects IBD vs. non-IBD", 
                                                                         "Adult Subjects IBS vs. non-IBD", "Pediatric Subjects IBD vs. non-IBD")), color=population) + geom_point(aes(y=value),colour="red") 
                               + geom_blank(data=ddummy, aes(population)) + geom_errorbar(aes(ymin=`lower CI`,ymax=`Upper CI`)) +coord_flip() + facet_wrap(~metric, scales="free"))  

combined_plot_with_ROC + expand_limits(y = 0) + scale_y_continuous(expand = c(0, 0))+ theme_bw() +theme(panel.border = element_blank(), panel.grid.major = element_blank(),  panel.grid.minor = element_blank()) + 
  theme(legend.title=element_blank(),legend.position="none")  +ylab("Test Characteristic (%)") +xlab("Population Evaluated")