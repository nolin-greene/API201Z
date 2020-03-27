#API 201Z PSET 1 Solution:
rm(list=ls())
setwd("~/Documents/R Training/API201Z PSET Recreation/PSET 1")
#install.packages("readxl")
library("readxl")
#install.packages("dplyr")
library("dplyr")
#install.packages("ggplot2")
library(ggplot2)
#install.packages("reshape")
library(reshape)
#install.packages("tidyr")
library(tidyr)

d<-read_excel("Pine Street Inn Length of Stay Data - Solutions.xls", sheet = 1, cell_cols(1:2))

#QUESTION 1L 1-7
mean(d$`Length of Stay (Nights)`)
#mean los = 26 nights
median(d$`Length of Stay (Nights)`)
#median los = 3 nights
summary(d$`Length of Stay (Nights)`)
#max = 727, min = 1
quantile(d$`Length of Stay (Nights)`, probs=c(.75, .90, .95))
#percentiles of LOS distribution are 17, 65 and 138 days for 75th, 95th and 99th percentiles
nrow(d) # there are 6556 guests

#Question 1 - Part 7 
colnames(d)<-c("n","los")
d%>%
  filter(los<=3) %>%
  summarize(n=n(), bednights=sum(los))
3322/6556 
4973/sum(d$los)

d%>%
  filter(los<=10 & los>3) %>%
  summarize(n=n(), bednights=sum(los))
1177/6556 
7328/sum(d$los)

d%>%
  filter(los<=35 & los>10) %>%
  summarize(n=n(), bednights=sum(los))
1048/6556 
21007/sum(d$los)

d%>%
  filter(los<=150 & los>35) %>%
  summarize(n=n(), bednights=sum(los))
721/6556 #51% of guests
53832/sum(d$los)

d%>%
  filter(los>150) %>%
  summarize(n=n(), bednights=sum(los))
288/6556 #4% of guests
84765/sum(d$los)

Length_of_Stay<-c("3 Days or Less", "4 to 10 Days", "11 to 35 Days", "36 to 150 Days", "151 Days or More")
Number_of_Guests<-c(721, 1177, 1048, 721, 288)
Number_of_Bed_Nights<-c(4973, 7328, 21007, 53832, 84765)
Fraction_of_Guests<-c(round(721/6556,2), round(1177/6556,2), round(1048/6556,2), round(721/6556,2), round(288/6556,2))
Fraction_of_Bed_Nights<-c(round(4973/sum(d$los),2), round(7328/sum(d$los),2), round(21007/sum(d$los),2), round(53832/sum(d$los),2), round(84765/sum(d$los),2))
 
one7<-data.frame(Length_of_Stay,Number_of_Guests,Number_of_Bed_Nights,Fraction_of_Guests,Fraction_of_Bed_Nights, row.names = 1)
one7<-rbind(one7, Total=c(sum(one7$Number_of_Guests), sum(one7$Number_of_Bed_Nights),NA, NA, NA))

names_spaced<-c("Number of <br>Guests", "Number of <br>Bed Nights", "Fraction <br>of Guests", "Fraction of <br>Bed Nights")
kable(one7,
             row.names = F,
             col.names = names_spaced, 
             escape = F)
install.packages("kableExtra",repos='http://cran.us.r-project.org')
Yes
#Question 8: COULDNT QUITE GET THIS GRAPH RIGHT
d<-d %>%
  mutate(bin= ifelse(los<4, "3 Days or Less", ifelse(los>3 & los<11, "4 to 10 Days",
              ifelse(los>10 & los <36, "11 to 35 Days", ifelse(los>35 & los<151, "36 to 150 Days",
              ifelse(los>150, "151 Days or More",NA))))))
d<-d %>%
  mutate(bin = case_when(
      los<4 ~ "3 Days or Less",
      los>3 & los<11 ~ "4 to 10 Days",
      los>10 & los <36 ~ "11 to 35 Days",
      los>35 & los<151 ~"36 to 150 Days",
      los>150 ~ "151 Days or More"),
    bin = factor(bin, levels = c("3 Days or Less","4 to 10 Days","11 to 35 Days",
                 "36 to 150 Days", "151 Days or More")))

d %>%
  group_by(bin) %>%
  summarize(n=n(), los=sum(los)) %>%
  gather(num, stat,-bin) %>%
  ggplot(aes(x=bin, y= stat, color = num))+
  geom_point()


#Question 9:
ggplot(d, aes(x=los))+
  geom_histogram(colour="black", fill = "slategray", binwidth = 10)+
  labs(x="Length of Stay (days)", y="Number of Guests", title="Distribution of Guests by Length of Stay")


temp <- tempfile()
download.file("http://www2.census.gov/govs/local/11statetypepu.zip",temp)
state_exp <- read.table(unz(temp, "11statetypepu.txt"))
unlink(temp)

str(state_exp)
