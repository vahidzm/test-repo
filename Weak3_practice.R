#% VZM
#% 6/05/2018

# cleaning data, Week 3

library(dplyr)
packageVersion("dplyr")
library(swirl)

install_from_swirl("Getting and Cleaning Data")
swirl()




# cran is a data frame table

by_package <- group_by(cran, package)
summarize(by_package, mean(size))

pack_sum <- summarize(by_package,
                      count = n() ,
                      unique = n_distinct(ip_id),
                      countries =n_distinct(country) ,
                      avg_bytes = mean(size) )



###   chaining
cran %>%
  select(ip_id, country, package, size) %>%
  mutate(size_mb = size / 2^20) %>%
  filter(size_mb <= 0.5) %>%
  arrange(desc(size_mb)) %>% print


# swirl -- tidying data with tidyr
library(readr)
library(tidyr)
gather(students, sex,count,-grade)

students2 %>%
  gather( sex_class,count ,-grade ) %>%
  separate(sex_class , c("sex", "class")) %>%
  print

students3 %>%
  gather(class, grade, class1:class5, na.rm = TRUE) %>%
  spread(test, grade) %>%
  ### Call to mutate() goes here %>%
  mutate(class = parse_number(class)) %>%
  print

bind_rows (passed, failed) 

sat %>%
  select(-contains("total")) %>%
  gather(part_sex, count, -score_range) %>%
  separate(part_sex, c("part", "sex")) %>%
  group_by(part,sex) %>%
  mutate(total = sum(count),
         prop = count/total
  ) %>% print


#####   Quiz 3
# P1 
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv",destfile = "/Users/vahid/Dropbox (Personal)/Data/Coursera/Data Cleaning/Week3/ID2006.csv")
nums = read.csv("/Users/vahid/Dropbox (Personal)/Data/Coursera/Data Cleaning/Week3/ID2006.csv",header = T, skip = 0)
which(nums$ACR==3 & nums$AGS==6)

# P2
library("jpeg")
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg",destfile = "/Users/vahid/Dropbox (Personal)/Data/Coursera/Data Cleaning/Week3/jeff.jpg")
jfjp <- readJPEG("/Users/vahid/Dropbox (Personal)/Data/Coursera/Data Cleaning/Week3/jeff.jpg", native=T)
quantile(jfjp,probs = c(.3,.8))

# P3
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
location <- "/Users/vahid/Dropbox (Personal)/Data/Coursera/Data Cleaning/Week3/"
fname <-"GDP.csv"
download.file(url,destfile = paste(location,fname))
GDP <- read.csv(paste(location,fname),skip = 5,nrows=190,header = F)
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
fname <- "fedstats.csv"
download.file(url,destfile = paste(location,fname))
fedstats <- read.csv(paste(location,fname))
GDP <- GDP %>% select_if(~ sum(is.na(.))<5) %>% mutate(V5 = parse_number(V5))

merged_data <- merge(GDP, fedstats, by.x = "V1", by.y = "CountryCode", all =F)
merged_data <- tbl_df(merged_data)
names(merged_data)
head(merged_data)
sum(!is.na(match(GDP$X,fedstats$CountryCode)))
sorted_gdp <- arrange(merged_data,(V5))
sorted_gdp[13,1:10]
sorted_gdp$V5
gr_data <- merged_data %>% group_by(Income.Group)
summarize(gr_data, mean(V2))
library("Hmisc")
GDPRankGrouped <- mutate(merged_data,GdpRankGr = cut(V2, breaks = quantile(V2, probs = c(0,.2,.4, .6, .8, 1))))
names(GDPRankGrouped)
table(GDPRankGrouped$Income.Group,GDPRankGrouped$GdpRankGr)
