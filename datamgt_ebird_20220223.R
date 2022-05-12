#############################################l
#
#
#     eBird data management and plotting
#
# Last updated: 03/02/2022
# by Nick Masto and co-authors
#
#
#############################################l

# Libraries-----
library(ggplot2)
library(tidyverse)


#load it in
dat <- as.data.frame(read.csv("data/eBird_table1.csv"))
head(dat)

#anything I need to do to the data?-----------------------------------
#need to make year a factor and binomial


dat <- dat %>%
  mutate(
    year_bin = case_when(
      Year == 2021 | Year == 2019 | Year == 2015 ~ 1, 
      TRUE ~ 0
    ) 
  )
      #Year < 2021 ~ 0)
  #)

dat$Year <- as.factor(dat$Year)
dat$year_bin <- as.factor(dat$year_bin)

dat2 <- dat %>%
  filter(Year != "2019" & Year != "2015")
  

#code 2021, 2019, and 2015 as ECEs; create data where 2019 and 2015 are removed


#create dataframes by flyway
Atlantic <- split(dat, dat$Flyway)[[1]]
Central <- split(dat, dat$Flyway)[[2]]
Mississippi <- split(dat, dat$Flyway)[[3]]
Pacific <- split(dat, dat$Flyway)[[4]]

#create dataframes by flyway
Atlantic2 <- split(dat2, dat2$Flyway)[[1]]
Central2 <- split(dat2, dat2$Flyway)[[2]]
Mississippi2 <- split(dat2, dat2$Flyway)[[3]]
Pacific2 <- split(dat2, dat2$Flyway)[[4]]

