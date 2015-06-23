# Param Singh
# IS 607 - Final Project

# 1) Load data sources

# Starbucks locations
sbux_loc_raw_df <- read.csv("All_Starbucks_Locations_in_the_US_-_Map.csv")

# Dunkin Donuts locations
dunkin_loc_raw_df <- read.csv("dunkin_donuts.csv")

# Zillow Median Sale Price, by zip code
zillow_median_raw_df <- read.csv("Zip_MedianSoldPrice_AllHomes.csv")
  
# 2) Transform data
sbux_loc_raw_df <- subset(sbux_loc_raw_df, select = c(Street.Line.1,  Street.Line.2, City, State, Zip))
sbux_loc_raw_df$Zip <- str_sub(sbux_loc_raw_df$Zip, 1, 5)

dunkin_loc_raw_df <- subset(dunkin_loc_raw_df, select =c(e_address, e_city, e_state, e_postal))

colnames(zillow_median_raw_df)[1] <- "Zip"
sbux_byzip <- merge(zillow_median_raw_df, sbux_loc_raw_df)
sbux_byzip <- subset(sbux_byzip, select = -c(CountyName))
sbux_byzip <- distinct(sbux_byzip)

# Dunkin data
colnames(dunkin_loc_raw_df)[4] <- "Zip"
dunkin_byzip <- merge(zillow_median_raw_df, dunkin_loc_raw_df)
dunkin_byzip <- subset(dunkin_byzip, select = -c(CountyName))
dunkin_byzip <- distinct(dunkin_byzip)

sbux_byzip <- na.omit(sbux_byzip)
dunkin_byzip <- na.omit(dunkin_byzip)

# get x-axis values which are column names in the data frame
xvalues <- colnames(sbux_byzip)

# Remove the vector entries that we don't want and perform some minor cleanup
badx <- c("Zip", "City", "State", "Metro", "Street.Line.1", "Street.Line.2")
xvalues <- xvalues[!xvalues %in% badx]
xvalues <- str_sub(xvalues, 2, 8)

# Pick 4 random cities from the data frame and perform a correlation check against the linear time x-values (quater-years)
sbux_sample <- sbux_byzip[sample(nrow(sbux_byzip), 4), ]

sample1 <- as.numeric(sbux_sample[1,])
remove <- c(1,2,3,4,234,235)
sample1 <- sample1[-remove]

sample2 <- as.numeric(sbux_sample[2,])
sample2 <- sample2[-remove]

sample3 <- as.numeric(sbux_sample[3,])
sample3 <- sample3[-remove]

sample4 <- as.numeric(sbux_sample[4,])
sample4 <- sample4[-remove]

# Similar operations for Dunkin
dunkin_sample <- dunkin_byzip[sample(nrow(dunkin_byzip), 4), ]

dsample1 <- as.numeric(dunkin_sample[1,])
remove <- c(1,2,3,4, 234, 235, 236)
dsample1 <- dsample1[-remove]

dsample2 <- as.numeric(dunkin_sample[2,])
dsample2 <- dsample2[-remove]

dsample3 <- as.numeric(dunkin_sample[3,])
dsample3 <- dsample3[-remove]

dsample4 <- as.numeric(dunkin_sample[4,])
dsample4 <- dsample4[-remove]

# Create data frames for use with ggplot
star1 <- data.frame(cbind(xvalues, sample1))
star2 <- data.frame(cbind(xvalues, sample2))
star3 <- data.frame(cbind(xvalues, sample3))
star4 <- data.frame(cbind(xvalues, sample4))

dunk1 <- data.frame(cbind(xvalues, dsample1))
dunk2 <- data.frame(cbind(xvalues, dsample2))
dunk3 <- data.frame(cbind(xvalues, dsample3))
dunk4 <- data.frame(cbind(xvalues, dsample4))

# 4) Visualize and plot
s1 <- ggplot(star1, aes(x=xvalues, y=sample1)) + geom_point()
s2 <- ggplot(star2, aes(x=xvalues, y=sample2)) + geom_point()
s3 <- ggplot(star3, aes(x=xvalues, y=sample3)) + geom_point()
s4 <- ggplot(star4, aes(x=xvalues, y=sample4)) + geom_point()
s1 <- s1 + scale_x_discrete(limit = c("1996.04", "1998.04", "2000.04", "2002.04","2004.04","2006.04", "2008.04","2010.04", "2015.04")) + scale_y_discrete(breaks=NULL)
s2 <- s2 + scale_x_discrete(limit = c("1996.04", "1998.04", "2000.04", "2002.04","2004.04","2006.04", "2008.04","2010.04", "2015.04")) + scale_y_discrete(breaks=NULL)
s3 <- s3 + scale_x_discrete(limit = c("1996.04", "1998.04", "2000.04", "2002.04","2004.04","2006.04", "2008.04","2010.04", "2015.04")) + scale_y_discrete(breaks=NULL)
s4 <- s4 + scale_x_discrete(limit = c("1996.04", "1998.04", "2000.04", "2002.04","2004.04","2006.04", "2008.04","2010.04", "2015.04")) + scale_y_discrete(breaks=NULL)
grid.arrange(s1, s2, s3, s4)

d1 <- ggplot(star1, aes(x=xvalues, y=dsample1)) + geom_point()
d2 <- ggplot(star2, aes(x=xvalues, y=dsample2)) + geom_point()
d3 <- ggplot(star3, aes(x=xvalues, y=dsample3)) + geom_point()
d4 <- ggplot(star4, aes(x=xvalues, y=dsample4)) + geom_point()
d1 <- d1 + scale_x_discrete(limit = c("1996.04", "1998.04", "2000.04", "2002.04","2004.04","2006.04", "2008.04","2010.04", "2015.04")) + scale_y_discrete(breaks=NULL)
d2 <- d2 + scale_x_discrete(limit = c("1996.04", "1998.04", "2000.04", "2002.04","2004.04","2006.04", "2008.04","2010.04", "2015.04")) + scale_y_discrete(breaks=NULL)
d3 <- d3 + scale_x_discrete(limit = c("1996.04", "1998.04", "2000.04", "2002.04","2004.04","2006.04", "2008.04","2010.04", "2015.04")) + scale_y_discrete(breaks=NULL)
d4 <- d4 + scale_x_discrete(limit = c("1996.04", "1998.04", "2000.04", "2002.04","2004.04","2006.04", "2008.04","2010.04", "2015.04")) + scale_y_discrete(breaks=NULL)
grid.arrange(d1, d2, d3, d4)
