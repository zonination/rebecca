# Set working directory, load libraries, read CSV
# Uncomment below if you need to set your working directory:
# setwd("C:/path/to/file") # Make sure you use "/" not "\"
library(tidyverse)
library(lubridate)

# Get all files in folder that match the pattern 20xx-Hx.csv
files<-list.files(pattern="20[0-9][0-9]-H[1-2].csv")
# Load the scale factors
scale2010<-read_csv("scale2010.csv", skip=2)
scale2012<-read_csv("scale2012.csv", skip=2)
scale2014<-read_csv("scale2014.csv", skip=2)
# Force headers
names(scale2010)<-c("date","index")
names(scale2012)<-c("date","index")
names(scale2014)<-c("date","index")
# Correct the YMD format issue
scale2010$date<-ymd(paste(scale2010$date, "-01", sep=""))
scale2012$date<-ymd(paste(scale2012$date, "-01", sep=""))
# Correct the "<1" issue forcing numerics in to characters
scale2010$index<-as.numeric(scale2010$index)
scale2012$index<-as.numeric(scale2012$index)
scale2014$index<-as.numeric(scale2014$index)
# Fix the NA issue with 2010 and correc to zeroes
scale2010$index[is.na(scale2010$index)]<-0
scale2012$index[is.na(scale2012$index)]<-0
scale2014$index[is.na(scale2014$index)]<-0

# Set up a blank data frame
df<-data.frame("date"=NA, "index"=NA, "scale2010"=NA,
               "scale2012"=NA, "scale2014"=NA)
df<-subset(df, !is.na(date))

# Load all files
for(n in 1:length(files)){
  # Import the file and sort the headers
  temp<-read_csv(files[n], skip=2)
  names(temp)<-c("date","index")
  temp$index<-as.numeric(temp$index)
  temp$index[is.na(temp$index)]<-0
  
  # Scale for 2010
  sc<-max(subset(scale2010, date>=min(temp$date) &
                    date<=max(temp$date))$index)
  temp$scale2010<-temp$index*sc/100
  
  # Scale for 2012
  sc<-max(subset(scale2012, date>=min(temp$date) &
                   date<=max(temp$date))$index)
  temp$scale2012<-temp$index*sc/100
  
  # Scale for 2014
  sc<-max(subset(scale2014, date>=min(temp$date) &
                   date<=max(temp$date))$index)
  temp$scale2014<-temp$index*sc/100
  
  # Load temp into our DF
  df<-rbind(df, temp)
}; rm(scale2010); rm(scale2012); rm(scale2014); rm(temp); rm(files); rm(sc); rm(n)
df$scale2010[df$scale2010==-Inf]<-NA
df$scale2012[df$scale2012==-Inf]<-NA
df$scale2014[df$scale2014==-Inf]<-NA

# Factor out weekdays
df$wday<-factor(wday(df$date),
                levels=7:1,
                labels=rev(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")))

# Plot final charts:

ggplot(subset(df, !is.na(scale2014)), aes(y=scale2014, x=wday))+
  geom_jitter(width=.2, aes(color=wday))+
  geom_boxplot(fill=NA, outlier.shape=NA)+
  scale_y_continuous(limits=c(0,100), breaks=seq(0,100,20))+
  guides(color=F)+
  labs(title="Google Searches for Rebecca Black",
       x="",
       y="Google Search Index (Adjusted)",
       caption="created by /u/zonination")+
  theme_bw()+
  coord_flip()+
  facet_grid(.~year(date))
ggsave("black1.png", height=5, width=20, dpi=120, type="cairo-png")

ggplot(subset(df, !is.na(scale2014) & wday=="Fri"), aes(year(date), scale2014))+
  geom_jitter(width=.2, color="#c49a00")+
  geom_boxplot(fill=NA, outlier.shape=NA, aes(group=year(date)))+
  scale_x_reverse()+
  scale_y_continuous(limits=c(0,100), breaks=seq(0,100,20))+
  labs(title="Google Searches for Rebecca Black",
       x="",
       y="Google Search Index on Friday (Adjusted)",
       caption="created by /u/zonination")+
  theme_bw()+
  coord_flip()
ggsave("black2.png", height=6, width=9, dpi=120, type="cairo-png")