require(plyr)
require(reshape)
require(ggplot2)
require(grid)
require(lattice)
require(zoo)
require(lubridate)
require(colorRamps)
require(scales)

setwd('/mnt/arctic/c/Share/kguay/proj/smap_proposal_brogers')

# SECTION 1
################################################################################

# load a dataset into a dataframe
df <- read.csv('BZ2_daily_2013.csv', skip=12)

# convert DATE column to Date object
df$DATE <- as.Date(df$DATE, "%m/%d/%y")

# add week column
df$WEEK <- as.numeric(format(df$DATE,"%U"))

# average my week
df2 <- ddply(df, c("YEAR","WEEK"), summarise, 
			 X.0.025=mean(X.0.025),
			 X.0.125=mean(X.0.125),
			 X.0.275=mean(X.0.275),
			 X.0.525=mean(X.0.525),
			 X.0.575=mean(X.0.575),
			 X.0.625=mean(X.0.625),
			 X.0.675=mean(X.0.675),
			 X.0.725=mean(X.0.725),
			 X.0.775=mean(X.0.775),
			 X.1.5=mean(X.1.5),
			 X.2.0=mean(X.2.0),
			 X.3.0=mean(X.3.0))

# average my week
df.air <- ddply(df, c("YEAR","WEEK"), summarise, 
 			         air=mean(AIR))
# rename columns
names(df.air) <- c('year','week','air')

# melt dataframe
df3 <- melt(df2, id=c('YEAR','WEEK'))
# rename columns
names(df3) <- c('year','week','depth','temp')
# remove "X." from begining of depth column
df3$depth <- as.numeric(substr(as.character(df3$depth), 3,nchar(as.character(df3$depth))))

# subset by year		 
df4 <- subset(df3, df3$year==2008 & (df3$week %in% seq(10,55,9))& df3$depth <= 1)
df.air2 <- subset(df.air, as.numeric(as.character(df.air$year))==2008 & (df.air$week %in% seq(10,55,10)))
#df4$temp <- df4$temp + (df4$week/3)


# plot depth profiles
ggplot(df4, aes(x=temp, y=depth, group=week, color=week)) + 
 geom_path(size=.7) + 
 #geom_point(data=df.air2, aes(x=air, y=rep(-0.1, nrow(df.air2)))) +
 scale_x_continuous() +
 scale_y_reverse() +
 geom_vline(xintercept = 0) +
 geom_hline(xintercept = 0) +
 theme_bw() +
 scale_colour_gradient(low="red", high="blue") +
 #facet_grid(. ~ week, space="fixed") +
 theme(panel.margin = unit(0, "lines"), 
 	   strip.background = element_rect(fill = 'white', line=0), 
	   plot.title = element_text(size=11)) + 
 labs(title="Temperature Profile (site: u70 0608; year=2008)", x="Temperature (°C)",y="Depth (meters below surface)")
 

ggsave(filename="plots/fig_2_tsp_freeze_thaw_depth.png", width=6, height=5, units="in", dpi=72)
 
# make a line tat x=0

# SECTION 2
################################################################################
tf <- read.csv('u70_0608_thaw.csv', skip=12)
# only keep columns that we need
tf <- tf[, c(1,2,3,4,25)]
# rename columns
names(tf) <- c('year','day','date','air','depth')

# convert the depth to numeric
tf$depth <- as.numeric(as.character(tf$depth))
# convert DATE column to Date object
tf$date <- as.Date(tf$date, "%m/%d/%y")

# subset for 2007
tf2 <- subset(tf, tf$year==2008)


g.top <- ggplot(tf2, aes(x=day, y=air)) + 
 #geom_vline(xintercept = c(125,131,180,216,231,247,269), size=0.4, color='gray') +
 geom_line(size=1, color='black') +
 theme_bw() +
 theme(plot.margin = unit(c(1,5,-30,0),units="points"),
         axis.title.y = element_text(vjust =0.25)) +
 xlim(100,300) +
 ylim(-20,25) +
 labs(title="Freeze-Thaw Depth (site: u70 0608)", y="Temperature (°C)")
 
g.bottom <- ggplot(tf2, aes(x=day, y=depth)) + 
  #geom_vline(xintercept = c(125,131,180,216,231,247,269), size=0.4, color='gray') +
  geom_line(size=1, color='black') +
  theme_bw() +
  scale_y_reverse() +
  theme(plot.margin = unit(c(0,5,1,1),units="points")) +
  xlim(100,300) +
  labs(x="Day of Year (2008)",y="Depth (meters below surface)")

draw line at max depth of active layer 

grid.arrange(g.top,g.bottom, heights = c(1.5/5, 2.5/5)) 

g <- arrangeGrob(g.top,g.bottom, heights = c(1.5/5, 2.5/5))
 
ggsave(file="plots/fig_3_tsp_freeze_thaw_depth_temp.png", g, width=6, height=5, units="in", dpi=72)

 # SECTION 3
################################################################################


# REPEAT OF SECTION 1 CODE
###############################################

setwd('/mnt/arctic/c/Share/kguay/proj/smap_proposal_brogers')

# load a dataset into a dataframe
df <- read.csv('u71_0608.csv', skip=12)

# convert DATE column to Date object
df$DATE <- as.Date(df$DATE, "%m/%d/%y")

# add week column
df$WEEK <- as.numeric(format(df$DATE,"%U"))
###############################################

# copy df to daily
daily <- df[, c(1,2,5:19)]

daily2 <- melt(daily, id=c('YEAR','DAY'))

# rename columns
names(daily2) <- c('year','day','depth','temp')
# daily2$depth <- as.numeric(substr(as.character(daily2$depth), 3,nchar(as.character(daily2$depth))))

daily2$depth <- as.numeric(substr(as.character(daily2$depth), 2,nchar(as.character(daily2$depth))-2))

# subset to 2008
daily2 <- subset(daily2, daily2$year == 2008 & !is.na(daily2$depth))

# make a temp dataframe with 1mm depth intervals
temp <- data.frame(year=rep(2008, 55266), day=unlist(lapply(seq(1,366), rep, 151)), depth=rep(seq(0.01,1.510,0.010),366))

# merge temp dataframe with daily2
d <- merge(temp, daily2, by=c('year','day','depth'), all=T)

# interpolate temperature 
d.fill <- ddply(d, .(year,day), mutate, temp=na.spline(temp))
d.fill$depth <- d.fill$depth*-1
d.fill$temp[d.fill$temp > 15] <- 15
d.fill$temp[d.fill$temp < -15] <- -15

# d.2007 <- d.fill
# d.2008 <- d.fill

d.78 <- rbind(d.2007,d.2008)
d.78$date <- as.Date(d.78$day - 1, origin = paste0(d.78$year,"-01-01"))

d.78 <- subset(d.78, d.78$date > as.Date('2007-02-01') & d.78$date < as.Date('2008-04-01') & d.78$depth >= -1.5)
d.78$group <- 'u71'

# png(filename='plots/plot_2a_depth_temp_color_map_u71_2007.png', width=500, height=500, units="px", pointsize=12)

levelplot(temp ~ date * depth,
		  data = d.78,
		  col.regions=blue2red(500),
   	   	  at=seq(from=-15,to=15,length=150),
		  scale=list(y=list(alternating=1,
 			  		 at=pretty(d.78$depth,10),
 					 labels=pretty(d.78$depth,10))),
		  xlab="                    2007                                     2008",
		  ylab="Depth (meters below surface)"
          )

# dev.off()

################################################################################
setwd('/mnt/arctic/c/Share/kguay/proj/smap_proposal_brogers')

# load a dataset into a dataframe
df <- read.csv('u70_0608.csv', skip=12)

###############################################

# copy df to daily
daily <- df[, c(1:2,6:17)]

daily2 <- melt(daily, id=c('YEAR','DAY'))

# rename columns
names(daily2) <- c('year','day','depth','temp')

daily2$depth <- as.numeric(substr(as.character(daily2$depth), 3,nchar(as.character(daily2$depth))))

# subset to 2008
daily2 <- subset(daily2, daily2$year == 2007 & !is.na(daily2$depth) & daily2$depth < 1.8)

# make a temp dataframe with 1mm depth intervals
temp <- data.frame(day=unlist(lapply(seq(1,365), rep, 121)), depth=rep(seq(0.005,0.605,0.005),365))

# merge temp dataframe with daily2
d <- merge(temp, daily2[, c(2:4)], by=c('day','depth'), all=T)

# interpolate temperature 
# TESTING
d.fill <- ddply(d, .(day), mutate, temp=na.spline(temp))
d.fill$depth <- abs(d.fill$depth)*-1
d.fill$temp[d.fill$temp > 15] <- 15
d.fill$temp[d.fill$temp < -15] <- -15

# fill in artifacts
d.fill$temp[d.fill$day > 283 & d.fill$temp > 0] <- -0.001

d.fill$group <- 'u70'
d.fill$year <- 2007
d.fill$date <- as.Date(d.fill$day - 1, origin = paste0(d.fill$year,"-01-01"))

d.fill <- subset(d.fill, d.fill$date > as.Date('2007-02-01') & d.fill$date < as.Date('2008-04-01') & d.fill$depth >= -1.5)

d.fill$temp[(d.fill$depth < -0.2 & d.fill$date < as.Date('2007-07-15') & d.fill$temp >= 0)] <- -0.001

d.all <- rbind(d.78,d.fill)


ppi <- 300
png(file='plots/plot_2a_depth_temp_color_map_2007.png', width=7*ppi,height=4*ppi,res=ppi)

levelplot(temp ~ date * depth | group,
		  data = d.all,
		  col.regions=blue2red(500),
   	   	  at=seq(from=-15,to=15,length=150),
		  scale=list(x=list(alternating=1,
					 rot=45,
					 relation='sliced',
					 tick.number=10),
					 y=list(alternating=1,
 			  		 at=pretty(d.all$depth,10),
 					 labels=abs(pretty(d.all$depth,10)),
					 limits=c(-1.2,0))),
		  par.settings = list(strip.background=list(col="white")),
		  xlab="Time",
		  ylab="Depth (meters below surface)"
          )

dev.off()



