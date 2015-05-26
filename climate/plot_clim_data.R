# Plot PRISM and NEX DCP-30 calimate data (min and max temp and precipitation)
#  for the Appalachian or Great Northern Landscape Conservation Copperative
#  (ALCC and GNLCC, respectively).

# by  Kevin C. Guay (kguay@whrc.org)
# on  17 February 2015
# mod 18 May 2015
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# PART ZERO: Set variables such as file path, units and type of protected area
#  (i.e. park/ pace)

# PART ONE: Import the data and change the format of the dataframe such that it
#  is easier to plot

# PART TWO: Subset and make aesthetic changes to the dataframe 
#  (i.e. change the variable name "pr" to "precipitation")

# PART THREE: Plot the data using ggplot2 (using color *OR* B&W)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# load required packages
require(ggplot2)
require(plyr)
require(zoo)
require(grid)
require(doBy)
require(reshape2)
require(grDevices)
require(scales)

# - - - - - - - - - - - - - - - - PART ZERO - - - - - - - - - - - - - - - - - - 
############ This is the only code that needs to be changed  
############ based on plotting preferences and file paths

# P A R K   A N D   S E A S O N   I N F O
#  set up variables
lcc. <- 'alcc' # 'alcc' or 'gnlcc'
seas. <- 'sum' # 'wnt', 'spr', 'sum', or 'fal'
kind. <- 'park' # 'park' or 'pace'

# M E T R I C   O R   E N G L I S H   U N I T S
unit <- "metric" # metric: mm/yr & °C or english: inches/yr & °F

# F I L E   P A T H
#  folder with the prism and dcp30 datasets
path <- '/mnt/arctic/c/Share/LCC-VP/kguay/climdata/'

############

# - - - - - - - - - - - - - - - - PART ONE - - - - - - - - - - - - - - - - - - -
# Massage the dataframe and aggregate to seasonal data

# function to shift a vector, x, n elements to the right (positive) or to the left (negative)
shift <- function(x,n){
	if(n >= length(x)) n <- (n %% length(x))
	else if(n <= (0-length(x))) n <- (n %% (0-length(x)))
	if(n > 0) c(x[(length(x)-(n-1)):length(x)], x[1:(length(x)-n)])
	else if(n < 0) c(x[(1-n):length(x)],x[1:(0-n)])
	else x
}

# load lcc dataframe
lcc <- get(load(paste(path,'data/dcp30_extracted_', lcc., '.RData', sep='')))

lcc <- data.frame(rcp=as.character(lcc$rcp),
				quan=as.character(lcc$quan),
				var=as.character(lcc$var),
				pa=as.character(lcc$pa),
				kind=as.character(lcc$kind),
				year=as.numeric(as.character(lcc$year)),
				seas=rep(shift(unlist(lapply(c('win','spr','sum','fal'), 
									  rep, 3)),-1),nrow(lcc)/12),
				month=lcc$month,
				mean=as.numeric(as.character(lcc$mean)))

lcc.var <- dcast(lcc, rcp + quan + pa + kind + year + seas + month ~ var, value.var='mean')

# create vector with days of the year
days <- c(31,28,31,30,31,30,31,31,30,31,30,31)
# convert precipitation per day to precipitation per month
for(i in c(1:12)){
	lcc.var$pr[lcc.var$month==i] <- lcc.var$pr[lcc.var$month==i] * days[i]
}

lcc.orig <- melt(lcc.var, id=1:7, variable.name='var', value.name = "mean")

lcc.short <- dcast(lcc.orig, rcp + var + pa + kind + year + seas + month ~ quan, value.var='mean')

# sumarize data by season
lcc.yr <- ddply(lcc.short, .(rcp,var,pa,kind,year), summarize, ea=mean(ea), p25=mean(p25), p50=mean(p50), p75=mean(p75))

# group dataframe by season
lcc.season <- summaryBy(mean ~ rcp+quan+var+pa+kind+year+seas, data=lcc.orig, FUN=mean)
lcc.season.short <- dcast(lcc.season, rcp + var + pa + kind + year + seas ~ quan, value.var='mean.mean')


# - - - - - - - - - - - - - - - - PART TWO - - - - - - - - - - - - - - - - - - -
# Plot the data

# load PRISM data
load(paste(path,'data/prism.sn.all.RData',sep=''))
#
# source('monthly2seasonal.R')
df <- rbind(lcc.season.short, prism.season)

lcc <- c('alcc','gnlcc')
lcc.short.names = unlist(list(c("dewa", "shen", "grsm"), c("glac", "gye", "romo"))[which(lcc==lcc.)])
lcc.long.names =  unlist(list(c("Deleware Water", "Shenandoah", "Gr. Smokeys"), c("Glacier", "Gr. Yellowstone", "Rocky Mtn."))[which(lcc==lcc.)])

# define variable names (short and long)
vars.short <- c('tasmin','tasmax','pr')
vars <- c('Minimum Temperature','Maximum Temperature','Precipitation')


# determine long name of selected season
seas.full <- c('Spring','Summer','Fall','Yearly')[which(seas.==c('spr','sum','fal','yr'))]

# subset dataframe based on park/ pace and season (set in part zero)
df.sub <- subset(df,(kind==kind. & seas==seas.))

# further subset dataframe by protected area (pa) depending on the LCC
if (lcc. == 'alcc'){
	df.sub <- subset(df.sub,(pa=='dewa' | pa=='shen' | pa=='grsm'))
}else if (lcc. == 'gnlcc'){
	df.sub <- subset(df.sub,(pa=='glac' | pa=='gye' | pa=='romo'))
}

# UNIT CONVERSION
unit.conversion <- list(english=list(pr=0.0393701, temp=c(1.8,32)), metric=list(pr=1,temp=c(1,0)))
units <- list(metric=c('(°C)','(°C)','(mm/yr)'), english=c('(°F)','(°F)','(in/yr)'))
units <- units[[unit]]

# function to convert units to english if necessary
conv <- function(x,var,fact,fact2=0){
	print(paste(fact, fact2))
	x$ea[as.character(x$var)==var] <- x$ea[as.character(x$var)==var] * fact + fact2
	x$p25[as.character(x$var)==var] <- x$p25[as.character(x$var)==var] * fact + fact2
	x$p50[as.character(x$var)==var] <- x$p50[as.character(x$var)==var] * fact + fact2
	x$p75[as.character(x$var)==var] <- x$p75[as.character(x$var)==var] * fact + fact2
	x
}

df.sub <- conv(df.sub, "tasmin", unit.conversion[[unit]]$temp[1], unit.conversion[[unit]]$temp[2])
df.sub <- conv(df.sub, "tasmax", unit.conversion[[unit]]$temp[1], unit.conversion[[unit]]$temp[2])
df.sub <- conv(df.sub, "pr", unit.conversion[[unit]]$pr)

# change the variable names to make them meaningful (as axis labels on plot)
levels(df.sub$var)[levels(df.sub$var)=="tasmin"] <- paste("Min Temp", units[1])
levels(df.sub$var)[levels(df.sub$var)=="tasmax"] <- paste("Max Temp", units[2])
levels(df.sub$var)[levels(df.sub$var)=="pr"] <- paste("Precipitation", units[3])

# change the protected area names to make them meaningful (as titles on plot)
levels(df.sub$pa)[levels(df.sub$pa)=="dewa"] <- "Deleware Water Gap (DEWA)"
levels(df.sub$pa)[levels(df.sub$pa)=="shen"] <- "Shenendoah (SHEN)"
levels(df.sub$pa)[levels(df.sub$pa)=="grsm"] <- "Great Smoky Mountain (GRSM)"
levels(df.sub$pa)[levels(df.sub$pa)=="glac"] <- "Glacier (GLAC)"
levels(df.sub$pa)[levels(df.sub$pa)=="gye"] <- "Greater Yellowstone (GYE)"
levels(df.sub$pa)[levels(df.sub$pa)=="romo"] <- "Rocky Mountain (ROMO)"

# change column name "rcp" to "scenario"
names(df.sub)[1] <- 'scenario'

### Code taken from: https://gist.github.com/holstius/2898533
# This function is used to create a rolling mean to smooth the data in ggplot
require(proto)
StatRollApplyR <- proto(ggplot2:::Stat, {
	required_aes <- c("x", "y")
	default_geom <- function(.) GeomLine
	objname <- "rollapplyr"
	calculate_groups <- function(., data, scales, ...) {
		.super$calculate_groups(., data, scales, ...)
	}
	calculate <- function(., data, scales, width, FUN, fill=NA, ...) {
		require(zoo)
		filtered <- rollapplyr(data$y, width, FUN, fill=fill, ...)
		result <- data.frame(x=data$x, y=filtered)
		return(result)
	}
})
stat_rollapplyr <- StatRollApplyR$new
###

# - - - - - - - - - - - - - - - - PART THREE - - - - - - - - - - - - - - - - - -

# -  -  -  -  -  -  -  -  -  -  - COLOR PLOT  -  -  -  -  -  -  -  -  -  -  -  -

ggplot(df.sub, aes(y=p50, x=year, group=interaction(scenario,pa))) +  
	   # apply a rolling mean to the plot (3 years "wide") to smooth
	   #  it out. To "turn off" smoothing, set width=1
	   stat_rollapplyr(aes(colour=scenario), width=3, FUN=mean) +
	   
	   # use the 25th and 75th percentiles as upper and lower bounds of uncertainty.
       geom_ribbon(aes(ymin=p25, ymax=p75, fill=scenario), alpha=0.2, linetype='solid') +
	   
	   # Since I can't figure out how to have multiple y-axis lables, I set 
	   #  them to an empty string and used the row strip headers as labels (on 
	   #  the right side of the plots). This isn't ideal, but it can be changed 
	   #  in an image editor.
	   ylab("") +
	   
	   # set the number of tics on the y axis
	   scale_y_continuous(breaks = pretty_breaks(n=5)) +
	  
	   # white background
	   theme_bw() + 
	   
	   # create a grid with column=protected areas (pa) and row=variables
	   facet_grid(var~pa, scales='free_y')  +
	   
	   # angle the x axis lables and adjust the font size and color of the 
	   # "strip", i.e. horizontal (top) and verticle (right) plot headers
	   theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1), 
	   		 strip.text.x = element_text(size=10),
	         strip.text.y = element_text(size=10),
	         strip.background = element_rect(colour="white", fill="white")) #+
	   
	   # Extras that I was experimenting with (e.g. main title)
	   #scale_x_discrete(breaks = unique(df.sub$year)[seq(100,150,20)]) +
	   #ggtitle(paste(seas.full,vars,'(NEX DCP30)')) + 
	   #scale_linetype_manual("Climate Projection",
	                           #values=c("solid", "solid", "solid", "solid"),
	   						   #breaks=c("rcp45", "rcp85", "prism"),
							   #labels=c("RCP 4.5", "RCP 8.5", "PRISM"))+

# save the plot
ggsave(filename=paste(path, 'plots/', lcc., '_', kind., '_prism_dcp30_plots_', unit, '.png',sep=''), width=9, height=6, dpi=300)


# -  -  -  -  -  -  -  -  -  -  -  B&W PLOT  -  -  -  -  -  -  -  -  -  -  -  -

# mostly the same as above, but with some color and line type changes
 ggplot(df.sub, aes(y=p50, x=year, group=interaction(scenario,pa), linetype=rcp)) +  
 	   # apply a rolling mean to the plot (3 years "wide") to smooth
 	   #  it out. To "turn off" smoothing, set width=1
 	   stat_rollapplyr(aes(colour=scenario), width=3, FUN=mean) +

 	   # use the 25th and 75th percentiles as upper and lower bounds of uncertainty.
        geom_ribbon(aes(ymin=p25, ymax=p75, fill=scenario), fill='black', alpha=0.08, linetype='solid') +

 	   # Since I can't figure out how to have multiple y-axis lables, I set 
 	   #  them to an empty string and used the row strip headers as labels (on 
 	   #  the right side of the plots). This isn't ideal, but it can be changed 
 	   #  in an image editor.
 	   ylab("") +

 	   # set the number of tics on the y axis
 	   scale_y_continuous(breaks = pretty_breaks(n=5)) +

 	   # white background
 	   theme_bw() + 

 	   # create a grid with column=protected areas (pa) and row=variables
 	   facet_grid(var~pa, scales='free_y')  +

 	   # angle the x axis lables and adjust the font size and color of the 
 	   # "strip", i.e. horizontal (top) and verticle (right) plot headers
 	   theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1), 
 	   		 strip.text.x = element_text(size=10),
 	         strip.text.y = element_text(size=10),
 	         strip.background = element_rect(colour="white", fill="white")) +
	   # Set the oclor of RCP/ PRISM classes
	   scale_colour_manual("RCP",
		   values=c("gray20","gray20","gray20","black"),
		   breaks=lcc.short.names,
		   labels=lcc.long.names) +
	   # Set linetype of climate projections
   	   scale_linetype_manual("Scenario",
           values=c("solid", "solid", "dashed", "dotted"),
		   breaks=c("rcp45", "rcp85", "prism"),
		   labels=c("RCP 4.5", "RCP 8.5", "PRISM"))

# save the plot
ggsave(filename=paste(path, 'plots/', lcc., '_', kind., '_prism_dcp30_plots_', unit, '_bw.png',sep=''), width=9, height=6, dpi=300)
