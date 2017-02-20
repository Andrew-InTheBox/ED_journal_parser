library(jsonlite)
library(scatterplot3d)
library(ggplot2)
library(plyr)
library(data.table)


file_list <- list.files(pattern = "*.log") #get all .log files

myrows <- lapply(file_list, readLines) #make list from lines of text in all files

myjson <- unlist(myrows) #unlist to get a simple list of characters, one list element per JSON entry

myjson1 <- lapply(myjson, jsonlite::fromJSON) #run fromJSON on each element in the list

myjson2 <- lapply(myjson1, unlist) #losing data types here

asFrame <- lapply(myjson2, as.data.frame, fix.empty.names = TRUE) #convert list elements to data frames

asFrameTrans <- lapply(asFrame, t) #transpose each data frame in the list to make into 1 row, multiple cols

asFrame1 <- ldply(asFrameTrans, as.data.frame, .id = timestamp)

asFrame2 <- lapply(asFrame1, as.character) #conv elements to character

asFrame3 <- lapply(asFrame2, type.convert) #use type convert to coerce to 'correct' types

asFrame4 <- as.data.frame(asFrame3) 

asFrame5 <- asFrame4[asFrame4$event != "Fileheader",] #remove fileheader events

asFrame6 <- subset(asFrame5, select=-c(part,language, gameversion, build)) #remove part and language cols

asFrame6$timestamp <- as.POSIXct(asFrame6$timestamp, format = "%Y-%m-%dT%H:%M:%SZ") #convert to POSIX date&time

asFrame6 <- asFrame6[order(asFrame6$timestamp),] #ensure ordered by date/time

rownames(asFrame6) <- seq(length=nrow(asFrame6)) #ensure row names start at 1


#non vectorized approach to filling star coords

Xpos <- 0
Ypos <- 0
Zpos <- 0
retval <- c("0","0","0")

fillcoords <- function(df) {
  if (df[2] == "FSDJump" | df[2] == "Location") {
    Xpos <<- df[17] 
    Ypos <<- df[18] 
    Zpos <<- df[19]
    retval <<- c(Xpos, Ypos, Zpos)
    return(retval)
  } else {
    return(retval)
  }
}

newobj <- apply(asFrame6, 1, fillcoords) #run fillcoords function
newobj <- t(newobj) #transpose result
newobj <- as.data.frame(newobj) #convert to dataframe
asFrame6[,17:19] <- newobj[,1-3] #replace values in original 

#non vectorized approach to filling StarSystem

StarSys <- "Initialize"
retval <- "Initialize"

fillStarSys <- function(df) {
  if (df[2] == "FSDJump" | df[2] == "Location") {
    StarSys <<- df[16] 
    retval <<- StarSys
    return(retval)
  } else {
    return(retval)
  }
}

newobj <- apply(asFrame6, 1, fillStarSys) #run fillcoords function
newobj <- as.data.frame(newobj) #convert to dataframe
asFrame6[,16] <- newobj[,1] #replace values in original 

write.csv(asFrame6, file="ParsedLogsFull.csv") #write a csv file of the parsed data

#####alternative vectorized approach to filling missing coords values, it's faster

#fillDown<-c('StarPos1','StarPos2','StarPos3')

#myBoolean <- asFrame6$event %in% c("FSDJump","Location")
#myCumSum <- cumsum(myBoolean)
#myRowNum<-seq(nrow(asFrame6))
#myGroup<-split(myRowNum,myCumSum)

# method1
#ignore<-sapply(myGroup,function(memberIndex){asFrame6[memberIndex,fillDown]<<-asFrame6[memberIndex[[1]],fillDown]})

# method2 (preferred)
#g1index<-unlist(sapply(myGroup,function(g) rep(g[[1]],length(g))))
#asFrame6[,fillDown]<-  asFrame6[g1index,fillDown]

######end of alt method for filling coords

#######################
#end of data wrangling#
#######################

names(asFrame6) #get column names

plot(asFrame6$StarType, log(asFrame6$StellarMass)) #plot something

g1 <- ggplot(asFrame6, mapping = aes((Age_MY), AbsoluteMagnitude, color = StarType))

g1 + geom_point()

g2 <- ggplot(asFrame6, mapping = aes(()))

#3d scatter plot of star coords
with(asFrame7, {
  scatterplot3d(StarPos1,   # x axis
                StarPos2,     # y axis
                StarPos3,    # z axis
                pch = 10,
                main="3-D Scatterplot Example 1")
})

#get ready to plot more stuff

asFrame7 <- asFrame6[4000:4500,] #pull out a few rows, can do this conditionally as in line below
asFrame7 <- asFrame7[asFrame7$event %in% c("Location", "Scan", "FSDJump"), ] #subset a few event types

# specify some colors for points to plot

rbPal <- colorRampPalette(c('red','blue')) #define a color gradient
asFrame7$pcolor <- rbPal(10)[as.numeric(cut(asFrame7$FuelLevel,breaks = 10))] #assign colors based on FuelLevel

asFrame7$pcolor[!is.na(asFrame7$Materials.sulphur)] <- "red" #color points by sulphur yes/no
asFrame7$pcolor[!is.na(asFrame7$Materials.iron)] <- "blue"  #color by iron yes/no
#asFrame7$pcolor[asFrame6$StarSystem] <- "darkgreen" #color StarSystem points green

#scatterplot3d - run it using colors set above
with(asFrame7, {
  s3d <- scatterplot3d(StarPos1, StarPos3, StarPos2,        # x y and z axis
                       color=pcolor, pch=19,        # circle color indicates colored points
                       type="h", lty.hplot=2,       # lines to the horizontal plane
                       scale.y=.75,                 # scale y axis (reduce by 25%)
                       main="Journal File 3d Plot Test: Color Gradient = fuel level",
                       xlab="X",
                       ylab="Y",
                       zlab="Z")
  s3d.coords <- s3d$xyz.convert(StarPos1, StarPos3, StarPos2)
  text(s3d.coords$x, s3d.coords$y,     # x and y coordinates
       labels=asFrame6$StarSystem,       # text to plot
       pos=4, cex=.3)                  # shrink text 50% and place to right of points)
  # add the legend
  legend("topleft", inset=.05,      # location and inset
         bty="n", cex=.5,              # suppress legend box, shrink text 50%
         title="Event Type",
         c("ScanResult - Sulphur", "ScanResult - Iron"), fill=c("red", "blue"))
})

#fill coords for events lacking them





