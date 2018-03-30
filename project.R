dat = read.csv("/Users/evert/Dropbox/School/Assignments/Crash_Reporting_-_Drivers_Data.csv",stringsAsFactors=FALSE)

x = c("Off.Road.Description", "Related.Non.Motorist", "Non.Motorist.Substance.Abuse")

apply(dat,2, function(x) sum(is.na(x)))

#assigning value 0 to "" in x vector
for( i in x)
{
  dat[[i]][dat[[i]] == ""] = 0
}

#assignig rest values to NA
for(i in names(dat))
{
  dat[[i]][dat[[i]] == "" | dat[[i]] == "N/A"] = NA
}

#Types of crashes under type of weather
z = table(dat$Weather,dat$Collision.Type)
y= z[,c("STRAIGHT MOVEMENT ANGLE","SINGLE VEHICLE","SAME DIRECTION RIGHT TURN","SAME DIRECTION LEFT TURN","HEAD ON LEFT TURN","OTHER")]
y = y[c("CLEAR","CLOUDY","RAINING"),]

yCol = c("STRGT-MV-AGL", "ONE-CAR","SM-DIR-R-TR","SM-DIR-L-TR","HD-ON-L-TR","OTHER")
for(i in 1:length(yCol))
{
  colnames(y)[i] = yCol[i]
}
y = y[,c("STRGT-MV-AGL","ONE-CAR","HD-ON-L-TR","OTHER")]
par(mar=c(1,.5,2,.3))
plot(y, main="Most common crash types based on weather", col="firebrick")

#What are the most common types of crashes
par(mar=c(3,14,2,2))
x = table(dat$Collision.Type)
barplot(x[x > 5000], horiz=TRUE, las=1, main="Most common types of car crash", col="firebrick")


#Whattypes of crash types caused the most reports by category

  #remove coluns from crash type 
y = table(dat$ACRS.Report.Type, dat$Collision.Type)
x= c("STRAIGHT MOVEMENT ANGLE","SINGLE VEHICLE","SAME DIR REAR END","OTHER","HEAD ON LEFT TURN")
y = y[,x]

yCol = c("STRGT-MV-AGL", "ONE-CAR","SM-DIR-R-ED","OTHER","HD-ON-L-TR")
for(i in 1:length(yCol))
{
  colnames(y)[i] = yCol[i]
}
par(mar=c(0.5,2,1,2))
plot(y, main="Different types of crashes with their report type", las=1,col="firebrick")

#amound of crashes per month
x =table(substr(dat$Crash.Date.Time,1,2))
names(x) = c("Jan.","Feb.","Mar.","Apr.","May","Jun.","Jul.","Aug.","Sep.","Oct.","Nov.","Dec.")
barplot(x, las = 1, col="firebrick")

#crashes by surface condition
yCol = c("HD-ON-L-TR","OTHER","SM-DIR-R-ED","SM-DIR-SDWE","ONE-CAR","STRGT-MV-AGL")

x = table(dat$Surface.Condition,dat$Collision.Type)


cols = c("HEAD ON LEFT TURN","OTHER","SAME DIR REAR END","SAME DIRECTION SIDESWIPE","SINGLE VEHICLE","STRAIGHT MOVEMENT ANGLE")
rows = c("DRY","WET","SNOW")
z = x[rows,cols]

for(i in 1:length(yCol))
{
  colnames(z)[i] = yCol[i]
}

par(mar=c(1,.01,2,.01))
plot(z, las=1,col="firebrick")
