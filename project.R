dat = read.csv("/Users/dariomolina/DataScience/Crash-Report-Project/Crash_Reporting_-_Drivers_Data.csv",stringsAsFactors=FALSE)

x = c("Off.Road.Description", "Related.Non.Motorist", "Non.Motorist.Substance.Abuse")

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

