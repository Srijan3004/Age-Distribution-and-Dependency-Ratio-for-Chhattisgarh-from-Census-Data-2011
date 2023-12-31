library(readxl)
data = read_excel(
  "/Users/srijan/Documents/Academic_Stuffs/B3_resources/ass_or_hw/Eco Stat/Project 3/DDW-2200B-01-Census.xls"
)
data1 = data[-c(1:7), ]
colnames(data1) <- data[1, ]
data1 = data1[, -c(10:ncol(data1))]
colnames(data1)[8] = "Male"
colnames(data1)[9] = "Female"
colnames(data1)[5] = "Sector"

#District, Sector, Gender




result = function(district, sector) {
  subdat = subset(data1, District == district & Sector == sector)
  subdat = subdat[-c(15, 16), c(6, 7, 8, 9)]
  poptot = as.numeric(subdat$Population[1]) - sum(as.numeric(subdat$Population[-1]))
  mtot = as.numeric(subdat$Male[1]) - sum(as.numeric(subdat$Male[-1]))
  ftot = as.numeric(subdat$Female[1]) - sum(as.numeric(subdat$Female[-1]))
  child1 = c("0-5", poptot, mtot, ftot)
  datfin = rbind(subdat[1, ], child1, subdat[-1, ])
  rpop = (sum(as.numeric(datfin$Population[2:4])) + sum(as.numeric(datfin$Population[12:14]))) /
    (as.numeric(datfin$Population[1]) - as.numeric(datfin$Population[15]))
  
  rm = (sum(as.numeric(datfin$Male[2:4])) + sum(as.numeric(datfin$Male[12:14]))) /
    (as.numeric(datfin$Population[1]) - as.numeric(datfin$Population[15]))
  
  rf = (sum(as.numeric(datfin$Female[2:4])) + sum(as.numeric(datfin$Female[12:14]))) /
    (as.numeric(datfin$Population[1]) - as.numeric(datfin$Population[15]))
  
  datfin = datfin[-15,]
  list = list(datfin, rpop, rm, rf)
  
  return(list)
}

district = unique(data1$District)
sector = unique(data1$Sector)
district_name = unique(data1$Area)

for (i in 1:length(district)) {
  res1 = result(district[i], sector[1])
  res2 = result(district[i], sector[2])
  res3 = result(district[i], sector[3])
  print(paste("For", district_name[i], "Age distribution is"))
  print(res1[[1]])
  print(paste("For", district_name[i], "Rural Sector, Age distribution is"))
  print(res2[[1]])
  print(paste("For", district_name[i], "Urban Sector, Age distribution is"))
  print(res3[[1]])
  dep = cbind(c(res1[[2]], res1[[3]], res1[[4]]),
              c(res2[[2]], res2[[3]], res2[[4]]),
              c(res3[[2]], res3[[3]], res3[[4]]))
  
  dep = as.data.frame(dep)
  colnames(dep) = c("Total", "Rural", "Urban")
  rownames(dep) = c("Total", "Male", "Female")
  print("The Dependency ratio is given below:")
  print(dep)
}
