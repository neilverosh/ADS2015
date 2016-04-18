readRE <-function(fname){
  #function that reads the sample from web or file
  sample<-read.csv(file=fname,header=TRUE,sep=',') # loading the data
  sample$sale_year<-as.POSIXlt(sample$sale_date)$year-100 #create year of sale in 2001=1, 2012=12, etc format
  sample$sale_month<-as.POSIXlt(sample$sale_date)$mon #create month of sale (0-Jan,11-Dec)
  return(sample)
}

filterZip <- function( zipcodes, scenario, sample ){
  #filter the sample - only selected zip codes, houses of the same type (scenario)
  samplefilt<-sample[(sample$sale_price>5e4)&(sample$sale_price<1e7)&(sample$gross_sq_feet>300)&(sample$gross_sq_feet<1e6)&(sample$zip_code>0)&(sample$sale_year<=15),] #select price range from 50k till 10M with reasonable square footage
  switch (scenario,
          ind={samplefilt<-samplefilt[(samplefilt$residential_units==1)&(samplefilt$commmercial_units==0),]}, #select single-residence houses
          multi={samplefilt<-samplefilt[(samplefilt$residential_units>=5)&(samplefilt$commmercial_units==0),]}, #select multi-residence houses
          comSingle={samplefilt<-samplefilt[(samplefilt$residential_units==0)&(samplefilt$commmercial_units==1),]}, #select single unit commercial buildings
          comMulti={samplefilt<-samplefilt[(samplefilt$residential_units==0)&(samplefilt$commmercial_units>=5),]} #select multi-unit commercial buildings
  ) 
  samplezip<-samplefilt[(samplefilt$zip_code)%in%zipcodes,]
}

singleFactor<-function(sample){
  ### for a given sample analyze impact of each single factor
  cat('Square footage:',cor(sample$sale_price,sample$gross_sq_feet),'\n')
  cat('Land area:',cor(sample$sale_price,sample$land_sq_feet),'\n')
  cat('Year built:',cor(sample$sale_price,sample$year_built),'\n')
  m<-mean(sample$sale_price)
  aggrMonth<-aggregate(sample$sale_price, by=list(sample$sale_month), FUN = mean)#/aggregate(sample$gross_sq_feet, by=list(sample$sale_month), FUN = mean)
  barplot(names.arg=aggrMonth$Group.1,height=aggrMonth$x-m)
  aggrYear<-aggregate(sample$sale_price, by=list(sample$sale_year), FUN = mean)#/aggregate(sample$gross_sq_feet, by=list(sample$sale_year), FUN = mean)
  barplot(names.arg=aggrYear$Group.1,height=aggrYear$x-m)
  aggrClass<-aggregate(sample$sale_price, by=list(sample$building_class_present), FUN = mean)#/aggregate(sample$gross_sq_feet, by=list(sample$building_class_present), FUN = mean)
  barplot(names.arg=aggrClass$Group.1,height=aggrClass$x-m)
}

splitSample <- function( sample ){
  #split sample (50/50%) into training and validation set
  validind=2*(1:(length(sample$sale_price)/2))
  trainind=trainind-1
  sampletrain<-sample[trainind,]
  samplevalid<-sample[validind,]
  return(list(train=sampletrain, valid=samplevalid))  
}
  
applyRegressionInd <- function( samplevalid,lmfit ){
  #apply regression trained on the other part of the sample and return R2
  pp<-predict(lmfit,newdata=samplevalid)
  RSS<-sum((samplevalid$sale_price-pp)^2)
  R2<-1-RSS/sum((samplevalid$sale_price-mean(samplevalid$sale_price))^2)
  return(R2)
}


sample<-readRE("/Users/stansobolevsky/Desktop/NYU/lectures/data/NYC_real.csv")
samplezip<-filterZip( 11234, 'ind', sample)
singleFactor(samplezip)

#add more fields for regression
samplezip$isAfter2008<-1*(samplezip$sale_year>8)
samplezip$yeartrend<-(!samplezip$isAfter2008)*(samplezip$sale_year)+8*samplezip$isAfter2008
samplezip$isA0<-1*(samplezip$building_class_present=="A0")
samplezip$isA3<-1*(samplezip$building_class_present=="A3")
samplezip$isA4<-1*(samplezip$building_class_present=="A4")
ss<-splitSample(samplezip)

#fit linear model
lmfit <- lm( sale_price ~  gross_sq_feet +   land_sq_feet  +  year_built + yeartrend +  isAfter2008 +   
              isA0 + isA3 + isA4 , data=ss$train ) 

summary(lmfit)

applyRegressionInd(ss$valid,lmfit)


##### multicollinearity example

samplezip2<-filterZip( 10466, 'multi', sample)
print(cor(data.frame(price=samplezip2$sale_price, footage=samplezip2$gross_sq_feet, land=samplezip2$land_sq_feet, units=samplezip2$residential_units)))
#visialize scallerplots
plot(samplezip2$gross_sq_feet,samplezip2$sale_price)
plot(samplezip2$land_sq_feet,samplezip2$sale_price) 
plot(samplezip2$residential_units,samplezip2$sale_price)

lmfit2 = lm( samplezip2$sale_price ~ samplezip2$gross_sq_feet + samplezip2$land_sq_feet + samplezip2$residential_units) #fit linear model
summary(lmfit2) #visualize statistics

