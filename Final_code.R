### TFM ###
# Title: 
# Author: David Merch?n Cano
# Data: 11/09/2020
##################################


# 2. DESCRIPTIVE ANALYSIS --------------------------------------------------------------------


# 2.1. Data  -------------------------------------------------------------------------


### Loading data 
rm(list=ls())

setwd("~/Master_DS/TFM/3.Data")
data=read.csv("SHARE_W7.csv", sep=";",na.strings=" ",dec=",")

### Cleaning data 
data=data[which(data$cciw_w7!="NA"),]
data=data[which(data$country!="Israel"),]
sum(is.na(data$cciw_w7))
class(data$cctaiw_w7)

### Designing the survey

# To design the survey we use the library survey. 

library(survey)
data.w = svydesign(ids = ~ 1, data=data, weights = data$cciw_w7)
summary(data.w)


## 2.2. Indicators --------------------------------------------------------------

### 2.2.1. Economic indicator

### Poverty subindicator
data$Poor2 = data$Poor2[which(data$Poor2!="#N/D")]
data$Poverty = ifelse(data$Poor2==0,0,1)
svytable(~data$Poverty, design=data.w)
barplot(svytable(~data$Poverty, design=data.w))

### Unemployment subindicator
sort(svytable(~data$cjs, design = data.w))
barplot(sort(svytable(~data$cjs, design = data.w)), cex.names=0.6)
data$unemployed = ifelse(data$cjs=="Unemployed",1,0)
svytable(~data$unemployed, data.w)
barplot(svytable(~data$unemployed, data.w))

### Financial distress subindicator
svytable(~data$fdistress,design=data.w)
data$ecostress = ifelse(data$fdistress == "Fairly easily" | data$fdistress == "With great difficulty" | data$fdistress == "With some difficulty",1,0)
svytable(~data$ecostress,data.w)
barplot(svytable(~data$ecostress,data.w), main="Economic distress")

### Weekly consume subindicator

svytable(~data$meat, data.w)
data$meat1 = ifelse(data$meat=="Not applicable", -99, data$meat)
data$meat1 = ifelse(data$meat=="Less than once a week", 0, data$meat1)
data$meat1 = ifelse(data$meat=="Once a week", 1, data$meat1)
data$meat1 = ifelse(data$meat=="Twice a week", 2, data$meat1)
data$meat1 = ifelse(data$meat=="3-6 times a week", 3, data$meat1)
data$meat1 = ifelse(data$meat=="Every day", 4, data$meat1)

svytable(~data$meat1, data.w)
barplot(svytable(~data$meat1, data.w)[2:6])
svytable(~data$legeggs, data.w)
data$legeggs1 = ifelse(data$meat=="Not applicable", -99, data$legeggs)
data$legeggs1 = ifelse(data$meat=="Less than once a week", 0, data$legeggs1)
data$legeggs1 = ifelse(data$meat=="Once a week", 1, data$legeggs1)
data$legeggs1 = ifelse(data$meat=="Twice a week", 2, data$legeggs1)
data$legeggs1 = ifelse(data$meat=="3-6 times a week", 3, data$legeggs1)
data$legeggs1 = ifelse(data$meat=="Every day", 4, data$legeggs1)

svytable(~data$legeggs1, data.w)
barplot(svytable(~data$legeggs1, data.w)[2:6])
data$weekconsume = data$meat1 + data$legeggs1
svytable(~data$weekconsume, data.w)
barplot(svytable(~data$weekconsume, data.w)[2:6])
abline(v=2.5, lty=3, col="red")

svytable(~data$weekconsume, data.w)
barplot(svytable(~data$weekconsume, data.w)[2:6])
abline(v=2.5, lty=3, col="red")
data$nutrition = ifelse(data$weekconsume==2 | data$weekconsume==0, 1,0)
svytable(~data$nutrition,data.w)
barplot(svytable(~data$nutrition,data.w), main="Nutrition")

### Subindicators distribution

jpeg("economicindicators.jpeg")
par(mfrow=c(2,2))
barplot(svytable(~data$Poverty, data.w),  col = c("Light blue","Dark blue"), main="Monetary poverty")
barplot(svytable(~data$unemployed, data.w),legend.text = c("Not vulnerable", "Vulnerable"),col = c("Light blue","Dark blue"), main="Unemployment" )
barplot(svytable(~data$ecostress, data.w), col = c("Light blue","Dark blue"), main = "Financial Distress")
barplot(svytable(~data$nutrition, data.w), col = c("Light blue","Dark blue"), main = "Weekly consume")
dev.off()

# Economic Indicator design

library(scales)
data$ecoind= data$Poverty + data$ecostress + data$nutrition + data$unemployed
data$ecoind=rescale(data$ecoind, to=c(0,10))

library(viridis)
svytable(~data$ecoind, data.w)
barplot(svytable(~data$ecoind, data.w), main="Economic indicator", col="#3182BD")

### 2.2.2. Health indicator

### Mental heatlh subindicator

#### Depression
svytable(~data$eurod,data.w)
data$depress=ifelse(data$eurod==1|data$eurod==2|data$eurod==3|data$eurod==4|data$eurod=="Not depressed",0,1)
svytable(~data$depress, data.w)
barplot(svytable(~data$depress, data.w))

#### Life satisfaction 
svytable(~data$lifesat, data.w)
data$satis = ifelse (data$lifesat==7|data$lifesat==8|data$lifesat==9|data$lifesat=="Completely satisfied",0,1)
svytable(~data$satis, data.w)
barplot(svytable(~data$satis, data.w))

#### Life happiness
svytable(~data$lifehap, data.w)
data$hap = ifelse(data$lifehap=="Never"| data$lifehap=="Rarely",1,0)
svytable(~data$hap, data.w)
barplot(svytable(~data$hap, data.w))

#### Indicator
data$mh = data$depress + data$satis + data$hap
svytable(~data$mh, data.w)
barplot(svytable(~data$mh, data.w))


### Physical health subindicator

##### Limitation with activities
svytable(~data$gali, data.w)
data$limited=ifelse(data$gali=="Limited",1,0)
svytable(~data$limited, data.w)
barplot(svytable(~data$limited, data.w))

##### Mobility limitations
svytable(~data$mobility,data.w)
data$mob = ifelse(data$mobility>0,1,0)
svytable(~data$mob,data.w)
barplot(svytable(~data$mob,data.w))

##### ADL
svytable(~data$adl,data.w)
data$alimit = ifelse(data$adl>0,1,0)
svytable(~data$alimit,data.w)
barplot(svytable(~data$alimit,data.w))

##### IADL
svytable(~data$iadl,data.w)
data$ialimit = ifelse(data$adl>0,1,0)
svytable(~data$ialimit,data.w)
barplot(svytable(~data$ialimit,data.w))

##### BMI

svymean(~data$bmi,data.w)
svyquantile(~data$bmi,data.w,c(0.25,0.5,0.75))
# The BMI categories are:
#   
#   * Underweight: $\leq18.5$
#   * Normal weigth: $18.5 - 24.9$
#   * Overweight: $25-29.9$
#   * Obesity: $\geq30$

svyhist(~data$bmi,data.w,probability = F, main="Histogram of BMI", xlab="BMI")
abline(v=c(18.5,24.9,29.9),lty=3, col=c("red","blue","green"))

data$bmi1 = ifelse(data$bmi<18.5 | data$bmi>25,1,0 )
svytable(~data$bmi1,data.w)
barplot(svytable(~data$bmi1,data.w))


#Physical health subindicator design
data$ph = data$limited+ data$mob + data$bmi1 + data$ialimit  + data$alimit

#Distribution:
svytable(~data$ph,data.w)
barplot(svytable(~data$ph,data.w))

### Health care subindicator

##### Seen/Talked to medical doctor

svytable(~data$doctor,data.w)
barplot(svytable(~data$doctor,data.w))
svyquantile(~data$doctor,data.w, c(0.25,0.5,0.75))
data$dvisit = ifelse(data$doctor==0 | data$doctor>=52,1,0)
svytable(~data$dvisit,data.w)
barplot(svytable(~data$dvisit,data.w))

##### Total nights stayed in hospital

svytable(~data$nhospital,data.w)
data$nhosp = ifelse(data$nhospital>1,1,0)
svytable(~data$nhosp, data.w)
barplot(svytable(~data$nhosp, data.w))


# Health care subindicator design

data$hh= data$dvisit + data$nhosp
svytable(~data$hh, data.w)
barplot(svytable(~data$hh, data.w))

### General health subindicator

#### Self-perceived 
svytable(~data$sphus,data.w)
data$sp=ifelse(data$sphus=="Poor"| data$sphus=="Fair",1,0)
svytable(~data$sp,data.w)
barplot(svytable(~data$sp,data.w))

#### Number of chronic deseases
svytable(~data$chronic,data.w)
data$chdeseases = ifelse(data$chronic>2,1,0)
svytable(~data$chdeseases,data.w)
barplot(svytable(~data$chdeseases,data.w))


# General health subindicator design:

data$gh = data$chdeseases + data$sp

#Distribution: 

svytable(~data$gh,data.w)
barplot(svytable(~data$gh,data.w))

### Subindicators distribution

library(RColorBrewer)
pal <- colorRampPalette(colors = c("lightgreen", "green"))(6)
par(mfrow=c(2,2))
barplot(svytable(~data$mh, data.w),  col = pal, main="Mental Health")
barplot(svytable(~data$ph, data.w),col = pal, main="Physical Health" )
barplot(svytable(~data$hh, data.w), col = pal, main = "Health Care")
barplot(svytable(~data$gh, data.w), col = pal, main = "General Health")


# Health Indicator design
data$healthind = data$mh + data$ph + data$hh + data$gh


#Distribution:
  
svytable(~data$healthind,data.w)
data$healthind=round(rescale(data$healthind,to=c(0,10)))
svytable(~data$healthind,data.w)
library(RColorBrewer)
barplot(svytable(~data$healthind,data.w), col = "#31A354", main="Health indicator")

### Sociodemograpic variables

#### Age

data$age1=ifelse(data$age<=60,1,data$age)
data$age1=ifelse(data$age>60 & data$age<=70,2,data$age1)
data$age1=ifelse(data$age>70 & data$age<=80,3,data$age1)
data$age1=ifelse(data$age>80,4,data$age1)
data$age1=as.factor(data$age1)
levels(data$age1) = c("<60", "61-70", "71-80","80<")

#### Years of Education

table(data$yedu)
svytable(~data$yedu,data.w)
data$yedu1=data$yedu
levels(data$yedu1)=gsub(",",".",levels(data$yedu))
levels(data$yedu1)[36:37]=c(98,99)
levels(data$yedu1)=as.numeric(levels(data$yedu1))

svytable(~data$yedu1, data.w)
data$yedu1=as.numeric(as.character(data$yedu1))
svytable(~data$yedu1, data.w)

data$yedu1 = ifelse(data$yedu1==10.5,10,data$yedu1)
data$yedu1 = ifelse(data$yedu1==17.5,17,data$yedu1)
data$yedu1 = ifelse(data$yedu1==20.5,20,data$yedu1)

svytable(~data$yedu1,data.w)
barplot(svytable(~data$yedu1,data.w))
svyquantile(~data$yedu1,data.w, c(0.25,0.5,0.75))

data$yeduf=ifelse(data$yedu1==98, "0", data$yedu1)
data$yeduf=ifelse(data$yedu1<=9,"1",data$yeduf)
data$yeduf=ifelse(data$yedu1>9 & data$yedu1<=12, "2", data$yeduf)
data$yeduf=ifelse(data$yedu1>12 & data$yedu1<98, "3", data$yeduf)
data$yeduf=ifelse(data$yedu1==99, "4", data$yeduf)

# data$yeduf=ifelse(data$yedu1==98, "0", data$yeduf)
# data$yeduf=ifelse(data$yedu1<=8,"1",data$yedu1)
# data$yeduf=ifelse(data$yedu1>8 & data$yedu1<=11, "2", data$yeduf)
# data$yeduf=ifelse(data$yedu1>11 & data$yedu1<98, "3", data$yeduf)
# data$yeduf=ifelse(data$yedu1==99, "5", data$yeduf)

data$yeduf = as.factor(data$yeduf)
svytable(~data$yeduf,data.w)
levels(data$yeduf) = c("Never went to school","At least 9", "10 - 12", "12<", "Still in school")

### Descriptive analysis 

#### Economic and Health indicator per gender
library(viridis)
par(mfrow=c(1,2))
boxplot(data$ecoind~data$gender, design=data.w, xlab="Gender", ylab="Economical indicator", col=viridis(2))
boxplot(data$healthind~data$gender, design=data.w, xlab="Gender", ylab="Health indicator", col=viridis(2))


#### Economic and health indicator per single or couple. 
par(mfrow=c(1,2))
dstrat1 <- svydesign(id = ~1, strata = ~single, weights = ~cciw_w7, data = data)
svyboxplot(ecoind~single,dstrat1,all.outliers=TRUE, xlab="Single", ylab="Economic indicator", col=viridis(2))
svyboxplot(healthind~single, dstrat1, all.outliers = TRUE, xlab="Single", ylab="Health indicator", col=viridis(2))

### Economic and health indicator per age. 
library("viridis")
par(mfrow=c(1,2))
boxplot(data$ecoind~data$age1, design=data.w, xlab="Age (years)", cex.axis=0.6, ylab="Economic indicator", col=viridis(4))
boxplot(data$healthind~data$age1, design=data.w, xlab="Age (years)", cex.axis=0.6, ylab="Health indicator",  col=viridis(4))

### Economic and health indicator per years of education. 
par(mfrow=c(1,2))
boxplot(data$ecoind~data$yeduf, design=data.w, xlab="Education", cex.axis=0.5, ylab="Economic indicator", col=viridis(6), las=2)
boxplot(data$healthind~data$yeduf, design=data.w, xlab="Education", cex.axis=0.5, ylab="Health indicator",  col=viridis(6), las=2)


### Economic vs health indicator


#### Mean value of Economic indicator per country
countries = unique(data$country)
countrymean1 = NULL
countrymedian1 = NULL

for (i in countries){
  countrymean1[i]=svymean(~data$ecoind[data$country==i], design=subset(data.w, data$country==i))
  countrymedian1[i]=svyquantile(~data$ecoind[data$country==i], design=subset(data.w, data$country==i), quantiles=0.5)
}
barplot(sort(countrymean1), las=2)

#### Mean value of health indicator per country
countries = unique(data$country)
countrymean2 = NULL
countrymedian2 = NULL

for (i in countries){
  countrymean2[i]=svymean(~data$healthind[data$country==i], design=subset(data.w, data$country==i))
  countrymedian2[i]=svyquantile(~data$healthind[data$country==i], design=subset(data.w, data$country==i), quantiles=0.5)
} 

barplot(sort(countrymean2), las=2)

library(ggplot2)

data3=as.data.frame(cbind(countrymean1,countrymean2))
ggplot(data3) + aes(y=countrymean1, x=countrymean2, label = rownames(data3)) + geom_point() + ylab("Economic Indicator") + xlab("Health Indicator") + xlim(2.25,4.25) + geom_text(aes(label=rownames(data3)))



### Maps

# In this section we created two european maps to analyzed the indicator across Europe.

datacountries = data.frame(countrymean1,countrymean2)
View(datacountries)

library(maptools)
data(wrld_simpl)
is(wrld_simpl)


euro_simpl=wrld_simpl[wrld_simpl@data$REGION==150,] #Only european countries
euro_simpl=euro_simpl[euro_simpl@data$UN!=744,]

euro_ggmap <- fortify(euro_simpl, region = "ISO3")
head(euro_ggmap)

ggplot(data=euro_ggmap, aes(x=long, y=lat, group=group)) + geom_polygon()

View(euro_ggmap)
datacountries[,3]=rownames(datacountries)
names(datacountries)[3]="NAME"
View(datacountries)
temp=euro_simpl@data[,c(3,5)]
datacountries=merge(datacountries,temp,by="NAME")


### Economic indicator
ggplot(datacountries) +
  geom_map(aes(map_id = ISO3, fill=countrymean1), map = euro_ggmap) +
  #expand_limits(x = euro_ggmap$long, y = euro_ggmap$lat)+
  coord_fixed (xlim= c(-9,30),
               ylim= c(36,71)) + 
  labs(title="Economic indicator") + 
  scale_fill_gradientn(name="Mean of the indicator per country", colours = viridis(5), limits=c(1,4.5))

### Health indicator
ggplot(datacountries) +
  geom_map(aes(map_id = ISO3, fill=countrymean2), map = euro_ggmap) +
  #expand_limits(x = euro_ggmap$long, y = euro_ggmap$lat)+
  coord_fixed (xlim= c(-9,30),
               ylim= c(36,71)) + 
  labs(title="Health indicator") + 
  scale_fill_gradientn(name="Mean of the indicator per country", colours = viridis(5), limits=c(1,4.5))


# 4. RESULTS -------------------------------------------------------------------------


# 4.1. Profiles -----------------------------------------------------------

## K selection


library(clustMixType)




dataf=data.frame(data$healthind,data$ecoind,data$gender,data$single, data$age1,data$yeduf,data$cciw_w7)
 

Es=NULL

set.seed(1234)

# The next computation is based on the kproto function from ClustMixType Package but with modifitications
# to use dataset with weights


for (h in 1:10){
  weights=dataf$data.cciw_w7
  lambda=NULL
  iter.max = 100
  nstart =1 # To reduce the random effect 
  na.rm = TRUE
  keep.data = TRUE
  verbose = TRUE
  k=h
  
  x = dataf[,-7]
  x.w = svydesign(ids= ~1,data=x, weights = weights)
  
  # initial error checks
  if(!is.data.frame(x)) stop("x should be a data frame!")
  if(ncol(x) < 2) stop("For clustering x should contain at least two variables!")
  if(iter.max < 1 | nstart < 1) stop("iter.max and nstart must not be specified < 1!")
  if(!is.null(lambda)){
    if(any(lambda < 0)) stop("lambda must be specified >= 0!")
    if(!any(lambda > 0)) stop("lambda must be specified > 0 for at least one variable!")
  }
  # check for numeric and factor variables
  numvars <- sapply(x, is.numeric)
  anynum <- any(numvars)
  catvars <- sapply(x, is.factor)
  anyfact <- any(catvars)
  if(!anynum) stop("\n No numeric variables in x! Try using kmodes() from package klaR...\n\n")
  if(!anyfact) stop("\n No factor variables in x! Try using kmeans()...\n\n")
  
  # treatment of missings
  NAcount <- apply(x, 2, function(z) sum(is.na(z)))
  if(verbose){
    cat("# NAs in variables:\n")
    print(NAcount)
  }
  if(any(NAcount == nrow(x))) stop(paste("Variable(s) have only NAs please remove them:",names(NAcount)[NAcount == nrow(x)],"!"))
  if(na.rm) {
    miss <- apply(x, 1, function(z) any(is.na(z)))
    if(verbose){
      cat(sum(miss), "observation(s) with NAs.\n")
      if(sum(miss) > 0) message("Observations with NAs are removed.\n")
      cat("\n")
    } 
    x <- x[!miss,]
  } # remove missings
  
  if(!na.rm){
    allNAs <- apply(x,1,function(z) all(is.na(z)))
    if(sum(allNAs) > 0){
      if(verbose) cat(sum(allNAs), "observation(s) where all variables NA.\n")
      warning("No meaningful cluster assignment possible for observations where all variables NA.\n")
      if(verbose) cat("\n")
      
    }
  }
  
  if(nrow(x) == 1) stop("Only one observation clustering not meaningful.")
  
  k_input <- k # store input k for nstart > 1 as clusters can be merged 
  
  # initialize prototypes
  if(!is.data.frame(k)){
    if (length(k) == 1){
      if(as.integer(k) != k){k <- as.integer(k); warning(paste("k has been set to", k,"!"))}
      if(sum(complete.cases(x)) < k) stop("Data frame has less complete observations than clusters!")
      ids <- sample(row.names(x[complete.cases(x),]), k)
      protos <- x[ids,]
    }
    if (length(k) > 1){
      if(nrow(x) < length(k)) stop("Data frame has less observations than clusters!")
      ids <- k
      k <- length(ids)
      if(length(unique(ids)) != length(ids)) stop("If k is specified as a vector it should contain different indices!")
      if(any(ids<1)|any(ids>nrow(x))) stop("If k is specified as a vector all elements must be valid indices of x!")
      #check for integer
      protos <- x[ids,]
      if(any(!complete.cases(protos))) stop("Choose initial prototypes without missing values!")
    }
    rm(ids)
  }
  if(is.data.frame(k)){
    if(nrow(x) < nrow(k)) stop("Data frame has less observations than clusters!")
    if(length(names(k)) != length(names(x))) stop("k and x have different numbers of columns!")
    if(any(names(k) != names(x))) stop("k and x have different column names!")
    if(anynum) {if( any(sapply(k, is.numeric) != numvars)) stop("Numeric variables of k and x do not match!")}
    if(anyfact) {if( any(sapply(k, is.factor) != catvars)) stop("Factor variables of k and x do not match!")}
    protos <- k
    if(any(!complete.cases(protos))) stop("Prototypes with missing values. Choose initial prototypes without missing values!")
    k <- nrow(protos)
  }
  if(k < 1) stop("Number of clusters k must not be smaller than 1!")

# Here we add functions from survey package to compute de variance for the numerical 
# and categorical variables: svyvar and svytable
  
  # automatic calculation of lambda
  if(length(lambda) > 1) {if(length(lambda) != sum(c(numvars,catvars))) stop("If lambda is a vector, its length should be the sum of numeric and factor variables in the data frame!")}
  if(is.null(lambda)){
    if(anynum & anyfact){
      vnum <- mean(sapply(x[,numvars, drop = FALSE], svyvar, design=x.w, na.rm = TRUE))
      myf=x[,catvars, drop = FALSE]
      r=rep(NA,ncol(myf))
      for (i in 1:ncol(myf)){
        r[i]=1-sum((svytable(~myf[,i],x.w)/sum(svytable(~myf[,i],x.w)))^2)
      }
      vcat <- mean(r)
      if (vnum == 0){
        if(verbose) warning("All numerical variables have zero variance.")
        anynum <- FALSE
      } 
      if (vcat == 0){
        if(verbose) warning("All categorical variables have zero variance.")
        anyfact <- FALSE
      } 
      if(anynum & anyfact){
        lambda <- vnum/vcat
        if(verbose) cat("Estimated lambda:", lambda, "\n\n")
      }else{
        lambda <- 1
      }
    }
  }
  
  # initialize clusters
  clusters  <- numeric(nrow(x)) 
  tot.dists <- NULL
  moved   <- NULL
  iter <- 1
  
  # check for any equal prototypes and reduce cluster number in case of occurence
  if(k > 1){
    keep.protos <- rep(TRUE,k)
    for(l in 1:(k-1)){
      for(m in (l+1):k){
        d1 <- sum((protos[l,numvars, drop = FALSE]-protos[m,numvars, drop = FALSE])^2) # euclidean for numerics
        d2 <- sum(protos[l,catvars, drop = FALSE] != protos[m,catvars, drop = FALSE]) # wtd simple matching for categorics 
        if((d1+d2) == 0) keep.protos[m] <- FALSE 
      }
    }
    if(!all(keep.protos)){
      protos <- protos[keep.protos,]
      k <- sum(keep.protos)
      if(verbose) message("Equal prototypes merged. Cluster number reduced to:", k, "\n\n")      
    }
  }
  
  # special case only one cluster
  if(k == 1){clusters <- rep(1, nrow(x)); size  <- table(clusters); iter <- iter.max} # REM: named vector size is needed later...
  
  
  
  # start iterations for standard case (i.e. k > 1)
  while(iter < iter.max){
    
    # compute distances 
    nrows <- nrow(x)
    dists <- matrix(NA, nrow=nrows, ncol = k)
    for(i in 1:k){
      #a0 <- proc.time()[3]      
      #d1 <- apply(x[,numvars],1, function(z) sum((z-protos[i,numvars])^2)) # euclidean for numerics
      d1 <- (x[,numvars, drop = FALSE] - matrix(rep(as.numeric(protos[i, numvars, drop = FALSE]), nrows), nrow=nrows, byrow=T))^2
      if(length(lambda) == 1) d1 <- rowSums(d1, na.rm = TRUE)
      if(length(lambda) > 1) d1 <- as.matrix(d1) %*% lambda[numvars]
      #a1 <- proc.time()[3]      
      #d2 <- lambda * apply(x[,catvars],1, function(z) sum((z != protos[i,catvars]))) # wtd simple matching for categorics 
      d2 <- sapply(which(catvars), function(j) return(x[,j] != rep(protos[i,j], nrows)) )
      d2[is.na(d2)] <- FALSE
      if(length(lambda) == 1) d2 <- lambda * rowSums(d2)
      if(length(lambda) > 1) d2 <- as.matrix(d2) %*% lambda[catvars]
      #a2 <- proc.time()[3]      
      dists[,i] <- d1 + d2
      #cat(a1-a0, a2-a1, "\n")
    }
    
    
    
    # assign clusters 
    old.clusters  <- clusters
    # clusters      <- apply(dists, 1, function(z) which.min(z))
    clusters      <- apply(dists, 1, function(z) {a <- which(z == min(z)); if (length(a)>1) a <- sample(a,1); return(a)}) # sample in case of multiple minima
    size          <- svytable(~clusters,x.w)  
    min.dists     <- apply(cbind(clusters, dists), 1, function(z) z[z[1]+1])
    within        <- as.numeric(by(min.dists, clusters, sum))
    tot.within    <- sum(within)
    # prevent from empty classes
    #tot.within    <- numeric(k)
    #totw.list     <- by(min.dists, clusters, sum) 
    #tot.within[names(totw.list)] <- as.numeric(totw.list)
    
    # ...check for empty clusters and eventually reduce number of prototypes    
    if (length(size) < k){
      k <- length(size)
      protos <- protos[1:length(size),]  
      if(verbose) cat("Empty clusters occur. Cluster number reduced to:", k, "\n\n")
    }
    
    # trace
    tot.dists <- c(tot.dists, sum(tot.within))      
    moved <- c(moved, sum(clusters != old.clusters))
    
    # compute new prototypes
    remids <- as.integer(names(size))
    for(i in remids){
      some_vals <- sapply(x[clusters == i, , drop = FALSE], function(z) !all(is.na(z))) # only update variables if not all values are NA
      if(any(some_vals & numvars)){
        protos[which(remids == i), some_vals & numvars] <- sapply(x[clusters == i, some_vals & numvars, drop = FALSE], svymean, design=subset(x.w, clusters == i), na.rm = TRUE)
      }
      if(any(some_vals & catvars)){
        myo = x[clusters == i, some_vals & catvars, drop = FALSE]
        sbset=subset(x.w,clusters==i)
        for (j in 1:ncol(myo)){
          protos[which(remids == i), some_vals & catvars][j]=levels(myo[,j])[which.max(svytable(~ myo[,j],design=sbset))]
        }      
      }
    }
    
    if(k == 1){clusters <- rep(1, length(clusters)); size <- table(clusters); iter <- iter.max; break}
    
    # check for any equal prototypes and reduce cluster number in case of occurence
    if(iter == (iter.max-1)){ # REM: for last iteration equal prototypes are allowed. otherwise less prototypes than assigned clusters.
      keep.protos <- rep(TRUE,k)
      for(l in 1:(k-1)){
        for(m in (l+1):k){
          d1 <- sum((protos[l,numvars, drop = FALSE]-protos[m,numvars, drop = FALSE])^2) # euclidean for numerics
          d2 <- sum(protos[l,catvars, drop = FALSE] != protos[m,catvars, drop = FALSE]) # wtd simple matching for categorics 
          if((d1+d2) == 0) keep.protos[m] <- FALSE 
        }
      }
      if(!all(keep.protos)){
        protos <- protos[keep.protos,]
        k <- sum(keep.protos)
        if(verbose) cat("Equal prototypes merged. Cluster number reduced to:", k, "\n\n")      
      }
    }
    
    # add stopping rules
    if(moved[length(moved)] ==  0) break
    
    if(k == 1){clusters <- rep(1, length(clusters)); size <- table(clusters); iter <- iter.max; break}
    
    #cat("iter", iter, "moved", moved[length(moved)], "tot.dists",tot.dists[length(tot.dists)],"\n" )      
    iter <- iter+1
  }
  
  
  ### Final update of prototypes and dists
  
  if(iter == iter.max){ # otherwise there have been no moves anymore and prototypes correspond to cluster assignments 
    # compute new prototypes
    remids <- as.integer(names(size))
    for(i in remids){
      some_vals <- sapply(x[clusters == i, , drop = FALSE], function(z) !all(is.na(z))) # only update variables if not all values are NA
      if(any(some_vals & numvars)){
        protos[which(remids == i), some_vals & numvars] <- sapply(x[clusters == i, some_vals & numvars, drop = FALSE], svymean, design=subset(x.w, clusters == i), na.rm = TRUE)
      }
      if(any(some_vals & catvars)){
        o = x[clusters == i, some_vals & catvars, drop = FALSE]
        for (j in 1:ncol(o)){
          protos[which(remids == i), some_vals & catvars][j]=levels(o[,j])[which.max(svytable(~o[,j],subset(x.w,clusters==i)))]
        }   
      }
    }
    
    # compute distances 
    nrows <- nrow(x)
    dists <- matrix(NA, nrow=nrows, ncol = k)
    for(i in 1:k){
      d1 <- (x[,numvars, drop = FALSE] - matrix(rep(as.numeric(protos[i, numvars, drop = FALSE]), nrows), nrow=nrows, byrow=T))^2
      if(length(lambda) == 1) d1 <- rowSums(d1, na.rm = TRUE)
      if(length(lambda) > 1) d1 <- as.matrix(d1) %*% lambda[numvars]
      d2 <- sapply(which(catvars), function(j) return(x[,j] != rep(protos[i,j], nrows)) )
      d2[is.na(d2)] <- FALSE
      if(length(lambda) == 1) d2 <- lambda * rowSums(d2)
      if(length(lambda) > 1) d2 <- as.matrix(d2) %*% lambda[catvars]
      dists[,i] <- d1 + d2
    }
    
    size          <- svytable(~clusters,x.w)  
    min.dists     <- apply(cbind(clusters, dists), 1, function(z) z[z[1]+1])
    within        <- as.numeric(by(min.dists, clusters, sum))
    tot.within    <- sum(within)
  }
  
  if(na.rm == FALSE){
    if(sum(allNAs) > 0){
      clusters[allNAs] <- NA
      dists[allNAs,] <- NA
    }
  }
  
  names(clusters) <- row.names(dists) <- row.names(x)
  rownames(protos) <- NULL
  # create result: 
  res <- list(cluster = clusters,  
              centers = protos, 
              lambda = lambda, 
              size = size,
              withinss = within,
              tot.withinss = tot.within,   
              dists = dists, 
              iter = iter, 
              trace = list(tot.dists = tot.dists, moved = moved))
  
  # loop: if nstart > 1:
  
  if(nstart > 1)
    for(j in 2:nstart){
      res.new <- kproto(x=x, k=k_input, lambda = lambda,  iter.max = iter.max, nstart=1, verbose=verbose, na.rm = na.rm)
      if(res.new$tot.withinss < res$tot.withinss) res <- res.new
    }  
  
  if(keep.data) res$data = x
  class(res) <- "kproto"
  res
  Es[h] = res$tot.withinss
}

# We plot the vulue of the objective function to know the optimal number of K

plot(1:10, Es, type = "b", ylab = "Objective Function", xlab = "# Clusters",
     main = "Scree Plot")

### Profiles

## Clusters

# We had a issue to use the next code as function, for that reason we use the entire code
# to compute the profiles

set.seed(1234)

weights=dataf$data.cciw_w7
lambda=NULL
iter.max = 100
nstart = 1
na.rm = TRUE
keep.data = TRUE
verbose = TRUE
k=4  

x = dataf[,-7]
x.w = svydesign(ids= ~1,data=x, weights = weights)

# initial error checks
if(!is.data.frame(x)) stop("x should be a data frame!")
if(ncol(x) < 2) stop("For clustering x should contain at least two variables!")
if(iter.max < 1 | nstart < 1) stop("iter.max and nstart must not be specified < 1!")
if(!is.null(lambda)){
  if(any(lambda < 0)) stop("lambda must be specified >= 0!")
  if(!any(lambda > 0)) stop("lambda must be specified > 0 for at least one variable!")
}
# check for numeric and factor variables
numvars <- sapply(x, is.numeric)
anynum <- any(numvars)
catvars <- sapply(x, is.factor)
anyfact <- any(catvars)
if(!anynum) stop("\n No numeric variables in x! Try using kmodes() from package klaR...\n\n")
if(!anyfact) stop("\n No factor variables in x! Try using kmeans()...\n\n")

# treatment of missings
NAcount <- apply(x, 2, function(z) sum(is.na(z)))
if(verbose){
  cat("# NAs in variables:\n")
  print(NAcount)
}
if(any(NAcount == nrow(x))) stop(paste("Variable(s) have only NAs please remove them:",names(NAcount)[NAcount == nrow(x)],"!"))
if(na.rm) {
  miss <- apply(x, 1, function(z) any(is.na(z)))
  if(verbose){
    cat(sum(miss), "observation(s) with NAs.\n")
    if(sum(miss) > 0) message("Observations with NAs are removed.\n")
    cat("\n")
  } 
  x <- x[!miss,]
} # remove missings

if(!na.rm){
  allNAs <- apply(x,1,function(z) all(is.na(z)))
  if(sum(allNAs) > 0){
    if(verbose) cat(sum(allNAs), "observation(s) where all variables NA.\n")
    warning("No meaningful cluster assignment possible for observations where all variables NA.\n")
    if(verbose) cat("\n")
    
  }
}

if(nrow(x) == 1) stop("Only one observation clustering not meaningful.")

k_input <- k # store input k for nstart > 1 as clusters can be merged 

# initialize prototypes
if(!is.data.frame(k)){
  if (length(k) == 1){
    if(as.integer(k) != k){k <- as.integer(k); warning(paste("k has been set to", k,"!"))}
    if(sum(complete.cases(x)) < k) stop("Data frame has less complete observations than clusters!")
    ids <- sample(row.names(x[complete.cases(x),]), k)
    protos <- x[ids,]
  }
  if (length(k) > 1){
    if(nrow(x) < length(k)) stop("Data frame has less observations than clusters!")
    ids <- k
    k <- length(ids)
    if(length(unique(ids)) != length(ids)) stop("If k is specified as a vector it should contain different indices!")
    if(any(ids<1)|any(ids>nrow(x))) stop("If k is specified as a vector all elements must be valid indices of x!")
    #check for integer
    protos <- x[ids,]
    if(any(!complete.cases(protos))) stop("Choose initial prototypes without missing values!")
  }
  rm(ids)
}
if(is.data.frame(k)){
  if(nrow(x) < nrow(k)) stop("Data frame has less observations than clusters!")
  if(length(names(k)) != length(names(x))) stop("k and x have different numbers of columns!")
  if(any(names(k) != names(x))) stop("k and x have different column names!")
  if(anynum) {if( any(sapply(k, is.numeric) != numvars)) stop("Numeric variables of k and x do not match!")}
  if(anyfact) {if( any(sapply(k, is.factor) != catvars)) stop("Factor variables of k and x do not match!")}
  protos <- k
  if(any(!complete.cases(protos))) stop("Prototypes with missing values. Choose initial prototypes without missing values!")
  k <- nrow(protos)
}
if(k < 1) stop("Number of clusters k must not be smaller than 1!")

# automatic calculation of lambda
if(length(lambda) > 1) {if(length(lambda) != sum(c(numvars,catvars))) stop("If lambda is a vector, its length should be the sum of numeric and factor variables in the data frame!")}
if(is.null(lambda)){
  if(anynum & anyfact){
    vnum <- mean(sapply(x[,numvars, drop = FALSE], svyvar, design=x.w, na.rm = TRUE))
    myf=x[,catvars, drop = FALSE]
    r=rep(NA,ncol(myf))
    for (i in 1:ncol(myf)){
      r[i]=1-sum((svytable(~myf[,i],x.w)/sum(svytable(~myf[,i],x.w)))^2)
    }
    vcat <- mean(r)
    if (vnum == 0){
      if(verbose) warning("All numerical variables have zero variance.")
      anynum <- FALSE
    } 
    if (vcat == 0){
      if(verbose) warning("All categorical variables have zero variance.")
      anyfact <- FALSE
    } 
    if(anynum & anyfact){
      lambda <- vnum/vcat
      if(verbose) cat("Estimated lambda:", lambda, "\n\n")
    }else{
      lambda <- 1
    }
  }
}

# initialize clusters
clusters  <- numeric(nrow(x)) 
tot.dists <- NULL
moved   <- NULL
iter <- 1

# check for any equal prototypes and reduce cluster number in case of occurence
if(k > 1){
  keep.protos <- rep(TRUE,k)
  for(l in 1:(k-1)){
    for(m in (l+1):k){
      d1 <- sum((protos[l,numvars, drop = FALSE]-protos[m,numvars, drop = FALSE])^2) # euclidean for numerics
      d2 <- sum(protos[l,catvars, drop = FALSE] != protos[m,catvars, drop = FALSE]) # wtd simple matching for categorics 
      if((d1+d2) == 0) keep.protos[m] <- FALSE 
    }
  }
  if(!all(keep.protos)){
    protos <- protos[keep.protos,]
    k <- sum(keep.protos)
    if(verbose) message("Equal prototypes merged. Cluster number reduced to:", k, "\n\n")      
  }
}

# special case only one cluster
if(k == 1){clusters <- rep(1, nrow(x)); size  <- table(clusters); iter <- iter.max} # REM: named vector size is needed later...



# start iterations for standard case (i.e. k > 1)
while(iter < iter.max){
  
  # compute distances 
  nrows <- nrow(x)
  dists <- matrix(NA, nrow=nrows, ncol = k)
  for(i in 1:k){
    #a0 <- proc.time()[3]      
    #d1 <- apply(x[,numvars],1, function(z) sum((z-protos[i,numvars])^2)) # euclidean for numerics
    d1 <- (x[,numvars, drop = FALSE] - matrix(rep(as.numeric(protos[i, numvars, drop = FALSE]), nrows), nrow=nrows, byrow=T))^2
    if(length(lambda) == 1) d1 <- rowSums(d1, na.rm = TRUE)
    if(length(lambda) > 1) d1 <- as.matrix(d1) %*% lambda[numvars]
    #a1 <- proc.time()[3]      
    #d2 <- lambda * apply(x[,catvars],1, function(z) sum((z != protos[i,catvars]))) # wtd simple matching for categorics 
    d2 <- sapply(which(catvars), function(j) return(x[,j] != rep(protos[i,j], nrows)) )
    d2[is.na(d2)] <- FALSE
    if(length(lambda) == 1) d2 <- lambda * rowSums(d2)
    if(length(lambda) > 1) d2 <- as.matrix(d2) %*% lambda[catvars]
    #a2 <- proc.time()[3]      
    dists[,i] <- d1 + d2
    #cat(a1-a0, a2-a1, "\n")
  }
  
  
  
  # assign clusters 
  old.clusters  <- clusters
  # clusters      <- apply(dists, 1, function(z) which.min(z))
  clusters      <- apply(dists, 1, function(z) {a <- which(z == min(z)); if (length(a)>1) a <- sample(a,1); return(a)}) # sample in case of multiple minima
  size          <- svytable(~clusters,x.w)  
  min.dists     <- apply(cbind(clusters, dists), 1, function(z) z[z[1]+1])
  within        <- svyby(min.dists,clusters,x.w, svytotal)[2][,1]
  tot.within    <- sum(within)
  # prevent from empty classes
  #tot.within    <- numeric(k)
  #totw.list     <- by(min.dists, clusters, sum) 
  #tot.within[names(totw.list)] <- as.numeric(totw.list)
  
  # ...check for empty clusters and eventually reduce number of prototypes    
  if (length(size) < k){
    k <- length(size)
    protos <- protos[1:length(size),]  
    if(verbose) cat("Empty clusters occur. Cluster number reduced to:", k, "\n\n")
  }
  
  # trace
  tot.dists <- c(tot.dists, sum(tot.within))      
  moved <- c(moved, sum(clusters != old.clusters))
  
  # compute new prototypes
  remids <- as.integer(names(size))
  for(i in remids){
    some_vals <- sapply(x[clusters == i, , drop = FALSE], function(z) !all(is.na(z))) # only update variables if not all values are NA
    if(any(some_vals & numvars)){
      protos[which(remids == i), some_vals & numvars] <- sapply(x[clusters == i, some_vals & numvars, drop = FALSE], svymean, design=subset(x.w, clusters == i), na.rm = TRUE)
    }
    if(any(some_vals & catvars)){
      myo = x[clusters == i, some_vals & catvars, drop = FALSE]
      sbset=subset(x.w,clusters==i)
      for (j in 1:ncol(myo)){
        protos[which(remids == i), some_vals & catvars][j]=levels(myo[,j])[which.max(svytable(~ myo[,j],design=sbset))]
      }      
    }
  }
  
  if(k == 1){clusters <- rep(1, length(clusters)); size <- table(clusters); iter <- iter.max; break}
  
  # check for any equal prototypes and reduce cluster number in case of occurence
  if(iter == (iter.max-1)){ # REM: for last iteration equal prototypes are allowed. otherwise less prototypes than assigned clusters.
    keep.protos <- rep(TRUE,k)
    for(l in 1:(k-1)){
      for(m in (l+1):k){
        d1 <- sum((protos[l,numvars, drop = FALSE]-protos[m,numvars, drop = FALSE])^2) # euclidean for numerics
        d2 <- sum(protos[l,catvars, drop = FALSE] != protos[m,catvars, drop = FALSE]) # wtd simple matching for categorics 
        if((d1+d2) == 0) keep.protos[m] <- FALSE 
      }
    }
    if(!all(keep.protos)){
      protos <- protos[keep.protos,]
      k <- sum(keep.protos)
      if(verbose) cat("Equal prototypes merged. Cluster number reduced to:", k, "\n\n")      
    }
  }
  
  # add stopping rules
  if(moved[length(moved)] ==  0) break
  
  if(k == 1){clusters <- rep(1, length(clusters)); size <- table(clusters); iter <- iter.max; break}
  
  #cat("iter", iter, "moved", moved[length(moved)], "tot.dists",tot.dists[length(tot.dists)],"\n" )      
  iter <- iter+1
}


### Final update of prototypes and dists

if(iter == iter.max){ # otherwise there have been no moves anymore and prototypes correspond to cluster assignments 
  # compute new prototypes
  remids <- as.integer(names(size))
  for(i in remids){
    some_vals <- sapply(x[clusters == i, , drop = FALSE], function(z) !all(is.na(z))) # only update variables if not all values are NA
    if(any(some_vals & numvars)){
      protos[which(remids == i), some_vals & numvars] <- sapply(x[clusters == i, some_vals & numvars, drop = FALSE], svymean, design=subset(x.w, clusters == i), na.rm = TRUE)
    }
    if(any(some_vals & catvars)){
      o = x[clusters == i, some_vals & catvars, drop = FALSE]
      for (j in 1:ncol(o)){
        protos[which(remids == i), some_vals & catvars][j]=levels(o[,j])[which.max(svytable(~o[,j],subset(x.w,clusters==i)))]
      }   
    }
  }
  
  # compute distances 
  nrows <- nrow(x)
  dists <- matrix(NA, nrow=nrows, ncol = k)
  for(i in 1:k){
    d1 <- (x[,numvars, drop = FALSE] - matrix(rep(as.numeric(protos[i, numvars, drop = FALSE]), nrows), nrow=nrows, byrow=T))^2
    if(length(lambda) == 1) d1 <- rowSums(d1, na.rm = TRUE)
    if(length(lambda) > 1) d1 <- as.matrix(d1) %*% lambda[numvars]
    d2 <- sapply(which(catvars), function(j) return(x[,j] != rep(protos[i,j], nrows)) )
    d2[is.na(d2)] <- FALSE
    if(length(lambda) == 1) d2 <- lambda * rowSums(d2)
    if(length(lambda) > 1) d2 <- as.matrix(d2) %*% lambda[catvars]
    dists[,i] <- d1 + d2
  }
  
  size          <- svytable(~clusters,x.w)  
  min.dists     <- apply(cbind(clusters, dists), 1, function(z) z[z[1]+1])
  within        <- svyby(min.dists,clusters,x.w, svytotal)[2][,1]
  tot.within    <- sum(within)
}

if(na.rm == FALSE){
  if(sum(allNAs) > 0){
    clusters[allNAs] <- NA
    dists[allNAs,] <- NA
  }
}

names(clusters) <- row.names(dists) <- row.names(x)
rownames(protos) <- NULL
# create result: 
res <- list(cluster = clusters,  
            centers = protos, 
            lambda = lambda, 
            size = size,
            withinss = within,
            tot.withinss = tot.within,   
            dists = dists, 
            iter = iter, 
            trace = list(tot.dists = tot.dists, moved = moved))

# loop: if nstart > 1:

if(nstart > 1)
  for(j in 2:nstart){
    res.new <- kproto(x=x, k=k_input, lambda = lambda,  iter.max = iter.max, nstart=1, verbose=verbose, na.rm = na.rm)
    if(res.new$tot.withinss < res$tot.withinss) res <- res.new
  }  

if(keep.data) res$data = x
class(res) <- "kproto"
res
k4=res

# Printing the results 
k4


###  Descriptive analysys for the profiles
object=k4
dstrat1 <- svydesign(id = ~1, strata = ~as.factor(object$cluster), weights = ~data.cciw_w7, data = dataf)
col=viridis(k)
object=k4

#### Economic and Health indicator
par(mfrow=c(1,2))
svyboxplot(data.ecoind~as.factor(k4$cluster),dstrat1,all.outliers=TRUE, col = col, main = "Economic indicator", xlab = " # Cluster")
#legend("topright", c("1","2","3", "4"), fill = col, title="Cluster")
svyboxplot(data.healthind~as.factor(k4$cluster),dstrat1,all.outliers=TRUE, col = col, main = "Health indicator", xlab = " # Cluster")
#legend("topright", c("1","2","3", "4"), fill = col, title="Cluster")

#### Gender
tab <- svytable(~dataf$data.gender+k4$cluster, dstrat1)
for(j in 1:length(k4$size)) tab[,j] <- (tab[,j]/k4$size[j])*100
barplot(tab, beside = F, main = "Gender", col = col, legend=T , args.legend ="Right", xlim=c(0,6), ylab="Percentage (%)")
tab <- svytable(~dataf$data.single+k4$cluster, dstrat1)
for(j in 1:length(k4$size)) tab[,j] <- (tab[,j]/k4$size[j])*100
barplot(tab, beside = F, main = "Single", col = col,legend=T , args.legend ="Right", xlim=c(0,5.5), ylab="Percentage (%)")

par(mfrow=c(1,1))
#### Age
tab <- svytable(~dataf$data.age1+k4$cluster, dstrat1)
for(j in 1:length(k4$size)) tab[,j] <- (tab[,j]/k4$size[j])*100
barplot(tab, beside = F, main = "Age", col = col, legend=T, args.legend ="Right", xlim=c(0,6), ylab="Percentage (%)")

#### Education
col=viridis(5)
tab <- svytable(~dataf$data.yeduf+k4$cluster, dstrat1)
for(j in 1:length(k4$size)) tab[,j] <- (tab[,j]/k4$size[j])*100
barplot(tab, beside = F, main = "Years of education", col = col, legend=T, args.legend ="Right", xlim=c(0,7.5), ylab="Percentage (%)")

##### Countries
tab <- svytable(~data$country+k4$cluster, data.w)
tab=tab/rowSums(tab)
tab=round(tab*100,digits = 2)

#### Profile maps 
### Maps
dataclusters = as.data.frame.matrix(tab)
dataclusters = dataclusters[-which(rownames(dataclusters)=="Israel"),]
View(dataclusters)
names(dataclusters)[1:4]=c("Cluster 1","Cluster 2", "Cluster 3", "Cluster 4")

library(ggplot2)
library(maptools)
data(wrld_simpl)
is(wrld_simpl)

euro_simpl=wrld_simpl[wrld_simpl@data$REGION==150,] #Only european countries
euro_simpl=euro_simpl[euro_simpl@data$UN!=744,]

euro_ggmap <- fortify(euro_simpl, region = "ISO3")
head(euro_ggmap)


View(euro_ggmap)
dataclusters[,5]=rownames(dataclusters)
names(dataclusters)[5]="NAME"
View(dataclusters)
temp=euro_simpl@data[,c(3,5)]
dataclusters=merge(dataclusters,temp,by="NAME")


# Profile 1 - The least vulnerable 
ggplot(dataclusters) +
  geom_map(aes(map_id = ISO3, fill=`Cluster 1`), map = euro_ggmap) +
  #expand_limits(x = euro_ggmap$long, y = euro_ggmap$lat)+
  coord_fixed (xlim= c(-9,30),
               ylim= c(36,71)) + 
  labs(title="Profile 1 (The least vulnerable)") + 
  scale_fill_gradientn(name="Proportion of population (%)", colours = viridis(6), breaks = c(10,20,30,40), labels=c(10,20,30,40),limits=c(10,50))

# Profile 2
ggplot(dataclusters) +
  geom_map(aes(map_id = ISO3, fill=`Cluster 2`), map = euro_ggmap) +
  coord_fixed (xlim= c(-9,30),
               ylim= c(36,71)) + 
  labs(title="Profile 2") + 
  scale_fill_gradientn(name="Proportion of population (%)", colours = viridis(6), breaks = c(10,20,30,40), labels=c(10,20,30,40),limits=c(10,48.5))

# Profile 3
ggplot(dataclusters) +
  geom_map(aes(map_id = ISO3, fill=`Cluster 4`), map = euro_ggmap) +
  coord_fixed (xlim= c(-9,30),
               ylim= c(36,71)) + 
  labs(title="Profile 3") + 
  scale_fill_gradientn(name="Proportion of population (%)", colours = viridis(6), breaks = c(10,20,30,40), labels=c(10,20,30,40),limits=c(10,48.5))

# Profile 4 - The most vulnerable
ggplot(dataclusters) +
  geom_map(aes(map_id = ISO3, fill=`Cluster 3`), map = euro_ggmap) +
  coord_fixed (xlim= c(-9,30),
               ylim= c(36,71)) + 
  labs(title="Profile 4 (The most vulnerable)") + 
  scale_fill_gradientn(name="Proportion of population (%)", colours = viridis(6), breaks = c(10,20,30,40), labels=c(10,20,30,40),limits=c(10,48.5))


## 4.2. Pandemic: A health shock -----------------------------------------

First at all, we use the same data set but with another name

datap1=data
sum(is.na(datap1$cciw_w7))
class(datap1$cciw_w7)

### Designing the survey
library(survey)
datap1.w = svydesign(ids = ~ 1, data=datap1, weights = datap1$cciw_w7)
summary(datap1.w)

### Shocks

# First shock: Employment and Self-perceived health

svytable(~datap1$chdeseases+datap1$cjs, datap1.w)
datap1$re012isco_1=addNA(datap1$re012isco_1)

datap1$unemployed=data$unemployed
datap1$unemployed=ifelse(datap1$cjs=="Employed or self-employed" & datap1$chdeseases==1,1,datap1$unemployed)
datap1$unemployed=ifelse(datap1$cjs=="Permanently sick" & datap1$chdeseases==1,1,datap1$unemployed)

datap1$unemployed=ifelse(datap1$re012isco_1=="3322" & datap1$cjs=="Employed or self-employed",1,datap1$unemployed)
datap1$unemployed=ifelse(datap1$re012isco_1=="3339" & datap1$cjs=="Employed or self-employed",1,datap1$unemployed)
datap1$unemployed=ifelse(datap1$re012isco_1=="3434" & datap1$cjs=="Employed or self-employed",1,datap1$unemployed)
datap1$unemployed=ifelse(datap1$re012isco_1=="5111" & datap1$cjs=="Employed or self-employed",1,datap1$unemployed)
datap1$unemployed=ifelse(datap1$re012isco_1=="5113" & datap1$cjs=="Employed or self-employed",1,datap1$unemployed)
datap1$unemployed=ifelse(datap1$re012isco_1=="5120" & datap1$cjs=="Employed or self-employed",1,datap1$unemployed)
datap1$unemployed=ifelse(datap1$re012isco_1=="5131" & datap1$cjs=="Employed or self-employed",1,datap1$unemployed)
datap1$unemployed=ifelse(datap1$re012isco_1=="5132" & datap1$cjs=="Employed or self-employed",1,datap1$unemployed)
datap1$unemployed=ifelse(datap1$re012isco_1=="5211" & datap1$cjs=="Employed or self-employed",1,datap1$unemployed)
datap1$unemployed=ifelse(datap1$re012isco_1=="5212" & datap1$cjs=="Employed or self-employed",1,datap1$unemployed)
datap1$unemployed=ifelse(datap1$re012isco_1=="5221" & datap1$cjs=="Employed or self-employed",1,datap1$unemployed)
datap1$unemployed=ifelse(datap1$re012isco_1=="5222" & datap1$cjs=="Employed or self-employed",1,datap1$unemployed)
datap1$unemployed=ifelse(datap1$re012isco_1=="5223" & datap1$cjs=="Employed or self-employed",1,datap1$unemployed)
datap1$unemployed=ifelse(datap1$re012isco_1=="5242" & datap1$cjs=="Employed or self-employed",1,datap1$unemployed)
datap1$unemployed=ifelse(datap1$re012isco_1=="5245" & datap1$cjs=="Employed or self-employed",1,datap1$unemployed)
datap1$unemployed=ifelse(datap1$re012isco_1=="5246" & datap1$cjs=="Employed or self-employed",1,datap1$unemployed)
datap1$unemployed=ifelse(datap1$re012isco_1=="9411" & datap1$cjs=="Employed or self-employed",1,datap1$unemployed)
datap1$unemployed=ifelse(datap1$re012isco_1=="9412" & datap1$cjs=="Employed or self-employed",1,datap1$unemployed)
svytable(~datap1$unemployed, datap1.w)
svytable(~data$unemployed,data.w)
svytable(~data$unemployed + data$cjs, data.w)
svytable(~datap1$unemployed + data$cjs, data.w)

datap1$sphus=data$sphus
svytable(~datap1$sphus,datap1.w)

datap1$sphus= as.character(datap1$sphus)
datap1$sphus=ifelse(datap1$chdeseases==1 & datap1$sphus=="Fair", "Poor", datap1$sphus)
datap1$sphus=ifelse(datap1$chdeseases==1 & datap1$sphus=="Good", "Fair", datap1$sphus)
datap1$sphus=ifelse(datap1$chdeseases==1 & datap1$sphus=="Very Good", "Good", datap1$sphus)
datap1$sphus=ifelse(datap1$chdeseases==1 & datap1$sphus=="Excellent", "Very Good", datap1$sphus)
datap1$sphus=factor(datap1$sphus)

svytable(~datap1$sphus,datap1.w)
svytable(~data$sphus,data.w)

datap1$sp=ifelse(datap1$sphus=="Poor"| datap1$sphus=="Fair",1,0)
svytable(~datap1$sp,datap1.w)
svytable(~data$sp,data.w)


# Shock in Economical distress and Consumtion of food products 


datap1$nutrition=data$nutrition
datap1$nutrition=ifelse(datap1$unemployed==1 & datap1$nutrition==0, 1, datap1$nutrition)
svytable(~datap1$nutrition,datap1.w)
svytable(~data$nutrition,data.w)

datap1$nutrition=data$nutrition
datap1$nutrition=ifelse(datap1$unemployed==1, 1, datap1$nutrition)
svytable(~datap1$nutrition,datap1.w)
svytable(~data$nutrition,data.w)
svytable(~datap1$nutrition + datap1$country,datap1.w)

datap1$ecostress=data$ecostress
datap1$ecostress=ifelse(datap1$unemployed==1, 1, datap1$ecostress)
svytable(~datap1$ecostress,datap1.w)
svytable(~data$ecostress,data.w)

#Now we shock the Life Happiness, depression and life satisfaction variables. 

# Life Happiness
datap1$hap=data$hap
svytable(~datap1$hap,datap1.w)
datap1$hap=ifelse((datap1$unemployed==1 & datap1$ecostress==1 & datap1$nutrition==1),1, datap1$hap)
datap1$hap=ifelse((datap1$sp==1 & datap1$chdeseases==1),1, datap1$hap)
svytable(~datap1$hap,datap1.w)
svytable(~data$hap,data.w)

# Depression
datap1$depress = data$depress
svytable(~datap1$depress,datap1.w)
datap1$depress=ifelse(datap1$ecostress==1 & datap1$nutrition==1 & datap1$unemployed,1, datap1$depress)
datap1$depress=ifelse(datap1$chdeseases==1 & datap1$sp==1,1, datap1$depress)
svytable(~datap1$depress,datap1.w)
svytable(~data$depress,data.w)

# Life satisfaction
svytable(~datap1$satis,datap1.w)
datap1$satis=data$satis
datap1$satis=ifelse(datap1$ecostress==1 & datap1$unemployed==1, 1, datap1$satis)
datap1$satis=ifelse(datap1$sp==1 & datap1$chdeseases==1,1,datap1$satis)
svytable(~datap1$satis,datap1.w)
svytable(~data$satis,data.w)

# BMI
datap1$bmi1=data$bmi1
datap1$bmi1=ifelse(datap1$unemployed==1 & datap1$nutrition==1 & datap1$ecostress==1, 1, datap1$bmi1)
datap1$bmi1=ifelse(datap1$sp==1 & datap1$chdeseases==1,1,datap1$bmi1)
svytable(~datap1$bmi1,datap1.w)
svytable(~data$bmi1,data.w)


###Recalculating indicators. 
############################

#### Economic Indicator

datap1$ecoind= datap1$Poverty + datap1$ecostress + datap1$nutrition + datap1$unemployed
svytable(~datap1$ecoind, datap1.w)

datap1$ecoind=rescale(datap1$ecoind,to=c(0,10))
sum(svytable(~datap1$ecoind, datap1.w))
sum(svytable(~data$ecoind,data.w))
sum(table(datap1$ecoind))
sum(table(data$ecoind))
barplot(svytable(~datap1$ecoind, datap1.w))

## Health Indicator

### Mental indicator
datap1$mh = datap1$depress + datap1$satis + datap1$hap

### Physical health indicator
datap1$ph = datap1$limited + datap1$ialimit + datap1$alimit + datap1$mob + datap1$bmi1


### Historical health indicator
datap1$hh= datap1$dvisit + datap1$nhosp


### General health indicator
datap1$gh = datap1$chdeseases + datap1$sp


### Final health indicator
datap1$healthind = datap1$mh + datap1$ph + datap1$hh + datap1$gh
svytable(~datap1$healthind,datap1.w)
datap1$healthind=round(rescale(datap1$healthind,to=c(0,10)))
svytable(~datap1$healthind,datap1.w)
svytable(~data$healthind,data.w)

barplot(svytable(~datap1$healthind,datap1.w))


# Mean economic indicator per country
countries = unique(datap1$country)
countrymean1p = NULL
countrymedian1p = NULL

for (i in countries){
  countrymean1p[i]=svymean(~datap1$ecoind[datap1$country==i], design=subset(datap1.w, datap1$country==i))
  countrymedian1p[i]=svyquantile(~datap1$ecoind[datap1$country==i], design=subset(datap1.w, datap1$country==i), quantiles=0.5)
  
} 


# Mean health indicator per country
countries = unique(datap1$country)
countrymean2p = NULL
countrymedian2p = NULL

for (i in countries){
  countrymean2p[i]=svymean(~datap1$healthind[datap1$country==i], design=subset(datap1.w, datap1$country==i))
  countrymedian2p[i]=svyquantile(~datap1$healthind[datap1$country==i], design=subset(datap1.w, datap1$country==i), quantiles=0.5)
  
}



data3p=as.data.frame(cbind(countrymean1p,countrymean2p))
ggplot(data3p) + aes(y=countrymean1p, x=countrymean2p, label = rownames(data3p)) + geom_point() + ylab("Economic Indicator") + xlab("Health Indicator") + geom_text(aes(label=rownames(data3p))) + ggtitle("After the shock")


# Comparisons before and after de shock

## Economic indicator

### Barplot
par(mfrow=c(1,2))
barplot(svytable(~data$ecoind,data.w), main="No pandemic", ylim = c(0,10e07), col="#3182BD")
barplot(svytable(~datap1$ecoind,datap1.w), main="With pandemic", ylim=c(0,10e07), col="#3182BD")

### Boxplot
par(mfrow=c(1,2))
svyboxplot(data$ecoind~1,data.w, main="No pandemic", col="#3182BD")
svyboxplot(datap1$ecoind~1,datap1.w, main="With pandemic", col="#3182BD")

## Health indicator

par(mfrow=c(1,2))
barplot(svytable(~data$healthind,data.w), main="No pandemic", ylim=c(0,7e07),col="#31A354")
barplot(svytable(~datap1$healthind,datap1.w), main="with pandemic",ylim=c(0,7e07), col="#31A354")

par(mfrow=c(1,2))
svyboxplot(data$healthind~1,data.w, main="No pandemic",col="#31A354")
svyboxplot(datap1$healthind~1,datap1.w, main="With pandemic",col="#31A354")



## Between countries

library(ggrepel)
par(mfrow=c(1,1))

#Before the shock
ggplot(data3) + aes(y=countrymean1, x=countrymean2, label = rownames(data3)) + geom_point() + ylab("Economic Indicator") + xlab("Health Indicator") + ggtitle("Before the shock") +xlim(c(2,4.75)) +ylim(c(1,5)) +geom_label_repel(aes(label=rownames(data3p)), box.padding = 0.2,point.padding = 0.35, segment.color = 'grey50')

#After the shock
ggplot(data3p) + aes(y=countrymean1p, x=countrymean2p, label = rownames(data3p)) + geom_point() + ylab("Economic Indicator") + xlab("Health Indicator") +  ggtitle("After the shock") +xlim(c(2,4.75)) +ylim(c(1,5)) + geom_label_repel(aes(label=rownames(data3p)), box.padding = 0.35,point.padding = 0.5, segment.color = 'grey50')


### Mean and median value of each indicator and subindicator per European Region

## Mean before the shock
countries = unique(data$country)
countryhealth = NULL
countrymh = NULL
countrygh = NULL
countryph = NULL
countryhh = NULL
countryeco = NULL
countryun = NULL
countrypo = NULL
countrynu = NULL
countryfe = NULL

# Health

for (i in countries){
  countryhealth[i]=svymean(~data$healthind[data$country==i], design=subset(data.w, data$country==i))
  countrymh[i]=svymean(~data$mh[datap1$country==i], design=subset(data.w, data$country==i))
  countrygh[i]=svymean(~data$gh[datap1$country==i], design=subset(data.w, data$country==i))
  countryph[i]=svymean(~data$ph[datap1$country==i], design=subset(data.w, data$country==i))
  countryhh[i]=svymean(~data$hh[datap1$country==i], design=subset(data.w, data$country==i))
}

# Economy 

for (i in countries){
  countryeco[i]=svymean(~data$ecoind[data$country==i], design=subset(data.w, data$country==i))
  countryun[i]=svymean(~data$unemployed[data$country==i], design=subset(data.w, data$country==i))
  countrypo[i]=svymean(~data$Poverty[data$country==i], design=subset(data.w, data$country==i))
  countrynu[i]=svymean(~data$nutrition[data$country==i], design=subset(data.w, data$country==i))
  countryfe[i]=svymean(~data$ecostress[data$country==i], design=subset(data.w, data$country==i))
}


# Mean after de shock
countries = unique(datap1$country)
countryhealth1 = NULL
countrymh1 = NULL
countrygh1 = NULL
countryph1 = NULL
countryhh1 = NULL
countryeco1 = NULL
countryun1 = NULL
countrypo1 = NULL
countrynu1 = NULL
countryfe1 = NULL

# Health

for (i in countries){
  countryhealth1[i]=svymean(~datap1$healthind[datap1$country==i], design=subset(datap1.w, datap1$country==i))
  countrymh1[i]=svymean(~datap1$mh[datap1$country==i], design=subset(datap1.w, datap1$country==i))
  countrygh1[i]=svymean(~datap1$gh[datap1$country==i], design=subset(datap1.w, datap1$country==i))
  countryph1[i]=svymean(~datap1$ph[datap1$country==i], design=subset(datap1.w, datap1$country==i))
  countryhh1[i]=svymean(~datap1$hh[datap1$country==i], design=subset(datap1.w, datap1$country==i))
}

# Economy 

for (i in countries){
  countryeco1[i]=svymean(~datap1$ecoind[datap1$country==i], design=subset(datap1.w, datap1$country==i))
  countryun1[i]=svymean(~datap1$unemployed[datap1$country==i], design=subset(datap1.w, datap1$country==i))
  countrypo1[i]=svymean(~datap1$Poverty[datap1$country==i], design=subset(datap1.w, datap1$country==i))
  countrynu1[i]=svymean(~datap1$nutrition[datap1$country==i], design=subset(datap1.w, datap1$country==i))
  countryfe1[i]=svymean(~datap1$ecostress[datap1$country==i], design=subset(datap1.w, datap1$country==i))
}


comtable = round(cbind(countryeco,countryun,countrypo,countrynu,countryfe,countryhealth,countrygh,countrymh,countryhh,countryph,countryeco1,countryun1,countrypo1,countrynu1,countryfe1,countryhealth1,countrygh1,countrymh1,countryhh1,countryph1),2)
comtable=as.data.frame(comtable)
library("writexl")
write_xlsx(comtable,"comtable.xlsx",)



### Median before the shock
countries = unique(data$country)
countryhealthm = NULL
countrymhm = NULL
countryghm = NULL
countryphm = NULL
countryhhm = NULL
countryecom = NULL
countryunm = NULL
countrypom = NULL
countrynum = NULL
countryfem = NULL

# Health

for (i in countries){
  countryhealthm[i]=svyquantile(~data$healthind[data$country==i], design=subset(data.w, data$country==i), c(0.5))
  countrymhm[i]=svyquantile(~data$mh[datap1$country==i], design=subset(data.w, data$country==i), c(0.5))
  countryghm[i]=svyquantile(~data$gh[datap1$country==i], design=subset(data.w, data$country==i), c(0.5))
  countryphm[i]=svyquantile(~data$ph[datap1$country==i], design=subset(data.w, data$country==i),c(0.5))
  countryhhm[i]=svyquantile(~data$hh[datap1$country==i], design=subset(data.w, data$country==i), c(0.5))
}

# Economy 

for (i in countries){
  countryecom[i]=svyquantile(~data$ecoind[data$country==i], design=subset(data.w, data$country==i), c(0.5))
  countryunm[i]=svyquantile(~data$unemployed[data$country==i], design=subset(data.w, data$country==i),c(0.5))
  countrypom[i]=svyquantile(~data$Poverty[data$country==i], design=subset(data.w, data$country==i), c(0.5))
  countrynum[i]=svyquantile(~data$nutrition[data$country==i], design=subset(data.w, data$country==i), c(0.5))
  countryfem[i]=svyquantile(~data$ecostress[data$country==i], design=subset(data.w, data$country==i), c(0.5))
}


### Median after the shock 
countries = unique(datap1$country)
countryhealth1m = NULL
countrymh1m = NULL
countrygh1m = NULL
countryph1m = NULL
countryhh1m = NULL
countryeco1m = NULL
countryun1m = NULL
countrypo1m = NULL
countrynu1m = NULL
countryfe1m = NULL

# Health

for (i in countries){
  countryhealth1m[i]=svyquantile(~datap1$healthind[datap1$country==i], design=subset(datap1.w, datap1$country==i), c(0.5))
  countrymh1m[i]=svyquantile(~datap1$mh[datap1$country==i], design=subset(datap1.w, datap1$country==i), c(0.5))
  countrygh1m[i]=svyquantile(~datap1$gh[datap1$country==i], design=subset(datap1.w, datap1$country==i), c(0.5))
  countryph1m[i]=svyquantile(~datap1$ph[datap1$country==i], design=subset(datap1.w, datap1$country==i), c(0.5))
  countryhh1m[i]=svyquantile(~datap1$hh[datap1$country==i], design=subset(datap1.w, datap1$country==i), c(0.5))
}

# Economy 

for (i in countries){
  countryeco1m[i]=svyquantile(~datap1$ecoind[datap1$country==i], design=subset(datap1.w, datap1$country==i), c(0.5))
  countryun1m[i]=svyquantile(~datap1$unemployed[datap1$country==i], design=subset(datap1.w, datap1$country==i), c(0.5))
  countrypo1m[i]=svyquantile(~datap1$Poverty[datap1$country==i], design=subset(datap1.w, datap1$country==i), c(0.5))
  countrynu1m[i]=svyquantile(~datap1$nutrition[datap1$country==i], design=subset(datap1.w, datap1$country==i), c(0.5))
  countryfe1m[i]=svyquantile(~datap1$ecostress[datap1$country==i], design=subset(datap1.w, datap1$country==i), c(0.5))
}


## Resume tables
comtablem = round(cbind(countryecom,countryunm,countrypom,countrynum,countryfem,countryhealthm,countryghm,countrymhm,countryhhm,countryphm,countryeco1m,countryun1m,countrypo1m,countrynu1m,countryfe1m,countryhealth1m,countrygh1m,countrymh1m,countryhh1m,countryph1m,fcluster$cluster),1)
comtablem=as.data.frame(comtablem)

clusterm=sort(unique(comtablem$V21))


median1=matrix(,nrow=4,ncol=20)
for (i in clusterm){
  for(j in 1:20){
    median1[i,j]=svyquantile(~comtablem[,j][comtablem$V21==i],subset(data.w,comtablem$V21==i),quantiles=0.5)
  }
}

median1=as.data.frame(median1)
names(median1)=names(comtablem)[1:20]
library("writexl")
write_xlsx(median1,"median.xlsx")


### Recalculating the profiles 

## Clusters

set.seed(1234)
datafp1=data.frame(datap1$healthind,datap1$ecoind,datap1$gender,datap1$single,datap1$age1,datap1$yeduf,datap1$cciw_w7)

## Profiles

# We used the predict function from clusMixType package 
names(datafp1)=c("data.healthind","data.ecoind","data.gender","data.single","data.age1","data.yeduf", "data.cciw_w7")
kpredict = predict(k4,datafp1[,-7])

object=kpredict
dstrat11 <- svydesign(id = ~1, strata = ~as.factor(object$cluster), weights = ~data.cciw_w7, data = datafp1)
col=viridis(k)



# Economic and Health indicator
par(mfrow=c(1,2))
svyboxplot(data.ecoind~as.factor(object$cluster),dstrat11,all.outliers=TRUE, col = col, main = "Economic indicator", xlab = " # Cluster")
svyboxplot(data.healthind~as.factor(object$cluster),dstrat11,all.outliers=TRUE, col = col, main = "Health indicator", xlab = " # Cluster")



# Gender
size=svytable(~kpredict$cluster, datap1.w)
par(mfrow=c(1,2))
tab <- svytable(~datafp1$data.gender+object$cluster, dstrat1)
for(j in 1:length(size)) tab[,j] <- tab[,j]/ size[j]
barplot(t(tab), beside = TRUE, main = "Gender", col = col)
tab <- svytable(~datafp1$data.single+object$cluster, dstrat1)
for(j in 1:length(size)) tab[,j] <- tab[,j]/ size[j]
barplot(t(tab), beside = TRUE, main = "Single", col = col)



par(mfrow=c(1,1))


# Age
tab <- svytable(~datafp1$data.age1+object$cluster, dstrat11)
for(j in 1:length(size)) tab[,j] <- tab[,j]/size[j]
barplot(t(tab), beside = TRUE, main = "Age", col = col)


# Education

tab <- svytable(~datafp1$data.yeduf+object$cluster, dstrat11)
for(j in 1:length(size)) tab[,j] <- tab[,j]/size[j]
barplot(t(tab), beside = TRUE, main = "Education", col = col, cex.names = 0.7)


##### Countries

tab <- svytable(~datap1$country+object$cluster, datap1.w)
tab=tab/rowSums(tab)
tab=round(tab*100,digits = 2)
tab


### Maps
dataclustersp1 = as.data.frame.matrix(tab)
dataclustersp1 = dataclustersp1[-which(rownames(dataclustersp1)=="Israel"),]
View(dataclustersp1)
names(dataclustersp1)[1:4]=c("Cluster 1","Cluster 2", "Cluster 3", "Cluster 4")

library(ggplot2)
library(maptools)
data(wrld_simpl)
is(wrld_simpl)

euro_simpl=wrld_simpl[wrld_simpl@data$REGION==150,] #Only european countries
euro_simpl=euro_simpl[euro_simpl@data$UN!=744,]


euro_ggmap <- fortify(euro_simpl, region = "ISO3")
head(euro_ggmap)



View(euro_ggmap)
dataclustersp1[,5]=rownames(dataclustersp1)
names(dataclustersp1)[5]="NAME"
View(dataclustersp1)
temp=euro_simpl@data[,c(3,5)]
dataclustersp1=merge(dataclustersp1,temp,by="NAME")


# Profile 1 the least vulnerable
ggplot(dataclustersp1) +
  geom_map(aes(map_id = ISO3, fill=`Cluster 1`), map = euro_ggmap) +
  #expand_limits(x = euro_ggmap$long, y = euro_ggmap$lat)+
  coord_fixed (xlim= c(-9,30),
               ylim= c(36,71)) + 
  labs(title="Profile 1 (The least vulnerable) after de shock") + 
  scale_fill_gradientn(name="Proportion of population (%)", colours = viridis(5), breaks = c(10,20,30,40), labels=c(10,20,30,40),limits=c(10,48.5))

# Profile 2
ggplot(dataclustersp1) +
  geom_map(aes(map_id = ISO3, fill=`Cluster 2`), map = euro_ggmap) +
  #expand_limits(x = euro_ggmap$long, y = euro_ggmap$lat)+
  coord_fixed (xlim= c(-9,30),
               ylim= c(36,71)) + 
  labs(title="Profile 2") + 
  scale_fill_gradientn(name="Proportion of population (%)", colours = viridis(5), breaks = c(10,20,30,40), labels=c(10,20,30,40),limits=c(10,48.5))

# Profile 3 
ggplot(dataclustersp1) +
  geom_map(aes(map_id = ISO3, fill=`Cluster 4`), map = euro_ggmap) +
  #expand_limits(x = euro_ggmap$long, y = euro_ggmap$lat)+
  coord_fixed (xlim= c(-9,30),
               ylim= c(36,71)) + 
  labs(title="Profile 3") + 
  scale_fill_gradientn(name="Proportion of population (%)", colours = viridis(5), breaks = c(10,20,30,40), labels=c(10,20,30,40),limits=c(10,48.5))

# Profile 4 The most vulnerable
ggplot(dataclustersp1) +
  geom_map(aes(map_id = ISO3, fill=`Cluster 3`), map = euro_ggmap) +
  #expand_limits(x = euro_ggmap$long, y = euro_ggmap$lat)+
  coord_fixed (xlim= c(-9,30),
               ylim= c(36,71)) + 
  labs(title="Profile 4 (The most vulnerable) after the shock") + 
  scale_fill_gradientn(name="Proportion of population (%)", colours = viridis(5), breaks = c(10,20,30,40), labels=c(10,20,30,40), limits=c(10,48.5))







