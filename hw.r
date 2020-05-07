library("readxl")
library("reshape")
library("dummies")
library("qcc")


#making the r code input ready

data = read_excel("eBayAuctions.xls")                                 #reads excel file
dataindicesvals =  sample(nrow(data), nrow(data)*.60)                 #selects the 60 percent data
train <- data[dataindicesvals,]                                       #selects train data
valid <- data[-dataindicesvals,]                                      #selects validation data

set.seed(10)


#using melt to select ("Category","currency","Duration","endDay" ) and compressing competitive

meltData = melt(train,c("Category","currency","Duration","endDay"),c("Competitive?"))

#creating various pivot tables as mentioned in the program requirements using the cast operator taking average values(mean)

pivotstableCurrency = cast(meltData, currency ~ variable, mean)
pivotstablecategory = cast(meltData, Category ~ variable, mean)
pivotstableEndday = cast(meltData, endDay ~ variable, mean)
pivotstableDuration = cast(meltData, Duration ~ variable, mean)



# function to replace values in train data 

#values replaced in train
replaceValuesTrain <- function (replacingValue,checkvalue,stringVal) {
  r1=replacingValue
  c1=checkvalue
  train[train[stringVal]==c1, stringVal] = r1               #replaces train data with the merged values for the given columnname(stirngval)
  return (train)
}

#values replaced in validation
replaceValuesValidation <- function (replacingValue,checkvalue,stringVal) {
  r1 = replacingValue
  c1 = checkvalue
  valid[valid[stringVal] == c1, stringVal] = r1             #replaces validationdata data with the merged values for the given columnname(stirngval)
  return (valid)
}



#merging for currency


#this function compares if any of the currency values can be combined to a single currency based on similarity

pivotstableCurrency['addedcolumn'] = pivotstableCurrency[1]  #adds extra column to keep track of which value to be changed
iterator <- 1
maxvalue = 3                                                 #maxValue hols the length of pivotstableCurrency
while(iterator<3) {
  j <- iterator+1
  while(j<4) {
    if (j <= maxvalue) {
      valueforone = pivotstableCurrency[iterator,2]          #holds first currency value
      valuefortwo = pivotstableCurrency[j,2]                 #holds second currency value
      differencevalues = valueforone - valuefortwo           #finds the difference
      if(differencevalues < 0 )                              #convert to positive number
        differencevalues = differencevalues * (-1)
      if (differencevalues < 0.05) {                         #if difference is less that 0.05 then it merges both the currencies
        requiredval = pivotstableCurrency[iterator,3]
        pivotstableCurrency[j,3] = requiredval              # the extra column created will hold the value of currency it is merged with
      }
    }
    j = j + 1 
  }
  iterator = iterator+1
}

print(pivotstableCurrency)


iterator <- 1
while(iterator < 4){                                              #iterator runs through the length of pivotstableCurreny
  replacingValue = pivotstableCurrency[iterator,3]
  checkvalue = pivotstableCurrency[iterator,1]
  stringval  = 'currency'
  train = replaceValuesTrain(replacingValue,checkvalue,stringval) #this function replaces the currency names with the merged currency name in train data
  iterator = iterator + 1
}



iterator <- 1
while(iterator < 4){                                                    #iterator runs through the length of pivotstableCurreny
  replacingValue = pivotstableCurrency[iterator,3]
  checkvalue = pivotstableCurrency[iterator,1]
  stringval  = 'currency'
  valid = replaceValuesValidation(replacingValue,checkvalue,stringval)  #this function replaces the currency names with the merged currency name in train data
  iterator = iterator + 1
}






#merging for category


#similarly performing the operation described above for category

pivotstablecategory['addedcolumn'] = pivotstablecategory[1]
iterator <- 1
maxvalue = 18
while(iterator<18) {
  j <- iterator+1
  while(j<19) {
    if (j <= maxvalue) {
      valueforone = pivotstablecategory[iterator,2]
      valuefortwo = pivotstablecategory[j,2]
      differencevalues = valueforone - valuefortwo 
      if(differencevalues < 0 )
        differencevalues = differencevalues * (-1)
      if (differencevalues < 0.05) {
        requiredval = pivotstablecategory[iterator,3]
        pivotstablecategory[j,3] = requiredval
      }
    }
    j = j + 1 
  }
  iterator = iterator+1
}


iterator <- 1
while(iterator < 19){
  replacingValue = pivotstablecategory[iterator,3]
  checkvalue = pivotstablecategory[iterator,1]
  StringVal = 'Category'
  train = replaceValuesTrain(replacingValue,checkvalue,StringVal)
  iterator = iterator + 1
}

iterator <- 1
while(iterator < 19){
  replacingValue = pivotstablecategory[iterator,3]
  checkvalue = pivotstablecategory[iterator,1]
  StringVal = 'Category'
  valid = replaceValuesValidation(replacingValue,checkvalue,StringVal)
  iterator = iterator + 1
}






#merging for endDay
#similarly performing the operation described above for endDay


pivotstableEndday['addedcolumn'] = pivotstableEndday[1]
iterator <- 1
maxvalue = 7
while(iterator<7) {
  j <- iterator+1
  while(j<8) {
    if (j <= maxvalue) {
      valueforone = pivotstableEndday[iterator,2]
      valuefortwo = pivotstableEndday[j,2]
      differencevalues = valueforone - valuefortwo 
      if(differencevalues < 0 )
        differencevalues = differencevalues * (-1)
      if (differencevalues < 0.05) {
        requiredval = pivotstableEndday[iterator,3]
        pivotstableEndday[j,3] = requiredval
      }
    }
    j = j + 1 
  }
  iterator = iterator+1
}


iterator <- 1
while(iterator < 8){
  replacingValue = pivotstableEndday[iterator,3]
  checkvalue = pivotstableEndday[iterator,1]
  StringVal = 'endDay'
  train = replaceValuesTrain(replacingValue,checkvalue,StringVal)
  iterator = iterator + 1
}

iterator <- 1
while(iterator < 8){
  replacingValue = pivotstableEndday[iterator,3]
  checkvalue = pivotstableEndday[iterator,1]
  StringVal = 'endDay'
  valid = replaceValuesValidation(replacingValue,checkvalue,StringVal)
  iterator = iterator + 1
}





#merging for Duration

#similarly performing the operation described above for duration

pivotstableDuration['addedcolumn'] = pivotstableDuration[1]
iterator <- 1
maxvalue = 5
while(iterator<5) {
  j <- iterator+1
  while(j<6) {
    if (j <= maxvalue) {
      valueforone = pivotstableDuration[iterator,2]
      valuefortwo = pivotstableDuration[j,2]
      differencevalues = valueforone - valuefortwo 
      if(differencevalues < 0 )
        differencevalues = differencevalues * (-1)
      if (differencevalues < 0.05) {
        requiredval = pivotstableDuration[iterator,3]
        pivotstableDuration[j,3] = requiredval
      }
    }
    j = j + 1 
  }
  iterator = iterator+1
}


iterator <- 1
while(iterator < 6){
  replacingValue = pivotstableDuration[iterator,3]
  checkvalue = pivotstableDuration[iterator,1]
  StringVal = 'Duration'
  train = replaceValuesTrain(replacingValue,checkvalue,StringVal)
  iterator = iterator + 1
}

iterator <- 1
while(iterator < 6){
  replacingValue = pivotstableDuration[iterator,3]
  checkvalue = pivotstableDuration[iterator,1]
  StringVal = 'Duration'
  valid = replaceValuesValidation(replacingValue,checkvalue,StringVal)
  iterator = iterator + 1
}



find <- function (stringval1,stringval2) {
  r1=stringval1
  c1=stringval2
  return(grepl(stringval1,stringval2))
}



#creating a dummy dataframe for use

train = dummy.data.frame(train)
print(dummy.data.frame(train))

#generating the linear regression model

fit.all <- glm(`Competitive?` ~ ., family=binomial,data=dummy.data.frame(trainval))
coefs <- coefficients(fit.all)            #select all coefficients in the glm

#add all the coefficient into a list 
list<-c()
iterator=1
while(iterator<17) {
  
  valueObtained<-abs(as.numeric(coefs[iterator]))
  list<-c(list,valueObtained)
  iterator=iterator+1
} 

#finds the maximum coefficient
maxvalue = max(list)


iterator <- 1
while(iterator < 17){
  print(abs(as.numeric(coefs[iterator])))
        iterator = iterator + 1
}
        

 



#finds the index position of maximum coefficient
counter <-1
iterator <- 1
while(iterator < 17){
  if(maxvalue == abs(as.numeric(coefs[iterator])))
     break
  else
    counter = counter + 1
  iterator = iterator + 1
}

print(names(fit.all$coefficients)[counter])


#finds the name of highest regr coefficient 
namerequired = ""
iterator <- 1
while(iterator < 9){
if(find(names(train)[iterator],names(coefs)[counter])){
  namerequired = names(train)[iterator]
}
  iterator = iterator + 1
}



#creating a subset for the data with hidgest regression coefficient
subset = c("Competitive?", namerequired)

#fit.single hold values of glm with the highest regression coefficient

fit.single = glm(`Competitive?` ~ Category, family=binomial,data=train[subset])



#calculating the significant predictors for the model


coeffs <- coefficients(summary(fit.all))           #calculate all coefficient for summary of fit.all

print(summary(fit.all))
iterator <- 1
lister<-c()                                        #creating list to hold all coefficient names that have Pr(>|z|) is less 0.05 in summary.fit
while(iterator < 16)
{
  if(coeffs[iterator,4]< 0.05)                     #checks if Pr(>|z|) is less 0.05
  {
    valueObtained = names(coefs)[iterator]         #gets name of coefficient
    lister<-c(lister,valueObtained)
  }
  iterator= iterator + 1
}



#reducing the model


reduced_name <- ("Competitive?") 
iterator <- 1
while(iterator < 9)        # 9 because train has 8 rows - length(train) + 1
{
  j <- 1
  while(j < 8)     # 8 becasue lister has 7 rows - length(lister) + 1
  {
    if(find(names(train)[iterator],lister[j])){               #checking if coeffs in train have a common pattern with names in lister 
       reduced_name = c(reduced_name,names(train)[iterator])  #if true adds to reduced_name list
       }
    j = j +1
  }
  iterator = iterator + 1 
}

  
fit.reduced = glm(`Competitive?` ~., family=binomial, data=train[unique(reduced_name)])  # the reduced model



#comparing the models using the anova test

anova(fit.reduced,fit.all,test='Chisq')


#-----------Checking for Over Dispersion------------


#s=rep(length(train$`Competitive?`), length(train$`Competitive?`))



createvector <- function (length) {              #used to create vector of size length*length for overdispersiontest
  val = length
  vector = rep(val,val)
  return (vector)
}


#overdispersion test


qcc.overdispersion.test(train$`Competitive?`, size=createvector(length(train$`Competitive?`)) ,type="binomial")



