# Q1 WhoamI
def WhoAmI():
    return('wl2905')








# Q2 get Bond Price
getBondPrice = function(y, face, couponRate, m, ppy=1){
  pvcfsum = 0
  for (i in (1:(m*ppy))){
    pvcfsum = pvcfsum + face/ppy*couponRate*((1+y/ppy)^(-i))
  }
  pvcfsum = pvcfsum + face*(1+y/ppy)^(-m*ppy)
  return(pvcfsum)
}


# Unit test

y = 0.03
face = 2000000
couponRate = 0.04
m = 10
ppy = 2

x = getBondPrice(y, face, couponRate, m,  1)
round(x) == 2170604
x = getBondPrice(y, face, couponRate, m,  2)
round(x) == 2171686







# Q3 Bond Duration
getBondDuration = function(y, face, couponRate, m){
  pvcfsum = 0
  pvcft = 0
  duration = 0
  for (i in 1:m){
    pvcfsum = pvcfsum + face*couponRate*((1+y)^(-i))
    pvcft = pvcft + i * face*couponRate*((1+y)^(-i))
  }
  pvcfsum = pvcfsum + face*(1+y)^(-m)
  pvcft = pvcft + face*(1+y)^(-m)*m
  duration = pvcft/pvcfsum
  
  return(duration) 
}

# Unit test

y = 0.03
face = 2000000
couponRate = 0.04
m = 10

x = getBondDuration(y, face, couponRate, m)
print(x)
round(x,2) == 8.51








Q4 get Bond Price_E
def getBondPrice_E(face, couponRate, m, yc):
    bondPrice=0
    for t,y in enumerate(yc,1):
        C = face*couponRate
        if t == m:
            C += face
        PV = 1/((1+y)**t)
        PVCF = C* PV
        bondPrice += PVCF
    return(bondPrice)


# unit test

yc = [.010,.015,.020,.025,.030]
face = 2000000
couponRate = .04
m = 5

x = getBondPrice_E(yc, face, couponRate, m)
print(x)
round(x,2) == 2,098,948.97 







Q5 get Bond Price_Z
def getBondPrice_Z(face, couponRate, times, yc):
    cpn=couponRate*face
    bondPrice=0
    for y,t in zip(yc,times):
        dfn=1/((1+y)**t)
        bondPrice += dfn*cpn
    bondPrice += face*dfn
        
    return(bondPrice)


# Test values
﻿
yc = [.010,.015,.020,.025,.030]
times=[1,1.5,3,4,7]
face = 2000000
couponRate = .04

x = getBondPrice_Z(yc, face, couponRate, m)
print(x)
round(x,2) == 1,996,533.27  








#Q6 Fizzbuzz
FizzBuzz <- function(start, finish) {
  outlist <- character(finish - start + 1)  # 创建一个字符向量，长度为结束值减去起始值加一
  for (i in start:finish) {
    if (i %% 3 == 0 && i %% 5 == 0) {
      outlist[i - start + 1] <- "fizzbuzz"
    } else if (i %% 3 == 0) {
      outlist[i - start + 1] <- "fizz"
    } else if (i %% 5 == 0) {
      outlist[i - start + 1] <- "buzz"
    } else {
      outlist[i - start + 1] <- as.character(i)
    }
  }
  return(outlist)
}

# Unit test
start <- 1
finish <- 15
result <- FizzBuzz(start, finish)

print(result)

