# qMatMult1

MatMult2 = function(vec,mat){
  
  nRowMat = dim(mat)[1]
  nColMat = dim(mat)[2]
  nRowVec = dim(vec)[1]
  nColVec = dim(vec)[2]
  
  ans = matrix(0,nRowVec,nColMat)
  
  # Your vectorized code
  # Only one loop allowed
  
  for (i in 1:nRowVec) {
    ans[i, ] = sum(vec[i, ] * mat[, ])
  }
  
  return(ans)
}

# Unit test

vec = matrix(data = 1:3,nrow = 1,ncol = 3)
mat = matrix(data = c(1,4,7),nrow = 3,ncol = 1)

x = MatMult2(vec,mat)
x[1] == 30 

functionText = capture.output(MatMult2)
if(length(grep("for",functionText))>1){
  cat("Only 1 'for' loop allowed!")
}

functionText = capture.output(MatMult2)
if(length(grep("%*%",functionText))>0){
  cat("No Cheating :)")
}


# qMatMult2

MatMult1 = function(vec,mat){
  
  nRowMat = dim(mat)[1]
  nColMat = dim(mat)[2]
  nRowVec = dim(vec)[1]
  nColVec = dim(vec)[2]
  
  ans = matrix(0,nRowVec,nColMat)
  
  # Your vectorized code
  # Only one loop allowed
  
  for (i in 1:nRowMat) {
    ans = ans + vec[, i] * mat[i, , drop = FALSE]
  }
  
  return(ans)
}

# Unit test

vec = matrix(data = 1:3,nrow = 1,ncol = 3)
mat = matrix(data = 1:9,nrow = 3,ncol = 3,byrow = T)

x = MatMult1(vec,mat)
x[1] == 30 & x[2] == 36 & x[3] == 42

functionText = capture.output(MatMult1)
if(length(grep("for",functionText))>1){
  cat("Only 1 'for' loop allowed!")
}

functionText = capture.output(MatMult1)
if(length(grep("%*%",functionText))>0){
  cat("No Cheating :)")
}


# qTMat1

TMAT1 = function(vec1,vec2){
  
  states <- sort(unique(c(vec1, vec2)))
  
  mat = matrix(0, nrow = length(states), ncol = length(states))
  
  for(i in 1:length(vec1)) {
    row = match(vec1[i], states)
    col = match(vec2[i], states)
    mat[row, col] = mat[row, col] + 1
  }
  
  for(i in 1:nrow(mat)) {
    total = sum(mat[i, ])
    if (total > 0){
      mat[i, ] = mat[i, ] / total
    }
  }
  return(mat)
}

rLast = rep(c('A','B','C'),c(3,4,3))
rNow  = rep(c('A','B','C'),c(5,0,5))

# Unit Tests
out = TMAT1(rLast,rNow)
answer = c(1,.5,0,0,0,0,0,.5,1)
dim(answer) = c(3,3)
all.equal(out,answer)


# qTMat2

MatMult = function(vec,mat){
  
  nRowMat = dim(mat)[1]
  nColMat = dim(mat)[2]
  nRowVec = dim(vec)[1]
  nColVec = dim(vec)[2]
  
  ans = matrix(0,nRowVec,nColMat)
  
  # Your vectorized code
  # Only one loop allowed
  for (i in 1:nRowVec) {
    ans[i, ] = vec[i, ] %*% mat
  }
  
  return(ans)
}

# Unit tests

vec = matrix(data = c(20,30,10),nrow = 1,ncol = 3)
mat = c(1,.5,0,0,0,0,0,.5,1)
dim(mat) = c(3,3)

x = MatMult(vec,mat)
x[1] == 35 & x[2] == 0 & x[3] == 25

functionText = capture.output(MyMatMult)
if(length(grep("for",functionText))!=1){
  cat("Only 1 'for' loop allowed!")
}


# qBondPrice

getBondPrice = function(y, face, couponRate, m, ppy=1){
  
  periods = m * ppy
  t = seq(1, periods, 1)
  couponPayment = face * couponRate / ppy
  discountFactor = 1 / (1 + y / ppy)^t
  bondPrice = sum(couponPayment * discountFactor) + face * discountFactor[length(discountFactor)]
  
  return(bondPrice)
}

# Unit tests

y = 0.03
face = 2000000
couponRate = 0.04
m = 10
ppy = 2

x = getBondPrice(y, face, couponRate, m,  1)
round(x) == 2170604

x = getBondPrice(y, face, couponRate, m,  2)
round(x) == 2171686

functionText = capture.output(getBondPrice)
if(length(grep("for",functionText))){
  cat("No 'for' loops allowed!")
}


# qBondDuration

getBondDuration = function(y, face, couponRate, m){
  
  cf = rep(face*couponRate, times = m)
  pv = (1 + y)^ (1: m)
  pvcf = cf / pv
  pvcfsum = sum(pvcf)
  pvface = face / (1 + y)^ m
  bondPrice = pvcfsum + pvface
  
  duration = (sum(pvcf* (1: m)) + pvface * m) / bondPrice
  
  return(duration)
}

# Unit tests

y = 0.03
face = 2000000
couponRate = 0.04
m = 10

x = getBondDuration(y, face, couponRate, m)
round(x,2) == 8.51

functionText = capture.output(getBondDuration)
if(length(grep("for",functionText))){
  cat("No 'for' loops allowed!")
}


# qBondPriceFromCurve

getBondPriceYC = function(y, face, couponRate, m){
  
  cf = c(rep(couponRate * face,9),(couponRate+1) * face)
  t = c(seq(1,m))
  pv = (1+y)**-t
  pvcf = pv*cf
  
  bondprice = sum(pvcf)
  
  return (bondprice)
}

# Unit tests

YC <- read_excel("/Users/chuchung/Desktop/C4Rm_Class5_ME-1.xlsx",
                 sheet = "qBondPriceFromCurve", range = "G3:G12",
                 col_names = FALSE)

YC = as.matrix(YC)
y = .03
face = 2000000
couponRate = 0.04
m = 10
ppy = 2

x1 = getBondPriceYC(y, face, couponRate, m)
round(x1) == 2170604

x2 = getBondPriceYC(YC, face, couponRate, m)
round(x2) ==  1267138

functionText = capture.output(getBondPriceYC)
if(
  length(grep("if",functionText))==0 &
  round(x1) == 2170604 &
  round(x2) ==  1267138
){
  cat("Congraulations!\n")
  cat("You answered without using 'if'.\n")
  cat("You get an extra point in this assignment.")
}


# qConditioning

FizzBuzz = function(start,finish){
  n = finish-start+1
  v = vector(mode = "character",length = n)
  for(i in 1:n) {
    j = start + i - 1
    # j = current number
    if(j %% 15 == 0) {
      v[i] = "fizzbuzz"
    } else if(j %% 3 == 0) {
      v[i] = "fizz"
    } else if(j %% 5 == 0) {
      v[i] = "buzz"
    } else {
      v[i] = as.character(j)
    }
  }
  return(v)
}

# Unit test

x = FizzBuzz(40,45)
x[1] == "buzz" & x[2] == "41" & x[6] == "fizzbuzz"


# qGetReturns

# Create a function called getReturns that returns a vector of returns
# You only need to install once
install.packages("quantmod")
library(quantmod)
getStockData = function(symbol){
  mydata = quantmod::getSymbols(Symbols = symbol,auto.assign = F)
  prices = mydata[,6]
  return(prices)
}

prices = getStockData('gs')
prices
class(prices)
pricevec = as.numeric(prices) # NEED TO CHANGE TO MATRIX OR NUMERIC

getReturns = function(pricevec){
  n = length(pricevec)
  ratiovec = pricevec[2:n]/pricevec[1:(n-1)]
  returns =  ratiovec - 1
  return(returns)
}
returns = getReturns(pricevec)
print(returns)