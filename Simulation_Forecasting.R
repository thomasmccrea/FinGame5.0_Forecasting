Fprice = c(96.55,100.08,103.92,100.45,96.48,96.56,104.27,100.04,93.70,102.81,106.33,103.91,92.43,106.04,109.79,104.91,98.03,107.00,113.82,105.84,102.68,107.58)
Fquant = c(90470,97650,113806,100071,88942,88389,116838,103233,78261,110505,118884,108513,82714,110092,131662,109263,89138,120936,148688,114574,110718,121530)
Aprice = c(97.69,99.67,104.95,101.65,96.04,96.37,105.94,100.66,94.72,103.30,107.26,103.96,93.73,106.27,110.56,104.62,97.36,107.26,115.18,106.93,103.30,107.92)
Aquant = c(90287,96565,111696,102201,85452,86504,114925,99124,82057,107219,119019,109212,78782,115524,128168,110964,89789,119010,142059,117793,107299,120512)

pricediff = Aprice - Fprice
quantdiff = Aquant - Fquant

mean(pricediff)
max(pricediff)
min(pricediff)
sd(pricediff)

mean(quantdiff)
max(quantdiff)
min(quantdiff)
sd(quantdiff)

plot(Aquant,Aprice)
plot(Fquant,Fprice)


sym = lm(Aprice~Aquant)
summary(sym)

q2price = c(97.69,101,103.5,96.55,96.55,98.5,95,104.5,93,89,120,105,98,100,97,98,96,98,99,96.55,97.5,103.5)
q2quant = c(90287,87365,85272,91293,91293,89572,92660,84365,94425,97955,70598,83836,90013,89100,90895,90013,91778,90013,89130,91293,90454,85257)
q2data = data.frame(q2price,q2quant)


plot(q2quant,q2price)

q2pricep = c(97.69,101,103.5,96.55,96.55,98.5,95,104.5,93,89,120,105,98,97,98,96,98,99,96.55,97.5,103.5)
q2quantp = c(90287,87365,85272,91293,91293,89572,92660,84365,94425,97955,70598,83836,90013,90895,90013,91778,90013,89130,91293,90454,85257)
plot(q2quantp,q2pricep)

combprice = c(q2pricep,Aprice)
combquant = c(q2quantp,Aquant)

plot(combquant,combprice)

solid = lm(q2pricep~q2quantp)
summary(solid)
summary(sym)
-1/sym$coefficients[2]
solid$coefficients[2]

-1/solid$coefficients[2]
sym$coefficients[2]
abline(sym)
abline(solid)

aslope = -1/sym$coefficients[2]
mslope = -1/solid$coefficients[2]
aintercept = (Aprice[1]/(-1/sym$coefficients[2]*Aquant[1]))
mintercept = (Aprice[1]/(-1/solid$coefficients[2]*Aquant[1]))

y1 = -1/sym$coefficients[2]*x + (Aprice[1]/(-1/sym$coefficients[2]*Aquant[1]))
y2 = -1/solid$coefficients[2]*x + (Aprice[1]/(-1/solid$coefficients[2]*Aquant[1]))
abline(aslope,aintercept,col="blue")
abline(mslope,mintercept,col="red")


forecastp = c(Aprice,Fprice)
forecastq = c(Aquant,Fquant)
actual = c(rep(1,length(Aprice)),rep(2,length(Fprice)))

forecast = data.frame(forecastp,forecastq,actual)
forecast

##For plotting Forecast lag1 and actual
plot(forecastq,forecastp,col=forecast$actual,pch=16)
abline(sym)

for (i in 1:length(Aprice)){
  segments(Aquant[i],Aprice[i],x1=Fquant[i],y1=Fprice[i],col = "blue")
}

##Adding new forecast without corresponding actual
points(134272,114.36)
pri = sym$coefficients[1]+sym$coefficients[2]*(134272-(2*sd(quantdiff)))
pri



sym-Fquant
reg = lm((Fquant-Aquant)~Fquant)
predict(reg, data.frame(Fquant = 100000))
summary(reg)

##In case it doesn't appear b/c out of original range
plot(x=c(Aquant,148688),y=c(Aprice,113.82))

outofrange = forecast
outofrange[nrow(outofrange)+1,] = c(113.82,148688,3)
outofrange[nrow(outofrange)+1,] = c(predict(sym,newdata = data.frame(Aquant=148688)),148688,4)

plot(outofrange$forecastq,outofrange$forecastp,col=outofrange$actual,pch=16)
abline(sym)

for (i in 1:length(Aprice)){
  segments(Aquant[i],Aprice[i],x1=Fquant[i],y1=Fprice[i],col = "blue")
}

ma = lm(Aquant~Aprice)
q = ma$coefficients[1] + ma$coefficients[2]*100.04
p = sym$coefficients[1] + sym$coefficients[2]*103233

##Adding new forecast without corresponding actual vertical/horizontal lines to SLR
points(q,100.04)
points(103233,p)
segments(103233,100.04,q,100.04)
segments(103233,100.04,103233,p)

F2price = c(97.97,104.60,103.22,94.88,94.60,106.48,101.08,96.15,103.53,106.64,103.66,95.23,105.01,111.95,101.98,98.06,105.00,112.80,106.68,103.44,105.28,115.29,105.60)
F2quant = c(91481,113056,108009,92712,93025,117876,91258,74657,117402,114442,108550,84422,126047,134936,109070,92610,126906,154069,114979,98841,116342,136954,115304)
F3price = c(103.07,99.55,97.70,93.56,108.38,102.27,93.46,103.65,103.13,101.94,92.79,108.70,106.86,104.88,94.42,104.62,115.06,107.36,103.97,104.10,111.75,104.45,103.06)
F3quant = c(122106,97199,94381,80074,110079,112563,70389,92886,104188,111423,78239,99000,139308,106912,99785,111265,135090,121648,111467,138171,121944,118-47,96540)
F4price = c(101.11,91.51,91.60,102.39,96.11,95.18,104.12,109.89,103.59,92.61,106.69,114.33,108.58,93.54,103.00,115.47,105.21,101.34,109.67,117.08,104.56,104.29,100.54)
F4quant = c(88177,97898,96421,95800,117385,84369,119509,102088,116692,75848,136094,123791,115712,92949,142397,148142,117675,92821,119113,159375,98969,93450,122427)


sd(F2quant[1:8]-Aquant[2:9])
length(F2quant)

##For individual look at lag n forecast and actual
for (i in 1:(length(Aprice)-1)){
  segments(Aquant[i+1],Aprice[i+1],x1=F2quant[i],y1=F2price[i],col = "green")
  points(F2quant[i],F2price[i],col="green")
}

for (i in 1:(length(Aprice)-2)){
  segments(Aquant[i+2],Aprice[i+2],x1=F3quant[i],y1=F3price[i],col = "orange")
  points(F3quant[i],F3price[i],col="orange")
}

for (i in 1:(length(Aprice)-3)){
  segments(Aquant[i+3],Aprice[i+3],x1=F4quant[i],y1=F4price[i],col = "purple")
  points(F4quant[i],F4price[i],col="purple")
}

biggestprice =c(Aprice,Fprice,F2price[1:(length(F2price)-1)],F3price[1:(length(F2price)-2)],F4price[1:(length(F2price)-3)])
biggestquant =c(Aquant,Fquant,F2quant[1:(length(F2price)-1)],F3quant[1:(length(F2price)-2)],F4quant[1:(length(F2price)-3)])
biggestcode =c(rep(1,length(Aprice)),rep(2,length(Fprice)),rep(3,(length(F2price)-1)),rep(4,(length(F3price)-2)),rep(5,(length(F4price)-3)))
biggestcast = data.frame(biggestprice,biggestquant,biggestcode)

#plot of all available information with corresponding actual
plot(biggestquant,biggestprice,col=biggestcast$biggestcode,pch=16)
abline(sym)

for (i in 1:length(Aprice)){
  segments(Aquant[i],Aprice[i],x1=Fquant[i],y1=Fprice[i],col = "blue")
  if (i <= (length(Aprice)-1)){
    segments(Aquant[i+1],Aprice[i+1],x1=F2quant[i],y1=F2price[i],col = "green")
  }
  if (i <= (length(Aprice)-2)){
    segments(Aquant[i+2],Aprice[i+2],x1=F3quant[i],y1=F3price[i],col = "orange")
  }
  if (i <= (length(Aprice)-3)){
    segments(Aquant[i+3],Aprice[i+3],x1=F4quant[i],y1=F4price[i],col = "purple")
  }
}

##To check code
abline(v=90287)
abline(v=96565)
abline(v=111696)


bigprice = c(Aprice[-c(1:3)],Fprice[-c(1:3)],F2price[-c(1:2,length(F2price))],F3price[-c(1,(length(F3price)-1),length(F3price))],F4price[-c((length(F4price)-2):length(F4price))])
bigquant = c(Aquant[-c(1:3)],Fquant[-c(1:3)],F2quant[-c(1:2,length(F2quant))],F3quant[-c(1,(length(F3quant)-1):length(F3quant))],F4quant[-c((length(F4quant)-2):length(F4quant))])
code = c(rep(1,(length(Aprice)-3)),rep(2,(length(Aprice)-3)),rep(3,(length(Aprice)-3)),rep(4,(length(Aprice)-3)),rep(5,(length(Aprice)-3)))
bigcast = data.frame(bigprice,bigquant,code)

##plot of all actual with 4 corresponding
plot(bigquant,bigprice,col=bigcast$code,pch=16)
abline(sym)

for (i in 1:(length(Aprice)-1)){
  segments(Aquant[i+3],Aprice[i+3],x1=Fquant[i+3],y1=Fprice[i+3],col = "blue")
  
    segments(Aquant[i+1],Aprice[i+1],x1=F2quant[i],y1=F2price[i],col = "green")
  
  
    segments(Aquant[i+2],Aprice[i+2],x1=F3quant[i],y1=F3price[i],col = "orange")
  
  
    segments(Aquant[i+3],Aprice[i+3],x1=F4quant[i],y1=F4price[i],col = "purple")
  
}

##Conic regression
q3p = c(99.67,101.00,107.00,100.18,101.75,126.00,110.00,106.5,98,130,95.00,97.00,100,117)
q3q = c(96565,92468,75116,94985,90192,29549,66737,76333,101815,21963,111547,105876,95541,48884)
q4p = c()
q4q = c()
q5p = c()
q5q = c()
q6p = c()
q6q = c()
q7p = c()
q7q = c()
q8p = c()
q8q = c()
q9p = c()
q9q = c()


demand = data.frame(Simulation_Demand_curve_data[3:26,3:27])
demand

q3conic = lm()



##Distance of forecast



##System of equations
A = rbind(c((82057**2),(82057*94.72),(94.72**2),(82057),(94.72),1),
          c((85258**2),(85258*93.5),(93.5**2),(85258),(93.5),1),
          c((84729**2),(84729*93.7),(93.7**2),(84729),(93.7),1),
          c((84756**2),(84756*93.69),(93.69**2),(84756),(93.69),1),
          c((68796**2),(68796*100),(100**2),(68796),(100),1),
          c((86585**2),(86585*93),(93**2),(86585),(93),1))
B = c(rep(0,6))

solve(A,B)

##example
C = rbind(c(1,2,3),
         c(9,3,8),
         c(4,4,6))
D = c(20,483,81)

coef = solve(C,D)

det(A)

row1 = c((82057**2),(82057*94.72),(94.72**2),(82057),(94.72),1)
row2 = c((85258**2),(85258*93.5),(93.5**2),(85258),(93.5),1)
row3 = c((84729**2),(84729*93.7),(93.7**2),(84729),(93.7),1)
row4 = c((84756**2),(84756*93.69),(93.69**2),(84756),(93.69),1)
row5 = c((68796**2),(68796*100),(100**2),(68796),(100),1)
row6 = c((86585**2),(86585*93),(93**2),(86585),(93),1)

a = matrix(c(row1,row2,row3,row4,row5,row6),nrow = 6,byrow = T)


##Q11 Quick moc up
q11p = c()
q11q = c()



##Landon SSE optimization

#y = a(x – h)^2
#y = ax^2 + bx + c

Q11data = data.frame(Simulation_Demand_curve_data[3:27,30:32])
Q11data

#Removing the dropout
Q11data = Q11data[-10,]

#removing any points without ending inventory
n=1
junk = c()
for (i in Q11data$Ending.Inv....32){
  if (i == 0){
    junk = c(junk,n)
  }
  n = n + 1
}
Q11data = Q11data[-junk,]

#removing repeating points
n = 1
junk = c()
exist = c()
for (i in Q11data$Price...31){
  if (!(i %in% exist)){
    exist = c(exist,i)
  }
  else{
    junk = c(junk,n)
  }
  n = n+1
}
Q11data = Q11data[-junk,]
Q11data

plot(Q11data$Quantity...30,Q11data$Price...31)
points(Q11data[1,1],Q11data[1,2],pch=16,col="red")
for (i in 2:length(Q11data$Price...31)){
  segments(Q11data[1,1],Q11data[1,2], x1 = Q11data[i,1], y1 = Q11data[i,2])
}

######################################################################################################################
#Q10 data

Q10data = data.frame(Simulation_Demand_curve_data[3:27,27:29])
Q10data

#Removing the dropout
Q10data = Q10data[-10,]

#removing any points without ending inventory
n=1
junk = c()
for (i in Q10data$Ending.Inv....29){
  if (i == 0){
    junk = c(junk,n)
  }
  n = n + 1
}
Q10data = Q10data[-junk,]

#removing repeating points
n = 1
junk = c()
exist = c()
for (i in Q10data$Price...28){
  if (!(i %in% exist)){
    exist = c(exist,i)
  }
  else{
    junk = c(junk,n)
  }
  n = n+1
}
Q10data = Q10data[-junk,]
Q10data

plot(Q10data$Quantity...27,Q10data$Price...28)
points(Q10data[1,1],Q10data[1,2],pch=16,col="red")
for (i in 2:length(Q10data$Price...28)){
  segments(Q10data[1,1],Q10data[1,2], x1 = Q10data[i,1], y1 = Q10data[i,2])
}

### Function to clean each quarter's data

clean = function(QuarterData){
  
  #Removing the dropout
  QuarterData = QuarterData[-10,]
  
  #removing any points without ending inventory
  n=1
  junk = c()
  for (i in QuarterData[,3]){
    if (i == 0){
      junk = c(junk,n)
    }
    n = n + 1
  }
  if (!(length(junk) == 0)){
    QuarterData = QuarterData[-junk,]
  }
  
  #removing repeating points
  n = 1
  junk = c()
  exist = c()
  for (i in QuarterData[,2]){
    if (!(i %in% exist)){
      exist = c(exist,i)
    }
    else{
      junk = c(junk,n)
    }
    n = n+1
  }
  if (!(length(junk) == 0)){
    QuarterData = QuarterData[-junk,]
  }
}


### Q13
Q13data = data.frame(Simulation_Demand_curve_data[3:27,36:38])
Q13data = clean(Q13data)

plot(Q13data$Quantity...36,Q13data$Price...37)
points(Q13data[1,1],Q13data[1,2],pch=16,col="red")
for (i in 2:length(Q13data$Price...37)){
  segments(Q13data[1,1],Q13data[1,2], x1 = Q13data[i,1], y1 = Q13data[i,2])
}

### Q14
Q14data = data.frame(Simulation_Demand_curve_data[3:27,39:41])
Q14data = clean(Q14data)

plot(Q14data$Quantity...39,Q14data$Price...40)
points(Q14data[1,1],Q14data[1,2],pch=16,col="red")
for (i in 2:length(Q14data$Price...40)){
  segments(Q14data[1,1],Q14data[1,2], x1 = Q14data[i,1], y1 = Q14data[i,2])
}


####
#back to Q11, setting up the landon solution

#adjust to orgin
Q11Qorgin = Q11data$Quantity...30 - Q11data$Quantity...30[1]
Q11Porgin = Q11data$Price...31 - Q11data$Price...31[1]
q11orgin = data.frame(Q11Qorgin,Q11Porgin)

#turn to vertical symmetry (guessing)


#### Marketable securities
rfyield = c(1.181,1.116,1.005,1.012,1.108,1.012,0.904,1.060,1.038,0.983,0.812,1.021,0.993,0.903,0.714,0.849,0.851,0.751,0.707,0.86,0.723,0.653,0.583)
plot(rfyield)

for (i in 1:(length(rfyield)-1)){
  segments(i,rfyield[i],x1=(i+1),rfyield[i+1])
}

r2yield = c(0,91023/2000000,47337/2000000,0,18188/500000,0,0,0,0,0,-1223/2750000,-644/(23932+25000),1745/(47434+15000),1849/63517,0,0,0,0,561/250000,0,0,0,0)
r3yield = c(0,39642/500000,6284/268992,0,0,0,0,-5412/(500000+193326),0,0,-148661/(681177+1250000),-93847/(1766832+2000000),108016/(1927209+1000000),0,0,0,0,0,0,0,0,0,0)
r4yield = c(0,0,0,0,0,0,0,0,0,0,0,0,304120/(3634519+3000000),48725/1000000,-734343/(6863949+3000000),0,0,0,0,0,0,0,0)
r5yield = c(0,34980/200000,0,0,0,29298/500000,0,0,0,0,-65994/500000,0,0,0,0,0,0,0,0,-5133/(248353+250000),0,0,0)
r9yield = c(0,0,0,0,126959/1000000,0,0,-44800/1000000,0,0,0,0,0,0,0,-286221/3000000,645441/(2671329+2000000),154999/(5232328+2000000),-17/1000,-204557/8000968,840600/47666742,7659842/(24507530+40000000),-308721/(34475276-17000000))

allyield = c(rfyield[1:length(r2yield)],100*r2yield,100*r3yield,100*r4yield,100*r5yield,100*r9yield)
yieldtime = c(1:length(r2yield))
yieldcode = c(rep(1,length(r2yield)),rep(2,length(r2yield)),rep(3,length(r2yield)),rep(4,length(r2yield)),rep(5,length(r2yield)),rep(9,length(r2yield)))

MSyield = data.frame(allyield,yieldtime,yieldcode)
MSyield
plot(rep(yieldtime,6),allyield,col=yieldcode,pch=16)          

MSreturn = function(yield){
  plot(c(1:length(yield)),yield,xlab="Quarters",ylab = deparse(substitute(yield)))
  
  for (i in 1:(length(yield)-1)){
    segments(i,yield[i],x1=(i+1),yield[i+1])
  }
}

MSreturn(rfyield)

MSsegment = function(yield){
  for (i in 1:(length(yield)-1)){
    segments(i,yield[i],x1=(i+1),yield[i+1])
  }
}

yieldnames = data.frame(rf=rfyield[1:length(r2yield)],r2=100*r2yield,r3=100*r3yield,r4=100*r4yield,r5=100*r5yield,r9=100*r9yield)

for (i in yieldnames){
  MSsegment(i)
}

?as.name

TRUEseg = function(yield){
  for (i in 1:(length(yield)-1)){
    if (!(yield[i] == 0) & !(yield[i+1] == 0)){
      segments(i,yield[i],x1=(i+1),yield[i+1])
    }
  }
}

for (i in yieldnames){
  TRUEseg(i)
}


#CAPM

MSdata = data.frame(rfyield,r2yield,r3yield,r4yield,r5yield,r9yield)
MSdata

beta = c(1:9)/5
sr1 = c()
sr2 = c()
sr3 = c()
sr4 = c()
sr5 = c()
sr6 = c()
sr7 = c()
sr8 = c()
sr9 = c()
rm = c()

for (i in 1:length(rfyield)){
  if (!(r5yield[i] == 0)){
    rm = c(rm,r5yield[i]*100)
  }
  else if (!(r2yield[i]+r3yield[i]+r4yield[i]+r9yield[i] == 0)){
    weighted = 0
    weights = 0
    if (!(r2yield[i] == 0)){
      weighted = weighted + ((((r2yield[i]*100) - rfyield[i])/beta[2])+rfyield[i])
      weights = weights + 1
    }
    if (!(r3yield[i] == 0)){
      weighted = weighted + ((((r3yield[i]*100) - rfyield[i])/beta[3])+rfyield[i])
      weights = weights + 1
    }
    if (!(r4yield[i] == 0)){
      weighted = weighted + ((((r4yield[i]*100) - rfyield[i])/beta[4])+rfyield[i])
      weights = weights + 1
    }
    if (!(r9yield[i] == 0)){
      weighted = weighted + ((((r9yield[i]*100) - rfyield[i])/beta[9])+rfyield[i])
      weights = weights + 1
    }
    rm = c(rm, weighted/weights)
  }
  else{
    rm = c(rm,0)
  }
}

while (0 %in% rm[2:length(rm)]){
  for (i in 2:length(rm)){
    if (rm[i] == 0){
      rm[i] = (rm[i-1] + rm[i+1])/2
    }
  }
}
    
for (i in 1:length(rfyield)){
  sr1 = c(sr1, (rfyield[i] + (beta[1])*(rm[i] - rfyield[i])))
  sr2 = c(sr2, (rfyield[i] + (beta[2])*(rm[i] - rfyield[i])))
  sr3 = c(sr3, (rfyield[i] + (beta[3])*(rm[i] - rfyield[i])))
  sr4 = c(sr4, (rfyield[i] + (beta[4])*(rm[i] - rfyield[i])))
  sr5 = c(sr5, (rfyield[i] + (beta[5])*(rm[i] - rfyield[i])))
  sr6 = c(sr6, (rfyield[i] + (beta[6])*(rm[i] - rfyield[i])))
  sr7 = c(sr7, (rfyield[i] + (beta[7])*(rm[i] - rfyield[i])))
  sr8 = c(sr8, (rfyield[i] + (beta[8])*(rm[i] - rfyield[i])))
  sr9 = c(sr9, (rfyield[i] + (beta[9])*(rm[i] - rfyield[i])))
}

simulatedyield = data.frame(sr1,sr2,sr3,sr4,sr5,sr6,sr7,sr8,sr9)
sally = c(sr1,sr2,sr3,sr4,sr5,sr6,sr7,sr8,sr9)
sallytime = c(1:length(rm))
sallycode = c()
for (i in 1:9){
  sallycode = c(sallycode,rep(i,length(rm)))
}
simdata = data.frame(sally,sallytime,sallycode)
plot(rep(sallytime,9),sally,col=sallycode,pch=16)  

#Checking correlations
temp = data.frame(Aquant,Aprice,rfyield[2:(length(Aquant)+1)],r2yield[2:(length(Aquant)+1)],r3yield[2:(length(Aquant)+1)],r4yield[2:(length(Aquant)+1)],r5yield[2:(length(Aquant)+1)],r9yield[2:(length(Aquant)+1)])
#not great due to using 0's as missing data, correlations inappropriate with them as they are
cor(temp)

plot(Aprice)

### Regression Model with all lags
corresponding = data.frame(P0=Aprice[-c(1:3)],Q0=Aquant[-c(1:3)],P1=Fprice[-c(1:3)],Q1=Fquant[-c(1:3)],P2=F2price[-c(1:2,(length(F2price)-1),length(F2price))],Q2=F2quant[-c(1:2,(length(F2quant)-1),length(F2quant))],P3=F3price[-c(1,(length(F3price)-2):length(F3price))],Q3=F3quant[-c(1,(length(F3quant)-2):length(F3quant))],P4=F4price[-c((length(F4price)-3):length(F4price))],Q4=F4quant[-c((length(F4quant)-3):length(F4quant))])
corresponding

bigreg = lm(P0~P1+Q1+P2+Q2+P3+Q3+P4+Q4,data = corresponding)
summary(bigreg)

library()


### GTA#2 ###

#Figure 1
plot(Aquant,xlab = "Quarters",ylab = "Quantity",main = "Figure 1")
for (i in 1:(length(Aquant)-1)){
 segments(i,Aquant[i],x1=i+1,y1=Aquant[i+1]) 
}

#Figure 2
x=c(97650,96943,94621)
y=c(100.08,98.47,98.58)
plot(x,y,pch=16,xlab = "Quantity",ylab = "Price",main = "Figure 2")
points(Aquant[2],Aprice[2])
points(mean(x),mean(y),pch=10)
for (i in 1:3){segments(x[i],y[i],x1=Aquant[2],y1=Aprice[2])}
segments(mean(x),mean(y),x1=Aquant[2],y1=Aprice[2],lty = 2)
legend("topleft",legend = c("Forecasts","Mean","Actual"),pch = c(16,10,1))

#Figure 3
plot(Aquant,Aprice,pch=16,xlab = "Quantity",ylab = "Price",main = "Figure 3")
abline(sym)

#Figure 4
plot(biggestquant,biggestprice,col=biggestcast$biggestcode,pch=16,xlab = "Quantity",ylab = "Price",main = "Figure 4")
legend("topleft",legend = c("Actual","Lag 1","Lag 2","Lag 3","Lag 4"),pch = c(rep(16,5)),col = c("black","red","green","blue","light blue"))
abline(sym)

for (i in 1:length(Aprice)){
  segments(Aquant[i],Aprice[i],x1=Fquant[i],y1=Fprice[i],col = "blue")
  if (i <= (length(Aprice)-1)){
    segments(Aquant[i+1],Aprice[i+1],x1=F2quant[i],y1=F2price[i],col = "green")
  }
  if (i <= (length(Aprice)-2)){
    segments(Aquant[i+2],Aprice[i+2],x1=F3quant[i],y1=F3price[i],col = "orange")
  }
  if (i <= (length(Aprice)-3)){
    segments(Aquant[i+3],Aprice[i+3],x1=F4quant[i],y1=F4price[i],col = "purple")
  }
}

#Figure 5
plot(forecastq,forecastp,col=forecast$actual,pch=16,xlab = "Quantity",ylab = "Price",main = "Figure 5")
legend("topleft",legend = c("Actual","Lag 1 forecast","New forecast"),pch = c(rep(16,2),1),col = c("black","red"))
abline(sym)

for (i in 1:length(Aprice)){
  segments(Aquant[i],Aprice[i],x1=Fquant[i],y1=Fprice[i],col = "blue")
}

points(95000,97)

#Figure 6
plot(c(newplot$foreq,predict(stepregquant,newdata = newdata)),c(newplot$forep,predict(stepreg,newdata = newdata)),col=c(newplot$code,3),pch=16,xlab = "Quantity",ylab = "Price",main = "Figure 6")
legend("topleft",legend = c("Actual","Better forecast","New forecast"),pch = c(rep(16,3)),col = c("black","red","green"))
abline(sym)

for (i in 1:(length(newplot$foreq)/2)){
  segments(Aquant[i+3],Aprice[i+3],x1=fitted(stepregquant)[i],y1=fitted(stepreg)[i],col = "blue")
}



#### Stepwise regression

library(olsrr)
ols_step_both_p(bigreg)
stepreg = lm(P0~P1+P2+Q4+P3,data = corresponding[1:13,])
summary(stepreg)
fitted(stepreg)
Aprice[-c(1:3)]
length(Aprice)

newpricediff = fitted(stepreg)[-c(14:15)] - Aprice[-c(1:3,17:18)]
sd(pricediff)
#can't do this (not taking into account df)
sd(newpricediff)

#instead
(sum(newpricediff**2)/8)**(1/2)


bigregquant = lm(Q0~P1+P2+P3+P4+Q1+Q2+Q3+Q4,data = corresponding)
ols_step_both_p(bigregquant)
stepregquant = lm(Q0~P1+P2+Q4+Q3+P3,data = corresponding[1:13,])
summary(stepregquant)
newquantdiff = Aquant[-c(1:3,17:18)] - fitted(stepregquant)[-c(14:15)]
sd(quantdiff)
#still inappropriate
sd(newquantdiff)

#instead
(sum(newquantdiff**2)/7)**(1/2)


newpri = sym$coefficients[1]+sym$coefficients[2]*(predict(stepregquant,newdata = newdata)-(2*sd(newquantdiff)))
newpri


newdata = data.frame(P1=113.82,P2=F2price[(length(F2price)-1)],Q4=F4quant[(length(F4quant)-3)],P3=F3price[(length(F3price)-2)])
predict(stepreg,newdata = newdata)-(2*sd(newpricediff))

newdataquant = data.frame(P1=113.82,P2=F2price[(length(F2price)-1)],Q4=F4quant[(length(F4quant)-3)],Q3=F3quant[(length(F3quant)-2)],P3=F3price[(length(F3price)-2)])
predict(stepregquant,newdata = newdataquant)

#checking if 97.5% min valid
#price
checkpri = c()
for (i in 1:(length(Fquant)-5)){
  checkfit = (fitted(stepreg)[i]) - (2*sd(newpricediff))
  checkpri = c(checkpri,checkfit)
}

checkpri
Aprice[-c(1:3,17:18)]

Aprice[-c(1:3,17:18)] - checkpri

#quantity
checkqua = c()
for (i in 1:(length(Fquant)-5)){
  checkfit = (fitted(stepregquant)[i]) - (2*sd(newquantdiff))
  checkqua = c(checkqua,checkfit)
}

checkqua
Aquant[-c(1:3,17:18)]

Aquant[-c(1:3,17:18)] - checkqua

#New visualization of prediction accuracy
newplot = data.frame(foreq = c(Aquant[-c(1:3,17:18)],fitted(stepregquant)),forep = c(Aprice[-c(1:3,17:18)],fitted(stepreg)),code = c(rep(1,(length(Fquant)-5)),rep(2,length(fitted(stepreg)))))
newplot

plot(newplot$foreq,newplot$forep,col=newplot$code,pch=16,xlab = "Quantity",ylab = "Price")
abline(sym)

for (i in 1:(length(newplot$foreq)/2)){
  segments(Aquant[i+3],Aprice[i+3],x1=fitted(stepregquant)[i],y1=fitted(stepreg)[i],col = "blue")
}

###updated models

u_newdata = data.frame(P1=107.58,Q1=121530,P2=F2price[length(F2price)-1],Q2=F2quant[length(F2quant)-1],P3=F3price[length(F3price)-2],Q3=F3quant[length(F3quant)-2],P4=F4price[length(F4price)-3],Q4=F4quant[length(F4quant)-3])

u_stepreg = lm(P0~P1+P2+Q1,data = corresponding)
summary(u_stepreg)
u_newpricediff = Aprice[-c(1:3)] - fitted(u_stepreg)
(sum(u_newpricediff**2)/11)**(1/2)
predict(u_stepreg,newdata = u_newdata)

u_stepregquant = lm(Q0~P1+P2+Q1+Q3,data = corresponding)
summary(u_stepregquant)
u_newquantdiff = Aquant[-c(1:3)] - fitted(u_stepregquant)
(sum(u_newquantdiff**2)/11)**(1/2)
predict(u_stepregquant,newdata = u_newdata)

#checking new price
points(predict(u_stepregquant,newdata = u_newdata),predict(u_stepreg,newdata = u_newdata))
u_pri = sym$coefficients[1]+sym$coefficients[2]*(predict(u_stepregquant,newdata = u_newdata)-(2*sd(u_newquantdiff)))
u_pri


###Checking real error of best predictions

for (i in 1:dim(corresponding)[1]){
  tempreg = lm(P0~P1+P2+P3+P4+Q1+Q2+Q3+Q4,data = corresponding[1:i,])
  bestpriceols = ols_step_both_p(tempreg)
  tempregquant = lm(Q0~P1+P2+P3+P4+Q1+Q2+Q3+Q4,data = corresponding[1:i,])
  bestquantols = ols_step_both_p(tempregquant)
  
  
}

library(olsrr)
te = ols_step_both_p(bigreg)
te$predictors[1]

lm(parse(text = "P0~P1+P2+Q1+Q3"),data = corresponding[1:4,])

?parse
parse(file = te$predictors[1])

