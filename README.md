# InvestmentGame-Mathematica-
(*Analyze statistical data such as expected values, standard deviation, and histogram trends to optimize     gambling algorithm
Consists of a dynamic betting strategy that changes based on buying power at any given time*)



initializeMoney := (totalMoney = 10);

returnedMoney[w_] := 
 Module[{x}, (x = RandomVariate[NormalDistribution[1.1, 1]]; 
   If[x < 0, x = 0];
   w*x)];
   
investStep[tot_] := 
  If[(tot < .01) || (tot < myWager[tot]), 0, 
   tot - myWager[tot] + returnedMoney[myWager[tot]]];
   
Clear[myWager];
myWager[totalMoney_] := 
  Piecewise[{{totalMoney/5, totalMoney <= 250}, {125, 
     250 < totalMoney < 600}, {75, 600 <= totalMoney < 800}, {50, 
     800 <= totalMoney < 1000}, {1, totalMoney >= 1000}}];
     
     (*A sample trial*)
initializeMoney;
accountBalances = NestList[investStep, totalMoney, 500];
accountBalances[[500]]
ListPlot[accountBalances, AxesLabel -> {Attempts Number, Total Money}]
![image](https://user-images.githubusercontent.com/102829980/161417101-006e797e-ec5e-48b7-82e0-c5d487f61e3e.png)

-----------------------------
trials = Table[NestList[investStep, totalMoney, 500], 1000];
endingvalues = trials[[All, 500]];
Mean[endingvalues]
Histogram[endingvalues, AxesLabel -> {Total Money, Amounts of Trials}]
![image](https://user-images.githubusercontent.com/102829980/161417113-7dbb500e-38d7-44fd-8260-0cfc6c416c43.png)
