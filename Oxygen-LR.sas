libname mylib "";
/* import the data */
proc import out = mylib.regression
Datafile = "" dbms = dlm
replace;
Getnames = YES;
run;quit;
/* view the data */
proc print data = mylib.regression; run;quit;
proc contents varnum data=mylib.regression;
   ods select position;
run;
/* ods word */
ods rtf file=""; 
ods graphics on;
/* linear regression */
proc reg data = mylib.regression alpha = 0.05
          /* make the plot of residual with smooth line */
          plots = (Residuals(smooth) DFBETAS ObservedByPredicted(label));
model Oxygen=Age Weight RunTime RunPulse RestPulse MaxPulse/ r cli clm ;
/* r cli clm is to show the predicted value and residual*/
/* restrict Age + Weight + RunTime + RunPulse + RestPulse + MaxPulse = -3;*/
/* test Weight = RestPulse = 0;*/
output out = result_out
predicted = Fitted
LCLM=Lower Est UCLM=Upper Est LCL=Lower Pred UCL=Upper Pred
residual = Residuals;
ods output ParameterEstimates = mylib_parameters;
ods output ANOVA = mylib_anova;
run;quit;
ods graphics off; ods rtf close;
/*proc anova data = mylib.regression ;
class Age Weight RunTime RunPulse RestPulse MaxPulse;
model Oxygen=Age Weight RunTime RunPulse RestPulse MaxPulse;
run;quit;*/
/* test hypothesis*/
proc reg data = mylib.regression alpha = 0.05;
model Oxygen=Age Weight RunTime RunPulse RestPulse MaxPulse;
/*mtest Weight , RestPulse;*/
test Weight =0 , RestPulse = 0;
run;quit;
/* add restriction */
proc reg data = mylib.regression alpha = 0.05;
model Oxygen=Age Weight RunTime RunPulse RestPulse MaxPulse;
/*delete Weight RestPulse;*/
restrict Weight =0, RestPulse = 0;
run;quit;
/* generalized least squares */
data a;
set mylib.regression;
Weights = 1/Oxygen**2;
run;
/*proc print data = a;run;*/
proc reg data = a;
model Oxygen=Age Weight RunTime RunPulse RestPulse MaxPulse;
weight Weights;
run;quit;
/* import the anova data*/ 
proc import out = mylib.anova
Datafile = "" dbms = excel
replace;
Getnames = YES;
run;quit;
/* view the data */
proc print data = mylib.anova; run;quit;
/* anova */
proc anova data = mylib.anova2;
class Cycle_Time Temperature Operator;
model Score = Cycle_Time | Temperature | Operator;
run;quit;


data mylib.anova2;
set mylib.anova;
Cycle_Time = (Cycle_Time-50)/10;
Temperature = (Temperature-325)/25;
Operator = Operator-2;
Cycle_TimeTemperature = Cycle_Time*Temperature;
Cycle_TimeOperator = Cycle_Time*Operator;
Cycle_TimeTemperatureOperator = Cycle_Time*Temperature*Operator;
run;
proc print data = mylib.anova2;run;quit;
proc reg data = mylib.anova2 /*all*/;
model Score = Cycle_Time  Temperature  Operator Cycle_TimeTemperature  Cycle_TimeOperator Cycle_TimeTemperatureOperator / r cli clm;
run;quit;


/* print data */
proc print data = result_out;
var Oxygen Fitted Lower2 Upper2 Residuals;
run;
proc print data = mylib_parameters;run;
proc print data = mylib_anova;run;quit;

/* useless */
proc contents varnum data=sashelp.baseball;
   ods select position;
run;
proc contents varnum data=sashelp.fish;
   ods select position;
run;
proc print data = sashelp.fitness;
run;quit;

proc reg data=sashelp.baseball;
   id name team league;
   model logSalary = nhits nruns nrbi nbb yrmajor crhits;
run;
data baseball;
   set sashelp.baseball(where=(name^="Rose, Pete"));
   YrMajor2 = yrmajor*yrmajor;
   CrHits2  = crhits*crhits;
run;
proc reg data=baseball
      plots=(diagnostics(stats=none) RStudentByLeverage(label)
             CooksD(label) Residuals(smooth)
             DFFITS(label) DFBETAS ObservedByPredicted(label));
   id name team league;
   model logSalary = nhits nruns nrbi nbb yrmajor crhits
                     yrmajor2 crhits2;
run;
ods graphics off;




data a ;
input type num @@;
cards ;
1 0.84 1 1.05 1 1.20 1 1.20 1 1.39 1 1.53 1 1.67 1 1.80 1 1.87 1 2.07 1 2.11
2 0.54 2 0.64 2 0.64 2 0.75 2 0.76 2 0.81 2 1.16 2 1.20 2 1.34 2 1.35 2 1.48 2 1.56
;
run ;
proc anova ;
class type ;
model num=type ;
means type ;
run ; quit ;


/* predicted */
data USPopulation;
   input Population @@;
   retain Year 1780;
   Year       = Year+10;
   YearSq     = Year*Year;
   Population = Population/1000;
   datalines;
3929 5308 7239 9638 12866 17069 23191 31443 39818 50155
62947 75994 91972 105710 122775 131669 151325 179323 203211
226542 248710 281422
;
/*proc reg data=USPopulation plots=ResidualByPredicted;
   var YearSq;
   model Population=Year / r clm cli;
run;*/
data USPop2;
   input Year @@;
   YearSq=Year*Year;
   datalines;
2010 2020 2030
;
data USPop2;
   set USPopulation USPop2;
run;
proc reg data=USPop2;
   id Year;
   model Population=Year YearSq / r cli clm;
run;quit;


data a;
seed=30;
do times=1 to 3;
do i=1 to 10;
call RANUNI(seed , x1);
x1=x1*10;
call RANUNI(seed , x2);
x2=x2*10;
call RANUNI(seed , x3);
x3=x3*10;
call RANNOR(seed , z);
z=z*2;
y=10+5*x1+2*x2+3*x3+z ;
output ;
end ;
end ;
run ;
proc reg data=a outest=b;
by times ;
model y=x1 x2 x3 ;
restrict x1+ x2+ x3=10;
model y=x1 x2 x3 ;
run;quit ;
