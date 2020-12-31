DATA candles;
PROC IMPORT OUT = candles DATAFILE = "/home/ashhansen60/sasuser.v94/STAT 307/Project/candles.csv" DBMS = CSV REPLACE;
GETNAMES = YES;
RUN;

PROC GLM ;
CLASS Wick Wax;
MODEL Rate = Wick Wax Wick*Wax;
CONTRAST 'Beeswax vs Soy' Wax 1 0 -1;
MEANS Wax / TUKEY;
RUN;

PROC GLM;
CLASS Wick Wax;
MODEL Rate = Wick Wax Wick*Wax;
CONTRAST 'Beeswax vs Paraffin' Wax 1 -1 0;
RUN;

PROC GLM;
CLASS Wick Wax;
MODEL Rate = Wick Wax Wick*Wax;
CONTRAST 'Paraffin vs ' Wax 0 -1 1;
RUN;