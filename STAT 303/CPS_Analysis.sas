proc import out= schooldata datafile="/home/u41994457/sasuser.v94/stressed.xlsx" dbms=xlsx;
getnames= yes;

proc sort data=schooldata;
by level;
run;

proc print data=schooldata;
run;

proc corr data=schooldata plots=matrix;
var attendance_status actstatus dropout_status ;
title "Correlation between Attendance and ACT Score";
run;

proc corr data= schooldata;
where level= "Level 1";
var attendance_status actstatus dropout_status ;
run;


proc corr data= schooldata;
where level= "Level 2";
var attendance_status actstatus dropout_status ;
run;

proc corr data= schooldata;
where level= "Level 3";
var attendance_status actstatus dropout_status ;
run;

Data nomissingdatahere;
set schooldata;
if level= "Not Enough Data" then delete;
if probation= "Not Applicable" then delete;
roundedact= round(actstatus);
run;

proc gchart data= nomissingdatahere;
vbar probation / group=level;
title "Probation Status by Level";
run;

proc gchart data= nomissingdatahere;
hbar actstatus / group=level;
title "Act Scores by Level";
run;

proc glm data=nomissingdatahere;
class level;
model actstatus=attendance_status;
run;

proc glm data=schooldata;
class level;
model dropout_status=attendance_status;
run;

proc glm data=schooldata;
class level;
model actstatus=dropout_status;
run;

proc reg data= schooldata;
Title "Regression";
model actstatus=attendance_status;
run;

proc reg data= schooldata;
Title "Regression";
model dropout_status=attendance_status;
run;

proc reg data= schooldata;
Title "Regression";
model actstatus=attendance_status;
run;

proc reg data= schooldata;
Title "Regression";
model freshmen_status=attendance_status;
run;

proc reg data= schooldata;
Title "Regression";
model level=attendance_status;
run;

data transform;
set schooldata;
roundedact= round(actstatus);
actstatussq=actstatus**2;
attendancesq=attendance_status**2;
attendanceln=log(attendance_status);
actstatusln=log(actstatus);
run;

proc reg data= transform;
Title "Regression";
model actstatusln=attendance_status attendanceln;
run;

proc gchart data= nomissingdatahere;
vbar roundedact / group= level discrete;
title
run;

Proc freq data= schooldata order=data;
tables level*network / chisq expected;
run;

Proc freq data= schooldata order=data;
tables level*probation / chisq expected;
run;

Proc freq data= schooldata order=data;
tables network*probation / chisq expected;
run;

proc npar1way data=schooldata;
class network;
var actstatus;
run;

proc npar1way data=schooldata;
class network;
var dropout_status;
run;

proc npar1way data=schooldata;
class probation;
var dropout_status;
run;