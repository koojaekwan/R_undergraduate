data store1;
input wk sales;
format sales dollar8.2;
cards;
1 515.07
2 772.29
3 888.88
4 1000.01
;
run;
proc print data=store1;
run;


data store2;
input wk sales;
format sales dollar8.2;
cards;
1 1368.99
2 1506.23
3 1200.57
4 1784.11
5 43.00
;
run;
proc print data=store2;
run;

proc sql;
select *
from store1, store2
where store1.wk=store2.wk;


data bonus;
input id bonus;
cards;
123 5000
456 7000
744 3500
;
run;
proc print data=bonus;
run;


data salary;
input id salary;
cards;
123 70000
456 80000
978 55000
;
run;
proc print data=salary;
run;

proc sql;
select s.*, bonus
from bonus as b
right join
salary as s
on b.id=s.id;


proc sql;
select a.*, duration
from groupa as a, groupb as b
where a.obs=b.obs;

data groupa;
input obs med;
cards;
1 10
2 20
3 30
;
run;

data groupb;
input obs duration;
cards;
1 9
3 8
4 7
;
run;


proc sql;
select a.*, duration
from groupa as a
inner join
groupb as b
on a.obs=b.obs;


proc sql;
select a.obs label='obs', med, b.obs label='obs',duration
from groupa as a, groupb as b
where a.obs=b.obs;


proc sql;
select *
from table1
left join
table2
on table1.g3=table2.g3;

data table1;
input g3 z $;
cards;
89 fl
46 ui
47 ba
;
run;
proc print data=table1;
run;


data table2;
input g3 r $;
cards;
46 bc
85 fl
99 ba
;
run;
proc print data=table2;
run;
