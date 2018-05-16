proc sql;
create table work.discount (destination char(3), begindate num format=date9., enddate num format=date9., discount num);

###set을 이용하여 행추가 하기###
proc sql;
insert into work.discount
set destination='koo', begindate='01mar2000'd, enddate='05mar2000'd, discount=0.33
set destination='kim', begindate='03mar2000'd, enddate='10mar2000'd, discount=0.15;


###모든 컬럼에 대해서 행 추가하기### - 컬럼을 명시해줘도 되고 안해줘도 됨
proc sql;
insert into work.discount
values('koo','01mar2000'd,'05mar2000'd,0.33)
values('kim','03mar2000'd,'01mar2000'd,0.15);

###일부 컬럼을 지정하여 행 추가하기###
proc sql;
insert into work.discount
(destination,discount)
values('koo',0.33);

-------------------------------------------
###복사해오기###
proc sql;
create table work.mechanicslevel3_new as
select * from sasuser.mechanicslevel3;

###3표에서 2표의 empid가 1653인것을 추가하기###
proc sql;
insert into work.mechanicslevel3_new
select empid, jobcode,salary
from sasuser.mechanicslevel2
where empid='1653';

--------------------------------------------
### 제약조건을 컬럼 뒤에 걸어주는 방법###
proc sql;
create table work.employees
(id char(5) primary key, name char(10), gender char(1) not null check(gender in ('M','F')), hdate date label='hire date');

### 제약조건을 테이블 생성 뒷부분에 적어주는 방법###
proc sql;
create table work.discount3
(destination char(3), begindate num format=date9., enddate num format=date9., discount num,
constraint check_discount check(discount <=0.5), constraint notnull_dest not null(destination));

-------------------------------------------
### undo_policy=none 은 성공한 행은 남아있는것
proc sql undo_policy=none;

### undo_policy=required 는 성공했던 것도 다 지워서 원래대로 테이블을 돌려놓는 것
proc sql undo_policy=required;

-------------------------------------------
### desctibe table constraint 로 제약조건을 보일 수 있다
proc sql;
describe table constraint work.discount3;


-----------------------------------------------
proc sql;
create table work.payrollmaster_new as
select * from sasuser.payrollmaster;
proc sql;
update work.payrollmaster_new
set salary=salary*1.05 where jobcode like '__1';

proc sql;
update work.payrollmaster_new
set salary=salary*
case when substr(jobcode,3,1)='1' then 1.05
		 when substr(jobcode,3,1)='2' then 1.10
		 when substr(jobcode,3,1)='3' then 1.15
		else 1.08
end;

###else를 생략하면 다른 경우는 미싱값을 가지게 된다###


--------------------------------------------
proc sql;
create table work.frequentflyer2 as
select ffid, milestraveled, pointsearned, pointsused
from sasuser.frequentflyers;

### 행을 지우는 방법
proc sql;
delete from work.frequentflyer2
where pointsearned-pointsused<=0;



proc sql;
alter table work.payrollmaster4
add bonus num format10.2, level char(3);

proc sql;
alter table work.payrollmaster4
drop bonus, level;

proc sql;
alter table work.payrollmaster4
modify salary format=doaar11.2 label='salary amt';


proc sql;
drop table work.payrollmaster4;
