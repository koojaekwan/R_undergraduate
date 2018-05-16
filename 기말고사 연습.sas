proc sql;
create table work.discount (destination char(3), begindate num format=date9., enddate num format=date9., discount num);

###set�� �̿��Ͽ� ���߰� �ϱ�###
proc sql;
insert into work.discount
set destination='koo', begindate='01mar2000'd, enddate='05mar2000'd, discount=0.33
set destination='kim', begindate='03mar2000'd, enddate='10mar2000'd, discount=0.15;


###��� �÷��� ���ؼ� �� �߰��ϱ�### - �÷��� ������൵ �ǰ� �����൵ ��
proc sql;
insert into work.discount
values('koo','01mar2000'd,'05mar2000'd,0.33)
values('kim','03mar2000'd,'01mar2000'd,0.15);

###�Ϻ� �÷��� �����Ͽ� �� �߰��ϱ�###
proc sql;
insert into work.discount
(destination,discount)
values('koo',0.33);

-------------------------------------------
###�����ؿ���###
proc sql;
create table work.mechanicslevel3_new as
select * from sasuser.mechanicslevel3;

###3ǥ���� 2ǥ�� empid�� 1653�ΰ��� �߰��ϱ�###
proc sql;
insert into work.mechanicslevel3_new
select empid, jobcode,salary
from sasuser.mechanicslevel2
where empid='1653';

--------------------------------------------
### ���������� �÷� �ڿ� �ɾ��ִ� ���###
proc sql;
create table work.employees
(id char(5) primary key, name char(10), gender char(1) not null check(gender in ('M','F')), hdate date label='hire date');

### ���������� ���̺� ���� �޺κп� �����ִ� ���###
proc sql;
create table work.discount3
(destination char(3), begindate num format=date9., enddate num format=date9., discount num,
constraint check_discount check(discount <=0.5), constraint notnull_dest not null(destination));

-------------------------------------------
### undo_policy=none �� ������ ���� �����ִ°�
proc sql undo_policy=none;

### undo_policy=required �� �����ߴ� �͵� �� ������ ������� ���̺��� �������� ��
proc sql undo_policy=required;

-------------------------------------------
### desctibe table constraint �� ���������� ���� �� �ִ�
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

###else�� �����ϸ� �ٸ� ���� �̰̽��� ������ �ȴ�###


--------------------------------------------
proc sql;
create table work.frequentflyer2 as
select ffid, milestraveled, pointsearned, pointsused
from sasuser.frequentflyers;

### ���� ����� ���
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
