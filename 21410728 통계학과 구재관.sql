# University 스키마를 사용하여, 다음 질의를 SQL로 작성하시오. MySQL Workbench에서 잘 작동하는지 확인할 것.

# 1. Find the names of all students who have taken at least one Comp.sci. course; 
# make sure there are no duplicate names in the results.
# 적어도 하나의 컴퓨터공학과(Comp. Sci.)의 수업을 들었던 모든 학생의 이름을 구하라. 결과에는 중복되는 이름이 없도록 하라.
select distinct name
from (select id from takes natural join course where dept_name='Comp. Sci. ') as t1 natural left join student;

select distinct name
from student
where id in (select id from takes join course where course.course_id=takes.course_id and dept_name='Comp. Sci. ');
# 2. For each department, find the maximum salary of instructors in that department.
# You may assume that every department has at least one instructor.
# Make sure the instructor's name, department, and salary are displayed in the output.
# 각 학과에 대해, 학과에서 교수들의 최대 급여를 구하라. 모든 학과는 적어도 한 명의 교수가 있다고 가정해도 된다. 
# 출력 결과에는 교수 이름, 학과, 급여가 나타나도록 하라.
select name, dept_name, salary
from university.instructor as t1
where salary =(select max(salary) from university.instructor as t2 where t1.dept_name=t2.dept_name);


select name, dept_name, salary
from university.instructor natural left join (select dept_name, max(salary) as max_salary from university.instructor group by dept_name) as koo
where salary=max_salary;
# 3. Find the lowest, across all departments, of the per-department maximum salary computed by the preceding query.
# Make sure the instructor's name, department, and salary are displayed in the output.
# 2번의 질의에서 계산한 학과별 최대 급여 중에서 모든 학과 중 가장 작은 값을 구하라. 출력 결과에는 교수 이름, 학과, 급여가 나타나도록 하라.
select name, dept_name, salary
from university.instructor as t1
where salary=(select min(salary) from university.instructor as t2 where salary=(select max(salary) from university.instructor as t3 
where t2.dept_name=t3.dept_name));
# 4. Find the IDs and names of all students who have not taken any course offering before Spring 2010.
# 2010년 봄 이전에 개설된 어떤 수업도 듣지 않은 모든 학생의 아이디와 이름을 구하라. <-2009년에 개설된 어떤 수업도 듣지 않았다는 의미와 같다.
select distinct t1.id, name
from university.student as t1 natural left join university.takes as t2
where id not in(select id from university.takes where year<2010);
# 5. Find the sections that had the maximum enrollment in Autumn 2009.
# 2009년 가을에 최대 등록자 수를 가지는 분반의 course id, section id, 등록자 수를 구하라.
select course_id, sec_id, count_id
from (select course_id, sec_id, count(id) as count_id 
		from takes where year=2009 and semester='fall'
		group by course_id, sec_id) as t1
where count_id=
		(select max(count_id) from (select course_id, sec_id, count(id) as count_id 
									from takes where year=2009 and semester='fall' group by course_id,sec_id) as t2);