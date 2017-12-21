module Student

sig Time {}

sig Name {}

fact { all n : Name | lone name.n }

sig Course {
	labs : some Lab
}

sig Lab {}

fact { all l : Lab | lone labs.l }

enum Grade { A,B,C,D,E }

sig Student {
	name : one Name,
	approved : Course -> Time,
	current : Course lone -> Time,
	donelabs : Lab -> Time,
	exams : Course -> Grade lone -> Time
}

fact { all t : Time | all s : Student | s.current.t not in s.approved.t }

fact { all t : Time | all s : Student | all c : s.approved.t | 
	c.labs in s.donelabs.t and c.(s.exams.t) in A+B+C+D
}

fact { all t : Time | all s : Student | 
	(s.current.t + s.approved.t + (s.exams.t).Grade) in ((students.t).s).courses 
}

sig College {
	students : Student -> Time,
	courses : set Course
}

fact { all t : Time | all s : Student | one (students.t).s }

run {} for 4 but exactly 1 Student, exactly 1 Time

pred newstudent [c : College, s : Student, t,t' : Time] {
	no (students.t).s
	no s.approved.t
	no s.current.t
	no s.donelabs.t
	no s.exams.t

	students.t' = students.t + c -> s
}

pred enroll [s : Student, c : Course, t,t' : Time] {
	no s.current.t
	c not in s.approved.t
	
	current.t' = current.t + s -> c
}

pred lab [s : Student, l : Lab, t,t' : Time] {
	some s.current.t
	l not in (s.donelabs.t)
	l in (s.current.t).labs

	donelabs.t' = donelabs.t + s -> l
}

pred exam [s : Student, g : Grade, t,t' : Time] {
	some s.current.t
	s.current.t not in (s.exams.t).Grade

	exams.t' = exams.t + s -> s.current.t -> g
}

pred approve [s : Student, t,t' : Time] {
	some s.current.t
	(s.current.t).(s.exams.t) in A+B+C+D
	(s.current.t).labs in s.donelabs.t

	approved.t' = approved.t + s -> s.current.t
	current.t' = current.t - s -> Course
}

pred quit [s : Student, t,t' : Time] {
	some s.current.t
	
	exams.t' = exams.t - s -> s.current.t -> Grade
	donelabs.t' = donelabs.t - s -> (s.current.t).labs
	current.t' = current.t - s -> Course
}
