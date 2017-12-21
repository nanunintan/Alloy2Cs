module FileSystem

sig Time {}

abstract sig FSObject {
	name : Name,
	parent : Dir lone -> Time
}

sig Dir, File extends FSObject {}

sig FileSystem {
	fsObjects : FSObject -> Time,
	root : Dir one -> Time,
	wd : Dir one -> Time
}

sig Name {}

fact { all t : Time | all x : FSObject | all y : x.(parent.t).~(parent.t) - x | x.name not in y.name }

fact { all t : Time | all o : FSObject | o not in o.^(parent.t) }

fact { all t : Time | all fs : FileSystem | fs.(root+wd).t in fs.fsObjects.t }

fact { all t : Time | all fs : FileSystem | all o : fs.fsObjects.t | o.parent.t in fs.fsObjects.t }

fact { all t : Time | all fs : FileSystem | fs.fsObjects.t in *(parent.t).(fs.root.t) }


run {} for 4 but exactly 1 Time, exactly 1 FileSystem

fun ls [fs : FileSystem,t:Time] : set FSObject {
	(parent.t).(fs.wd.t) & fs.fsObjects.t
}

pred rm [fs : FileSystem, f : File, t,t' : Time] {
	f in ls[fs,t]
	fs.fsObjects.t' = fs.fsObjects.t - f
	fs.root.t' = fs.root.t
	fs.wd.t' = fs.wd.t
}

pred cd [fs : FileSystem, d : Dir, t,t' : Time] {
	d in fs.fsObjects.t
	fs.wd.t' = d
	fs.fsObjects.t' = fs.fsObjects.t
	fs.root.t' = fs.root.t
}

run rm for 4 but 1 FileSystem, 2 Time
