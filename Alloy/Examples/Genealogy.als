module Genealogy

sig Time { }

abstract sig Person { father : lone Man, mother : lone Woman, nationality : Nationality one -> Time}

enum Nationality {Argentine,Portuguese}

sig Man extends Person { wife : Woman lone -> Time }

sig Woman extends Person { husband : Man lone -> Time }

fun ancestry [self : Person,t : Time] : set Person {
   self.^{x : univ,v0 : {y : Person | ((y in Man) && (y in x.father)) || ((y in Woman) && (y in x.mother))} | no none}
}

fact {all t : Time | all self : Person | self not in ancestry[self,t]}

fact {all t : Time | all self : Person | all n : Nationality | (some ancestry[self,t] and n in self.(nationality.t)) => (some p : ancestry[self,t] | n in p.(nationality.t))}

fact {all t : Time | all p : Woman | p.(husband.t) = (wife.t).p}

fact {all t : Time | all p : Person | no (p.(wife+husband).t & ancestry[p,t])}

run {} for 4 but exactly 1 Time

pred change [self : Person,n : Nationality,t,t' : Time] {
   some p : ancestry[self,t] | n in p.(nationality.t)
   n in self.(nationality.t')
}

pred marry [m : Man, w : Woman, t,t' : Time] {
	no m.wife.t
	w in m.wife.t'
	all x : Man-m | x.wife.t' = x.wife.t
}
