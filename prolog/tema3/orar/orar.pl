
:-ensure_loaded('probleme.pl').
:-ensure_loaded('testing.pl').

get_days(Context, Days) :-
    member(T, Context),
    T = days(Days).

get_times(Context, Times) :-
    member(T, Context),
    T = times(Times).

get_rooms(Context, Rooms) :-
    member(T, Context),
    T = rooms(Rooms).

get_groups(Context, Groups) :-
    member(T, Context),
    T = groups(Groups).

get_activities(Context, Activities) :-
    member(T, Context),
    T = activities(Activities).

get_staff(Context, Staff) :-
    member(T, Context),
    T = staff(Staff).

get_constraints(Context, Constraints) :-
    member(T, Context),
    T = constraints(Constraints), !.
get_constraints(_, []).

get_all_slots(Context, AllSlots) :-
    findall(slot(A, G, D, T, R, P), (
				   get_days(Context, Days),
				   member(D, Days),
				   get_times(Context, Times),
				   member(T, Times),
				   get_rooms(Context, Rooms),
				   member(R, Rooms),
				   get_groups(Context, Groups),
				   member(G, Groups),
				   get_activities(Context, Activities),
				   member(At, Activities),
				   (A, _) = At,
				   get_staff(Context, Staff),
				   member(Pt, Staff),
				   (P, Acts) = Pt,
                                   member(A, Acts)
            ), AllSlots).

combs([],[]).
combs([H|T],[H|T2]) :-
    combs(T,T2).
combs([_|T],T2) :-
    combs(T,T2).

pbm(Problem, Sol) :-
	problem(Problem, Context),
	schedule(Context, Sol),
        write(Sol).
getContext(Problem, Context) :-
    problem(Problem, Context),
    write(Context).

get_hours_per_group([], 0).
get_hours_per_group([(_, Ore)|Tail], N1) :-
    get_hours_per_group(Tail, N),
    N1 is N + Ore.

validG(Slots, Slot) :-
    slot(_,Gs,Ds,Ts,_,_) = Slot,
    findall(slot(_,Gs,Ds,Ts,_,_), member(slot(_,Gs,Ds,Ts,_,_), Slots), Bag),
    length(Bag, 1).
validR(Slots, Slot) :-
    slot(_,_,Ds,Ts,Rs,_) = Slot,
    findall(slot(_,_,Ds,Ts,Rs,_), member(slot(_,_,Ds,Ts,Rs,_), Slots), Bag),
    length(Bag, 1).
validP(Slots, Slot) :-
    slot(_,_,Ds,Ts,_,Ps) = Slot,
    findall(slot(_,_,Ds,Ts,_,Ps), member(slot(_,_,Ds,Ts,_,Ps), Slots), Bag),
    length(Bag, 1).
validA(Slots, Group, Activity, Instances) :-
    findall(_, member(slot(Activity, Group, _, _, _, _), Slots), Bag),
    length(Bag, Instances).
validMaxInstances(Slots, Class, InstancesPerDay, Day, Group) :-
    findall(_, member(slot(Class, Group, Day, _, _, _), Slots), Bag),
    length(Bag, BagLen),
    BagLen =< InstancesPerDay.

% schedule(+Context, -Sol)
% pentru contextul descris, întoarce o soluție care respectă
% constrângerile fizice și de curiculă.
%schedule(_, _):-fail.

%schedule(Context, (Context, [])):-
%    get_rooms(Context, [] ).
%    getContext(no_staff, Context), findall(X, (get_staff(Context, Staff), member(Pt, Staff), (_,L) = Pt,member(X, L)), StuffActivities), get_activities(Context, Activities), findall(A, (member(At, Activities), (A,_) = At, member(A,StuffActivities)), Bag), sort(Bag, NoDuplicates), not((length(NoDuplicates, N), length(Activities, N)))

%schedule(Context, (Context, [])):-
%    get_activities(Context, [] ), !.
schedule(Context, Sol) :-
	%for no staff
	findall(X, (get_staff(Context, Staff), member(Pt, Staff), (_,L) = Pt,member(X, L)), StuffActivities),
	get_activities(Context, Activities),
	findall(A, (member(At, Activities), (A,_) = At, member(A,StuffActivities)), Bag),
	sort(Bag, NoDuplicates),
	length(NoDuplicates, N), length(Activities, N),

	%for no time
	get_rooms(Context, Rooms),
	length(Rooms, R),
	get_days(Context, Days),
	length(Days, D),
	get_times(Context, Times),
	length(Times, T),

	%get_activities(Context, Activities),
	get_hours_per_group(Activities, Act),
	get_groups(Context, Groups),
	length(Groups, Gru),
	G is Act * Gru,
        R * D * T >= G,
	length(Slots, G),
        get_all_slots(Context, AllSlots),
	combs(AllSlots, Slots),
	foreach(member(Slot, Slots),validR(Slots, Slot)),
	foreach(member(Slot, Slots),validG(Slots, Slot)),
	foreach(member(Slot, Slots),validP(Slots, Slot)),
	foreach((member((Activity, Instances), Activities), member(Group, Groups)), validA(Slots, Group, Activity, Instances)),
	foreach((member(constraints(Constraints), Context), member(max_instances(Class, InstancesPerDay), Constraints), member(Day, Days), member(Gr, Groups)), validMaxInstances(Slots, Class, InstancesPerDay, Day, Gr)),
        %foreach((member(T, Context), T = constraints(Constraints), member(max_instances(Class, InstancesPerDay), Constraints), member(Day, Days), member(Gr, Groups)), validMaxInstances(Slots, Class, InstancesPerDay, Day, Gr)),
	Sol = (Context, Slots)
	.
costMaxHours(_, [], _, 0).
costMaxHours(Slots, [H|T], Duration, CostMax) :-
    (Entity, Hours, Cost, Day) = H,
    findall(_, (member(slot(_,_,Day,_,_,Entity), Slots); member(slot(Entity,_,Day,_,_,_), Slots);  member(slot(_,_,Day,_,Entity,_), Slots)), Bag),
    length(Bag, BagLen),
    NoHours is BagLen * Duration,
    (NoHours > Hours -> TempCost is NoHours - Hours ; TempCost is 0),
    TCost is TempCost * Cost,
    costMaxHours(Slots, T, Duration, Cost1),
    CostMax is Cost1 + TCost.

costMinHours(_, [], _, 0).
costMinHours(Slots, [H|T], Duration, CostMax) :-
    (Entity, Hours, Cost, Day) = H,
    findall(_, (member(slot(_,_,Day,_,_,Entity), Slots); member(slot(Entity,_,Day,_,_,_), Slots);  member(slot(_,_,Day,_,Entity,_), Slots)), Bag),
    length(Bag, BagLen),
    write(ss),
    write(BagLen),
    NoHours is BagLen * Duration,
    (NoHours < Hours -> TempCost is Hours - NoHours; TempCost is 0),
    TCost is TempCost * Cost,
    costMinHours(Slots, T, Duration, Cost1),
    CostMax is Cost1 + TCost.


% cost(+Sol, -Cost)
% pentru soluția dată, întoarce costul implicat de constrângerile de
% preferință care au fost încălcate.
cost(Sol, Cost) :-
%    write(Sol),
   (Context, Slots) = Sol,
   get_times(Context, Times),
   member(Entr, Times), !,
   (_, Duration) = Entr,
   %write(Duration),
   get_days(Context, Days),
   member(constraints(Constraints), Context),
   findall((Entity, Hours, Cost, Da), (member(max_hours(Entity, Hours, Cost), Constraints), member(Da, Days)), CostBag),
   findall((EntityM, HoursM, CostM, Day), (member(min_hours(EntityM, HoursM, CostM), Constraints), member(Day, Days)), CostMinBag),
   costMaxHours(Slots, CostBag, Duration, CostMax),
   costMinHours(Slots, CostMinBag, Duration, CostMin),
   Cost is CostMax + CostMin.

% schedule_best(+Context, -Sol, -Cost)
% pentru contextul descris, întoarce soluția validă cu cel mai bun (cel
% mai mic) cost (sau una dintre ele, dacă există mai multe cu același
% cost)
schedule_best(_, _, _) :- fail.

