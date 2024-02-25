% Define nodes (places)
node(admin_office).
node(cafeteria).
node(social_sciences_bld).
node(institute_x).
node(institute_y).
node(lecture_hall_a).
node(engineering_bld).
node(library).

% Define edges with weights (travel time)
edge(admin_office, library, 1).
edge(admin_office, engineering_bld, 3).
edge(admin_office, cafeteria, 4).
edge(cafeteria, library, 5).
edge(cafeteria, social_sciences_bld, 2).
edge(social_sciences_bld, library, 2).
edge(social_sciences_bld, institute_x, 8).
edge(institute_y, lecture_hall_a, 3).
edge(institute_y, library, 3).
edge(lecture_hall_a, engineering_bld, 2).
edge(engineering_bld, library, 5).


connected(Place1, Place2, Time) :- 
    edge(Place1, Place2, Time).

connected(Place1, Place2, Time) :- 
    edge(Place2, Place1, Time).

% Define delivery personnel
% delivery_person(ID, Capacity, WorkHours, CurrentJob, Location)
delivery_person(dp1, small, [0, 4], none, lecture_hall_a) :- format('Asserted delivery_person: ~w\n', [dp1]).
delivery_person(dp2, medium, [0, 4, 16], none, admin_office) :- format('Asserted delivery_person: ~w\n', [dp2]).
delivery_person(dp3, medium, [12, 16], none, cafeteria) :- format('Asserted delivery_person: ~w\n', [dp3]).
delivery_person(dp4, medium, [8, 12, 16, 20], none, library) :- format('Asserted delivery_person: ~w\n', [dp4]).
delivery_person(dp5, medium, [4, 8, 12, 16, 20], none, social_sciences_bld) :- format('Asserted delivery_person: ~w\n', [dp5]).
delivery_person(dp6, medium, [0, 4, 8, 12, 16, 20], none, institute_y) :- format('Asserted delivery_person: ~w\n', [dp6]).
delivery_person(dp7, medium, [0, 8, 16], none, engineering_bld) :- format('Asserted delivery_person: ~w\n', [dp7]).
delivery_person(dp8, medium, [ 12, 16], none, cafeteria) :- format('Asserted delivery_person: ~w\n', [dp8]).
delivery_person(dp9, medium, [0, 4, 8], none, library) :- format('Asserted delivery_person: ~w\n', [dp9]).
delivery_person(dp10, medium, [0, 4, 8, 12, 16, 20], none, admin_office) :- format('Asserted delivery_person: ~w\n', [dp10]).

% Define objects
% object(ID, Weight, Pickup, DropOff, Urgency, DeliveryPersonID)
object(o1, light, lecture_hall_a, social_sciences_bld, high, none) :- format('Asserted object: ~w\n', [o1]). % Shortest Path Length: 8
object(o2, medium, engineering_bld, cafeteria, medium, none) :- format('Asserted object: ~w\n', [o2]). % Shortest Path Length: 7
object(o3, heavy, admin_office, institute_x, low, none) :- format('Asserted object: ~w\n', [o3]). % Shortest Path Length: 11
object(o4, light, institute_y, library, high, none) :- format('Asserted object: ~w\n', [o4]). % Shortest Path Length: 3
object(o5, medium, cafeteria, institute_y, medium, none) :- format('Asserted object: ~w\n', [o5]). % Shortest Path Length: 7
object(o6, heavy, library, social_sciences_bld, low, none) :- format('Asserted object: ~w\n', [o6]). % Shortest Path Length: 2
object(o7, light, social_sciences_bld, cafeteria, high, none) :- format('Asserted object: ~w\n', [o7]). % Shortest Path Length: 2
object(o8, medium, lecture_hall_a, engineering_bld, medium, none) :- format('Asserted object: ~w\n', [o8]). % Shortest Path Length: 2
object(o9, heavy, engineering_bld, library, low, none) :- format('Asserted object: ~w\n', [o9]). % Shortest Path Length: 4
object(o10, light, library, cafeteria, high, none) :- format('Asserted object: ~w\n', [o10]). % Shortest Path Length: 4
object(o11, medium, cafeteria, social_sciences_bld, medium, none) :- format('Asserted object: ~w\n', [o11]). % Shortest Path Length: 2
object(o12, heavy, social_sciences_bld, library, low, none) :- format('Asserted object: ~w\n', [o12]). % Shortest Path Length: 2
object(o13, light, institute_y, cafeteria, high, none) :- format('Asserted object: ~w\n', [o13]). % Shortest Path Length: 7
object(o14, medium, cafeteria, library, medium, none) :- format('Asserted object: ~w\n', [o14]). % Shortest Path Length: 4
object(o15, heavy, library, social_sciences_bld, low, none) :- format('Asserted object: ~w\n', [o15]). % Shortest Path Length: 2
object(o16, light, social_sciences_bld, cafeteria, high, none) :- format('Asserted object: ~w\n', [o16]). % Shortest Path Length: 2
object(o17, medium, lecture_hall_a, engineering_bld, medium, none) :- format('Asserted object: ~w\n', [o17]). % Shortest Path Length: 2
object(o18, heavy, engineering_bld, library, low, none) :- format('Asserted object: ~w\n', [o18]). % Shortest Path Length: 4
object(o19, light, library, cafeteria, high, none) :- format('Asserted object: ~w\n', [o19]). % Shortest Path Length: 4
object(o20, medium, cafeteria, social_sciences_bld, medium, none) :- format('Asserted object: ~w\n', [o20]). % Shortest Path Length: 2
object(o21, heavy, admin_office, cafeteria, low, none) :- format('Asserted object: ~w\n', [o21]). % Shortest Path Length: 4
object(o22, heavy, cafeteria, admin_office,  low, none) :- format('Asserted object: ~w\n', [o22]). % Shortest Path Length: 4
object(o23, heavy, engineering_bld, institute_x,  low, none) :- format('Asserted object: ~w\n', [o23]). % Shortest Path Length: 4

% Initialize all distances to infinity, except for the start node which is 0.
initialize_distances(Start, Nodes, Distances) :-
    findall(Node, node(Node), Nodes),
    initialize_distance_pairs(Nodes, Start, Distances).

% Update the distance structure to include predecessors
initialize_distance_pairs([], _, []).
initialize_distance_pairs([Node|Nodes], Start, [(Node, Distance, Predecessor)|Distances]) :-
    (Node == Start -> Distance = 0; Distance = inf),
    Predecessor = none,  % No predecessor for the initial state
    initialize_distance_pairs(Nodes, Start, Distances).

% Check if a delivery person is available for an object using Dijkstra algorithm
available_delivery_person(ObjectID, DeliveryPersonID, TotalTime) :-
    object(ObjectID, Weight, Pickup, DropOff, _, none),
    delivery_person(DeliveryPersonID, Capacity, WorkBlocks, none, Location),
    can_carry(Capacity, Weight),
    dijkstra(Location, Pickup, PathTime1, Path1),
    dijkstra(Pickup, DropOff, PathTime2, Path2),
    TotalTime is PathTime1 + PathTime2,
    format('Dijkstra Path from ~w to ~w: ~w, Time: ~w\n', [Location, Pickup, Path1, PathTime1]),
    format('Dijkstra Path from ~w to ~w: ~w, Time: ~w\n', [Pickup, DropOff, Path2, PathTime2]),
    check_work_hours(WorkBlocks, TotalTime),
    format('Available: ~w for delivery of ~w (Weight: ~w, TotalTime: ~w)\n', [DeliveryPersonID, ObjectID, Weight, TotalTime]).
available_delivery_person(ObjectID, _, _) :-
    format('No available delivery person for object: ~w\n', [ObjectID]), fail.

% Sum up the total available work hours and check against the total delivery time
sum_work_hours([], 0).
sum_work_hours([H|T], Total) :-
    sum_work_hours(T, Rest),
    Total is H + Rest.

check_work_hours(WorkBlocks, TotalTime) :-
    sum_work_hours(WorkBlocks, SumHours),
    TotalTime =< SumHours * 4.  % Each block represents 4 hours



% Implementing Dijkstra algorithm entry point
dijkstra(Start, End, Time, Path) :-
    findall(Node, node(Node), Nodes),
    initialize_distances(Start, Nodes, Distances),
    dijkstra_main_loop([], Distances, FinalDistances),
    member((End, Time, _), FinalDistances),  % Get the time for the end node
    reconstruct_path(End, FinalDistances, Path).

% Helper predicates like can_carry, find_shortest_path, within_work_hours will need to be defined.

% can_carry(Capacity, Weight)
can_carry(large, _).  % A large capacity can carry any weight
can_carry(medium, light).
can_carry(medium, medium).
can_carry(small, light).  % Assuming small capacity can only carry light objects

% Main loop of Dijkstra algorithm
dijkstra_main_loop(Visited, Distances, FinalDistances) :-
    select_smallest_distance(Distances, Visited, Node, Distance),
    (Node \= none ->  % If Node is none, then there are no more nodes to visit
        findall(Neighbor, connected(Node, Neighbor, _), Neighbors),
        update_distances(Node, Neighbors, Distance, Distances, UpdatedDistances),
        dijkstra_main_loop([Node|Visited], UpdatedDistances, FinalDistances)
    ;   FinalDistances = Distances
    ).

% Selects the unvisited node with the smallest known distance
select_smallest_distance(Distances, Visited, Node, Distance) :-
    filter_unvisited(Distances, Visited, UnvisitedDistances),
    sort(2, @=<, UnvisitedDistances, SortedDistances),
    (SortedDistances = [(Node, Distance, _)|_] -> true; Node = none, Distance = inf).

% Helper predicate to update distances
update_distances(_, [], _, Distances, Distances).
update_distances(Node, [Neighbor|Neighbors], NodeDistance, Distances, UpdatedDistances) :-
    update_distance(Node, Neighbor, NodeDistance, Distances, TempDistances),
    update_distances(Node, Neighbors, NodeDistance, TempDistances, UpdatedDistances).

% Updates the update_distance predicate to include predecessors
update_distance(Node, Neighbor, NodeDistance, Distances, UpdatedDistances) :-
    member((Neighbor, OldDistance, _), Distances),
    connected(Node, Neighbor, EdgeDistance),
    NewDistance is NodeDistance + EdgeDistance,
    (NewDistance < OldDistance ->
        replace(Distances, (Neighbor, OldDistance, _), (Neighbor, NewDistance, Node), UpdatedDistances)
    ;   UpdatedDistances = Distances
    ).

% Replaces an element in a list with another element
replace([], _, _, []).
replace([(N,_,_)|T], (N,_,_), (N,ND,NP), [(N,ND,NP)|T]) :- !.
replace([H|T], Old, New, [H|R]) :-
    replace(T, Old, New, R).


% Reconstructs the shortest path from the predecessors
reconstruct_path(End, Distances, Path) :-
    reconstruct_path_helper(End, Distances, [], ReversePath),
    reverse(ReversePath, Path).

reconstruct_path_helper(none, _, Path, Path).  % Reached the start node
reconstruct_path_helper(Node, Distances, Acc, Path) :-
    member((Node, _, Predecessor), Distances),
    reconstruct_path_helper(Predecessor, Distances, [Node|Acc], Path).

% Helper predicate to check if a node is unvisited
is_unvisited(Node, Visited) :-
    \+ memberchk(Node, Visited).

% Helper predicate to filter out visited nodes from the distances list
filter_unvisited([], _, []).
filter_unvisited([(Node, Distance, Predecessor)|T], Visited, Result) :-
    (   memberchk(Node, Visited)
    ->  filter_unvisited(T, Visited, Result)  % Node is visited, skip it
    ;   Result = [(Node, Distance, Predecessor)|FilteredTail],  % Node is not visited, keep it
        filter_unvisited(T, Visited, FilteredTail)
    ).

% within_work_hours(WorkHours, TotalTime)
within_work_hours(WorkHours, TotalTime) :-
	TotalTime =< WorkHours.

% find_delivery_person(ObjectID, DeliveryPersonID, TotalTime)
find_delivery_person(ObjectID, DeliveryPersonID, TotalTime) :-
    object(ObjectID, Weight, Pickup, DropOff, _, none),
    delivery_person(DeliveryPersonID, Capacity, WorkHours, none, Location),
    can_carry(Capacity, Weight),
    dijkstra(Location, Pickup, PathTime1, _),
    dijkstra(Pickup, DropOff, PathTime2, _),
    TotalTime is PathTime1 + PathTime2,
    within_work_hours(WorkHours, TotalTime).

% Assigns delivery person to an object
assign_delivery_person(ObjectID, DeliveryPersonID, TotalTime) :-
    available_delivery_person(ObjectID, DeliveryPersonID, TotalTime),
    retract(object(ObjectID, Weight, Pickup, DropOff, Urgency, _)),
    assert(object(ObjectID, Weight, Pickup, DropOff, Urgency, DeliveryPersonID)),
    format('Assigned: Object ~w to delivery person ~w\n', [ObjectID, DeliveryPersonID]).
assign_delivery_person(ObjectID, _, _) :-
    format('Failed to assign delivery person for object: ~w\n', [ObjectID]), fail.

object_path_time(ObjectID, PathTime) :-
    object(ObjectID, _, PickupLocation, DropOffLocation, _, _),
    dijkstra(PickupLocation, DropOffLocation, PathTime, _).

% Asserts all objects and delivery personnel
assert_all :-
    forall(object(ObjectID, Weight, Pickup, DropOff, Urgency, DeliveryPersonID), assert(object(ObjectID, Weight, Pickup, DropOff, Urgency, DeliveryPersonID))),
    forall(delivery_person(DeliveryPersonID, Capacity, WorkHours, ObjectID, Location), assert(delivery_person(DeliveryPersonID, Capacity, WorkHours, ObjectID, Location))).

check_delivery_status(ObjectID, DeliveryPersonID, _) :-
    object(ObjectID, _, _, _, _, DeliveryPersonID),
    DeliveryPersonID \= none,
    format('Object ~w is already being delivered by ~w.\n', [ObjectID, DeliveryPersonID]).

check_delivery_status(ObjectID, _, _) :-
    object(ObjectID, _, _, _, _, none),
    findall((DP, TT), available_delivery_person(ObjectID, DP, TT), Deliveries),
    print_delivery_options(Deliveries).

print_delivery_options([]) :-
    format('No available delivery persons.\n').

print_delivery_options([(DP, TT)|Deliveries]) :-
    format('Available: Delivery person ~w (Total time: ~w)\n', [DP, TT]),
    print_delivery_options(Deliveries).