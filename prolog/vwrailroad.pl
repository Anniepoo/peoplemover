:- module(vwrailroad, [
              set_capability/1,
              init_trains/0,
              reset_trains/0,
              locomotive/2,
              uncouple/1,
              car_goal/3
          ]).
/** <module>  API to control trains in the virtual world
 *
 *  See the README for usage
 */

:- use_module(library(http/http_client)).
:- use_module(library(uri)).
:- use_module(library(chr)).

		 /*******************************
		 *     Student Problem setup    *
		 *******************************/

%!  goal(-Type:atom, -Start:atom, -End:atom) is nondet
%
%  cars people want moved from CP Start to CP End
%

%!  car_goal(-UUID:atom, -Start:atom, -End:atom) is nondet
%
%   When a 'run' of the exercise is set up, these are the
%   specific cars that need to be moved
%
:- dynamic goal/3, car_goal/3.

%!   select_problem is det
%
%   Select a problem for the student
%   Note that this does **NOT** update the virtual world
%   must be followed with reset_trains
select_problem :-
    number_to_move(N),
    retractall(goal(_, _, _)),
    retractall(car_goal(_, _, _)),
    make_car_moves([], [], N).

%!  number_to_move(-Number:integer) is det
%
%   Number of cars to move in problem
number_to_move(6).

% ! make_car_moves(+SoFarStarts:list, +SoFarEnds:list, +N:integer) is det
%
%  create the set of goals - the problem the student will solve
%
make_car_moves(_, _, 0).
make_car_moves(SoFarStarts, SoFarEnds, N) :-
    N > 0,
    pick_car_move(Type-Start-End),
    \+ member(Start, SoFarStarts),
    \+ member(End, SoFarEnds),
    asserta(goal(Type, Start, End)),
    succ(NN, N),
    make_car_moves([Start| SoFarStarts], [End | SoFarEnds], NN).

% famulus for make_car_moves
pick_car_move(Type-Start-End) :-
    setof(T-S, cp_type(T, S), Locs),
    random_member(Type-Start, Locs),
    random_member(Type-End, Locs).


		 /*******************************
		 *   Define track and industries *
		 *******************************/

%!  cp_type(-ControlPoint:atom, -Type:atom) is nondet
%
%   @arg ControlPoint a control point
%   @arg Type car type that can be placed there
%
cp_type(CP, T) :-
    between(10, 20,X),
    atom_number(XA, X),
    atom_concat(cp , XA, CP),
    member(T, [ball, alien, box, radioactive]).
cp_type(cp1, ball).
cp_type(cp2, ball).
cp_type(cp3, alien).
cp_type(cp4, alien).

%!  cp_loc(-CP:atom, -Loc:dict, -Rot:dict) is nondet
%
%   @arg CP a control point
%   @arg Loc a loc dict, the position of a car at this control point
%   @arg Rot a rot dict, the rotation of a car at this control point
%
cp_loc(cp1, loc{x: 207.0, y:71.0, z:26.5},
              rot{x:0.0, y:0.0, z:0.0, s:1.0}).
cp_loc(cp2, loc{x: 227.0, y:61.0, z:26.5},
              rot{x:0.0, y:0.0, z:0.0, s:1.0}).
cp_loc(cp3, loc{x: 157.0, y:31.0, z:26.5},
              rot{x:0.0, y:0.0, z:0.0, s:1.0}).
cp_loc(cp4, loc{x: 157.0, y:41.0, z:26.5},
              rot{x:0.0, y:0.0, z:0.0, s:1.0}).
cp_loc(cp0, loc{x: 237.0, y:71.0, z:26.5},
              rot{x:0.0, y:0.0, z:0.0, s:1.0}).

cp_conn(cp0, cp1).
cp_conn(cp1, cp2).
cp_conn(cp2, cp4).
cp_conn(cp4, cp3).


		 /*******************************
		 *          Move Loco           *
		 *******************************/

%!   locomotive(+Direction:atom, +Offset:integer) is semidet
%
%   Move the locomotive.
%
%   @arg Direction  oneof `up` and `down`
%   @arg Offset number of car lengths to offset, positive for up
%
%   fails silently if the movement is impossible
%
locomotive(Direction, Offset) :-
    all_cars(Cars),
    assemble_move(Direction, Offset, Move),
    vw_send_move(Move),
    update_car_positions,
    clear_chr.

car_pos_to_chr :-
    setof(car_loc(Type, Pos, Rot, UUID, CP),
          car_pos(Type, Pos, Rot, UUID, CP),
          Locs),
    maplist(call, Locs).





		 /*******************************
		 *      Initialize Trains       *
		 *******************************/

%!  init_trains is det
%
%   Set up a train problem in the virtual world
%
%   This predicate blocks while the virtual world
%   responds
init_trains :-
    select_problem,
    reset_trains.

reset_trains :-
    vw_clear_all,
    rez_loco,
    bagof(T-S-E, goal(T, S, E), Cars),
    maplist(rez_car, Cars).

rez_car(T-S-E) :-
    cp_loc(S, Loc, Rot),
    vw_make_car(T, Loc, Rot, UUID),
    set_car_pos(T, Loc, Rot, UUID, S),
    asserta(car_goal(UUID, S, E)).


init_loco_pos(cp0).

rez_loco :-
    init_loco_pos(CP),
    cp_loc(CP, Pos, Rot),
    vw_make_car(loco, Pos, Rot, UUID),
    set_car_pos(loco, Pos, Rot, UUID, CP).

		 /*******************************
		 *           Track Cars         *
		 *******************************/

%! car_pos(-Type:atom, -Pos:dict, -Rot:dict, -UUID:atom, -CP:atom) is
%! nondet
%
%   @arg Type  car/loco type
%   @arg Pos   loc dict with the region coordinates of car
%   @arg Rot   rot dict with the global rotation of the car
%   @arg UUID  UUID of the car
%   @arg CP    if an atom, at a CP. if a dict, between two CPs
%
:- dynamic car_pos/5.

%! car_pos(+Type:atom, +Pos:dict, +Rot:dict, +UUID:atom, +CP:atom) is
%! nondet
%
%   @arg Type  car/loco type
%   @arg Pos   loc dict with the region coordinates of car
%   @arg Rot   rot dict with the global rotation of the car
%   @arg UUID  UUID of the car
%   @arg CP    if ground, car is at a control point (and is an atom)
%
set_car_pos(Type, Pos, Rot, UUID, CP) :-
    retractall(car_pos(Type, _, _, UUID, _)),
    asserta(car_pos(Type, Pos, Rot, UUID, CP)).


		 /*******************************
		 *    Virtual World Interface   *
		 *******************************/


:- dynamic capability/1.

%!  set_capability(+Capability:atom) is det
%
%   @arg Capability atom representation of the URL provided by the claim
%   region tool
%
%   Set the capability to be used by this pack.
%
%   This is the connection between the virtual world and Prolog.

set_capability(Capability) :-
    uri_components(Capability, uri_components(Scheme, Authority, Path, Search, Fragment)),
    uri_authority_components(Authority, uri_authority(_, _, _, Port)),
    uri_authority_components(GlobalAuthority, uri_authority(_, _, 'partyserver.rocks', Port)),
    uri_components(FixedCapability, uri_components(Scheme, GlobalAuthority, Path, Search, Fragment)),
    retractall(capability(_)),
    asserta(capability(FixedCapability)).


vw_make_car(Type, Loc, Rot, UUID) :-
    capability(URL),
    format(codes(Content), 'rez, ~w, <~1f,~1f,~1f>, <~1f,~1f,~1f,~1f>', [
                               Type,
                               Loc.x, Loc.y, Loc.z,
                               Rot.x, Rot.y, Rot.z, Rot.s]),
    http_post(URL, codes(Content), UUID, []).

vw_clear_all :-
    capability(URL),
    http_post(URL, atom(clearall), _, []).

		 /*******************************
		 *  Thread Component            *
		 *******************************/

create_chr_thread :-
   message_queue_create(_, [ alias(sub) ]),
   message_queue_create(_, [ alias(par) ]),
   thread_create(polling_sub, _, [ alias(chr),
           at_exit(debug(lines, 'CHR thread exited', []))]).

polling_sub :-
   % listen for new message on `sub` queue
   thread_get_message(sub, sync(ActionCHR, ResultCHR)),
   debug_constraints(polling_sub),
   % do the actual constraint call
   (   call(ActionCHR)
   ;
       debug(constraint(polling_sub),
             'action constraint ~w failed unexpectedly~n',
             [ActionCHR])
   ),
   debug_constraints(polling_sub),
   % get the result using the get_foo pattern
   ResultCHR =.. List,
   append(StubList, [_], List),
   append(StubList, [Result], CallMeList),
   CallMe =.. CallMeList,
   (   call(CallMe)
   ;
       debug(constraint(polling_sub),
             'result constraint ~w failed unexpectedly~n',
             [ResultCHR])
   ),
   !, % nondet calls not allowed
   % send it back to the `par` message queue
   thread_send_message(par, Result),
   % repeat
   polling_sub.

%!  do_in_chr_thread(+ActionCHR:chr_constraint,
%!         +ResultCHR:chr_constraint) is det
%
%   queries ActionCHR in the chr thread, which must be
%   grounded chr_constraint or prolog predicate,
%   then calls ResultCHR, whose last argument must be unbound.
%   the last argument will be bound as if a direct chr call
%   was made.
%
% eg to touch the egg to the pan and then get the egg's costume do
% do_in_chr_thread(touch(S, egg, pan), get_costume(S, egg, Costume))
%
% Note that these are effectively called in once/1
%
do_in_chr_thread(ActionCHR, ResultCHR) :-
   ResultCHR =.. List,
   append(_, [Result], List),
   thread_send_message(sub, sync(ActionCHR, ResultCHR)),
   thread_get_message(par, Result).



