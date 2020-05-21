:- module(vwpeoplemover, [
              set_capability/1,
              move_people/2,
              cp_conn/3,
              up/2,
              down/2,
              loc_region_loc/2
          ]).
/** <module>  API to control the people mover in Vertical City
 *
 *  For overall documentation see the google doc
 *
 */

:- use_module(library(http/http_client)).
:- use_module(library(uri)).

		 /*******************************
		 *            Main loop         *
		 *******************************/

%!  move_people(+Callback:term, +UserState:term) is nondet
%
%   @arg Callback called with (State, UserState, NewState, NewUserState)
%   @arg UserState a term, the initial UserState
%
%   @throws  error(invalid_move(Hint)) if the new state can't be done
%
%   This predicate never succeeds normally. Unless it throws it will
%   run forever. If the Callback fails, however, it will fail.
%
%   You must call set_capability/1 prior to calling this predicate
%
%   Callback is called with a system state State - people in the system.
%   State is a list of terms of the form `pass(UUID, Loc, Goal)`.
%
%    * UUID is a UUID uniquely identifying the passenger
%    * Loc is a term  `loc(CP, Offset, Target)` (see below)
%    * Goal is the name of a control point (atom)
%
%    The system is a series of links between _|control points|_.
%    If you visit Vertical City in world you'll see these links as
%    purple rays between the gold and black tetrahedral control points.
%
%    Links have a definite direction. The end that _owns_ the link is
%    called CP. The 'other' end is the _target_. Passengers move 10
%    meters every cycle of the system (if they move at all). Offset is
%    the number of cycles travelled since the passenger entered this
%    link. So a passenger at `loc(groundplein, 2, elbow)` will be 20
%    meters along the Zurgon beam to control point *elbow* from the
%    control point *groundplein*.
%
%    Passengers move along these links. A passengers location is
%    `loc(CP, Offset, Target)`
%
%   The second argument to callback is the `UserState`. The callback may
%   wish to keep some additional state. It's passed as the second
%   argument and the new UserState bound to the last argument, to in
%   turn be passed in as the second argument on the next cycle.
%
%   Callback is expected to bind it's third argument to a modification
%   of State. To be valid, *|there must not be two passengers at the same|*
%   *|location|*, and *|no passenger must be moved more than one 'step'|*.
%   See `up/2` and `down/2` to find possible next steps.
%
%   Callback should bind the last argument to it's new `UserState`
%

:- meta_predicate move_people(4, +), move_people(+, +, 2).

move_people(CallBack, UserState) :-
    vw_clear_all,
    move_people([], UserState, CallBack).

move_people(State, UserState, CallBack) :-
    catch(
        call(CallBack, State, UserState, NewState, NewUserState),
        Error,
        vw_throw(Error)),
    !,
    % passengers must not be created or destroyed by user
    (   length(State, Len),
        length(NewState, Len)
    ;   vw_throw(error(invalid_move(object_constancy(State, NewState))))
    ),
    maplist(validate_a_movement(State), NewState),
    exclude(not_colliding(NewState), NewState, Colliding),
    (   Colliding \= []
    ->  vw_throw(error(invalid_move(collision(Colliding, State))))
    ;   true
    ),
    partition(at_goal, NewState, AtGoal, NS1),
    maplist(do_delete_pass, AtGoal),
    make_up_people(NS1, NewFolkModel),
    maplist(pass_create, NewFolkModel, NewFolks),
    append(NS1, NewFolks, NS2),
    exclude(stationary(State), NS1, PassToMove),
    vw_moveall(PassToMove),
    sleep(2.0),
    move_people(NS2, NewUserState, CallBack).

stationary(OldLocs, pass(UUID, Loc, _)) :-
    member(pass(UUID, Loc, _), OldLocs).

pass_create(pass_model(Type, Start, Goal), pass(UUID, loc(Start, 0, Face), Goal)) :-
    cp_conn(Start, Face, _),
    cp_loc(Start, RegionLoc),
    vw_make_pass(Type, RegionLoc, UUID).

do_delete_pass(pass(UUID, _, _)) :-
    vw_clear(UUID).

make_up_people(Current, Models) :-
    findall(S, cp_conn(S, _, _), Locations),
    exclude(occupied(Current), Locations, EmptyLocs),
    random_member(N, [0,0,0,0, 1, 1, 2]),
    random_permutation(EmptyLocs, PassLocs),
    length(NLocs, N),
    (   append(NLocs, _, PassLocs),
        Locs = NLocs
    ;   Locs = PassLocs
    ),
    maplist(make_up_one, Locs, Models).

make_up_one(Start, pass_model(Type, Start, Goal)) :-
    random_member(Type, [clanker, bureaucratbot, biomorph, tentacles]),
    setof(Loc, cp_conn(Loc, _, _), Locs),
    random_member(Goal, Locs).

occupied(Current, CP) :-
    member(pass(_, loc(CP, _, _), _), Current).

validate_a_movement(State, pass(UUID, NLoc, _)) :-
    member(pass(UUID, Loc, _), State),
    (   Loc = NLoc
    ;   up(Loc, NLoc)
    ;   down(Loc, NLoc)
    ).
validate_a_movement(State, Pass) :-
    vw_throw(error(invalid_move(moved_too_far(Pass, State)))).

not_colliding(NS, pass(UUID, Loc, _)) :-
    findall(Other, member(pass(Other, Loc, _), NS), [UUID]).

%!  up(+Loc:location, -Next:location) is nondet
%
%   @arg Loc a term representing a location in the system (see
%   `move_people/2`)
%   @arg Next one move upwards on any route from this location
%
up(loc(CP, Offset, Towards), loc(CP, NO, Towards)) :-
    succ(Offset, NO),
    cp_conn(CP, Towards, Len),
    NO < Len.
up(loc(CP, Offset, Towards), loc(Towards, 0, NT)) :-
    cp_conn(CP, Towards, Len),
    Len is Offset + 1,
    cp_conn(Towards, NT, _).

%!  down(+Loc:location, -Next:location) is nondet
%
%   @arg Loc a term representing a location in the system (see
%   `move_people/2`)
%   @arg Next one move downwards on any route from this location
%
down(loc(CP, Offset, Towards), loc(CP, NO, Towards)) :-
    succ(NO, Offset).
down(loc(CP, 0, _), loc(NCP, NO, CP)) :-
    cp_conn(NCP, CP, Len),
    NO is Len - 1.

at_goal(pass(_, loc(CP, 0, _), CP)).   % stubs ??

		 /*******************************
		 *   Define tracks              *
		 *******************************/

%!  cp_loc(-CP:atom, -Loc:dict) is nondet
%
%   @arg CP a control point
%   @arg Loc a loc dict, the position of a passenger at this control
%   point
%
%   location of control points in region coordinates
%
cp_loc('groundplein', loc{x: 116, y:134, z:28.78035}).
cp_loc('groundhouse1', loc{x: 154, y:131, z:28.78035}).
cp_loc('groundhouse2', loc{x: 152.5, y:160, z:28.78035}).
cp_loc('groundhouse3', loc{x: 132, y:90.299492, z:26.857834}).
cp_loc('groundhouse4', loc{x: 90.5, y:100, z:26.857834}).
cp_loc('forestlawn2', loc{x: 137.5, y:138.76, z:58.74}).
cp_loc('forestlawn4', loc{x: 108.470253, y:98.370552, z:57.419533}).
cp_loc('forestlawn5', loc{x: 102.610046, y:135.76, z:99.75898}).
cp_loc('forestlawn6', loc{x: 131.5, y:134.5, z:99.75898}).
cp_loc('isolated', loc{x: 80, y:192, z:130.208405}).
cp_loc('forestlawnjunction', loc{x: 98.660202, y:130.743668, z:124.765785}).
cp_loc('elbow', loc{x: 126, y:102, z:184.71405}).
cp_loc('futureflowers', loc{x: 106.5, y:80.5, z:170.228195}).
cp_loc('ff2', loc{x: 113, y:66.620499, z:181.251114}).
cp_loc('ff3', loc{x: 148.5, y:88.240654, z:170.071182}).
cp_loc('trunk1', loc{x: 123, y:101, z:212.756927}).
cp_loc('bubbleapts', loc{x: 119, y:134, z:207.926819}).
cp_loc('trunk2', loc{x: 119, y:93.144119, z:308.367706}).
cp_loc('trunk3', loc{x: 119, y:93.144119, z:329.446259}).
cp_loc('trunk4', loc{x: 117, y:92, z:464.970306}).
cp_loc('trunk5', loc{x: 117, y:92, z:486.07608}).
cp_loc('clankingrobotco', loc{x: 117, y:67.825661, z:488.258728}).
cp_loc('trunk6', loc{x: 110, y:101.128082, z:676.866333}).
cp_loc('upperbubble', loc{x: 106, y:126, z:676.866333}).
cp_loc('uphigh', loc{x: 106, y:126, z:753.207825}).
cp_loc('interstellar', loc{x: 119, y:78.26033, z:329.446259}).

%!  cp_conn(-CP:atom, -Target:atom, -Dist:integer) is nondet
%
%   @arg CP       control point atom
%   @arg Target   control point atom, other end of arc
%   @arg Dist     integer number of cycles to get to Target
%
%  Define which CPs the Zurgon beams connect
%
cp_conn('groundhouse1', 'groundplein', 4).
cp_conn('groundhouse2', 'groundplein', 4).
cp_conn('groundhouse3', 'groundplein', 5).
cp_conn('groundhouse4', 'groundplein', 4).
cp_conn('groundplein', 'elbow', 16).
cp_conn('groundplein', 'forestlawnjunction', 10).
cp_conn('forestlawn2', 'trunk1', 16).
cp_conn('forestlawn3', 'trunk1', 27).
cp_conn('forestlawn4', 'forestlawnjunction', 8).
cp_conn('forestlawn5', 'forestlawnjunction', 3).
cp_conn('forestlawn6', 'forestlawnjunction', 4).
cp_conn('isolated', 'forestlawnjunction', 6).
cp_conn('forestlawnjunction', 'elbow', 7).
cp_conn('elbow', 'trunk1', 3).
cp_conn('futureflowers', 'elbow', 3).
cp_conn('ff2', 'elbow', 4).
cp_conn('ff3', 'elbow', 3).
cp_conn('trunk1', 'trunk2', 10).
cp_conn('bubbleapts', 'trunk1', 3).
cp_conn('trunk2', 'trunk3', 2).
cp_conn('trunk3', 'trunk4', 14).
cp_conn('trunk4', 'trunk5', 2).
cp_conn('clankingrobotco', 'trunk5', 2).
cp_conn('trunk5', 'trunk6', 19).
cp_conn('trunk6', 'upperbubble', 3).
cp_conn('upperbubble', 'uphigh', 8).
cp_conn('interstellar', 'trunk3', 1).

%!  loc_region_loc(+Loc:location, -RegionLoc:dict) is det
%
%   @arg Loc a term of form `loc(CP, Offset, Towards)`
%   @arg RegionLoc a dict of form loc{x:X, y:Y, z:Z}
%
%   Succeeds when RegionLoc is the region coordinates of Loc
%
loc_region_loc(loc(CP, Offset, Towards), loc{x:X, y:Y, z:Z}) :-
    cp_loc(CP, loc{x:CPX, y:CPY, z:CPZ}),
    cp_loc(Towards, loc{x:TX, y:TY, z:TZ}),
    D is sqrt( (CPX - TX)*(CPX-TX) +
               (CPY - TY)*(CPY-TY) +
               (CPZ - TZ)*(CPZ-TZ)),
    X is CPX + Offset * 10.0 * (TX-CPX)/D,
    Y is CPY + Offset * 10.0 * (TY-CPY)/D,
    Z is CPZ + Offset * 10.0 * (TZ-CPZ)/D.



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

vw_make_pass(Type, Loc, UUID) :-
    capability(URL),
    format(codes(Content), 'rez, ~w, <~1f,~1f,~1f>', [
                               Type,
                               Loc.x, Loc.y, Loc.z]),
    http_post(URL, codes(Content), UUID, []).

vw_clear_all :-
    capability(URL),
    http_post(URL, atom(clear), _, []).

vw_throw(Term) :-
    vw_clear_all,
    throw(Term).

vw_clear(UUID) :-
    capability(URL),
    format(codes(Content), 'd, ~w', [UUID]),
    http_post(URL, codes(Content), _, []).

vw_moveall([]).
vw_moveall([H|T]) :-
    phrase(move_lang([H|T]), MoveStrCodes),
    capability(URL),
    http_post(URL, codes(MoveStrCodes), _, []).

move_lang(List) -->
    `m`,
    move_args(List).

move_args([H]) -->
    a_move_arg(H).
move_args([H|T]) -->
    {T \= []},
    a_move_arg(H),
    move_args(T).

a_move_arg(pass(UUID, Loc, _)) -->
    {loc_region_loc(Loc, RegionLoc),
     format(codes(Codes), ', ~w, <~1f,~1f,~1f>', [UUID, RegionLoc.x, RegionLoc.y, RegionLoc.z])},
     Codes.

		 /*******************************
		 *           Error reporting    *
		 *******************************/


:- multifile prolog:message//1.

prolog:message(error(invalid_move(collision(Colliding, State)))) -->
                [ 'You have collisions.', nl ],
                collision_msg(Colliding),
                ['complete state is ~w'-[State], nl].


prolog:message(error(invalid_move(object_constancy(State, NewState)))) -->
    { length(State, LS),
      length(NewState, LNS),
      LNS > LS
    },
    [ 'You made up a passenger!', nl],
    dup_passenger(NewState, State).
prolog:message(error(invalid_move(object_constancy(State, NewState)))) -->
    { length(State, LS),
      length(NewState, LNS),
      LNS < LS
    },
    [ 'Please don\'t delete the passengers!', nl],
    missing_passenger(State, NewState).
prolog:message(error(invalid_move(object_constancy(State, NewState)))) -->
    [ 'Baffled!', nl,
    'I handed you ~q'-[State], nl,
    'You handed me ~q'-[NewState], nl],
    dup_passenger(NewState, State).


missing_passenger([], _) --> [].
missing_passenger([pass(UUID, _, _) | T], NewState) -->
    { member(pass(UUID, _, _), NewState) },
    missing_passenger(T, NewState).
missing_passenger([pass(UUID, Loc, Goal) | T], NewState) -->
    { \+ member(pass(UUID, _, _), NewState) },
    [ 'What happened to passenger ~w who was at ~q going to ~w?'-[UUID, Loc, Goal],
      nl],
    missing_passenger(T, NewState).


dup_passenger([], _) --> [].
dup_passenger([pass(UUID, loc(_CP, _Off, _Target), _Goal) | T], State) -->
    { member(pass(UUID, _, _), State) },
    dup_passenger(T, State).
dup_passenger([pass(UUID, loc(CP, Off, Target), Goal) | T], State) -->
    { \+ member(pass(UUID, _, _), State) },
    [ 'Passenger ~w at loc(~w,~w,~w) going to ~w'-[UUID, CP, Off, Target, Goal],
      nl],
    dup_passenger(T, State).


collision_msg([]) --> [].
collision_msg([H|T]) -->
    ['~w'-[H], nl],
    collision_msg(T).


