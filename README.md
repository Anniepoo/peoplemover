# Robot Railroad

## Using the railroad

Go to an unused railroad region and claim it for your use by clicking on the
claim region tool. Please be courteous and **DO NOT** claim regions that are actively in use.
The tool will respond with a "capability" in chat. Copy this URL and provide it to your prolog
program. Call `set_capability/1` with this URL as an atom. Now the other APIs will work.

Your first call should be to `init_trains/0`. This will set up a problem with cars that need
moved to their proper positions. If you want to start the problem over again, use `reset_trains/0`.
If you want a new problem, call `init_trains/0` again.

The track is marked with 'control points'(CP). Instructions to the loco are given in terms
of the CPs, and cars will start and be delivered to CPs.

There is a locomotive. You can move the locomotive 'east' or 'west'. The loco will run
to the next CP it encounters. You can include an offset if you want to stop one of the cars,
rather than the loco, at a control point. To move the locomotive call `locomotive/2`.
`locomotive/2` _blocks_ until the locomotive reaches it's destination. If the move would cause a crash,
the predicate throws an exception.

As an example, suppose you have 3 cars behind the loco, and want to drop off the last one at
the CP.  Stop the loco at CP + 2. Locations _east_ of the CP are positive.

If you move the loco or a car into a stationary car, it will 'couple' to the train, and subsequently
move with it.  If you want to leave cars behind, uncouple them with `uncouple/1`. The argument
should be an integer. If positive, that many cars to the _east_ of the loco will be uncoupled.
If negative, that many cars to the _west_ of the loco will be uncoupled. Only cars at the end
of the train may be uncoupled.

The track diverges at various points, a 'turnout' or 'points' (switch in the US), shaped a
bit like capital Y.  The train can run from the stem to either leg, or from either leg to the
stem, but not from one leg to the other. You need to control the turnout. If it goes left,

The turnout can be set to _left_ or _right_. If you stand at the stem of the turnout these
designate which direction a train will go. You **also need to set the turnout** if the train
is **coming** from that leg.

Attempts to move a train from a leg to a turnout set the other direction will cause `locomotive/2` to
throw an exception.

Each turnout has a label next to it with a number. So, for example, to change turnout t3 to the left,
call  `turnout(t3, left)`.

## The Goal

when you call `init_trains` a new problem will be set up.

You can then call `move_car_to/3` to discover where to move cars.

Move all the cars to their positions.

Note that some cars remain in position. If these cars are moved, they must be moved back.

## User API

## API into Opensim

The body of the call is a series of lines as `text/plain`.

If the call starts with "rez", and it is a 4 tuple in llCSV2List format of 

 - "rez"
 - string - name of car (inventory name)
 - vector - position of car
 - rotation - rotation of car

The call will rez the appropriate car in the appropriate spot
and respond with `text/plain` with the UUID of the car in the body.

Otherwise, the body will be split by occurances of the # character and every resulting
string converted by llCSV2List. 

If the first element of list is the string "r" the remainder of tghe list will be llRegionSay'ed over the command channel. If the first element is "u" the second element will be interpreted as
a UUID, and the remainder will be llRegionSayAt'ed to that UUID. If the first element is "t",
the remainder will be llRegionSay'ed on the turnout channel.






 * action=create   a car is created and the car's key is returned as body
   car=key      key is the UUID of the car
   x= 
   y=
   z=
   t=    angle in degrees to rotate car
   p=    angle in degrees 'front' end of car is up
 
 * action=move same params as above

 * action=crash
   car=key
   

   action=deleteall   clears the whole region


