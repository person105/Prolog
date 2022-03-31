/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Wumpus World - by Alexander Cs�ka Roque
   Written in S�o Paulo, Brazil, April 2018
   This was developed as a exercise for the Artificial Intelligence
   Course at Universidade de S�o Paulo.
   Even though the entire code was made from scratch, I referenced
   Markus Triska's work (www.metalevel.at/wumpusworld/wumpus.pl),
   hilios(github.com/hilios/wumpus-prolog/blob/master/src/heuristic.pl)
   and more importantly, Pilar Pozos, Edgardo Yescas and Jacob V�squez
   in "Planning using situation calculus, prolog and a mobile robot".

   IMPORTANT FOR UNDERSTANDING THE WORK
   This is one of the many different approaches for the Wumpus World.
   I've combined Situational Calculus, State-successor axioms, Planning
   and Heuristics to create an Intelligent Agent capable of solving the
   problem. The characteristics of the problem are the ones presented on
   the book Artificial Intellince: A Modern Approach (AIMA).

   R=r(X,Y) will be used as reference for a certain room at position X,Y.
   S is used for situations. do(A,S) is the situation after doing
   action A on situation S.
   w_ is a prefix used for world characteristics, unknown by the agent.

   Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

% These must be dynamic so that many different worlds might be created
% at runtime.
:- abolish(w_Wall/1).
:- abolish(w_Wumpus/1).
:- abolish(w_Gold/1).
:- abolish(w_Pit/1).


:- dynamic([
  w_Wall/1,
  w_Wumpus/1,
  w_Gold/1,
  w_Pit/1
]).

%CONSTANTS
maxNumberOfMoves(40).
maxNumberOfActionsPlanned(20).

%INITIAL CONDITIONS AND STATE-SUCCESSOR AXIOMS
% These clauses indicate a certain initial situation and how world object
% react to actions the agent performs in the world. Any objects state in
% any situation can be obtained by an initial situation and a series of
% actions (Situation) using recursion.

% Hunter Position and Facing Direction on each situation
hunter(r(1,1),e,s0). %Start at cave entry r(1,1), facing east
hunter(Reference,Direction,do(Action,Situation)) :- hunter(PreviousReference,PreviousDirection,Situation), %get hunter info at last situation
    ( %if action changes hunter position
        (Action = left, Reference = PreviousReference, ((PreviousDirection = n, Direction = w)
                                                        ;
                                                        (PreviousDirection = e, Direction = n)
                                                        ;
                                                        (PreviousDirection = s, Direction = e)
                                                        ;
                                                        (PreviousDirection = w, Direction = s)))
                                                        ; %turn left

        (Action = right, Reference = PreviousReference, ((PreviousDirection = n, Direction = e)
                                                        ;
                                                        (PreviousDirection = e, Direction = s)
                                                        ;
                                                        (PreviousDirection = s, Direction = w)
                                                        ;
                                                        (PreviousDirection = w, Direction = n)))
                                                        ; %turn right

        (Action = forward, Direction = PreviousDirection, getForwardRoom(PreviousReference,PreviousDirection,CurrentReference), 
                                                        !, %no backtrack
                                                        (w_Wall(CurrentReference) -> Reference = PreviousReference;
                                                        Reference = CurrentReference)) %go forward
    ); %or
    ( %no actions that change hunter position happened
        hunter(Reference,Direction,Situation), %Position of hunter now same as before
        %not provable
        \+Action = left,
        \+Action = right,
        \+Action = forward
    ).

%Hunters Life
hunterAlive(s0). %Hunter is alive if he was alive in the last situation and the action taken didnt kill him now
hunterAlive(do(Action,Situation)) :- hunterAlive(Situation),
    (hunter(Reference,_,do(Action,Situation)), \+w_Pit(Reference), (wumpusAlive(do(Action,Situation)) -> \+w_Wumpus(Reference) ; true)).

%Wumpus Life
wumpusAlive(s0). %Starts alive
wumpusAlive(do(Action,Situation)) :- wumpusAlive(Situation), %if he was dead in previous situations, remain dead

    %if hunter is facing wumpus and hunter shoots, wumpus dies else wumpus alive
    ((Action = shoot, hasArrow(Situation), hunter(PreviousReference,PreviousDirection,Situation), w_Wumpus(WumpusReference), 
    isFacing(PreviousReference,PreviousDirection,WumpusReference)) -> false ; true).

%Has the one arrow that can kill a Wumpus
hasArrow(s0). %Starts with arrow
hasArrow(do(Action,Situation)) :- %Return false and stays false after using shoot action once
    (Action = shoot -> false ; hasArrow(Situation)).

%Indicates whether hunter has the gold
hasGold(s0) :- false. %Starts without gold
hasGold(do(Action,Situation)) :-
    (   hasGold(Situation) %If hunter had the gold before, still has now
    ;   (Action = grab, w_Gold(Reference), hunter(Reference,_,Situation)) %If hunter use grab at the same room as gold, pick it up
    ).

%PERCEPTIONS
% Much like the state-successor axioms, the perceptions work seamlessly
% with the situations. These are supplied to the agent to generate world
% knowledge.
perceiveBump(do(Action,Situation)) :- Action = forward, hunter(Reference,Direction,Situation), 
                                        hunter(Reference,Direction,do(Action,Situation)). %If uses forward but remains the same, hit a wall, hence feels bump
perceiveGlitter(Situation) :- hunter(Reference,_,Situation), w_Gold(Reference). %If at the same room as gold, perceive Glitter
perceiveBreeze(Situation) :- hunter(Reference,_,Situation), w_Pit(ReferencePit), isAdjacent(Reference,ReferencePit). %If adjacent to a Pit, perceive breeze
perceiveStench(Situation) :- hunter(Reference,_,Situation), w_Wumpus(ReferencePit), isAdjacent(Reference,ReferencePit). %If adjacent to a Wumpus (Dead or Alive), perceive Stench
perceiveScream(do(Action,Situation)) :- wumpusAlive(Situation), \+wumpusAlive(do(Action,Situation)). %If Wumpus was alive and now is dead, perceive Scream

%WORLD KNOWLEDGE
% Also work seamlessly by using information from past situations and
% perceptions to build on useful world knowledge.
wall(Reference,do(Action,Situation)) :- (perceiveBump(do(Action,Situation)), hunter(WallReference,Dh,do(Action,Situation)), getForwardRoom(WallReference,Dh,Reference)) ; wall(Reference,Situation). %Agent remembers bumping into walls
visited(r(1,1),s0).
visited(Reference,do(Action,Situation)) :- hunter(Reference,_,do(Action,Situation)) ; visited(Reference,Situation). %Agent remembers all rooms he has visited
gold(Reference,do(Action,Situation)) :- (perceiveGlitter(do(Action,Situation)),hunter(Reference,_,do(Action,Situation))) ; gold(Reference,Situation). %Once gold is found, remember where it was
breeze(Reference,s0) :- perceiveBreeze(s0),hunter(Reference,_,s0).
breeze(Reference,do(Action,Situation)) :- (perceiveBreeze(do(Action,Situation)),hunter(Reference,_,do(Action,Situation))) ; breeze(Reference,Situation). %Remember where breeze was perceived
stench(Reference,s0) :- perceiveStench(s0),hunter(Reference,_,s0).
stench(Reference,do(Action,Situation)) :- (perceiveStench(do(Action,Situation)),hunter(Reference,_,do(Action,Situation))) ; stench(Reference,Situation). %Remember where stench was perceived
heardWumpusScream(s0) :- false.
heardWumpusScream(do(Action,Situation)) :- perceiveScream(do(Action,Situation)) ; heardWumpusScream(Situation). %Represent actual knowledge of hearing the wumpus scream (and hopefully die)

%WORLD EVALUATION
% These are extra tools given to the agent to evaluate world
% characteristics. These should be used by the heuristic to evaluate
% different possible actions and choose the best.

% A room is considered ok if there is no chance it has a pit or an alive wumpus
isOkRoom(Reference,Situation) :- \+possiblePit(Reference,Situation), (\+heardWumpusScream(Situation) -> \+possibleWumpus(Reference,Situation);true).

% Evaluates possibility of pit in a certain room. Checks if all adjacent
% rooms that were visited had breezes
possiblePit(Reference,Situation) :- \+visited(Reference,Situation), getAdjacentRooms(Reference,AdjList), trimNotVisited(AdjList,Situation,RoomsList), (RoomsList = []; checkBreezeList(RoomsList,Situation)).
checkBreezeList([],_).
checkBreezeList([H|T],Situation) :- checkBreezeList(T,Situation), breeze(H,Situation).

% One can only be certain of a pits position if there is a room with
% breeze where 3 adjacent rooms were visited and don't have a pit. The
% pit is in the fourth room certainly.
certainPit(ReferencePit,Situation) :-
    getAdjacentRooms(ReferencePit,AdjList),
    trimNotVisited(AdjList,Situation,RoomsList),
    checkPitCertainty(ReferencePit,RoomsList,Situation).

checkPitCertainty(_,[],_) :- false.
checkPitCertainty(ReferencePit,[H|T],Situation) :-
    breeze(H,Situation),
    (
        (
        getAdjacentRooms(H,AdjList),
        trimVisited(AdjList,Situation,RoomsList),
        trimWall(RoomsList,Situation,LT2),
        LT2 = [ReferencePit]
        )
        ; checkPitCertainty(ReferencePit,T,Situation)
    ).

% Evaluates possibility of Wumpus in a certain room. Checks if all
% adjacent rooms that were visited had stench
possibleWumpus(Reference,Situation) :-
    (certainWumpus(WumpusReference,Situation) -> Reference = WumpusReference %a certain Wumpus is also a possible Wumpus
    ;   (\+visited(Reference,Situation), getAdjacentRooms(Reference,AdjList), trimNotVisited(AdjList,Situation,RoomsList), (RoomsList = []; checkStenchList(RoomsList,Situation)))).
checkStenchList([],_).
checkStenchList([H|T],Situation) :- checkStenchList(T,Situation), stench(H,Situation).

% More easily than checking for pits, as we know there is only one
% Wumpus, one can mix and match adjacent rooms of two or more rooms with
% stench. If only one room that wasnt visited remains, the Wumpus must
% be there.
certainWumpus(WumpusReference,do(Action,Situation)) :-
     certainWumpus(WumpusReference,Situation); %Check first Wumpus certainty before, because if he is killed and the hunter visits the space the rest of the code returns false
     (
        setof(Reference,stench(Reference,do(Action,Situation)),[H|T]), %H is going to be used as reference, and T will help
        getAdjacentRooms(H,AdjList),
        trimVisited(AdjList,Situation,AdjListTail),
        trimNotAdjacent(AdjListTail,T,RoomsList),
        RoomsList = [WumpusReference] %If only one room is reached, that is where the wumpus is
     ).

% HEURISTIC
% Here is where you teach the intelligent agent different strategies
% to make decisions. At the end of the heuristic cycle, a major action
% must be returned. Possible major actions are:
%   move(R) - Move to a certain room. Should be used to explore or move
%       to strategic places.
%   grabGold - Should only be used if gold position is known. If not on
%       gold position, moves through known squares to go grab it.
%   shootWumpus - Should only be used if wumpus position is known. Does
%       the least amount of moves on known squares and shoots Wumpus.
%   exitCave - moves to cave entrance and climb out.
%   left, right, forward, grab, shoot, climb - does action without
%       checking anything.
%
% If you want to implement you own heuristic or strategy, do changes
% in the code below.
%
% This heuristic explores with the least amount of actions possible,
% shoots Wumpus as soon as he is certain and doesn't take risks on
% exploration. This will sometimes take a while!
heuristic(Situation,H) :-
    getAllExplorableRooms(Situation,List), %Get entire list of all rooms adjacent to rooms that were visited
    (\+List=[] -> getBetterExplorableRoom(Situation,List,P,Reference) ; P = 5000), %Only run ranking of rooms if there are rooms to rank
    (   hasGold(Situation) -> H = exitCave %If hunter has gold, proceed to exit
    ;   (\+hasGold(Situation), gold(_R,Situation)) -> H = grabGold %If doesn't have gold but knows where it is, go get it
    ;   (certainWumpus(_RW,Situation),hasArrow(Situation),\+heardWumpusScream(Situation)) -> H = shootWumpus %If is certain of where the Wumpus is, has arrow and Wumpus is alive, shoot him
    ;   P < 100 -> H = move(Reference) %Only move if best room to explore is not dangerous
    ;   H = exitCave %If no rooms to explore, exit cave
    ).

getBetterExplorableRoom(Situation,List,P,Reference) :-
    rankRooms(List,Situation,RL),
    sort(RL,SRL),
    [Head|_] = SRL,
    rr(P,Reference) = Head.

% Ranks rooms by number of actions to explore and danger levels
rankRooms([],_,[]).
rankRooms([H|T],Situation,RL) :-
    rankRooms(T,Situation,RoomsList),
    %Count actions
    doMove(H,ST,Situation),
    countActions(ST,Situation,NActions),
    %Check breeze and stench
    (isOkRoom(H,Situation) -> DangerPoints = 0; DangerPoints = 100),
    %Check certain Pit and Wumpus
    (certainPit(H,Situation) -> CertainPitPoints = 1000; CertainPitPoints = 0),
    ((\+heardWumpusScream(Situation), certainWumpus(H,Situation)) -> CertainWumpusPoints = 1000; CertainWumpusPoints = 0),
    Total is NActions + DangerPoints + CertainPitPoints + CertainWumpusPoints, %Saves rank for each room
    RR = rr(Total,H),
    add(RR,RoomsList,RL).

%PLANNING
% The following clauses should be used for planning of actions. Planning
% will perform a Breadth First Search from a certain situation using
% actions to reach a desired goal. When performing planning for
% sequences of more than 10 actions this can take a long while.

% Preconditions for primitive actions. Define whether an action can be
% taken at each situation.
poss(forward,Situation) :- %Allow planning only on visited and ok rooms.
    hunter(Reference,Direction,Situation),
    getForwardRoom(Reference,Direction,RF),
    isOkRoom(RF,Situation).
poss(left,s0).
poss(left,do(Action,_S)) :- \+Action = right. %Limit redundant turning
poss(right,s0).
poss(right,do(Action,_S)) :- \+Action = left. %Limit redundant turning

%Legality axioms - Makes certain that a situation is possible and legal
%legal(S,CurrentSituation) reads: If CurrentSituation is legal, return whether S is legal
legal(Situation,Situation). %If S is legal, S is legal
legal(do(Action,Situation),CurrentSituation):-
    maxNumberOfActionsPlanned(Max), %Get maximum allowed number of actions
    legal(Situation,CurrentSituation), %Tries to find legal actions, starting from provided situation CurrentSituation
    countActions(Situation,CurrentSituation,N), %Count number of actions from CurrentSituation to S
    (N > Max -> (!, write('REACHED MAX NUMBER OF ACTIONS PLANNED'),false) ; true), %If too many actions are being taken, probably there is no solution, hence return false
    poss(Action,Situation). %Check which actions are allowed at S

% Movement planner - The last forward action is forced, even if it
% doesn't result in the hunter's movement. That must be done because if
% the hunter hits a wall it won't know it hasn't moved until it receive
% a bump as a perception.
% doMove(R,S,CurrentSituation) returns a plan of a sequence of movement actions that
% make the hunter in situation CurrentSituation move to R. S is returned as the
% resulting situation.
doMove(MoveReference, CurrentSituation, CurrentSituation) :- hunter(MoveReference,_,CurrentSituation). %Moving to where the hunter is returns no actions
doMove(MoveReference, do(forward,Situation), CurrentSituation) :- legal(Situation,CurrentSituation),hunter(Reference,Direction,Situation),isAdjacent(Reference,MoveReference),isFacing(Reference,Direction,MoveReference),!. %Reads: Which is a situation S supposing CurrentSituation is legal, where the hunter is at R?

doFace(MoveReference, Situation, CurrentSituation) :- legal(Situation,CurrentSituation),hunter(Reference,Direction,Situation),isFacing(Reference,Direction,MoveReference),!. %Similar to doMove, but only faces de target

%ACTUATOR
% After the heuristic defines a major action, this clause will convert
% that action to a situation with planning. Passing this situation to
% the next loops counts as acting.
doActions(H,Situation,CurrentSituation) :-
    (   H = move(Reference) -> doMove(Reference,Situation,CurrentSituation) %Move
    ;   H = grabGold -> (gold(Reference,CurrentSituation), doMove(Reference,SI,CurrentSituation), Situation = do(grab, SI)) %Move and then grab
    ;   H = shootWumpus -> (certainWumpus(WumpusReference,CurrentSituation), doFace(WumpusReference,SI,CurrentSituation), Situation = do(shoot, SI)) %Face Wumpus and shoot
    ;   H = exitCave -> (hunter(PreviousReference,_,s0), doMove(PreviousReference,SI,CurrentSituation), Situation = do(climb, SI)) %Moves to entry and climbs
    ;   H = climb -> Situation = do(climb, CurrentSituation) %Climb
    ;   H = forward -> Situation = do(forward, CurrentSituation) %Does Forward
    ;   H = left -> Situation = do(left, CurrentSituation) %Does Left
    ;   H = right -> Situation = do(right, CurrentSituation) %Does Right
    ;   H = grab -> Situation = do(grab, CurrentSituation) %Does Grab
    ;   H = shoot -> Situation = do(shoot, CurrentSituation) %Does Shoot
    ).

%INTELLIGENT AGENT LOOP
% An entire Agent Loop consists of perceptions, Gathering World
% Knowledge, Heuristic (Deciding actions), Planning and acting. The
% following clauses will run the loops while printing relevant
% information so that we can watch our little AI moving. Each loop
% consists of a few smaller actions that are planned, but only one major
% heuristic action.
% In this version of the program, one should use the runManyMaps(N0,NF)
% clause to run a bunch of maps in sequence.
runManyMaps(N0,NF) :- %Runs map N0 until NF inclusive in sequence.
    consult('worldBuilder.pl'), %This file has information for different maps
    make, %Reset files if changed
    runInSequence(N0,NF). %Runs many maps in sequence

% run :-
%     consult('worldBuilder.pl'), %This file has information for different maps
%     run(100). %Ruins AIMA Map


% runInSequence(N0,NF) :- %This loops through different maps and runs agent in each one
%     run(N0),
%     N1 is N0+1,
%     (N1 =< NF -> runInSequence(N1,NF) ; true). %Run next map if not done.

run(NumberOfCoins, [_ | ListCoinsCoord], WumpusCoord, NumberOfPits, [_ |ListPitsCoord]) :-
    format('List of coins: ~p', [ListCoinsCoord]), nl,
    format('No of coins: ~p', getLength([ListCoinsCoord],N)), nl,
    format('Wumpus Coord: ~p', WumpusCoord), nl,
    format('No of Pits: ~p', NumberOfPits), nl,
    format('List of Pits: ~p', getLength([ListPitsCoord],N)),

    clearWorld,
    buildWalls,
    SetMode = 1,
    iterateCoinsList([ListCoinsCoord]), %set coins
    iteratePitsList([ListPitsCoord]), %set pits
    asserta(w_Wumpus(r(WumpusCoord))), %set wumpus
    
    % buildWorld(NumberOfCoins, [_ |ListCoinsCoord], WumpusCoord, NumberOfPits, [_ |ListPitsCoord]),
    format('~n~n~n   Playing on world ~d ~n~n~n', 1),
    callTime(runloop(0,s0)).

iterateCoinsList([]).
iterateCoinsList([H|T]) :- 
    asserta(w_Gold(r(H))),
    iterateCoinsList(T).

iteratePitsList([]).
iteratePitsList([H|T]) :- 
    asserta(w_Pit(r(H))),
    iteratePitsList(T).

getLength([], 0).
getLength([H|T], N) :- N is N1+1, getLength(T, N1).

%Removes all objects from world
clearWorld :-
    retractall(w_Wall(_)),
    retractall(w_Gold(_)),
    retractall(w_Wumpus(_)),
    retractall(w_Pit(_)).

%Builds 7x6 outer walls to limit world
buildWalls :-
    asserta(w_Wall(r(0,0))),
    asserta(w_Wall(r(0,1))),
    asserta(w_Wall(r(0,2))),
    asserta(w_Wall(r(0,3))),
    asserta(w_Wall(r(0,4))),
    asserta(w_Wall(r(0,5))),
    asserta(w_Wall(r(0,6))),


    asserta(w_Wall(r(1,7))),
    asserta(w_Wall(r(2,7))),
    asserta(w_Wall(r(3,7))),
    asserta(w_Wall(r(4,7))),
    asserta(w_Wall(r(5,7))),
    asserta(w_Wall(r(6,7))),
    asserta(w_Wall(r(7,7))),

    asserta(w_Wall(r(7,6))),
    asserta(w_Wall(r(7,5))),
    asserta(w_Wall(r(7,4))),
    asserta(w_Wall(r(7,3))),
    asserta(w_Wall(r(7,2))),
    asserta(w_Wall(r(7,1))),

    asserta(w_Wall(r(8,0))),
    asserta(w_Wall(r(7,0))),
    asserta(w_Wall(r(6,0))),
    asserta(w_Wall(r(5,0))),
    asserta(w_Wall(r(4,0))),
    asserta(w_Wall(r(3,0))),
    asserta(w_Wall(r(2,0))),
    asserta(w_Wall(r(1,0))).

% This clause is called before the actual loop to check if maximum
% number of moves has been reached (Stops if its taking too long)
runloop(T,_) :-
    maxNumberOfMoves(Max),
    T >= Max,
    write('Reached max number of moves'), !, false.

%Main loop.
runloop(T,CurrentSituation) :-
    %Gets current hunter position and prints.
    hunter(r(X,Y),D,CurrentSituation),
    format('Hunter at [~d,~d, ~w], ', [X,Y,D]),!,

    %Get action from heuristic (Strategy) in this situation
    heuristic(CurrentSituation,H),
    format('wants to do ~w, ', [H]), %Prints desired action

    %Calls actuators which use planning to do Actions
    doActions(H,Situation,CurrentSituation),
    write('does '),
    planToActionList(Situation,CurrentSituation,[],List),
    printList(List), %Prints list of all smaller actions that were done.
    ((certainWumpus(WumpusReference,Situation),\+heardWumpusScream(Situation)) -> format('I am certain Wumpus is at ~w',WumpusReference);true), %Prints Wumpus position if certain
    format('~n'),

    NT is T+1, %Set new timestep

    %The following are needed to check if hunter climbed out of the cave
    do(Action,_) = Situation, %Get last action done
    hunter(CurrentReference,_,Situation), %Get hunters position now
    hunter(PreviousReference,_,s0), %Get Cave entry

    %If hunter climbed out or died, endloop. If not, run next loop.
    (
        ((Action = climb, CurrentReference = PreviousReference)  ; \+hunterAlive(Situation)) -> endLoop(Situation,Action)
        ;   (!,runloop(NT,Situation))
    ),!.

% After Ending the loop, game is over, print everything that is
% interesting to file.
endLoop(Situation,Action) :-
    countActions(Situation,s0,N),
    format('~n~n   '),
    (hasGold(Situation) -> write('Got the gold'); write('Couldnt find gold')),
    ( \+hunterAlive(Situation) -> write(' and died') ; write(' and left the cave')),
    format(' in a total of ~d actions! ~n~n',N),
    %Scoring
    ((hasGold(Situation),Action = climb) -> GoldPoints is 1000 ; GoldPoints is 0),
    (\+hunterAlive(Situation) -> DeathPoints is -1000 ; DeathPoints is 0),
    (hasArrow(Situation) ->ArrowPoints is 0 ; ArrowPoints is -10),
    Score is GoldPoints + DeathPoints + ArrowPoints - N,
    format('   Made a total of ~d Points.',Score).

%HELPERS
%These are helper functions that make the programming above easier
add(Element,List,[Element|List]). %Adds element to list

countList([],0). %Counts number of elements in list
countList([_|Tail], N) :- countList(Tail, N1), N is N1 + 1.

trimNotVisited([],_,[]). %Removes rooms that weren't visited from list of rooms
trimNotVisited([H|T],Situation,RoomsList) :- trimNotVisited(T,Situation,List), (visited(H,Situation) -> append([H],List,RoomsList); RoomsList = List).

trimVisited([],_,[]). %Removes rooms that were visited from list of rooms
trimVisited([H|T],Situation,RoomsList) :- trimVisited(T,Situation,List), (visited(H,Situation) -> RoomsList = List; append([H],List,RoomsList)).

trimWall([],_,[]). %Removes rooms that have been confirmed as walls from list of rooms
trimWall([H|T],Situation,RoomsList) :- trimWall(T,Situation,List), (wall(H,Situation) -> RoomsList = List; append([H],List,RoomsList)).

trimNotAdjacent([],_,[]). %used as trimNotAdjacent(L,T,LT)
trimNotAdjacent(_,[],[]). %Removes rooms from List L that are not adjacent to any room in list T
trimNotAdjacent([AdjListHead|AdjListTail],[TrimHead|TrimTail],RoomsList) :-
    trimNotAdjacent([AdjListHead],TrimTail,LT1),
    trimNotAdjacent(AdjListTail,[TrimHead|TrimTail],LT2),
    append(LT1,LT2,LT3),
    (isAdjacent(AdjListHead,TrimHead) -> append([AdjListHead],LT3,RoomsList) ; RoomsList = LT3).

%Converts plan (Actions from one situation to another) to Action list
planToActionList(Situation,Situation,ACC,ACC).
planToActionList(do(Action,NextSituation),CurrentSituation,ACC,X) :- planToActionList(NextSituation,CurrentSituation,[Action|ACC],X).

%Prints List
printList([]).
printList([A|B]) :-
    format('~w, ', A),
    printList(B).

%Returns room in front of another in a certain direction
getForwardRoom(r(X0,Y0),PreviousDirection,r(XN,YN)) :-
    (PreviousDirection = n, XN is X0, YN is Y0+1)
    ;
    (PreviousDirection = e, XN is X0+1, YN is Y0)
    ;
    (PreviousDirection = s, XN is X0, YN is Y0-1)
    ;
    (PreviousDirection = w, XN is X0-1, YN is Y0).

%Checks if one room is adjacent to another room
isAdjacent(r(X,Y),r(XT,YT)) :-
    (X =:= XT, Y =:= YT+1);
    (X =:= XT, Y =:= YT-1);
    (X =:= XT+1, Y =:= YT);
    (X =:= XT-1, Y =:= YT).

%Checks if a hunter in room R, looking to Direction D is facing room RT
isFacing(r(X,Y),D,r(XT,YT)) :-
    (D = n, X =:= XT, YT > Y);
    (D = s, X =:= XT, YT < Y);
    (D = e, Y =:= YT, XT > X);
    (D = w, Y =:= YT, XT < X).

%Returns list of all adjacent rooms
getAdjacentRooms(r(X,Y),List) :-
    XL is X-1,
    XR is X+1,
    YD is Y-1,
    YU is Y+1,
    append([r(XL,Y), r(XR,Y), r(X,YU), r(X,YD)],[],List).

% The following functions are used to get a list of explorable rooms.
% Those are rooms adjacent to rooms that were already visited. All rooms
% on the border of what has been explored. In a certain situation S a
% list L is returned with all possible rooms.
getAllExplorableRooms(Situation,List) :- getAllExplorableRooms(Situation,Situation,List). %Simplifies call
getAllExplorableRooms(Situation,s0,List) :-
    hunter(Reference,_,s0),
    getAdjacentRooms(Reference,AdjList),
    appendWithExplorableCheck(AdjList,Situation,[],List).
getAllExplorableRooms(Situation,do(Action,CurrentSituation),List) :-
    getAllExplorableRooms(Situation,CurrentSituation,L0),
    hunter(Reference,_,do(Action,CurrentSituation)),
    getAdjacentRooms(Reference,AdjList),
    appendWithExplorableCheck(AdjList,Situation,L0,List).

appendWithExplorableCheck([],_,L2,L2).
appendWithExplorableCheck([H|T],Situation,L2,List) :-
    appendWithExplorableCheck(T,Situation,L2,RoomsList),
    (   isExplorable(H,Situation,RoomsList) -> List = [H|RoomsList] ; List = RoomsList).

isExplorable(Reference,Situation,List) :- \+member(Reference,List), \+wall(Reference,Situation), \+visited(Reference,Situation).

%Counts number of actions between two situations
countActions(s0,s0,0).
countActions(Situation,Situation,0).
countActions(do(_A,Situation),CurrentSituation,N) :- %Count number of actions between two situations
    countActions(Situation,CurrentSituation,N0),
    N is N0+1.

callTime(G,T) :- %Returns Call Time
    statistics(runtime,[T0|_]),
    G,
    statistics(runtime,[T1|_]),
    T is T1 - T0.

callTime(G) :- %Prints Call Time
    callTime(G,T),
    format('~n~n[Time to run in ms: ~d]',T).

