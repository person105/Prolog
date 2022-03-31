from pyswip import Prolog
from swiplserver import *

prolog = Prolog()
prolog.consult("main.pl")
# prolog.consult("worldBuilder.pl")

# buildWorld(NumberOfCoins, ListCoinsCoord, WumpusCoord, NumberOfPits, ListPitsCoord).
#  asserta(w_Gold(r(2,3))),
#     asserta(w_Wumpus(r(1,3))),
#     asserta(w_Pit(r(3,1))),
#     asserta(w_Pit(r(3,3))),
#     asserta(w_Pit(r(4,4))).

def main():

    for res in prolog.query("run(3, [_,(2,3)], (1,3), 2, [_,(3,1),(3,3),(4,4)])."):
        print(res)


if __name__ == "__main__":
    main()