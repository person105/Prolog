from pyswip import Prolog
from swiplserver import *
import re

prolog = Prolog()
prolog.consult("main.pl")
# prolog.consult("worldBuilder.pl")

# buildWorld(NumberOfCoins, ListCoinsCoord, WumpusCoord, NumberOfPits, ListPitsCoord).
#  asserta(w_Gold(r(2,3))),
#     asserta(w_Wumpus(r(1,3))),
#     asserta(w_Pit(r(3,1))),
#     asserta(w_Pit(r(3,3))),
#     asserta(w_Pit(r(4,4))).

# run(3, [_,(2,3)], (1,3), 2, [_,(3,1),(3,3),(4,4)]).

def menu() -> None:

    while True:
        try:
            choice = int(input("""
                                \n1) Run default test world \
                                \n2) Run with custom inputs \
                                \n3) Exit                   \
                                \n"""))

            if choice == 1:
                run_default()
                break

            elif choice == 2:
                get_input()
                break

            elif choice == 3:
                break

            else:
                print("Invalid choice.")

        except:
            print("Please enter a numerical value.")

def run_default() -> None:
    for res in prolog.query("run(3, [_,(2,3)], (1,3), 2, [_,(3,1),(3,3),(4,4)])."):
        print(res)

def get_input() -> None:

    list_of_coin_coord = ""
    list_of_pit_coord = ""
    
    while True:
        try:
            number_of_coins = int(input("\nPlease enter Number of Coins: "))
            
            #Arbitrary ceiling on the number of coins- to be replaced 
            if number_of_coins > 0 and number_of_coins < 6:
                index = 0

                while index < number_of_coins:
                    index += 1

                    coin_coord = input("Please enter Coordinate for Coin {}: ".format(index))
                    if (re.match('\([1-7],[1-6]\)', coin_coord)):
                        #TODO: Check that coord is not occupied
                        list_of_coin_coord+=","+coin_coord

                    else:
                        print("Please enter a coordinate in (X,Y) where 0 < X < 8 and 0 < Y < 7. \n")
                        index -= 1

                break
        except:
            print("Please enter a numerical value between 1 and 5 \n")


    while True:
        try:
            number_of_pits = int(input("Please enter Number of Pits: "))

            #Arbitrary ceiling on the number of pits- to be replaced 
            if number_of_pits > 0 and number_of_pits < 6:
                index = 0

                while index < number_of_pits:
                    index += 1
                    pit_coord = input("Please enter Coordinate for Pit {}: ".format(index))
                    if (re.match('\([1-7],[1-6]\)', pit_coord)):
                        #TODO: Check that coord is not occupied
                        list_of_pit_coord+=","+pit_coord

                    else:
                        print("Please enter a coordinate in (X,Y) where 0 < X < 8 and 0 < Y < 7. \n")
                        index -= 1
                
                break                
        except:
            print("Please enter a numerical value between 1 and 5 \n")

    while True:
        wumpus_coord = input("Please enter Coordinate for Wumpus: ")

        if (re.match('\([1-7],[1-6]\)', wumpus_coord)):
            #TODO: Check that coord is not occupied
            for res in prolog.query("run({}, [_{}], {}, {}, [_{}]).".format(number_of_coins, 
                                                                            list_of_coin_coord, 
                                                                            wumpus_coord, 
                                                                            number_of_pits,
                                                                            list_of_pit_coord)):
                print(res)
            break

        print("Please enter a coordinate in (X,Y) where 0 < X < 8 and 0 < Y < 7. \n")

def main() -> None:
    menu()
    

if __name__ == "__main__":
    main()