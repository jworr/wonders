module Game exposing ()

import Rules exposing (CardName)

type Action = Free CardName                --use resource or upgrade chain
            | Buy CardName Int             --spend money
            | WithTrade CardName Int Bool  --trade with a neighbor to buy
            | Sell CardName                --sell the card instead of purchase
            | Build CardName               --build a stage of the wonder with card

--play an entire game TODO

--play an era of the game TODO

--play a turn of the game TODO

--have the player make a random valid move TODO

--have the player make a greedy move, parametetrized by a discount rate TODO

--have the playe make a balanced move, attempt to have some of each color TODO
--of card

--returns the valid moves the player can take TODO

--performs the "battles" after an era TODO

--yields all the players with their neighbors TODO
