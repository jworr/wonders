module Wonders exposing (main)

import Browser
import Html exposing (Html, div, text, br, ul, li, h2, h3)
import Html exposing (span, input, label, select, option, button)
import Html.Attributes exposing (style, type_, placeholder, value)
import Html.Attributes as HA
import Html.Events exposing (onClick, onInput)

import Random exposing (generate)
import Random.List exposing (shuffle)

import Rules exposing (Civ, CardName, Color(..), Wonder(..), CardName(..))
import Rules exposing (BattleOutcome(..), Science(..), Discovery(..))
import Rules exposing (Resource(..), Production(..), Era(..), Hand)
import Rules exposing (cardColor, cardName, initCiv, startingHandSize)
import Rules exposing (createAI, createCards)

--messages to run the experiments
type Msg = 
           SetPlayers Int
         | SetSimulations Int
         | SetWonder Wonder
         | SetError String
         | Run                    
         | Simulate
         | GenCivs (List Wonder)
         | GenHands (Era, List CardName)

--the summary of a series of games
type alias Model = { player      : Civ
                   , comps       : List Civ
                   , hands       : List (Era, List Hand)
                   , simulations : Int
                   , players     : Int
                   , results     : List Outcome
                   , error       : String
                   }
                   
type alias Outcome = { win       : Bool
                     , playerCiv : Civ
                     , compCivs  : List Civ
                     }

allWonders : List Wonder
allWonders = [Alexandria, Babylon, Rhodes, Olympus, Halicarnassus, Gyza, Ephesus]

--start the simulator
main = Browser.element { init = initialize
                       , update = update
                       , view = view
                       , subscriptions = (\_ -> Sub.none)}

--update the model
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
   case msg of

   --updates from user input
   SetSimulations sims  -> ({model | simulations = sims, error=""}, Cmd.none)

   --set the number of players, resets the errors
   SetPlayers players   -> ({model | error ="", players = players}, Cmd.none)  

   --sets the player's wonder
   SetWonder wonder -> let
                        updateWonder p w = {p | wonder = w}
                       in
                       (
                        {model | player = (updateWonder model.player wonder)}
                        , Cmd.none
                       ) 

   --set an error message for the user
   SetError errMsg -> ({model | error = errMsg}, Cmd.none)

   --Starts the simulation, generates other civs, trigger "GenCivs"
   Run -> ({model | results = [], hands = []}
          , shuffleWonders model.player.wonder
          )

   Simulate -> (model, Cmd.none)
 
   --adds the AI civilizations to the model, triggers "GenHands"
   GenCivs wonders -> let 
                       compCivs = createAI (model.players - 1) wonders

                       --TODO remove, just for debugging
                       outcome = { win = False
                                 , playerCiv = model.player
                                 , compCivs = compCivs
                                 }

                       newOutcomes = (outcome :: model.results)
                      in
                      (
                       {model | comps = compCivs, results = newOutcomes}

                       --go through the eras in reverse order
                       , shuffleCards model.players Three 
                      ) 

   --adds the starting hands to the model, run through the eras backwards
   GenHands (era, cards) -> 
                     let
                        hands = (era, createHands cards) :: model.hands
                        newModel = {model | hands = hands}
                     in
                     (
                       newModel 
                       , case era of
                              Three -> shuffleCards model.players Two
                              Two   -> shuffleCards model.players One
                              One   -> Cmd.none
                     )


--shuffles all the wonders besides the given wonder
--creates Cmd Msg
shuffleWonders : Wonder -> Cmd Msg
shuffleWonders playerWonder = 
   let
      otherWonders = List.filter (\w -> w /= playerWonder) allWonders
   in
      Random.generate GenCivs (Random.List.shuffle otherWonders)

--Creates the player hands, creates a Cmd Msg
shuffleCards : Int -> Era -> Cmd Msg
shuffleCards numPlayers era =
   let
      cards = createCards numPlayers era
   in
      Random.generate (\c -> GenHands (era,c)) (Random.List.shuffle cards)

--recursively split the long list of cards into hands
createHands : List CardName -> List Hand
createHands cards =
   let
      (hand, rest) = split startingHandSize cards
   in
      case rest of
         [] -> [hand]
         _  -> hand :: createHands rest
      
--splits a list at a given point
split : Int -> List a -> (List a, List a)
split n values = (List.take n values, List.drop n values)

--create the initial model
initialize : () -> (Model, Cmd Msg)
initialize _ = (default, Cmd.none)

--display the model
view : Model -> Html Msg
view state = 
   let
      results = List.map displayOutcome state.results
      red = style "color" "red"
      margin = style "margin" "2px"
   in
      div [] 
      ([h2 [] [text "7 Wonders Simulator"], 
        br [] [],
        makeForm state,
        br [] [],
        span [red, margin] [text state.error],
        h3 [] [text "Simulation Results"]] 
        ++ (printHand state.hands)
        ++ results
      )

--TODO remove debug function
printHand : List (Era, List Hand) -> List (Html Msg)
printHand = (List.concatMap (\(f,s) -> s)) 
            >> List.concat 
            >> (List.map displayCard)

--make the form to setup the simulation
makeForm : Model -> Html Msg
makeForm model = 
   let
      margin   = style "margin" "3px"
      space    = style "white-space" "pre"
      wText t  = span [space] [text t]

      makeOption w = option [value (wonderName w)] [text (showWonder w)]
      wonderOps = List.map makeOption allWonders
 
   in
      div [] 
      [
         label [] [wText "  Players  "], 
            input [margin, type_ "number", 
                   onInput (String.toInt >> setPlayers),
                   placeholder "Players", HA.min "3", HA.max "7"] [],
         
         label [] [wText "    Simulations  ", 
            input [margin, type_ "number", placeholder "Simulations",
                   onInput (String.toInt >> setSimulations),
                   HA.min "1", HA.max "1000"] []],

         label [] [wText "    Wonder  ", 
            select [margin] wonderOps],

         button [margin, onClick (checkRun model)] [text "Start"]
      ]

--determines if the number of players is valid
validPlayers : Int -> Bool
validPlayers num = 3 <= num && num <= 7

--determines if the number of simulations is valid
validSims : Int -> Bool
validSims num = 1 <= num && num <= 1000

--function to set the number of players in the simulation
setPlayers : Maybe Int -> Msg
setPlayers str = 
   case str of
      Just num -> 
         if validPlayers num then
            SetPlayers num
         else
            SetError "Players should be between 3 and 7."

      _  -> SetError "Error with number of players"

--function to set the number of simulations
setSimulations : Maybe Int -> Msg
setSimulations str = 
   case str of
      Just num -> 
         if validSims num then
            SetSimulations num
         else
            SetError "Simulations should be between 1 and 1000."

      _  -> SetError "Error with number of simulations"

--function to set wonders
setWonder : String -> Msg
setWonder str =
   case str of
      "Alexandria"    -> SetWonder Alexandria
      "Babylon"       -> SetWonder Babylon
      "Rhodes"        -> SetWonder Rhodes
      "Olympus"       -> SetWonder Olympus
      "Halicarnassus" -> SetWonder Halicarnassus
      "Gyza"          -> SetWonder Gyza
      "Ephesus"       -> SetWonder Ephesus
      _               -> SetError "Error with wonder"

--function to check if the simulator is ready to run
checkRun : Model -> Msg
checkRun model =
   if validSims model.simulations && validPlayers model.players
   then
      Run
   else
      SetError "Either/both players and simulations is incorrect"
        
--display a game's outcome
displayOutcome : Outcome -> Html Msg
displayOutcome gameResult = 
   let
       others = List.map (displayCiv Nothing) gameResult.compCivs
       displayPlayer = displayCiv (Just gameResult.win)
   in
      div [] ([displayPlayer gameResult.playerCiv] ++ others)

--Generates HTML/CSS to display a player/AI civ
displayCiv : Maybe Bool -> Civ -> Html Msg
displayCiv maybeWon civ =
   let
      background    = style "background-color" "lightpink"
      border        = style "border-style" "solid"
      pad           = style "padding" "4px"
      margin        = style "margin" "2px"
      cards         = List.map displayCard civ.cards
      showField f v = f ++ ": " ++ (String.fromInt v)
      title         = civ.playerName ++ ": " 
                      ++ (showWonder civ.wonder) ++ " " ++ won

      won = case maybeWon of
               Just result -> if result then "(Won)" else "(Lost)"
               _           -> ""
   in
      div [background, border, pad, margin] 
      ([
         h3 [] [text title],
         span [margin] [text (showField "Money" civ.money)],
         span [] [text ", "],
         span [margin] [text (showField "Stages" civ.finishedStages)],
         br [] [],
         br [] []
      ]
      ++ cards)

--Generates HTML/CSS to display a card
displayCard : CardName -> Html Msg
displayCard card = 
  let
      color    = style "background-color" ((showColor << cardColor) card)
      padding  = style "padding" "2px"
      margin   = style "margin" "1px"
   in
      span [color, padding, margin] [text (cardName card)]

--shows a wonder
showWonder : Wonder -> String
showWonder wonder = case wonder of
   Alexandria     -> "Lighthouse of Alexandria"
   Babylon        -> "Hanging Gardens of Babylon"
   Rhodes         -> "Colossus of Rhodes"
   Olympus        -> "Statue of Zeus at Olympus"
   Halicarnassus  -> "Mausoleum of Halicarnassus"
   Gyza           -> "Pyramids of Gyza"
   Ephesus        -> "Temple of Artemis at Ephesus"

--shows the wonder's name, matches the wonder type exactly
wonderName : Wonder -> String
wonderName wonder = case wonder of
   Alexandria     -> "Alexandria"
   Babylon        -> "Babylon"
   Rhodes         -> "Rhodes"
   Olympus        -> "Olympus"
   Halicarnassus  -> "Halicarnassus"
   Gyza           -> "Gyza"
   Ephesus        -> "Ephesus"

--shows a color (to css color string)
showColor : Color -> String
showColor color = case color of
   Blue       -> "lightblue"
   Brown      -> "tan"
   Purple     -> "lightpurple"
   Red        -> "lightred"
   Green      -> "lightgreen"
   Yellow     -> "lightyellow"
   Grey       -> "lightgrey"

---------------------Defaults/Example Code--------------------
--TODO specify some starting values for development etc
exampleCiv : Civ
exampleCiv = { playerName="Player"
             , wonder=Babylon
             , cards=[LumberYard, OreVein, Altar, Workshop]
             , finishedStages=0
             , money=3
             , battles=[]
             }

otherCiv = { playerName="Comp 1"
           , wonder=Alexandria
           , cards=[Altar, Workshop]
           , finishedStages=0
           , money=3
           , battles=[]
           }

otherCiv2 = { playerName="Comp 2"
            , wonder=Halicarnassus
            , cards=[Altar, Workshop]
            , finishedStages=0
            , money=3
            , battles=[]}

defaultOutcomes : List Outcome
defaultOutcomes = 
   [{win = True, 
     playerCiv=exampleCiv, 
     compCivs=[otherCiv, otherCiv2]
    }] --TODO test games

defaultCiv : Civ
defaultCiv = { cards = []
             , playerName = "Player"
             , wonder = Alexandria
             , finishedStages = 0
             , money = 3
             , battles = []
             }

default : Model
default = { player = defaultCiv
          , comps = []
          , hands = []
          , players = 0
          , simulations = 0
          , results = defaultOutcomes
          , error   = ""
          }
