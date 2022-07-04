module Rules exposing ( Civ, Wonder(..), Resource(..), Color(..), Discovery(..), CardName(..), BattleOutcome(..), Science(..), Production(..), Era(..), Hand, cardColor, cardCost, cardUpgrades, cardProduces, cardScores, cardName, initCiv, createAI, createCards, startingHandSize)

startingMoney = 3
startingHandSize = 7

--everything a player has
type alias Civ = { cards           : Hand
                 , playerName      : String
                 , wonder          : Wonder
                 , finishedStages  : Int
                 , money           : Int
                 , battles         : List BattleOutcome
                 }

--the wonders
type Wonder = Alexandria     --lighthouse
            | Babylon        --hanging gardens
            | Rhodes         --colossus
            | Olympus        --statue of Zeus
            | Halicarnassus  --Mausoleum
            | Gyza           --Pyramids 
            | Ephesus        --Temple of Artemis

--types of resources
type Resource = Stone | Brick | Wood | Iron | Cloth | Paper | Glass | Money Int

--types of production
type Production = All (List Resource)
                | Exclusive (List Resource)

--card color
type Color = Blue | Brown | Purple | Red | Green | Yellow | Grey

--science type
type Discovery = Gear | Compass | Tablet

--science production
type Science = Single Discovery
             | DiscoveryChoice

--battle strength
type alias Strength = Int

type alias Hand = List CardName

--battle outcome
type BattleOutcome = Win Int | Loss

--the eras of the game
type Era = One | Two | Three 

--list all the specific cards
type CardName = LumberYard | OreVein | ClayPool | StonePit | TimberYard 
              | ClayPit | Excavation | ForestCave | TreeFarm | Mine | Loom 
              | Glassworks | Press | EastTradingPost | WestTradingPost 
              | MarketPlace | Tavern | Altar | Theatre | Baths | PawnShop 
              | Stockade | Barracks | GuardTower | Apothecary | Workshop 
              | Scriptorium | Sawmill | Foundary | Brickyard
              | Quarry | Caravansery | Forum | Vineyard | Bazaar | Temple
              | Courthouse | Statue | Aqueduct | Library | Laboratory 
              | Stables | ArcheryRange | Walls | TrainingGround | Dispensary
              | School | Gardens | Senate | TownHall | Pantheon | Palace
              | University | Observatory | Lodge | Study | Academy
              | SiegeWorkshop | Fortification | Arsenal | Circus | Arena
              | Lighthouse | Haven | ChamberOfCommerce | WorkersGuild
              | CraftsmansGuild | ShipOwnersGuild | TradersGuild
              | MagistratesGuild | BuildersGuild | PhilosophersGuild
              | ScientistsGuild | SpiesGuild | StrategistsGuild
              | ArchitectsGuild | GamersGuild


--TODO each wonder/civ needs abilities

--score a civilization TODO
scoreCiv : Civ -> Civ -> Civ -> Int
scoreCiv civ left right =
   let
       baseScore   = List.sum (List.map (cardScores civ left right) civ.cards)
       battleScore = List.sum (List.map scoreBattle civ.battles)
       sciScore    = scoreScience civ
   in
      baseScore + battleScore + sciScore

--TODO double check that replacing all the science choices
--with the same option is optimal
scoreScience : Civ -> Int
scoreScience player =
   let
      --get single discoveries
      getSingle d = case d of
                       Single s -> Just s
                       _        -> Nothing

      --replace choices with a discover type
      replace choice dis = case dis of 
                              Single s -> s
                              _        -> choice

      --get all the science
      science = List.filterMap cardScience player.cards

      --try a replacement option
      try choice = scoreDiscoveries (List.map (replace choice) science)

      options = [try Gear, try Tablet, try Compass]
   in
      --if there is a choice, try all three
      if List.member DiscoveryChoice science then
         case List.minimum options of
            Just score -> score
            _          -> 0   --will never happen
      else
         scoreDiscoveries (List.filterMap getSingle science)

--scores a list of discoveries
scoreDiscoveries : List Discovery -> Int
scoreDiscoveries sci = 
   let
      gears   = countDiscovery sci Gear
      comps   = countDiscovery sci Compass
      tablets = countDiscovery sci Tablet
   in
      List.sum [gears * gears, comps * comps, tablets * tablets]
      + (7 * (Maybe.withDefault 0 (List.minimum [gears, comps, tablets])))

--scores a battle
scoreBattle : BattleOutcome -> Int
scoreBattle battle = 
   case battle of
      Win points -> points
      _          -> -1

--initialize civilization, starts with default values
initCiv : String -> Wonder -> Civ
initCiv name wonder = Civ [] name wonder 0 startingMoney []

--creates a specified number of AI civilizaions
--assumes the list of wonders is pre-shuffled
createAI : Int -> List Wonder -> List Civ
createAI num wonders =
   let
       --takes an int and transforms it into a list of names
       mkNames = (List.range 1) 
                 >> (List.map String.fromInt) 
                 >> (List.map (\i -> "Comp " ++ i))
       
       names = mkNames num
   in
      List.map (\(n,w) -> initCiv n w) (List.map2 Tuple.pair names wonders)

--creates all the cards needed per era
createCards : Int -> Era -> List CardName
createCards numPlayers era =
   let
      expand card = List.repeat (cardsPerEra numPlayers era card) card
   in
      List.concatMap expand (cardSet era)
   
--produces the color of the card
cardColor : CardName -> Color
cardColor card = 
   let pairs = [ (brownCards, Brown), (greyCards, Grey), 
                 (yellowCards, Yellow),
                 (blueCards, Blue), (redCards, Red), 
                 (greenCards, Green)]

       match = List.filter (\(cards,_) -> List.member card cards) pairs
   in
   case match of
      []              -> Purple
      (_, color) :: _ -> color

--produces the cost of the card
cardCost : CardName -> List Resource
cardCost card = case card of
   TimberYard           -> [Money 1]
   ClayPit              -> [Money 1]
   Excavation           -> [Money 1]
   ForestCave           -> [Money 1]
   TreeFarm             -> [Money 1]
   Mine                 -> [Money 1]
   Baths                -> [Stone]
   Stockade             -> [Wood]
   Barracks             -> [Iron]
   GuardTower           -> [Brick]
   Apothecary           -> [Cloth]
   Workshop             -> [Glass]
   Scriptorium          -> [Paper]
   Sawmill              -> [Money 1]
   Foundary             -> [Money 1]
   Brickyard            -> [Money 1]
   Quarry               -> [Money 1]
   Caravansery          -> [Wood, Wood]
   Forum                -> [Brick, Brick]
   Temple               -> [Wood, Brick, Glass]
   Courthouse           -> [Brick, Brick, Paper]
   Statue               -> [Iron, Iron, Wood]
   Aqueduct             -> [Stone, Stone, Stone]
   Library              -> [Stone, Stone, Cloth]
   Laboratory           -> [Brick, Brick, Paper]
   Stables              -> [Brick, Wood, Iron]
   ArcheryRange         -> [Wood, Wood, Iron]
   Walls                -> [Stone, Stone, Stone]
   TrainingGround       -> [Iron, Iron, Wood]
   Dispensary           -> [Iron, Iron, Glass]
   School               -> [Wood, Paper]
   Gardens              -> [Brick, Brick, Wood]
   Senate               -> [Wood, Wood, Stone, Iron]
   TownHall             -> [Stone, Stone, Iron, Glass]
   Pantheon             -> [Brick, Brick, Iron, Glass, Paper, Cloth]
   Palace               -> [Stone, Iron, Wood, Brick, Glass, Paper, Cloth]
   University           -> [Wood, Wood, Paper, Glass]
   Observatory          -> [Iron, Iron, Glass, Cloth]
   Lodge                -> [Brick, Brick, Paper, Cloth]
   Study                -> [Wood, Paper, Cloth]
   Academy              -> [Stone, Stone, Stone, Glass]
   SiegeWorkshop        -> [Brick, Brick, Brick, Wood]
   Fortification        -> [Iron, Iron, Iron, Stone]
   Arsenal              -> [Wood, Wood, Iron, Cloth]
   Circus               -> [Stone, Stone, Stone, Iron]
   Arena                -> [Stone, Stone, Iron]
   Lighthouse           -> [Stone, Glass]
   Haven                -> [Wood, Iron, Cloth]
   ChamberOfCommerce    -> [Brick, Brick, Paper]
   WorkersGuild         -> [Iron, Iron, Brick, Stone, Wood]
   CraftsmansGuild      -> [Iron, Iron, Stone, Stone]
   ShipOwnersGuild      -> [Wood, Wood, Wood, Glass, Paper]
   TradersGuild         -> [Glass, Paper, Cloth]
   MagistratesGuild     -> [Wood, Wood, Wood, Stone, Cloth]
   BuildersGuild        -> [Stone, Stone, Brick, Brick, Glass]
   PhilosophersGuild    -> [Brick, Brick, Brick, Paper, Cloth]
   ScientistsGuild      -> [Wood, Wood, Iron, Iron, Paper]
   SpiesGuild           -> [Brick, Brick, Brick, Glass]
   StrategistsGuild     -> [Iron, Iron, Stone, Cloth]
   ArchitectsGuild      -> [Iron, Iron, Iron, Brick, Cloth, Paper]
   GamersGuild          -> [Stone, Brick, Wood, Iron]
   _                    -> []


--produces the upgrades available from having the given card
cardUpgrades : CardName -> List CardName
cardUpgrades card = case card of
   Altar                -> [Temple]
   Theatre              -> [Statue]
   Baths                -> [Aqueduct]
   Apothecary           -> [Stables, Dispensary]
   Workshop             -> [ArcheryRange, Laboratory]
   Scriptorium          -> [Courthouse, Library]
   Caravansery          -> [Lighthouse]
   EastTradingPost      -> [Forum]
   WestTradingPost      -> [Forum]
   MarketPlace          -> [Caravansery]
   Forum                -> [Haven]
   Temple               -> [Pantheon]
   Statue               -> [Gardens]
   Library              -> [Senate, University]
   Laboratory           -> [SiegeWorkshop, Observatory]
   Walls                -> [Fortification]
   TrainingGround       -> [Circus]
   Dispensary           -> [Arena, Lodge]
   School               -> [Academy, Study]
   _                    -> []


--determines the resources the card produces
cardProduces : Civ -> Civ -> Civ -> CardName -> Maybe Production
cardProduces playerCiv left right card  = 
   
   let 
       numYellow      = countYellow playerCiv.cards
       numBrown       = countBrown playerCiv.cards
       numGrey        = countGrey playerCiv.cards
       adjNumYellow   = (countYellow left.cards) + (countYellow right.cards)
       adjNumBrown    = (countBrown left.cards) + (countBrown right.cards)
       adjNumGrey     = (countGrey left.cards) + (countGrey right.cards)
   in
   case card of
      LumberYard           -> Just (All [Wood])
      OreVein              -> Just (All [Iron])
      ClayPool             -> Just (All [Brick])
      StonePit             -> Just (All [Stone])
      TimberYard           -> Just (Exclusive [Stone, Wood])
      ClayPit              -> Just (Exclusive [Brick, Iron])
      Excavation           -> Just (Exclusive [Stone, Brick])
      ForestCave           -> Just (Exclusive [Wood, Iron])
      TreeFarm             -> Just (Exclusive [Wood, Brick])
      Mine                 -> Just (Exclusive [Stone, Iron])
      Loom                 -> Just (All [Cloth])
      Glassworks           -> Just (All [Glass])
      Press                -> Just (All [Paper])
      Tavern               -> Just (All [Money 5])
      Sawmill              -> Just (All [Wood, Wood])
      Foundary             -> Just (All [Iron, Iron])
      Brickyard            -> Just (All [Brick, Brick])
      Quarry               -> Just (All [Stone, Stone])
      Caravansery          -> Just (Exclusive [Wood, Stone, Iron, Brick])
      Forum                -> Just (Exclusive [Cloth, Paper, Glass])
      Vineyard             -> Just (All [Money (numBrown + adjNumBrown)])
      Bazaar               -> Just (All [Money (2 * (numGrey + adjNumGrey))])
      _                    -> Nothing


--scores the card
cardScores :  Civ -> Civ -> Civ -> CardName -> Int
cardScores playerCiv left right card = 
   let
      numBrown             = countBrown playerCiv.cards
      numGrey              = countGrey playerCiv.cards
      numPurple            = countPurple playerCiv.cards
   in
   case card of
      Temple               -> 3
      Courthouse           -> 4
      Statue               -> 4
      Aqueduct             -> 5
      Altar                -> 2
      Theatre              -> 2
      Baths                -> 3
      PawnShop             -> 3
      Gardens              -> 5
      Senate               -> 6
      TownHall             -> 6
      Pantheon             -> 7
      Palace               -> 8
      Arena                -> playerCiv.finishedStages
      Lighthouse           -> countYellow playerCiv.cards
      Haven                -> numBrown
      ChamberOfCommerce    -> 2 * (countGrey playerCiv.cards)
      WorkersGuild         -> (countBrown left.cards) + (countBrown right.cards)
      CraftsmansGuild      -> 2 * ((countGrey left.cards) + (countGrey right.cards))
      TradersGuild         -> (countYellow left.cards) + (countYellow right.cards)
      MagistratesGuild     -> (countBlue left.cards) + (countBlue right.cards)
      PhilosophersGuild    -> (countGreen left.cards) + (countGreen right.cards)
      SpiesGuild           -> (countRed left.cards) + (countRed right.cards)
      ArchitectsGuild      -> 3 * ((countPurple left.cards) + (countPurple right.cards))
      GamersGuild          -> playerCiv.money
      ShipOwnersGuild      -> Maybe.withDefault 0 (List.minimum [numBrown, numGrey, numPurple])
      StrategistsGuild     -> (countLosses left) + (countLosses right)
      _                    -> 0
 

--determine the battle strength of a card
cardStrength : CardName -> Int
cardStrength card = case card of
   Stockade                -> 1
   Barracks                -> 1
   GuardTower              -> 1
   Stables                 -> 2
   ArcheryRange            -> 2
   Walls                   -> 2
   TrainingGround          -> 2
   SiegeWorkshop           -> 3
   Fortification           -> 3
   Arsenal                 -> 3
   Circus                  -> 3
   _                       -> 0


--determines which science discoveries the card produces
cardScience : CardName -> Maybe Science
cardScience card = case card of
   Apothecary              -> Just (Single Compass)
   Workshop                -> Just (Single Gear)
   Scriptorium             -> Just (Single Tablet)
   Library                 -> Just (Single Tablet)
   Laboratory              -> Just (Single Gear)
   Dispensary              -> Just (Single Compass)
   School                  -> Just (Single Tablet)
   University              -> Just (Single Tablet)
   Observatory             -> Just (Single Gear)
   Lodge                   -> Just (Single Compass)
   Study                   -> Just (Single Gear)
   Academy                 -> Just (Single Compass)
   ScientistsGuild         -> Just DiscoveryChoice
   _                       -> Nothing


--determines the name (as a string) of the card
cardName : CardName -> String
cardName card = case card of
   LumberYard                 -> "Lumber Yard"
   OreVein                    -> "Ore Vein"
   ClayPool                   -> "Clay Pool"
   StonePit                   -> "Stone Pit"
   TimberYard                 -> "Timber Yard"
   ClayPit                    -> "Clay Pit"
   Excavation                 -> "Excavation"
   ForestCave                 -> "Forest Cave"
   TreeFarm                   -> "Tree Farm"
   Mine                       -> "Mine"
   Loom                       -> "Loom"
   Glassworks                 -> "Glassworks"
   Press                      -> "Press"
   EastTradingPost            -> "East Trading Post"
   WestTradingPost            -> "West Trading Post"
   MarketPlace                -> "Market Place"
   Tavern                     -> "Tavern"
   Altar                      -> "Altar"
   Theatre                    -> "Theatre"
   Baths                      -> "Baths"
   PawnShop                   -> "Pawn Shop"
   Stockade                   -> "Stockade"
   Barracks                   -> "Barracks"
   GuardTower                 -> "Guard Tower"
   Apothecary                 -> "Apothecary"
   Workshop                   -> "Workshop"
   Scriptorium                -> "Scriptorium"
   Sawmill                    -> "Sawmill"
   Foundary                   -> "Foundary"
   Brickyard                  -> "Brickyard"
   Quarry                     -> "Quarry"
   Caravansery                -> "Caravansery"
   Forum                      -> "Forum"
   Vineyard                   -> "Vineyard"
   Bazaar                     -> "Bazzar"
   Temple                     -> "Temple"
   Courthouse                 -> "Courthouse"
   Statue                     -> "Statue"
   Aqueduct                   -> "Aqueduct"
   Library                    -> "Library"
   Laboratory                 -> "Laboratory"
   Stables                    -> "Stables"
   ArcheryRange               -> "Archery Range"
   Walls                      -> "Walls"
   TrainingGround             -> "Training Ground"
   Dispensary                 -> "Dispensary"
   School                     -> "School"
   Gardens                    -> "Gardens"
   Senate                     -> "Senate"
   TownHall                   -> "Town Hall"
   Pantheon                   -> "Pantheon"
   Palace                     -> "Palace"
   University                 -> "University"
   Observatory                -> "Observatory"
   Lodge                      -> "Lodge"
   Study                      -> "Study"
   Academy                    -> "Academy"
   SiegeWorkshop              -> "Siege Workshop"
   Fortification              -> "Fortification"
   Arsenal                    -> "Arsenal"
   Circus                     -> "Circus"
   Arena                      -> "Arena"
   Lighthouse                 -> "Lighthouse"
   Haven                      -> "Haven"
   ChamberOfCommerce          -> "Chamber Of Commerce"
   WorkersGuild               -> "Workers Guild"
   CraftsmansGuild            -> "Craftsmans Guild"
   ShipOwnersGuild            -> "Ship Owners Guild"
   TradersGuild               -> "Traders Guild"
   MagistratesGuild           -> "Magistrates Guild"
   BuildersGuild              -> "Builders Guild"
   PhilosophersGuild          -> "Philosophers Guild"
   ScientistsGuild            -> "Scientists Guild"
   SpiesGuild                 -> "Spies Guild"
   StrategistsGuild           -> "Strategists Guild"
   ArchitectsGuild            -> "Architects Guild"
   GamersGuild                -> "Gamers Guild"


--expands the card counts into a list of cards
expandCardCounts : List (CardName, Int) -> List CardName
expandCardCounts = List.concatMap (\(c, n) -> List.repeat n c)


--list out all the cards in the first era
firstEra : List CardName
firstEra = [LumberYard, OreVein, ClayPool, StonePit, TimberYard, ClayPit,
              Excavation, ForestCave, TreeFarm, Mine, Loom, Glassworks,
              Press, EastTradingPost, WestTradingPost, MarketPlace, Tavern,
              Altar, Theatre, Baths, PawnShop, Stockade, Barracks, GuardTower,
              Apothecary, Workshop, Scriptorium]

--list out all the cards in the second era
secondEra : List CardName
secondEra = [Sawmill, Foundary, Brickyard, Quarry, Caravansery, Forum, 
             Vineyard, Bazaar, Temple, Courthouse, Statue, Aqueduct, Library,
             Laboratory, Stables, ArcheryRange, Walls, TrainingGround, 
             Dispensary, School, Loom, Glassworks, Press]

--list out all the cards in the third ea
thirdEra : List CardName
thirdEra = [Gardens, Senate, TownHall, Pantheon, Palace, University, 
            Observatory, Lodge, Study, Academy, SiegeWorkshop, Fortification, 
            Arsenal, Circus, Arena, Lighthouse, Haven, ChamberOfCommerce,
            WorkersGuild, CraftsmansGuild, ShipOwnersGuild, TradersGuild,
            MagistratesGuild, BuildersGuild, PhilosophersGuild,
            ScientistsGuild, SpiesGuild, StrategistsGuild, ArchitectsGuild]


--colors (types) of cards
--purple cards are not explicitly listed
brownCards = [LumberYard, OreVein, ClayPool, StonePit, TimberYard, ClayPit,
              Excavation, ForestCave, TreeFarm, Mine, Sawmill, Foundary, 
              Brickyard, Quarry]

greyCards   = [Loom, Glassworks, Press]

yellowCards = [EastTradingPost, WestTradingPost, MarketPlace, Tavern, 
               Caravansery, Forum, Vineyard, Bazaar, Arena, Lighthouse, 
               Haven, ChamberOfCommerce]

blueCards   = [Altar, Theatre, Baths, PawnShop, Temple, Courthouse, Statue, 
               Aqueduct, Gardens, Senate, TownHall, Pantheon, Palace]

redCards    = [Stockade, Barracks, GuardTower, Stables, ArcheryRange, Walls, 
               TrainingGround, SiegeWorkshop, Fortification, Arsenal, Circus]

greenCards  = [Apothecary, Workshop, Scriptorium, Library, Laboratory, 
               Dispensary, School, University, Observatory, Lodge, Study, 
               Academy]


--determines the value at a given index
getAt : Int -> List Int -> Int
getAt index = (List.drop index) >> List.head >> (Maybe.withDefault 0)

--returns the set of cards for the 
cardSet : Era -> List CardName
cardSet era =
   case era of
      One   -> firstEra
      Two   -> secondEra
      Three -> thirdEra

--returns the number of cards per era
cardsPerEra : Int -> Era -> CardName -> Int
cardsPerEra numPlayers era card =
   case era of
      One   -> numCardsFstEra numPlayers card
      Two   -> numCardsSndEra numPlayers card
      Three -> numCardsTrdEra numPlayers card

--returns the numbers a type of card that is needed in the first era
--note that the first three spots, indexes 0,1,2, are zero because the game
--must be played with 3 or more players
numCardsFstEra : Int -> CardName -> Int
numCardsFstEra numPlayers card = 
   let
      n = getAt numPlayers
   in
      case card of
         LumberYard                 -> n [0,0,0,1,2,2,2,2]
         OreVein                    -> n [0,0,0,1,2,2,2,2]
         ClayPool                   -> n [0,0,0,1,1,2,2,2]
         StonePit                   -> n [0,0,0,1,1,2,2,2]
         TimberYard                 -> n [0,0,0,1,1,1,1,1]
         ClayPit                    -> n [0,0,0,1,1,1,1,1]
         Excavation                 -> n [0,0,0,0,1,1,1,1]
         ForestCave                 -> n [0,0,0,0,0,1,1,1]
         TreeFarm                   -> n [0,0,0,0,0,0,1,1]
         Mine                       -> n [0,0,0,0,0,0,1,1]
         Loom                       -> n [0,0,0,1,1,1,2,2]
         Glassworks                 -> n [0,0,0,1,1,1,2,2]
         Press                      -> n [0,0,0,1,1,1,2,2]
         EastTradingPost            -> n [0,0,0,1,1,1,1,2]
         WestTradingPost            -> n [0,0,0,1,1,1,1,2]
         MarketPlace                -> n [0,0,0,1,1,1,2,2]
         Tavern                     -> n [0,0,0,0,1,2,2,3]
         Altar                      -> n [0,0,0,1,1,2,2,2]
         Theatre                    -> n [0,0,0,1,1,1,2,2]
         Baths                      -> n [0,0,0,1,1,1,1,2]
         PawnShop                   -> n [0,0,0,0,0,1,1,2]
         Stockade                   -> n [0,0,0,1,1,1,1,2]
         Barracks                   -> n [0,0,0,1,1,2,2,2]
         GuardTower                 -> n [0,0,0,1,2,2,2,2]
         Apothecary                 -> n [0,0,0,1,1,2,2,2]
         Workshop                   -> n [0,0,0,1,1,1,1,2]
         Scriptorium                -> n [0,0,0,1,2,2,2,2]
         _                          -> 0

--determines the number of cards needed in the second era based on the number
--of players
numCardsSndEra : Int -> CardName -> Int
numCardsSndEra numPlayers card = 
   let
      n = getAt numPlayers
   in
      case card of
         Sawmill                    -> n [0,0,0,1,2,2,2,2]
         Foundary                   -> n [0,0,0,1,2,2,2,2]
         Brickyard                  -> n [0,0,0,1,2,2,2,2]
         Quarry                     -> n [0,0,0,1,2,2,2,2]
         Loom                       -> n [0,0,0,1,1,2,2,2]
         Glassworks                 -> n [0,0,0,1,1,2,2,2]
         Press                      -> n [0,0,0,1,1,2,2,2]
         Caravansery                -> n [0,0,0,1,1,2,3,3]
         Forum                      -> n [0,0,0,1,1,1,2,3]
         Vineyard                   -> n [0,0,0,1,1,1,2,2]
         Bazaar                     -> n [0,0,0,0,1,1,1,2]
         Temple                     -> n [0,0,0,1,1,1,2,2]
         Courthouse                 -> n [0,0,0,1,1,2,2,2]
         Statue                     -> n [0,0,0,1,1,1,1,2]
         Aqueduct                   -> n [0,0,0,1,1,1,1,2]
         Library                    -> n [0,0,0,1,1,1,2,2]
         Laboratory                 -> n [0,0,0,1,1,2,2,2]
         Stables                    -> n [0,0,0,1,1,2,2,2]
         ArcheryRange               -> n [0,0,0,1,1,1,2,2]
         Walls                      -> n [0,0,0,1,1,1,1,2]
         TrainingGround             -> n [0,0,0,0,1,1,2,3]
         Dispensary                 -> n [0,0,0,1,2,2,2,2]
         School                     -> n [0,0,0,1,1,1,1,2]
         _                          -> 0


--determines the number of cards needed in the third era based on the number
--of players
numCardsTrdEra : Int -> CardName -> Int
numCardsTrdEra numPlayers card = 

   let
      n = getAt numPlayers
   in
      case card of
         Gardens                    -> n [0,0,0,1,2,2,2,2]
         Senate                     -> n [0,0,0,1,1,2,2,2]
         TownHall                   -> n [0,0,0,1,1,2,3,3]
         Pantheon                   -> n [0,0,0,1,1,1,2,2]
         Palace                     -> n [0,0,0,1,1,1,1,2]
         University                 -> n [0,0,0,1,2,2,2,2]
         Observatory                -> n [0,0,0,1,1,1,1,2]
         Lodge                      -> n [0,0,0,1,1,1,2,2]
         Study                      -> n [0,0,0,1,1,2,2,2]
         Academy                    -> n [0,0,0,1,1,1,1,2]
         SiegeWorkshop              -> n [0,0,0,1,1,2,2,2]
         Fortification              -> n [0,0,0,1,1,1,1,2]
         Arsenal                    -> n [0,0,0,1,2,2,2,3]
         Circus                     -> n [0,0,0,0,1,2,3,3]
         Arena                      -> n [0,0,0,1,1,2,2,3]
         Lighthouse                 -> n [0,0,0,1,1,1,2,2]
         Haven                      -> n [0,0,0,1,2,2,2,2]
         ChamberOfCommerce          -> n [0,0,0,0,1,1,2,2]
         WorkersGuild               -> n [0,0,0,1,1,1,1,1]
         CraftsmansGuild            -> n [0,0,0,1,1,1,1,1]
         ShipOwnersGuild            -> n [0,0,0,1,1,1,1,1]
         TradersGuild               -> n [0,0,0,1,1,1,1,1]
         MagistratesGuild           -> n [0,0,0,1,1,1,1,1]
         BuildersGuild              -> n [0,0,0,1,1,1,1,1]
         PhilosophersGuild          -> n [0,0,0,1,1,1,1,1]
         ScientistsGuild            -> n [0,0,0,1,1,1,1,1]
         SpiesGuild                 -> n [0,0,0,1,1,1,1,1]
         StrategistsGuild           -> n [0,0,0,1,1,1,1,1]
         ArchitectsGuild            -> n [0,0,0,1,1,1,1,1]
         GamersGuild                -> n [0,0,0,1,1,1,1,1]
         _                          -> 0


--determine the number of losses
countLosses : Civ -> Int
countLosses civ = List.length (List.filter (\c -> c == Loss) civ.battles)


--determine the number of cards of a given color
countColor : Color -> List CardName-> Int
countColor color hand =
   List.length (List.filter (\c -> c == color) (List.map cardColor hand))


--specific counting functions
countYellow    = countColor Yellow
countBrown     = countColor Brown
countGrey      = countColor Grey
countBlue      = countColor Blue
countPurple    = countColor Purple
countGreen     = countColor Green
countRed       = countColor Red

--counts a the instances of a given discovery
countDiscovery : List Discovery -> Discovery -> Int
countDiscovery collection target =
   List.length (List.filter (\d -> d == target) collection)
