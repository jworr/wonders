package hw.wonders

import hw.wonders.item._

/**
Represents the game state
*/
object state
{
	/**
	The state of the Player
	*/
	class Player(val buildings:Set[Building], val money:Int, val battles:Set[BattleResult])
	{
		/**
		Computes the player's score based 
		*/
		def score:Int =
		{
		}

		/**
		Scores the science buildings
		*/
		def scoreScience:Int =
		{
			val sci = buildings.collect {case s:Science => s}
		}
		
		/**
		Scores the commerce buildings
		*/
		def scoreCommmerce:Int =
		{
			val comm = buildings.collect {case c:Commerce => c}
		}

		/**
		Scores the civics buildsing
		*/
		def scoreCivics(civ:Iterable[Civics]):Int

		/**
		Scores the battles
		*/
		def scoreBattles:Int

		/**
		Scores the money
		*/
		def scoreMoney:Int
	}

	/**
	The State of the game
	*/
	class Game(val players:Seq[Player])
	{
	}
}
