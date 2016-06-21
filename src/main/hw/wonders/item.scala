package hw.wonders

import hw.wonders.rule.{ScoringRule, MoneyRule}

/**
The library of Seven Wonders game items
*/
object item 
{
	/**
	Resources, the general requirements for buildings
	*/
	type Resource = String
	val BasicResources:Set[Resource] = Set("Wood", "Stone", "Iron", "Brick")
	val AdvResources:Set[Resource] = Set("Paper", "Cloth", "Glass")
	val Resources:Set[Resource] = BasicResources ++ AdvResources

	/**
	Types of scientific knowledge
	*/
	type Knowledge = String
	val KnowledgeTypes:Set[Knowledge] = Set("Gears", "Tablet", "Ruler")

	/**
	The results of a battle
	*/
	case class BattleResult(val score:Int)
	{
		def victory:Boolean = score > 0
	}


	type Bag[T] = Map[T:Int]

	/**
	A standard card in the game
	name - the name of the building
	resourceCost - the resources required the build the building
	parent - the prerequisit buildings in the tech tree
	children - the builds that can be built for free after this one is built
	*/
	class Building(val name:String, val resourceCost:Bag[Resource], val parent:Set[String], val children:Set[String])
	{
	}

	class Production(name:String, resourceCost:Bag[Resource], parent:Set[String], children:Set[String], val output:Set[Resource]) 
		extends Building(name, resourceCost, parent, children)

	class Science(name:String, resourceCost:Bag[Resource], parent:Set[String], children:Set[String], val output:Knowledge) 
		extends Building(name, resourceCost, parent, children)

	class Commerce(name:String, resourceCost:Bag[Resource], parent:Set[String], children:Set[String], val points:Option[ScoringRule], val money:Option[MoneyRule])
		extends Building(name, resourceCost, parent, children)

	class Civics(name:String, resourceCost:Bag[Resource], parent:Set[String], children:Set[String], val points:Int) 
		extends Building(name, resourceCost, parent, children)

	class Military(name:String, resourceCost:Bag[Resource], parent:Set[String], children:Set[String], val war:Int) 
		extends Building(name, resourceCost, parent, children)
}
