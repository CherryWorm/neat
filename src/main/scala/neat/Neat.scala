package neat

case class Neat(population: List[Individual])(implicit params: Parameters, experiment: Experiment) {
	
	def step(): Neat = {
		val species = speciate(population)
		implicit val newParams = params changeCompatiblityModifier species.length
		val offspring = breed(species)
		val mutated = offspring map (_.mutate())
		Neat(mutated)
	}
	
	private def breed(species: List[Species]): List[Individual] = {
		val totalFitness = species map (_.totalFitness) sum
		val avgFitness = totalFitness / population.length
		
		val pop = species flatMap (s => s.breed((s.avgFitness / avgFitness * population.length).toInt))
		
		val bestSpecies = species maxBy (_.avgFitness)
		
		if(pop.length > population.length)
			pop.drop(pop.length - population.length)
		else if(pop.length < population.length)
			(bestSpecies breed (population.length - pop.length)) ++ pop
		else
			pop
	}
	
	private def speciate(population: List[Individual]): List[Species]  = {
		population.foldRight(List[Species]()) { case (i, s) =>
			def insert(i: Individual, s: List[Species]): Option[List[Species]] = s match {
				case Nil => None
				case (head :: tail) =>
					if (head accepts i)
						Some(head.add(i) :: tail)
					else
						insert(i, tail) map (head :: _)
			}
			insert(i, s) getOrElse (Species(i, List(i)) :: s)
		}
	}
	
	
}
