package neat

case class Neat(population: List[Individual], inno: InnovationTracker, species: List[Species])(implicit params: Parameters, experiment: Experiment) {
	
	lazy val best = population.maxBy(_.fitness)
	lazy val avgFitness = population.map(_.fitness).sum / population.length
	lazy val avgDistance = population.map(i => population.map(_.dist(i)).sum / population.length).sum / population.length
	lazy val avgSpeciesAge = species.map(_.age).sum * 1.0 / species.length
	
	def step(): Neat = {
		val newSpecies = speciate(population, species.map(_.empty)).filterNot(_.individuals.isEmpty).filter(_.age < 15)
		implicit val newParams = params changeCompatiblityModifier newSpecies.length
		val offspring = breed(newSpecies)
		val mutated = mutate(offspring, inno)(newParams)
		Neat(mutated, inno, newSpecies)(newParams, experiment)
	}
	
	private def mutate(individuals: List[Individual], inno: InnovationTracker)(implicit params: Parameters): List[Individual] = individuals match {
		case Nil => Nil
		case (x :: xs) =>
			val (mutated, newInno) = x mutate inno
			mutated :: mutate(xs, newInno)
	}
	
	private def breed(species: List[Species]): List[Individual] = {
		val totalFitness = species map (_.totalFitness) sum
		val avgFitness = totalFitness / population.length
		
		val pop = species flatMap (s => s.breed((s.avgFitness / avgFitness * population.length).toInt, true))
		
		val bestSpecies = species maxBy (_.avgFitness)
		
		if (pop.length > population.length)
			pop.drop(pop.length - population.length)
		else if (pop.length < population.length)
			bestSpecies.breed(population.length - pop.length, false) ++ pop
		else
			pop
	}
	
	private def speciate(population: List[Individual], species: List[Species]): List[Species] = {
		population.foldRight(species) { case (i, s) =>
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

object Neat {
	def apply(size: Int)(implicit parameters: Parameters, experiment: Experiment): Neat = Neat(randomPopulation(size), InnovationTracker(), List())
	private def randomPopulation(size: Int)(implicit parameters: Parameters, experiment: Experiment): List[Individual] = {
		if(size <= 0) Nil
		else Individual() :: randomPopulation(size - 1)
	}
}
