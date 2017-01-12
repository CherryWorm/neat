package neat

import scala.util.Random

case class Species(repr: Individual, individuals: List[Individual], best: Double = 0, age: Int = 0)(implicit params: Parameters) {
	lazy val totalFitness: Double = individuals map (_.fitness) sum
	lazy val avgFitness: Double = totalFitness / individuals.length
	lazy val sorted = individuals.sortBy(_.fitness)
	lazy val maxFitness = individuals.map(_.fitness).max
	
	def dist(o: Individual)(implicit params: Parameters): Double = repr dist o
	
	def accepts(o: Individual)(implicit params: Parameters): Boolean = dist(o) < params.compatibilityThreshold
	
	def add(o: Individual): Species = Species(repr, o :: individuals)
	lazy val empty: Species = Species(repr, List(), maxFitness, if(maxFitness > best) 0 else age + 1)
	
	def breed(offspring: Int, keepBest: Boolean): List[Individual] = {
		if(offspring <= 0)
			Nil
		else if (keepBest) {
			if (individuals.length > 5)
				individuals.maxBy(_.fitness) :: breed(offspring, false)
			else
				breed(offspring, false)
		}
		else {
			val mom = selectRandom(sorted.drop((sorted.length * 0.8).toInt), params.random)
			val dad = selectRandom(sorted.drop((sorted.length * 0.8).toInt), params.random)
			mom.breed(dad) :: breed(offspring - 1, false)
		}
	}
	
	private def selectRandom[E](l: List[E], rand: Random) = l(rand.nextInt(l.length))
}
