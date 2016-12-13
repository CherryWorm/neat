package neat

case class Species(repr: Individual, individuals: List[Individual]) {
	lazy val totalFitness = individuals map (_.fitness) sum
	lazy val avgFitness = totalFitness / individuals.length
	
	def dist(o: Individual)(implicit params: Parameters): Int = repr dist o
	def accepts(o: Individual)(implicit params: Parameters): Boolean = dist(o) < params.compatibilityThreshold
	def add(o: Individual): Species = Species(repr, o :: individuals)
	
	def breed(offspring: Int): List[Individual] = ???
}
