package neat

case class ConnectionGene(in: Int, out: Int, enabled: Boolean, weight: Double)
case class NodeGene(id: Int)

case class Individual(conn: List[ConnectionGene], nodes: List[NodeGene])(implicit experiment: Experiment) {
	
	lazy val fitness = experiment evaluate this
	
	def mutate()(implicit params: Parameters): Individual = ???
	def pair(o: Individual)(implicit params: Parameters): Individual = ???
	def dist(o: Individual)(implicit params: Parameters): Int = ???
	
}
