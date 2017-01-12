package neat

import scala.collection.mutable

case class Node(edges: List[ConnectionGene], var activation: Double = 0.0, var newActivation: Double = 0.0)

/*
  * Pöse Mutierbarkeit und gruslige Nebeneffekte!!
  */
case class NeuralNetwork(ind: Individual)(implicit params: Parameters, experiment: Experiment) {
	
	private val nodesMap = mutable.HashMap[Int, Node]()
	
	private val nodes = {
		val adj = mutable.HashMap[Int, List[ConnectionGene]]()
		for(e <- ind.conn) {
			val list = adj.getOrElseUpdate(e.in.id, List())
			adj.update(e.in.id, e :: list)
		}
		
		for (n <- ind.nodes) yield {
			val node = Node(adj.getOrElse(n.id, List()))
			nodesMap.update(n.id, node)
			node
		}
	}
	
	def step(in: mutable.Buffer[Double]): mutable.Buffer[Double] = {
		if(experiment.biasNode) {
			nodesMap.get(experiment.biasIndex).map(_.activation = 1)
		}
		if(experiment.randNode) {
			nodesMap.get(experiment.randIndex).map(_.activation = params.random.nextDouble())
		}
		for(i <- experiment.inputRange) {
			nodesMap.get(i).map(_.activation = in(i))
		}
		for(node <- nodes) {
			for(edge <- node.edges) {
				nodesMap(edge.out).newActivation += node.activation * edge.weight
			}
		}
		for(node <- nodes) {
			node.activation = node.newActivation
		}
		val out = mutable.Buffer[Double]()
		for(i <- experiment.outputRange) {
			out += nodesMap(i).activation
		}
		out
	}
}
