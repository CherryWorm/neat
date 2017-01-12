package neat

import scala.collection.immutable.HashMap

case class InnovationTracker(currentEdgeID: Int = 0, currentNodeID: Int = 0, edges: HashMap[(Int, Int), Int] = HashMap(), nodes: HashMap[Int, Int] = HashMap()) {
	def clear = InnovationTracker(currentEdgeID, currentNodeID)
	
	def insertOrGetEdge(from: NodeGene, to: NodeGene): (InnovationTracker, Int) = {
		val o = edges.get((from.id, to.id))
		o.map((this, _)).getOrElse(copy(currentEdgeID = currentEdgeID + 1,
			edges = edges.updated((from.id, to.id), currentEdgeID)), currentEdgeID)
	}
	
	def insertOrGetNode(split: ConnectionGene): (InnovationTracker, Int) = {
		val o = nodes.get(split.innovation)
		o.map((this, _)).getOrElse(copy(currentNodeID = currentNodeID + 1,
			nodes = nodes.updated(split.innovation, currentNodeID)), currentNodeID)
	}
}
