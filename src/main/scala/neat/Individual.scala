package neat

import util._

import scala.annotation.tailrec

case class ConnectionGene(innovation: Int, in: NodeGene, out: NodeGene, enabled: Boolean, weight: Double)

case class NodeGene(id: Int)

case class Individual(conn: List[ConnectionGene], nodes: List[NodeGene])(implicit params: Parameters, experiment: Experiment) {
	
	def this(conn: List[ConnectionGene])(implicit params: Parameters, experiment: Experiment) = {
		this(conn, conn.flatMap(i => List(i.in, i.out)).distinct)
	}
	
	lazy val fitness: Double = experiment evaluate this
	lazy val sorted: List[ConnectionGene] = conn.sortBy(_.innovation)
	
	def constructNetwork(): NeuralNetwork = {
		NeuralNetwork(this)
	}
	
	def flipEdge(n: Int): List[ConnectionGene] = {
		conn find(_.innovation == n) map (i => i.copy(enabled = !i.enabled) :: (conn filter (_.innovation != n))) getOrElse conn
	}
	
	def mutate(innovationTracker: InnovationTracker): (Individual, InnovationTracker) = {
		
		val insertNodes: (Individual, InnovationTracker) => (Individual, InnovationTracker) = { case (i, tracker) =>
			def findEdge(n: Int): Option[ConnectionGene] = n match {
				case 0 => None
				case n =>
					val edge = rand(i.conn)
					if(edge.enabled)
						Some(edge)
					else
						None
			}
			val edge = findEdge(20)
			edge map { e =>
				val (tracker2, nodeInnovation) = tracker.insertOrGetNode(e)
				val node = NodeGene(nodeInnovation)
				val (tracker3, firstInnovation) = tracker2.insertOrGetEdge(e.in, node)
				val (tracker4, secondInnovation) = tracker3.insertOrGetEdge(node, e.out)
				(new Individual(ConnectionGene(firstInnovation, e.in, node, true, e.weight)
					:: ConnectionGene(secondInnovation, node, e.out, true, e.weight)
					:: i.flipEdge(e.innovation)), tracker4)
			} getOrElse(i, tracker)
		}
		val insertEdges: (Individual, InnovationTracker) => (Individual, InnovationTracker) = { case (i, tracker) =>
			val from = rand(i.nodes)
			val to = rand(i.nodes)
			val (newTracker, inno) = tracker.insertOrGetEdge(from, to)
			(new Individual(ConnectionGene(inno, from, to, true, params.random.nextDouble() * 2 - 1) :: i.conn), newTracker)
		}
		val toggleEdges: (Individual, InnovationTracker) => (Individual, InnovationTracker) = { case (i, tracker) =>
			(Individual(i.flipEdge(rand(conn).innovation), i.nodes), tracker)
		}
		val mutateEdges: (Individual, InnovationTracker) => (Individual, InnovationTracker) = { case (i, tracker) =>
			val severe = p(0.5)
			val newConn = i.conn map { gene =>
				def clamp(c: Double) = c.min(8).max(-8)
				val (gausspoint, coldGausspoint) =
					if (severe) (0.3, 0.1)
					else if(p(0.5)) (0.0, -0.1)
					else (0.0, 0.0)
				val rand = params.random.nextDouble() * params.weightMutatePower - 2 * params.weightMutatePower
				val randchoice = params.random.nextDouble()
				if(randchoice > gausspoint)
					gene.copy(weight = clamp(gene.weight + rand))
				else if(randchoice > coldGausspoint)
					gene.copy(weight = clamp(rand))
				else
					gene
			}
			(Individual(newConn, i.nodes), tracker)
		}
		
		val combined = maybeP(params.mutateAddNodeProb, insertNodes.tupled) compose
			maybeP(params.mutateAddLinkProb, insertEdges.tupled) compose
			maybeP(params.mutateToggleProb, toggleEdges.tupled) compose
			maybeP(params.mutateLinkWeightsProb, mutateEdges.tupled)
		
		combined (this, innovationTracker)
	}
	
	def breed(o: Individual): Individual = {
		val fitter = fitness > o.fitness
		def h(i: List[ConnectionGene], j: List[ConnectionGene]): List[ConnectionGene] = (i, j) match {
			case (i, Nil) => if(fitter) i else Nil
			case (Nil, j) => if(fitter) Nil else j
			case ((ihead :: itail), (jhead :: jtail)) =>
				if(ihead.innovation < jhead.innovation)
					if(fitter)
						ihead :: h(itail, j)
					else
						h(itail, j)
				else if(ihead.innovation > jhead.innovation)
					if(fitter)
						h(i, jtail)
					else
						jhead :: h(i, jtail)
				else
					if(p(if(fitter) 0.7 else 0.3))
						ihead :: h(itail, jtail)
					else
						jhead :: h(itail, jtail)
		}
		val genome = h(sorted, o.sorted)
		val nodes = genome.foldRight(List[NodeGene]())((a, b) => a.in :: a.out :: b)
		Individual(h(sorted, o.sorted), nodes.distinct)
	}
	
	def dist(o: Individual): Double = {
		@tailrec
		def h(i: List[ConnectionGene], j: List[ConnectionGene], acc: (Int, Int, Double)): (Int, Int, Double) = (i, j, acc) match {
			case (Nil, rest, (n, m, o)) => (n + rest.length, m, o)
			case (rest, Nil, (n, m, o)) => (n + rest.length, m, o)
			case (ihead :: itail, jhead :: jtail, (n, m, o)) =>
				if (ihead.innovation < jhead.innovation)
					h(itail, j, (n + 1, m, o))
				else if (ihead.innovation > jhead.innovation)
					h(i, jtail, (n + 1, m, o))
				else
					h(itail, jtail, (n, m + 1, o + (ihead.weight - jhead.weight).abs))
		}
		
		val (disjoint, matching, wd) = h(sorted, o.sorted, (0, 0, 0))
		(disjoint * 1.0 / conn.length.max(o.conn.length)) * params.disjointCoefficient +
			(if (matching != 0) wd / matching else 0) * params.weightDifferenceCoefficient
	}
	
}

object Individual {
	def apply()(implicit params: Parameters, experiment: Experiment): Individual = {
		val combs = for(i <- experiment.sensorRange; o <- experiment.outputRange) yield (i, o)
		val genome = combs.zipWithIndex.map {
			case ((i, o), n) => ConnectionGene(n, NodeGene(i), NodeGene(o), true, params.random.nextDouble())
		}
		val nodes = experiment.sensorRange.map(NodeGene) ++ experiment.outputRange.map(NodeGene)
		Individual(genome.toList, nodes.toList)
	}
}