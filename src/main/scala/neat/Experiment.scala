package neat

trait Experiment {
	def evaluate(individual: Individual): Double
	val inputNodes: Int
	val outputNodes: Int
	val biasNode: Boolean
	private val biasWidth = if(biasNode) 1 else 0
	val randNode: Boolean
	private val randWidth = if(randNode) 1 else 0
	
	lazy val inputRange = 0 until inputNodes
	lazy val biasIndex = inputNodes + biasWidth
	lazy val randIndex = biasIndex + randWidth
	lazy val outputRange = randIndex until (randIndex + outputNodes)
	lazy val sensorRange = 0 until randIndex
}
