package neat

import scala.util.Random

case class Parameters(compatibilityThreshold: Double = 6.0,
                      speciesTarget: Int = 10,
                      compatibilityModifier: Double = 0.3,
	                  disjointCoefficient: Double = 2.0,
	                  weightDifferenceCoefficient: Double = 2.0,
	                  survivalThreshold: Double = 0.2,
	                  interspeciesMateRate: Double = 0.05,
	                  mutateAddNodeProb: Double = 0.0025,
	                  mutateAddLinkProb: Double = 0.1,
					  mutateLinkWeightsProb: Double = 0.9,
					  mutateToggleProb: Double = 0.02,
					  weightMutatePower: Double = 2.5,
	                  random: Random = new Random()
                      ) {
	
	def changeCompatiblityModifier(nSpecies: Int): Parameters = {
		val change =
			if (nSpecies < speciesTarget)
				-compatibilityModifier
			else if (nSpecies > speciesTarget)
				compatibilityModifier
			else
				0
		copy(compatibilityThreshold = (compatibilityThreshold + change).max(compatibilityModifier))
	}
}