package neat

case class Parameters(
                     compatibilityThreshold: Double,
                     speciesTarget: Int,
                     compatibilityModifier: Double
                     ) {
	def changeCompatiblityModifier(nSpecies: Int): Parameters = {
		val change =
			if(nSpecies < speciesTarget)
				compatibilityThreshold
			else if(nSpecies > speciesTarget)
				-compatibilityThreshold
			else
				0
		copy(compatibilityThreshold = compatibilityThreshold + change)
	}
}