package utopia.flow.conversion

import utopia.flow.generic.casting.ConversionHandler
import utopia.flow.generic.model.mutable.DataType
import utopia.java.flow.generics.Value

/**
 * Contains data types used in conversions between java and scala
 * @author Mikko Hilpinen
 * @since 2.10.2019, v1+
 */
object ConversionDataTypes
{
	/**
	 * Represents a java-style value in a scala value environment
	 */
	object JavaValueType extends DataType
	{
		override def name = "Value"
		override def supportedClass = classOf[Value]
		override def superType = None
	}
	
	/**
	 * Sets up all required data types
	 */
	def setup() = ConversionHandler.addCaster(JavaToScalaValueCaster)
}
