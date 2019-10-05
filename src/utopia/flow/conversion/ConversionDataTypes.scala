package utopia.flow.conversion

import utopia.flow.generic.{ConversionHandler, DataType}
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
	object JavaValueType extends DataType("Value", classOf[Value])
	
	/**
	 * Sets up all required data types
	 */
	def setup() =
	{
		DataType.setup()
		DataType.introduceTypes(JavaValueType)
		ConversionHandler.addCaster(JavaToScalaValueCaster)
	}
}
