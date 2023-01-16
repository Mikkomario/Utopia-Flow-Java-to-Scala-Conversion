package utopia.flow.conversion

import utopia.flow.conversion.JavaTypes.JInt

import scala.language.implicitConversions

/**
 * An object used as a mapping function when scala integers need to be casted to java
 * @author Mikko Hilpinen
 * @since 16.1.2023
 */
object ToJavaInt
{
	/**
	 * @param i A scala integer
	 * @return A java integer matching that integer
	 */
	def apply(i: Int): JInt = i
	
	implicit def toFunction(t: ToJavaInt.type): Function[Int, JInt] = t.apply
}
