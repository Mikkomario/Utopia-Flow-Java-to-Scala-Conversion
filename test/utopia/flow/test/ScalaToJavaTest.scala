package utopia.flow.test

import utopia.flow.datastructure.immutable.Value
import utopia.flow.conversion.ConversionDataTypes
import utopia.flow.conversion.ScalaToJava._
import utopia.flow.generic.ValueConversions._
import utopia.java.flow.structure
import utopia.java.flow.structure.range.IntRange

import scala.collection.immutable.{HashMap, HashSet}
import scala.util.{Failure, Success}

/**
 * Used for testing scala to java conversions
 * @author Mikko Hilpinen
 * @since 5.10.2019, v1+
 */
object ScalaToJavaTest extends App
{
	ConversionDataTypes.setup()
	
	val sv1 = Vector(1, 3, 4, 5)
	assert(sv1.toJava == structure.ImmutableList.withValues(1, 3, 4, 5))
	
	val st1 = ("a", 1)
	val st2 = (1, 2)
	assert(st1.toJava == new structure.Pair("a", 1))
	assert(st2.toJavaDuo == new structure.Duo(1, 2))
	
	val so1: Option[Int] = None
	val so2 = Some(2)
	assert(so1.toJava.isEmpty)
	assert(so2.toJava.contains(2))
	
	val sm1 = HashMap("a" -> 1)
	assert(sm1.toJava == structure.ImmutableMap.withValue("a", 1))
	
	val se1: Either[String, Int] = Left("a")
	val se2: Either[String, Int] = Right(1)
	assert(se1.toJava.left().contains("a"))
	assert(se2.toJava.right().contains(1))
	
	val ss1 = HashSet(1, 2, 3, 5, 6)
	assert(ss1.toJava == structure.IntSet.withValues(1, 2, 3, 5, 6))
	
	val str1 = Success(1)
	val str2 = Failure[Int](new NullPointerException("Test"))
	assert(str1.toJava.success().contains(1))
	assert(str2.toJava.isFailure)
	
	val sr1 = 1 to 5
	val sr2 = 1 until 5
	assert(sr1.toJavaInclusive == IntRange.inclusive(1, 5))
	assert(sr2.toJavaExclusive == IntRange.exclusive(1, 5))
	
	val sval1: Value = 1
	val sval2: Value = "a"
	val sval3 = Value.empty
	assert(sval1.toJava.toInteger == 1)
	assert(sval2.toJava.toString == "a")
	assert(sval3.toJava.isEmpty)
	
	println("Success!")
}
