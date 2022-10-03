package utopia.flow.test

import utopia.flow.conversion.ConversionDataTypes
import utopia.flow.conversion.JavaToScala._
import utopia.flow.collection.CollectionExtensions._
import utopia.java.flow.structure
import utopia.java.flow.generics
import utopia.java.flow.structure.range.IntRange

import scala.collection.immutable.{HashMap, HashSet}

/**
 * Tests java to scala conversions
 * @author Mikko Hilpinen
 * @since 5.10.2019, v1+
 */
object JavaToScalaTest extends App
{
	ConversionDataTypes.setup()
	
	val jl1 = structure.ImmutableList.withValues(1, 2, 3)
	assert(jl1.toScala == Vector(1, 2, 3))
	
	val jp1 = new structure.Pair("a", 1)
	assert(jp1.toScala == ("a", 1))
	
	val jo1 = structure.Option.none[Int]()
	val jo2 = structure.Option.some(5)
	assert(jo1.toScala.isEmpty)
	assert(jo2.toScala.contains(5))
	
	val jm1 = structure.ImmutableMap.withValue("a", 1)
	assert(jm1.toScala == HashMap("a" -> 1))
	
	val je1 = structure.Either.left[String, Int]("a")
	val je2 = structure.Either.right[String, Int](1)
	assert(je1.toScala.leftOption.contains("a"))
	assert(je2.toScala.rightOption.contains(1))
	
	val js1 = structure.IntSet.withValues(1, 2, 3, 5, 6)
	assert(js1.toScala == HashSet(1, 2, 3, 5, 6))
	
	val jt1 = structure.Try.success(1)
	val jt2 = structure.Try.failure[Int](new NullPointerException("Test"))
	assert(jt1.toScala.toOption.contains(1))
	assert(jt2.toScala.isFailure)
	
	val jr1 = IntRange.inclusive(1, 5)
	val jr2 = IntRange.exclusive(1, 5)
	assert(jr1.toScala == (1 to 5))
	assert(jr2.toScala == (1 until 5))
	
	val jv1 = generics.Value.of("a")
	val jv2 = generics.Value.of(1: java.lang.Integer)
	val jv3 = generics.Value.EMPTY
	assert(jv1.toScala.getString == "a")
	assert(jv2.toScala.getInt == 1)
	assert(jv2.toScala.getString == "1")
	assert(jv3.toScala.isEmpty)
	
	println("Success!")
}
