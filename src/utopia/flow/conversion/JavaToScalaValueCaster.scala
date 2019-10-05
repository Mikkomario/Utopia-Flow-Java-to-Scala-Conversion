package utopia.flow.conversion

import java.time.{LocalDate, LocalDateTime, LocalTime}

import utopia.flow.datastructure.immutable.Value
import utopia.flow.generic.ConversionReliability.{DANGEROUS, PERFECT}
import utopia.flow.generic.{BooleanType, Conversion, DataType, DoubleType, FloatType, IntType, LocalDateTimeType, LocalDateType, LocalTimeType, LongType, StringType, ValueCaster}
import utopia.flow.conversion.ConversionDataTypes.JavaValueType
import utopia.flow.conversion.JavaToScala._
import utopia.java.flow.generics

import scala.collection.immutable.HashSet

/**
 * Casts java values to scala values
 * @author Mikko Hilpinen
 * @since 2.10.2019, v1+
 */
object JavaToScalaValueCaster extends ValueCaster
{
	private type JavaValue = generics.Value
	
	// Converts basic types both ways (java to scala is unsure because underlying java type is unknown)
	// TODO: Add vector support
	override val conversions = HashSet(StringType, IntType, LongType, FloatType, DoubleType, BooleanType,
		LocalTimeType, LocalDateType, LocalDateTimeType).flatMap {
		t => Vector(Conversion(JavaValueType, t, DANGEROUS), Conversion(t, JavaValueType, PERFECT)) }
	
	override def cast(value: Value, toType: DataType) =
	{
		val result =
		{
			if (toType == JavaValueType)
				javaValueOf(value)
			else
			{
				val jValue = value.content.map {
					case v: JavaValue => v
					case _ => generics.Value.EMPTY
				}.getOrElse(generics.Value.EMPTY)
				
				toType match
				{
					case StringType => valToString(jValue)
					case IntType => valToInt(jValue)
					case LongType => valToLong(jValue)
					case FloatType => valToFloat(jValue)
					case DoubleType => valToDouble(jValue)
					case BooleanType => valToBoolean(jValue)
					case LocalTimeType => valToTime(jValue)
					case LocalDateType => valToDate(jValue)
					case LocalDateTimeType => valToDateTime(jValue)
					case _ => None
				}
			}
		}
		
		result.map { objValue => new Value(Some(objValue), toType) }
	}
	
	// TODO: Add conversion from vector to value
	private def javaValueOf(v: Value): Option[JavaValue] =
	{
		v.dataType match
		{
			case StringType => Some(generics.Value.of(v.getString))
			case IntType => Some(generics.Value.of(v.getInt: java.lang.Integer))
			case LongType => Some(generics.Value.of(v.getLong: java.lang.Long))
			case FloatType => Some(generics.Value.of(v.getFloat: java.lang.Float))
			case DoubleType => Some(generics.Value.of(v.getDouble: java.lang.Double))
			case BooleanType => Some(generics.Value.of(v.getBoolean))
			case LocalTimeType => Some(generics.Value.of(v.getLocalTime))
			case LocalDateType => Some(generics.Value.of(v.getLocalDate))
			case LocalDateTimeType => Some(generics.Value.of(v.getLocalDateTime))
			case _ => None
		}
	}
	
	private def valToString(v: JavaValue): Option[String] = v.toStringOption.toScala
	
	private def valToInt(v: JavaValue): Option[Int] = v.toIntegerOption.toScala.map { i => i }
	
	private def valToLong(v: JavaValue): Option[Long] = v.toLongOption.toScala.map { l => l }
	
	private def valToFloat(v: JavaValue): Option[Float] = v.toFloatOption.toScala.map { f => f }
	
	private def valToDouble(v: JavaValue): Option[Double] = v.toDoubleOption.toScala.map { d => d }
	
	private def valToBoolean(v: JavaValue): Option[Boolean] = v.toBooleanOption.toScala.map { b => b }
	
	private def valToTime(v: JavaValue): Option[LocalTime] = v.toLocalTimeOption.toScala
	
	private def valToDate(v: JavaValue): Option[LocalDate] = v.toLocalDateOption.toScala
	
	private def valToDateTime(v: JavaValue): Option[LocalDateTime] = v.toLocalDateTimeOption.toScala
}
