package utopia.flow.conversion

import utopia.flow.collection.WeakList
import utopia.flow.datastructure.mutable.Lazy
import utopia.java.flow.structure.{Duo, Pair}
import utopia.java.flow.structure
import utopia.java.flow.generics
import utopia.java.flow.parse
import utopia.java.flow.structure.Try.TryFailedException
import utopia.java.flow.structure.range.{ExclusiveIntRange, InclusiveIntRange}
import utopia.flow.async.AsyncExtensions._
import utopia.java.flow.async.Attempt
import utopia.flow.datastructure.immutable.Value
import utopia.flow.datastructure.template.{Model, Property}
import utopia.flow.conversion.ConversionDataTypes.JavaValueType
import utopia.flow.parse.XmlElement

import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
 * Extensions for converting scala object versions into their java representatives
 * @author Mikko Hilpinen
 * @since 4.10.2019, v1+
 */
object ScalaToJava
{
	implicit class SFlowSeq[A](val s: Seq[A]) extends AnyVal
	{
		/**
		 * @return an immutable list with the same elements and order as this sequence
		 */
		def toJava: structure.ImmutableList[A] =
		{
			val b = new structure.ListBuilder[A](s.size)
			s.foreach(b.add)
			b.result()
		}
	}
	
	implicit class SFlowTuple[A, B](val t: (A, B)) extends AnyVal
	{
		/**
		 * @return a pair from this tuple
		 */
		def toJava: structure.Pair[A, B] = new Pair(t._1, t._2)
	}
	
	implicit class SFlowDuo[A](val t: (A, A)) extends AnyVal
	{
		/**
		 * @return a new duo from this tuple
		 */
		def toJavaDuo: structure.Duo[A] = new Duo(t._1, t._2)
	}
	
	implicit class SFlowOption[A](val o: Option[A]) extends AnyVal
	{
		/**
		 * @return A java flow option from this option's contents
		 */
		def toJava: structure.Option[A] = o match
		{
			case Some(v) => structure.Option.some(v)
			case None => structure.Option.none()
		}
	}
	
	implicit class SFlowMap[K, V](val m: Map[K, V]) extends AnyVal
	{
		/**
		 * @return A new immutable map based on this map's contents
		 */
		def toJava: structure.ImmutableMap[K, V] =
		{
			val b = new structure.MapBuilder[K, V](m.size)
			m.foreach { case (k, v) => b.put(k, v) }
			b.result()
		}
	}
	
	implicit class SFlowEither[L, R](val e: Either[L, R]) extends AnyVal
	{
		/**
		 * @return A java flow either with content from this either
		 */
		def toJava: structure.Either[L, R] = e match
		{
			case Left(l) => structure.Either.left(l)
			case Right(r) => structure.Either.right(r)
		}
	}
	
	implicit class SFlowIntSet(val s: Set[Int]) extends AnyVal
	{
		/**
		 * @return An int set with same content as this one
		 */
		def toJava =
		{
			val b = new structure.IntSetBuilder(s.size)
			s.foreach { i => b.add(i) }
			b.result()
		}
	}
	
	implicit class SFlowLazy[A](val l: Lazy[A]) extends AnyVal
	{
		/**
		 * @return A java flow lazy based on this lazy instance
		 */
		def toJava: structure.Lazy[A] = new structure.Lazy[A]({() => l.get})
	}
	
	implicit class SFlowTry[A](val t: Try[A]) extends AnyVal
	{
		/**
		 * @return A java flow try with content equal to this one
		 */
		def toJava: structure.Try[A] = t match
		{
			case Success(v) => structure.Try.success(v)
			case Failure(e) => e match
			{
				case e1: Exception => structure.Try.failure(e1)
				case t: Throwable => structure.Try.failure(
					new TryFailedException("Converted try with non-exception cause", t))
			}
		}
	}
	
	implicit class SFlowWeakList[A <: AnyRef](val w: WeakList[A]) extends AnyVal
	{
		/**
		 * @return A java flow weak list identical to this one
		 */
		def toJava: structure.WeakList[A] =
		{
			val b = new structure.WeakListBuilder[A](w.size)
			w.foreach(b.add)
			b.result()
		}
	}
	
	implicit class SFlowInclusiveRange(val r: Range.Inclusive) extends AnyVal
	{
		/**
		 * @return An inclusive integer range identical to this one
		 */
		def toJavaInclusive = new InclusiveIntRange(r.start, r.last)
	}
	
	implicit class SFlowRange(val r: Range) extends AnyVal
	{
		/**
		 * @return An exclusive integer range identical to this one
		 */
		def toJavaExclusive = new ExclusiveIntRange(r.start, r.end)
	}
	
	implicit class SFlowFuture[A](val f: Future[A]) extends AnyVal
	{
		/**
		 * @return An attempt based on the results of this future
		 */
		def toJava: Attempt[A] = Attempt.tryAsynchronous { () => structure.Try.run({ () => f.waitFor().get }) }
	}
	
	implicit class SFlowValue(val v: Value) extends AnyVal
	{
		/**
		 * @return A java flow value with content from this value
		 */
		def toJava = v.withType(JavaValueType).content.map
		{
			case v2: generics.Value => v2
			case _ => generics.Value.EMPTY
			
		}.getOrElse(generics.Value.EMPTY)
	}
	
	implicit class SFlowModel(val m: Model[Property]) extends AnyVal
	{
		/**
		 * @return A mutable java flow model with content equal to this model's current properties
		 */
		def toJava: generics.Model[generics.Variable] =
		{
			val result = generics.Model.createBasicModel()
			m.attributes.foreach { a => result.addAttribute(a.name, a.value.toJava, false) }
			result
		}
	}
	
	implicit class SFlowXmlElement(val e: XmlElement) extends AnyVal
	{
		/**
		 * @return A java flow xml element based on this element's contents
		 */
		def toJava: parse.XmlElement = new parse.XmlElement(e.name, e.value.toJava.toStringOption,
			e.children.map { _.toJava }.toJava, e.attributes.toMap { _.string }.toJava)
	}
}
