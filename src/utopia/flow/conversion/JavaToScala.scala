package utopia.flow.conversion

import utopia.java.flow.async.{Attempt, Completion}
import utopia.flow.collection.WeakList
import utopia.flow.datastructure.immutable.{Model, Value}
import utopia.flow.datastructure.mutable.{Lazy, PointerLike}
import utopia.flow.conversion.ConversionDataTypes.JavaValueType
import utopia.flow.parse.XmlElement
import utopia.java.flow
import utopia.java.flow.generics.Variable
import utopia.java.flow.structure.range.{ExclusiveRange, InclusiveRange}
import utopia.java.flow.generics
import utopia.java.flow.structure.{ImmutableList, ImmutableMap, IntSet, Mutable, Option, Pair, RichIterable}

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.VectorBuilder
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

import utopia.flow.generic.ValueConversions._

/**
 * Contains extensions that allow conversion between Java and Scala Flow tools
 * @author Mikko Hilpinen
 * @since 2.10.2019, v1.0+
 */
object JavaToScala
{
	implicit class JFlowIterable[A](val i: RichIterable[A]) extends AnyVal
	{
		/**
		 * Collects elements from this iterable into a scala collection
		 * @param cbf A canBuildFrom for the target collection (implicit)
		 * @tparam To Target collection type
		 * @return A new collection with all same instances as in this one
		 */
		def collectToScala[To](implicit cbf: CanBuildFrom[_, A, To]) =
		{
			val builder = cbf()
			i.estimatedSize().forEach(builder.sizeHint(_))
			i.forEach(builder.+=)
			builder.result()
		}
		
		/**
		 * @return Converts this iterable instance to a Vector
		 */
		def toVector: Vector[A] = collectToScala
	}
	
	implicit class JFlowPair[A, B](val p: Pair[A, B]) extends AnyVal
	{
		/**
		 * @return A tuple representation of this pair
		 */
		def toScala = (p.first(), p.second())
	}
	
	implicit class JFlowOption[A](val o: Option[A]) extends AnyVal
	{
		/**
		 * @return A scala option representation of this option
		 */
		def toScala = if (o.isDefined) Some(o.get()) else None
	}
	
	implicit class JFlowList[A](val l: ImmutableList[A]) extends AnyVal
	{
		/**
		 * @return A scala vector based on the contents of this list
		 */
		def toScala =
		{
			val builder = new VectorBuilder[A]()
			builder.sizeHint(l.size())
			l.forEach(builder.+=)
			builder.result()
		}
	}
	
	implicit class JFlowMap[K, V](val m: ImmutableMap[K, V]) extends AnyVal
	{
		/**
		 * @return A scala map with same content as this map
		 */
		def toScala = m.toList.toScala.map { _.toScala }.toMap
	}
	
	implicit class JFlowEither[L, R](val e: flow.structure.Either[L, R]) extends AnyVal
	{
		/**
		 * @return A scala either with same content as this one
		 */
		def toScala: Either[L, R] = e.handleMap(Left(_), Right(_))
	}
	
	implicit class JFlowIntSet(val s: IntSet) extends AnyVal
	{
		/**
		 * @return A set based on the contents of this set
		 */
		def toScala =
		{
			val b = Set.newBuilder[Int]
			b.sizeHint(s.size())
			s.forEach { i => b += i.toInt }
			b.result()
		}
	}
	
	implicit class JFlowLazy[A](val l: flow.structure.Lazy[A]) extends AnyVal
	{
		/**
		 * @return A scala flow version of lazy based on this instance
		 */
		def toScala: Lazy[A] = Lazy { l.get() }
	}
	
	implicit class JFlowMutable[A](val m: Mutable[A]) extends PointerLike[A]
	{
		override def get = m.get()
		
		override def set(newVal: A) = m.set(newVal)
	}
	
	implicit class JFlowTry[A](val t: flow.structure.Try[A]) extends AnyVal
	{
		/**
		 * @return A scala try with same contents as this one
		 */
		def toScala: Try[A] = t.handleMap(Success(_), Failure(_))
	}
	
	implicit class JFlowWeakList[A <: AnyRef](val l: flow.structure.WeakList[A]) extends AnyVal
	{
		/**
		 * @return A scala flow weak list identical to this one
		 */
		def toScala: WeakList[A] = l.collectToScala
	}
	
	implicit class JFlowInclusiveIntRange(val r: InclusiveRange[Integer]) extends AnyVal
	{
		/**
		 * @return A range identical to this one
		 */
		def toScala = r.first().toInt to r.last().toInt
	}
	
	implicit class JFlowExclusiveIntRange(val r: ExclusiveRange[Integer]) extends AnyVal
	{
		/**
		 * @return A range identical to this one
		 */
		def toScala = r.first().toInt until r.end().toInt
	}
	
	implicit class JFlowPromise[A](val p: utopia.java.flow.async.Promise[A]) extends AnyVal
	{
		/**
		 * @param context Execution context (implicit)
		 * @return A future for the contents of this promise
		 */
		def future(implicit context: ExecutionContext): Future[A] = Future { p.waitFor() }
		
		/**
		 * @param context Execution context (implicit)
		 * @return A future for the completion of this promise / process
		 */
		def scalaCompletion(implicit context: ExecutionContext): Future[Unit] = Future { p.waitFor() }
	}
	
	implicit class JFlowAttempt[A](val a: Attempt[A]) extends AnyVal
	{
		/**
		 * @param context Implicit execution context
		 * @return A future based on the results of this attempt
		 */
		def toScala(implicit context: ExecutionContext): Future[Try[A]] = Future { a.waitFor().toScala }
	}
	
	implicit class JFlowCompletion(val c: Completion) extends AnyVal
	{
		/**
		 * @param context Implicit execution context
		 * @return A future for the completion of this process
		 */
		def toScala(implicit context: ExecutionContext) = c.scalaCompletion
	}
	
	implicit class JFlowValue(val v: generics.Value) extends AnyVal
	{
		/**
		 * @return A scala flow version of this value
		 */
		def toScala =
		{
			if (v.isDefined)
				new Value(Some(v), JavaValueType)
			else
				Value.emptyWithType(JavaValueType)
		}
	}
	
	implicit class JFlowModel(val m: generics.Model[Variable]) extends AnyVal
	{
		/**
		 * @return An immutable scala flow model with same content as this model's current state
		 */
		def toScala = Model(m.getAttributes.toScala.map { a => (a.getName, a.getValue.toScala) })
	}
	
	implicit class JFlowXmlElement(val e: utopia.java.flow.parse.XmlElement) extends AnyVal
	{
		/**
		 * @return A scala flow version of this xml element
		 */
		def toScala: XmlElement = new XmlElement(e.getName, e.getValue.toScala, Model.fromMap(e.getAttributes.toScala),
			e.getChildren.toScala.map { _.toScala })
	}
}
