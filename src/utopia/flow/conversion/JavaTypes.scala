package utopia.flow.conversion

import utopia.java.flow.async
import utopia.java.flow.generics
import utopia.java.flow.structure

/**
 * Contains java type aliases
 * @author Mikko Hilpinen
 * @since 16.1.2023
 */
object JavaTypes
{
	type JInt = java.lang.Integer
	type JDouble = java.lang.Double
	
	type JDuration = java.time.Duration
	
	type JLazy[A] = structure.Lazy[A]
	type JPointer[A] = structure.Mutable[A]
	type JOption[A] = structure.Option[A]
	type JTuple[L, R] = structure.Pair[L, R]
	type JPair[A] = structure.Duo[A]
	type JEither[L, R] = structure.Either[L, R]
	type JIterator[A] = structure.iterator.RichIterator[A]
	type JIterable[A] = structure.RichIterable[A]
	type JVector[A] = structure.ImmutableList[A]
	type JMap[K, V] = structure.ImmutableMap[K, V]
	type JTree[A] = structure.TreeNode[A]
	
	type JValue = generics.Value
	type JVariable = generics.Variable
	type JModel = generics.Model[JVariable]
	
	type JTry[A] = structure.Try[A]
	type JPromise[A] = async.Promise[A]
	type JFuture[A] = async.Attempt[A]
	
	type JXmlElement = utopia.java.flow.parse.XmlElement
}
