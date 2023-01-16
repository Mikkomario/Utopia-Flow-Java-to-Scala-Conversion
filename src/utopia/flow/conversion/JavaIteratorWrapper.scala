package utopia.flow.conversion

/**
 * A wrapper class for a java iterator
 * @author Mikko Hilpinen
 * @since 16.1.2023
 */
class JavaIteratorWrapper[A](source: java.util.Iterator[A]) extends Iterator[A]
{
	override def hasNext: Boolean = source.hasNext
	override def next(): A = source.next()
}
