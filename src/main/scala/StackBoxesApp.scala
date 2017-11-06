package com.ngs.stackboxes




object StackBoxesApp extends App {

  object StackBoxes {
    import scala.collection.mutable.ListBuffer

    case class UBox( id: Long, length: Int, width: Int, height: Int )

    object UBox {
      private[UBox] val rnd = scala.util.Random

      def apply( l:Int, w:Int, h:Int ): UBox = { UBox( Uid.getNextId(), l, w, h ) }
      def apply( ): UBox = { UBox( Uid.getNextId(), rnd.nextInt( 15 ) +1, rnd.nextInt( 15 ) +1, rnd.nextInt( 15 ) +1 ) }

      private[UBox] object Uid {
        private[Uid] var uid = 1
        def getNextId(): Int = { val tmp = uid; uid += 1; tmp }
        def apply(): Int = getNextId()
      }
      def randomBoxes( n: Int): Seq[UBox] = ( 1 to n ).map( _ => UBox() )
    }

    sealed trait BTree[+T]

    case class BTreeNode[+T]( value: T, next: Seq[ BTree[T]] ) extends BTree[T] {
      override def toString = {
        def elems = {
          next.map{ n => n match {
            case nd: BTreeNode[UBox] => s"( ${nd.value.id}, ${nd.value.length}, ${nd.value.width}, ${nd.value.height} )"
            case _ => ""
          } }
        }
        s"( ${value.toString}, ${elems} )"}
    }

    object BTreeNode {
      def apply[T](value: T): BTreeNode[T] = BTreeNode( value, Seq.empty[ BTree[T] ] )
    }

    def apply()= {

      val b1 = UBox( 5, 5, 5 )
      val b2 = UBox( 4, 4, 4 )
      val b3 = UBox( 3, 3, 3 )
      val b4 = UBox( 2, 2, 2 )
      val b5 = UBox( 1, 1, 1 )
      val b6 = UBox( 3, 6, 20 )

      val bx = Seq( b1, b2, b3, b4, b5 )
      println( bx )
      val tree = createUBoxTree( bx )
      val maxStack = getTallestStack( tree )
      println( maxStack )
    }

    def printBTree( data: BTree[ UBox ] ): Unit = {
      def print_( tree: BTree[UBox]): Unit = {
        tree match {
          case node: BTreeNode[UBox] =>
            println( node.toString )
            node.next.foreach( e => print_( e ) )
          case _ => "bad match"
        }
      }
      print_( data )
    }

    def stackBoxes( boxes: Seq[ UBox ] ): Seq[ UBox ] = getTallestStack( createUBoxTree( boxes ) )

    def createUBoxTree( data: Seq[UBox] ): BTree[ UBox ] = {

      def sqFilter( b: UBox, sq: Seq[UBox] ) = sq.filter( ubindex => ubindex.id != b.id && ubindex.width < b.width && ubindex.length < b.length )

      def createTree_( b: UBox, bx: Seq[UBox] ): BTree[ UBox ] = {
        bx match {
          case Nil => BTreeNode( b )
          case x :: xs =>
            val sp = sqFilter( b, bx )
            BTreeNode( b, sp.map{ case ub: UBox => createTree_( ub, sqFilter(ub, sp ) ) } )
          case x => throw new Exception(s"bad match ${x.toString}")
        }
      }
      val sp:ListBuffer[UBox] = ListBuffer[UBox]() ++ data
      BTreeNode( UBox(0, 0, 0), data.map{ case ub:UBox => createTree_( ub, sp.filter( _.id != ub.id ).toSeq ) } )
    }

    def getTallestStack( data: BTree[ UBox ] ): Seq[UBox] = {

      def sum(bs: Seq[UBox]) = bs.foldLeft(0) { (ac, b) => ac + b.height }

      def getTallestStack_(tree: BTree[UBox], cur: Seq[UBox] = Seq.empty[UBox]): Seq[UBox] = {
        tree match {
          case node: BTreeNode[UBox] if (node.next.isEmpty) => cur :+ node.value
          case node: BTreeNode[UBox] =>
            node.next.foldLeft(Seq.empty[UBox]) { (ac, n) => {
              val res = getTallestStack_(n, cur :+ node.value)
              if (sum(ac) < sum(res)) res else ac
            } }
        }
      }
      getTallestStack_( data )
    }

  }



}
