package com.rajeshpatkar

object Program {
  def main(args: Array[String]): Unit = {
    val p1 = Point2D(1,2)
    val p2 = Point3D(5,6,7)
    val p3 = PointPolar(1,34)
    (Stack[Point]() < p1 < p2 < p3 ~).visit(_*2).visit(_*33).visit(_-2);
  }
}

abstract class Point
case class Point2D(val x:Int , val y:Int) extends Point
case class Point3D(val x:Int,val y:Int,val z:Int) extends Point
case class PointPolar(val r:Int ,val theta:Int) extends Point

trait Visitable[T]{
  def visit(p:(Int)=>Int):T
}

case class Stack[T] extends Iterable[T] with Visitable[Stack[T]]{
  class Node (val value : T , val next : Node);
  var h : Node = null;
  def < ( value : T ) :Stack[T]= { h = new Node(value,h); this }
  def > (op: (Option[T])=>Unit):Stack[T] = { 
         var value:Option[T] = if(h == null) None
                     else {
                            val t = h.value
                            h = h.next
                            Some(t)
                     }  
         op(value)           
         println("Value Popped -->" + value)     
         this 
  }
  def ~ () : Stack[T] = { 
                       println("Printing Stack...")
                       foreach(println)
                       this;
  }
  override def iterator():Iterator[T] = {
                              var data = List[T]()
                              var temp = h;
                              while(temp != null){
                                  data = temp.value::data
                                  temp=temp.next
                              }
                              data.iterator
  }
                                              
  override def visit(p:(Int)=>Int):Stack[T] = 
  {
     for(v <- iterator){
       v match {
         case Point2D(x,y) => println("x = "+ p(x)+ " y = " + p(y))
         case Point3D(x,y,z) => println("x = "+ p(x) +" y = "+ p(y) +" z = "+ p(z))
         case PointPolar(r,theta) => println("r = " + p(r) + " theta = "+ p(theta))
         case _ => println("Match not found")
       };
       
     }
     this;
  }
 }