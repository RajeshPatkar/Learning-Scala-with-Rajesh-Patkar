package com.rajeshpatkar

class Stack extends Iterable[Int]{
  class Node (val value : Int , val next : Node);
  var h : Node = null;
  def < ( value : Int ) :Stack= { h = new Node(value,h); this }
  def > (op: (Int)=>Unit):Stack = { 
         var value = if(h == null) -1
                     else {
                            val t = h.value
                            h = h.next
                            t
                     }  
         op(value)           
         println("Value Popped -->" + value)     
         this 
  }
  def ~ () : Stack = { 
                       println("Printing Stack...")
                       foreach(println)
                       this;
  }
  override def iterator():Iterator[Int] = {
                 var data = List[Int]()
                 var temp = h;
                 while(temp != null){
                    data = temp.value::data
                    temp=temp.next
                 }
                 data.iterator
               } 
  }


object Program {
    def main(args: Array[String]): Unit = {
    val s1 = new Stack();
     s1<10<20<30<40<50<60<70~;
    var x=0;var y=0;
    val X = (v:Int)=>x=v
    val Y = (v:Int)=>y=v
    ((((s1>X>Y~)>(x=_)>(y=_)~)>{x=_}>{y=_}~)<6<7~)<1<2<3~;
    println("x= "+x+" y= "+y);
    s1.foreach(println);
    println("Printing filtered numbers..");
    s1.filter(_>3).foreach(println);
    }
}
