package com.rpise

class Stack {
  class Node (val value : Int , val next : Node);
  var head : Node = null;
  def < ( value : Int ) :Stack= { head = new Node(value,head); this }
  def > (op: (Int)=>Unit):Stack = { 
         println("Value Popped -->" + ((v:Int)=>{op(v);v})(
                                        if(head == null) -1
                                         else {
                                           val t = head.value
                                           head = head.next
                                           t
                                         }
                                       ));       
         this 
  }
  def ~ () : Stack = { 
        println("Printing Stack...");
        var temp:Node = head; 
        while(temp != null){println(temp.value);temp=temp.next};
        this;
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
  }
}
