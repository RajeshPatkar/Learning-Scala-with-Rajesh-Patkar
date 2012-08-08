package com.rpise

class Stack {
  var head : Node = null;
  def < ( value : Int ) : Stack = 
  {
        head = new Node(value,head);
        this
  }
  def > (op: (Int)=>Unit):Stack = 
  { 
       op(
            if(head == null) -1 
            else{
              val t:Int = head.value ; head = head.next; t  
            }
         );
       this 
  }
  def ~ : Stack = 
  { 
       println("Printing Stack...");
       var temp:Node = head; 
       while(temp != null){println(temp.value);temp=temp.next};
       this
  }
}

class Node (val value : Int , val next : Node);


object Program {
  def main(args: Array[String]): Unit = {
    val s1 = new Stack();
    s1<10<20<30<40<50<60<70~;
    var x=0;var y=0;
    val X = (v:Int)=>x=v;
    val Y = (v:Int)=>y=v;
    s1>X>Y~;
    println("first value popped " + x);
    println("second value popped " + y);
    s1>(x=_)>(y=_)~;
    println("third value popped " + x);
    println("fourth value popped " + y);
    (s1>{x=_}>{y=_}~)<6<7;
    println("fifth value popped " + x);
    println("sixth value popped " + y);
    (s1~)<1<2<3~;
  }
}