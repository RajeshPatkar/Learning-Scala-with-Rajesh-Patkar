package datastructuresinscalastack
 
object Main {
  def main(args: Array[String]): Unit = {
    println("Welcome to Simple Stack");
    var s1 = new Stack(10)
    val v = (1::2::3::s1).!.pop
    println("Popped Value is "+ v)
    s1 !
  }
}
class Stack(v:Int){
  var stk = new Array[Int](v)
  var sp = v
  def ::(v:Int) = { if(sp!=0){sp-=1;stk(sp)= v} else println("Overflow") ; this}
  def pop = if(sp!=v) {sp = sp + 1; stk(sp-1)} else {println("underflow"); -1}          
  def ! = { println("Printing Stack"); for( i <- sp to stk.length-1) println(stk(i)) ; this }
}