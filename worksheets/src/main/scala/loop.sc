def WHILE(condition: => Boolean)(command : => Unit ) :Unit =
 if (condition) {
   command
   WHILE(condition)(command)
 } else ()

def REPEAT(command: => Unit)(condition: => Boolean):Unit = {
  command
  if (condition) () else REPEAT(command)(condition)
}


def REPEAT_UNTIL (command: => Unit) = new {
  def UNTIL (condition: => Boolean): Unit = {
    command
    if (condition) () else UNTIL(condition)
  }

}

var i = 0
REPEAT{
  System.out.println("Hello")
  i = i +1
} {
  i > 5
}


i = 0
REPEAT_UNTIL{
  System.out.println("Hello")
  i = i +1
} UNTIL{
  i > 8
}
