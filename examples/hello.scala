object Hello {
  println("Constructing Hello")
  def main(args: Array[String]): Unit = greet("Po Co")
  def greet(name: String): Unit = {
    print("Hello, ")
    print(name)
    println("!")
  }
}
