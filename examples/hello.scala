object Hello {
  def main(): Unit = {
    greet("Po Co")
  }
  def greet(name: String): Unit = {
    print("Hello, ")
    print(name)
    println("!")
  }
}
