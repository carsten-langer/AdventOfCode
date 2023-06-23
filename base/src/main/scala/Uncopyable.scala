trait Uncopyable {
  // noinspection ScalaUnusedSymbol
  // see https://blog.leifbattermann.de/2017/04/26/a-new-scala-feature-for-making-illegal-states-unrepresentable/
  private def copy(): Unit = ()
}
