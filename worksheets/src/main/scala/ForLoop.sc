case class Book (title: String, authors: List[String])

val books= List(
  Book("Structure and Interpretation of computer Programs", List("Abelson, Harald", "Sussman, Gerald J.")),
  Book("Introduction to Functional programming", List("Bird, Richard", "Wadler, Phil")),
  Book("Effective Java", List("Bloch, Joshua")),
  Book("Effective Java2", List("Bloch, Joshua")),
  Book("Java puzzlers", List("Bloch, Joshua", "Gafter, Neal")),
  Book("Programming in Scala", List("Odersky, Martin", "Spoon, Lex", "Venners, Bill"))
)

val birdBooks = for  (b <- books; a <- b.authors; if (a startsWith "Bird")) yield b.title
val programBooks = for  (b <- books; if (b.title contains "Program")) yield b.title

val authorsTwoBooks01 = for {
  b1 <- books
  b2 <- books
  if b1.title < b2.title
  a1 <- b1.authors
  a2 <- b2.authors
  if a1 == a2
} yield a1


val authorsTwoBooks02 = {
  for {
    b1 <- books
    b2 <- books
    if b1.title != b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1
}.distinct

val setOfBooks =  Set(
  Book("Structure and Interpretation of computer Programs", List("Abelson, Harald", "Sussman, Gerald J.")),
  Book("Introduction to Functional programming", List("Bird, Richard", "Wadler, Phil")),
  Book("Effective Java", List("Bloch, Joshua")),
  Book("Effective Java2", List("Bloch, Joshua")),
  Book("Java puzzlers", List("Bloch, Joshua", "Gafter, Neal")),
  Book("Programming in Scala", List("Odersky, Martin", "Spoon, Lex", "Venners, Bill"))
)

val autorsTwoBooks03 = for {
  b1 <- setOfBooks
  b2 <- setOfBooks
  if b1.title != b2.title
  a1 <- b1.authors
  a2 <- b2.authors
  if a1 == a2
} yield a1

val birdBooksMapTranslatedFirstStep = books flatMap(b => for(a <- b.authors.withFilter(a => a startsWith "Bird")) yield b.title)
val birdBooksMapTranslated = books flatMap(b => b.authors.withFilter(a => a startsWith "Bird").map(x=> b.title))

