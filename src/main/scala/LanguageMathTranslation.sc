// COSC 455 - Programming Languages: Implementation and Design
// Project 2
// NAME: Brandon Cavender

val chinese : List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu", "qi",
  "ba", "jiu", "shi")
val english : List[String] = List("zero", "one", "two", "three", "four", "five", "six",
  "seven", "eight", "nine", "ten")

val eList: List[String] = List("one", "nine", "six", "eight")
val cList: List[String] = List("yi", "nine", "six", "ba")
val tList: List[String] = List("yi", "Josh", "three", "si")

def translate(sList : List[String]) : List[Int] = {
  sList.map {
    case ("zero") => 0
    case ("one") => 1
    case ("two") => 2
    case ("three") => 3
    case ("four") => 4
    case ("five") => 5
    case ("six") => 6
    case ("seven") => 7
    case ("eight") => 8
    case ("nine") => 9
    case ("ten") => 10
    case ("ling") => 0
    case ("yi") => 1
    case ("er") => 2
    case ("san") => 3
    case ("si") => 4
    case ("wu") => 5
    case ("liu") => 6
    case ("qi") => 7
    case ("ba") => 8
    case ("jiu") => 9
    case ("shi") => 10
    case _ => -1
  }
    .filter(_ > -1)
}

def printTranslation(iList : List[Int]) : Unit = {
  if (!iList.isEmpty) {
    print(iList.head + " ")
    printTranslation(iList.tail)
  }
  else
    Nil
}

def sum(iList : List[Int]) : Int = {
  iList match {
    case Nil => 0
    case head :: tail => head + sum(tail)
  }
}

def printAddNums(iList : List[Int]) : Unit = {
  (iList.isEmpty, iList.length.equals(1)) match {
    case (true, _) => Nil
    case (false, true) => {
      print(iList.head)
      printAddNums(iList.tail)
    }
    case (false, false) => {
      print(iList.head + " + ")
      printAddNums(iList.tail)
    }
  }
}

def addString(iList : List[Int]) : Unit = {
  println(printAddNums(iList) + " = " + sum(iList))
}

def product(iList : List[Int]) : Int = {
  iList match {
    case Nil => 1
    case head :: tail => head + product(tail)
  }
}

def printMultNums(iList : List[Int]) : Unit = {
  (iList.isEmpty, iList.length.equals(1)) match {
    case (true, _) => Nil
    case (false, true) => {
      print(iList.head)
      printMultNums(iList.tail)
    }
    case (false, false) => {
      print(iList.head + " + ")
      printMultNums(iList.tail)
    }
  }
}

def multString(iList : List[Int]) : Unit = {
  println(printMultNums(iList) + " = " + product(iList))
}

def LanguageMathTranslation(sList : List[String]) : Unit = {
  val iList : List[Int] = translate(sList)
  print("Translation: ")
  printTranslation(iList)
  println()
  print("Addition: ")
  addString(iList)
  print("Multiplication: ")
  multString(iList)
  println()
}

LanguageMathTranslation(eList)
LanguageMathTranslation(cList)
LanguageMathTranslation(tList)