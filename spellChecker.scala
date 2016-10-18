import scala.io.Source, scala.io.Source._
import scala.collection.mutable._

object spellChecker {
  
  def fileToDict(filename: String) : List[String] = {
    val stream = Source.fromFile(filename)
    return List() ++ stream.getLines.toList
  }
  
  def main(args: Array[String]) = {
    if (args.length == 0) {
      println("Please remember to input a word.")
    } 
    else {
      val wordList = fileToDict("wordsEn.txt")
      if (wordList.exists(_ == args(0))) {
        println("'" + args(0) + "'" + " is a valid word.")
      }
      else {
        println("'" + args(0) + "'" + " is not in the dictionary.")
        val suggestions = search(args(0), wordList)
        if (suggestions.size == 0) {
          println("No suggestions found for your word.")
        } else {
          println("List of Suggestions: ")
          printSuggestions(suggestions) 
        }
      }
    }
  }
  
  def search(word: String, wordList: List[String]) : Set[String] = {
    val lower : String = word.toLowerCase() 
    val output = new HashSet[String]()
    addLetter(lower, wordList, output)
    removeLetter(lower, wordList, output)
    switchTwoLetters(lower, wordList, output)
    return output 
  }

  def addLetter(word: String, wordList: List[String], 
                    output: HashSet[String]) =
  {
    val alphabet: List[Char] = List.range('a','{')
    for (c <- alphabet) {
      val candidate1 : String = c + word
      val candidate2 : String = word + c
      if (wordList.contains(candidate1)) {
        output.add(candidate1)
      }
      if (wordList.contains(candidate2)) {
        output.add(candidate2)
      }
    }
  }
  
  def removeLetter(word: String, wordList: List[String], 
                    output: HashSet[String]) =
  {
    for (i <- 0 to (word.length - 1)) {
      var candidate = word.slice(0, i) + word.slice(i+1, word.length)
      if (wordList.contains(candidate)) {
        output.add(candidate)
      }
    }
  }
  
  def switchTwoLetters(word: String, wordList: List[String], 
                        output: HashSet[String]) =
  {
    for (i <- 1 until word.length) {
      val c1 = word.apply(i-1)
      val c2 = word.apply(i)
      val candidate = word.slice(0,i-1) + c2 + c1 + 
        word.slice(i+1, word.length)
      if (wordList.contains(candidate)) {
        output.add(candidate)
      }
    }
  }

  def printSuggestions(set: Set[String]) {
    var count = 0
    for (word <- set) {
      if (count < 10) {
        print(word + " ") 
        count = count + 1
      }
      else {
        println()
        print(word + " ")
        count = 1
      }
    }
    println()
  }

}
