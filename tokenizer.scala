/**
  Class Tokenizer which tokenizes every single word in the input.
  separatorPattern -> regular expression for the initial separator pattern
  cliticPattern    -> regular expression for the initial clitic pattern
  abbreviationList -> A list of all abbreviations
 */
class Tokenizer(val separatorPattern: String, val cliticPattern: String, val abbreviationList: List[String]) {

	/**
	  Checks if a word is a consonant or not.
	 */
	def isConsonant(word: String):Boolean = {
		var flag:Boolean = true
		val vowels = Set("A", "E", "I", "O", "U", "Y", "a", "e", "i", "o", "u", "y")

		word.toList.foreach {
			c => if(vowels.contains(c.toString))
					flag = false
		}
		flag
	}

	/**
	  Gets the clitic mapping for a sequence of letters.
	 */
	def getCliticWord(word: String):String = {
		var cliticMap = Map(
			"'m" -> "am",
			"'re" -> "are",
			"'ve" -> "have",
			"n't" -> "not",
			"'d" -> "would",
			"'ll" -> "will",
			"'M" -> "AM",
			"'RE" -> "ARE",
			"'VE" -> "HAVE",
			"N'T" -> "NOT",
			"'D" -> "WOULD",
			"'LL" -> "WILL")
		if(word.matches("'s") || word.matches("'S") || word.matches("'d") || word.matches("'D")) {
			return ""
		}
		var value = cliticMap.get(word)
		return value.get
	}

	/**
	  Gets the abbreviation fullform
	 */
	def getFullWord(word: String):String = {
		var fullWordMap = Map(
			"Dr." -> "Doctor",
			"Mr." -> "Mister",
			"Mrs." -> "Missus")
		if(fullWordMap.contains(word)) {
			var value = fullWordMap.get(word)
			return value.get
		}else {
			return ""
		}
	}

	/**
	  Method to tokenize each and every word from the input.
	 */
	def tokenize(text: String):List[String] = {
		var tempString:String = text
		
		//For any character on the separator list, insert a space before and after the character.
		tempString = separatorPattern.r.replaceAllIn(tempString, " $0 ")

		//For any comma that doesn't have a digit immediately before and after it, insert a
		//space before and after the comma.
		val commaPattern = """([a-zA-Z]+),([\s+a-zA-Z]+)""".r
		tempString = """([a-zA-Z]+),([\s+a-zA-Z]+)""".r.replaceAllIn(tempString, _ match {
						case commaPattern(prefixComma, suffixComma) => prefixComma + " , " + suffixComma
						})

		//Put spaces around any single quote that isn't proceeded by a letter.
		val singleQuotePattern = """([a-zA-Z]+)'([^a-zA-Z0-9]+)""".r
		tempString = 	"([a-zA-Z]+)'([^a-zA-Z0-9]+)".r.replaceAllIn(tempString, _ match {
						case singleQuotePattern(prefixQuote, suffixQuote) => prefixQuote + " ' " + suffixQuote
						})

		//For any word that has a suffix matching one of the clitic patterns, put a space before the suffix.
		tempString = cliticPattern.r.replaceAllIn(tempString, " $0")

		//Split the string into a list of tokens.
		val tokens: List[String] = tempString.split("\\s+").toList
		var tempList: List[String] = List[String]()
		
		//For each word on the list:
		//	If it ends in a period
		//		and isn't on the abbreviation list
		//		and isn't a sequence of letters and periods
		//		and isn't all consonents
		//			then split the token into two words.
		//	Convert any clitics into complete words.
		tokens.foreach {
			word => var flag = false								//Flag to check if word has been added to the list or not.
					if(word.endsWith(".") && !abbreviationList.contains(word) && word.matches("[a-zA-Z]+.") && !isConsonant(word)) {
						val periodPos = word.indexOf(".")
						tempList = tempList :+ word.substring(0, periodPos)
						tempList = tempList :+ "."
						flag = true
					}

					if(abbreviationList.contains(word)) {
						val fullWord = getFullWord(word)
						if(!fullWord.equals("")) {
							tempList = tempList :+ getFullWord(word)
						}
						flag = true
					}

					//Check if word contains a clitic
					if(word.contains("'") && word.length > 1) {
						val cliticWord = getCliticWord(word)
						if(!cliticWord.equals("")) {
							tempList = tempList :+ getCliticWord(word)
						}
						flag = true
					}

					//if(!flag && !word.matches("[~!?|@#$%^&*()_+{}\".,:;\']+$")) {
					if(!flag) {
						tempList = tempList :+ word
					}
		}
		return tempList
	}

}

object Tokenizer {
	def main(args: Array[String]): Unit = {
		val abbreviationList = List("Dr.", "Mrs.", "Mr.")
		val separatorPattern = """[\?!()";/\|]"""
		val cliticPattern = """'|:|-|'S|'D|'M|'LL|'RE|'VE|N'T|'s|'d|'m|'ll|'re|'ve|n't"""
		val t = new Tokenizer(separatorPattern, cliticPattern, abbreviationList)
		if(args.length == 1) {
			val list = t.tokenize(args(0))
			//val list = t.tokenize("Hello, Mr. Test, this is a test String. Is it going to work? I don't know|Let's see. We'll come to know. hjk...")
			list.foreach(println)
		}
	}
}