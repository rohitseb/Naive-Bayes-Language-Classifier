package lin567_p1


class NaiveBayes {
  // Use these to compute P( Language )
  var docLanguageCounts = scala.collection.mutable.Map[Language,Double]().withDefaultValue(0D)
  var docCount = 0D

  // Use these to compute P( Word | Language )
  var languageWordCounts = scala.collection.mutable.Map[Tuple2[Language,String],Double]().withDefaultValue(0D)
  var langVocSize = scala.collection.mutable.Map[Language,Double]().withDefaultValue(0D)
  var vocabulary = Array[String]()//for keeping track of distinct words in the vocabulary

  // This should increment counts so you can compute P( Language ) and P( Word | Language )
  def train( corpus:Set[Document] ) {
    // This loops over the set of documents, and provides variables for the document id as a String,
    // the document text as an Array[String], and the language as a Language
    corpus.foreach{ case Document( id, text, language ) =>
	docCount=docCount+1

	if(!docLanguageCounts.contains(language)){
		docLanguageCounts(language)=1
	}
	else{
		docLanguageCounts(language)=docLanguageCounts(language)+1
	}
	
	if(!langVocSize.contains(language)){
		langVocSize(language)=0
	}
	
	for ( i <- 0 to (text.length - 1) ) {
		
		if(!vocabulary.contains(text(i))){
			vocabulary=vocabulary:+text(i)
		}
		
		if(!languageWordCounts.contains((language,text(i)))){
			languageWordCounts((language,text(i)))=1
		}
		else{
			languageWordCounts((language,text(i)))=languageWordCounts((language,text(i)))+1
		}
		langVocSize(language)=langVocSize(language)+1
	}
    }
  }

  // Should compute P( word | language ). Implement with add-lambda smoothing.
  def p_wordGivenLg( word:String, language:Language, lambda:Double ) = {
    var wordCount=languageWordCounts((language,word))
    var langWordCount=langVocSize(language)
    var denominator=langWordCount+lambda*vocabulary.length
    if(!vocabulary.contains(word)){//for unknown words
	denominator=denominator+lambda
    }
    var prob=(wordCount+lambda)/denominator
    prob
  }

  // Should compute P( Language )
  def p_Lg( language:Language ) = {
    var count = docLanguageCounts(language)
    var prob_Lg = count/docCount
    prob_Lg
  }


  // Should compute P( Word, Language )= P( Language )\prod_{Word in Document}P( Word | Language )
  def p_docAndLg( document:Array[String], language:Language, lambda:Double ) = {
    var probLang = p_Lg(language)
    var probword = 1.0
    for ( i <- 0 to (document.length - 1) ) {
	probword=probword*p_wordGivenLg(document(i),language,lambda)	
    }
    var prob= probLang*probword
    prob
  }


  // This function takes a document as a parameter, and returns the highest scoring language as a
  // Language object. 
  def mostLikelyLanguage( document:Array[String], lambda:Double ) = {
    // Loop over the possible languages (they should accessible in docLanguageCounts.keys), and find
    // the language with the highest P( Document, Language ) score
    var prob=0D
    var temp=0D
    var lang= ""
    var keys = docLanguageCounts.keys.toArray
    for ( i <- 0 to (keys.length - 1) ) {
	temp=p_docAndLg(document,keys(i),lambda)
	if(temp>prob){
		prob=temp
		lang=keys(i).toString
	}
    }
    Language(lang)
  }


}

