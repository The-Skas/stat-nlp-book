package uk.ac.ucl.cs.mr.statnlpbook.assignment2

import scala.collection.mutable

/**
 * Created by Georgios on 05/11/2015.
 */

object Features {

  /**
   * a feature function with two templates w:word,label and l:label.
   * Example for Trigger Exraction
   * @param x
   * @param y
   * @return
   */
  def defaultTriggerFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex)
    val token = thisSentence.tokens(begin) //first token of event
    val feats = new mutable.HashMap[FeatureKey,Double]
    feats += FeatureKey("label bias", List(y)) -> 1.0
    feats += FeatureKey("first argument word", List(token.word, y)) -> 1.0

    feats.toMap
  }

  def get_all_of_index(index : Integer, sentence: Sentence) : List[(Token,Token,String)] = {
    var a_list = List[(Token,Token,String)]()

    for(dep <- sentence.deps) {
      if(dep.head == index || dep.mod == index)
        a_list = a_list ++ List((sentence.tokens(dep.head),sentence.tokens(dep.mod), dep.label))
    }
    return a_list;
  }
  /**
   * a feature function with two templates w:word,label and l:label.
   * Example for Argument Exraction
   * @param x
   * @param y
   * @return
   */
  def defaultArgumentFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex)
    val event = thisSentence.events(x.parentIndex) //use this to gain access to the parent event
    val eventHeadToken = thisSentence.tokens(event.begin) //first token of event
    val feats = new mutable.HashMap[FeatureKey,Double]
    feats += FeatureKey("label bias", List(y)) -> 1.0
    val token = thisSentence.tokens(begin) //first word of argument
    feats += FeatureKey("first argument word", List(token.word, y)) -> 1.0


    //feats += FeatureKey("contains a number", List(has_num(token.word), y)) -> 1.0

    //This is saying an if check for protein && eventheadToken && label
    feats += FeatureKey("is protein_first trigger word", List(x.isProtein.toString,eventHeadToken.word, y)) -> 1.0
    feats.toMap
  }

  //TODO: make your own feature functions
  def myTriggerFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex) //use this to gain access to the parent sentence
    val feats = new mutable.HashMap[FeatureKey,Double]
    val token = thisSentence.tokens(begin) //first token of Trigger
    //*********
    //Lexical
    //*********
    //So that we don't get an index error
    val last_token_index = thisSentence.tokens.size - 1
    if(begin > 0) {
      val prev_token = thisSentence.tokens(begin-1)
      feats += FeatureKey("prev trigger word", List(prev_token.word, y)) -> 1.0
      feats += FeatureKey("prev trigger POS", List(prev_token.pos,y)) -> 1.0
      feats += FeatureKey("prev trigger stem", List(prev_token.stem,y)) -> 1.0
    }
    if(begin < last_token_index) {
      val next_token = thisSentence.tokens(begin+1)
      feats += FeatureKey("next trigger word", List(next_token.word ,y)) -> 1.0
      feats += FeatureKey("next trigger POS", List(next_token.pos,y)) -> 1.0
      feats += FeatureKey("next trigger stem", List(next_token.stem,y)) -> 1.0


    }
    feats += FeatureKey("first trigger word", List(token.word, y)) -> 1.0
    feats += FeatureKey("first trigger word pos", List(token.pos, y)) -> 1.0
    feats += FeatureKey("First trigger word stem: ", List(token.stem,y)) -> 1.0


    //*********
    //********
    //*********

    //DEPENDENCIES
    for ((token_1,token_2,depend) <- get_all_of_index(token.index, thisSentence)) {
      //as modifier or head?
      feats += FeatureKey("Dependency ", List(depend, y)) -> 1.0
      feats += FeatureKey("Depend: Head.pos, Mod.pos ", List(token_1.pos,token_2.pos, y)) -> 1.0
    }

    //MENTOINS: CALCULATE PROTEIN DISTANCE
    for(protein <- thisSentence.mentions) {
      val index_begin = protein.begin
      val index_end   = protein.end
      val protein_token = thisSentence.tokens(index_begin)

      feats += FeatureKey("Protein pos: ", List(protein_token.pos, y)) -> 1.0
      feats += FeatureKey("Protein stem: ", List(protein_token.stem, y)) -> 1.0

      val abs_dist_beg = scala.math.abs(index_begin - token.index)
      val abs_dist_end = scala.math.abs(index_end - token.index)

      //Get smallest protein distance
      val abs_dist = if(abs_dist_beg < abs_dist_end) abs_dist_beg else abs_dist_end
      feats += FeatureKey("Protein Distance ", List(abs_dist.toString(), y)) -> 1.0

      //Counts the number of proteins in a sentence
    }

    feats.toMap

  }
  def myArgumentFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex) //use this to gain access to the parent sentence
    val feats = new mutable.HashMap[FeatureKey, Double]
    val event = thisSentence.events(x.parentIndex) //use this to gain access to the parent event
    val eventHeadToken = thisSentence.tokens(event.begin)
    val token = thisSentence.tokens(begin) //first token of Trigger
    //*********
    //Lexical
    //*********
    //So that we don't get an index error
    val last_token_index = thisSentence.tokens.size - 1
    if (begin > 0) {
      val prev_token = thisSentence.tokens(begin - 1)
      feats += FeatureKey("prev and first trigger word", List(prev_token.word,eventHeadToken.stem, y)) -> 1.0
      feats += FeatureKey("prev and first trigger POS", List(prev_token.pos,eventHeadToken.stem, y)) -> 1.0
    }
    if (begin < last_token_index) {
      val next_token = thisSentence.tokens(begin + 1)
      feats += FeatureKey("next and first trigger word", List(next_token.word,eventHeadToken.stem, y)) -> 1.0
      feats += FeatureKey("next and first trigger POS", List(next_token.pos,eventHeadToken.stem, y)) -> 1.0

    }
    feats += FeatureKey("first trigger word", List(token.word,eventHeadToken.stem, y)) -> 1.0
    feats += FeatureKey("first trigger word pos", List(token.pos, eventHeadToken.stem,y)) -> 1.0
    feats += FeatureKey("First trigger word stem: ", List(token.stem,eventHeadToken.stem, y)) -> 1.0
    //*********
    //********
    //*********


    //DEPENDENCIES
    for ((token_1, token_2, depend) <- get_all_of_index(token.index, thisSentence)) {
      //as modifier or head?
      feats += FeatureKey("Dependency ", List(depend, y)) -> 1.0
      feats += FeatureKey("Depend: Head.pos, Mod.pos ", List(token_1.pos, token_2.pos, y)) -> 1.0

    }
    for ((token_1, token_2, depend) <- get_all_of_index(eventHeadToken.index, thisSentence)) {
      //as modifier or head?
      feats += FeatureKey("Dependency ", List(depend ,y)) -> 1.0
      feats += FeatureKey("Depend: Head.pos, Mod.pos ", List(token_1.pos, token_2.pos, y)) -> 1.0

    }
    //MENTOINS: CALCULATE PROTEIN DISTANCE
    for (protein <- thisSentence.mentions) {
      val index_begin = protein.begin
      val index_end = protein.end
      val protein_token = thisSentence.tokens(index_begin)



      val abs_dist_beg = scala.math.abs(index_begin - token.index)
      val abs_dist_end = scala.math.abs(index_end - token.index)
      val abs_dist = if (abs_dist_beg < abs_dist_end) abs_dist_beg else abs_dist_end
      if(abs_dist <= 1) {
        feats += FeatureKey("Protein Distance ", List(abs_dist.toString(), y)) -> 1.0
      }
      feats += FeatureKey("Protein pos: ", List(protein_token.pos, y)) -> 1.0
      feats += FeatureKey("Protein stem: ", List(protein_token.stem, y)) -> 1.0

      //Counts the number of proteins in a sentence
    }

    feats.toMap
  }

}
