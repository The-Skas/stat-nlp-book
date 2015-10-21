package uk.ac.ucl.cs.mr.statnlp2015.assignment1

import java.io.File
import uk.ac.ucl.cs.mr.statnlpbook.Tokenizer

import uk.ac.ucl.cs.mr.statnlpbook.chapter.languagemodels.Util

/**
 * This stub is provided as help for you to solve the first question.
 * @author riedel
 */
object TokenizationQuestion {

  def main(args: Array[String]) {
    print(new java.io.File( "." ).getCanonicalPath)
    //the directory that contains the raw documents
    val dataDir = new File("data")

    //the file that contains the target gold tokenization
    val goldFile = new File("data/p1/goldtokens.txt")

    //the actual raw documents
    val raw = Assignment1Util.loadTokenizationQuestionDocs(dataDir)

    //TODO: The tokenizer you need to improve to match the gold tokenization
    val split_if_end_bar_ahead = """(?=\[/BAR\])"""
    val split_if_end_bar_behind ="""(?<=\[/BAR\])"""
    val split_end_bar = (s"$split_if_end_bar_ahead|$split_if_end_bar_behind")

    val split_if_start_bar_ahead = """(?=\[BAR\])"""
    val split_if_start_bar_behind ="""(?<=\[BAR\])"""
    val split_start_bar = (s"$split_if_start_bar_ahead|$split_if_start_bar_behind")

    //This should cause the regex to split at '['|'Chorus' when the value 'Chorus' is found and prepending it a '['
    val split_chorus_block ="""(?=\[Chorus\])"""
    val split_chorus_left_bracket  = """(?=Chorus\])"""
    val split_chorus_right_bracket = """(?=\])(?<=Chorus)|(?<=Chorus])"""
    val split_chorus = s"""$split_chorus_block|$split_chorus_left_bracket|$split_chorus_right_bracket"""



    val split_verse_block ="""(?=\[Verse \d+\])"""
                                            //Verse DDD
    val split_verse_left_bracket  = """(?=Verse \d{1,3}\])"""
    val split_verse_right_bracket = """(?=\])(?<=Verse \d{1,3})|(?<=Verse \d{1,3}])"""
    val split_verse = s"""$split_verse_block|$split_verse_left_bracket|$split_verse_right_bracket"""
    
    val split_all_brackets = """(?=\[.*?\])"""

    val split_unparsed_contractions = ("""(?i)(?=can't)|(?i)(?<=can't)|(?i)(?=it's)|(?i)(?<=it's)""")
    val contraction_cant = ("""(?i)(?<=ca)(?=n't)""")
    val contraction_its =  ("""(?i)(?<=it)(?='s)""")
    val split_contractions = (s"""$split_unparsed_contractions|$contraction_cant|$contraction_its""")

    val split_punctuation = """(?=\.|\,|\"))"""
    //val split_chorus = """(?=\[Chorus\])|(?=Chorus\])|(?=\])(?<=Chorus)|(?<=Chorus])"""
    //TODO: Create a 'seperate_regx(str_patten)' method, which returns (?=pattern|?<=pattern)

    val in_bracket = """(.{1,3}(?=\]))"""
    val tokenizer = Tokenizer.fromRegEx(s"\\s|$split_verse|$split_end_bar|$split_start_bar|$split_chorus|$split_contractions|$split_punctuation")

    //the result of the tokenization
    val result = Util.words(raw map tokenizer)

    //the gold tokenization file
    val gold = Assignment1Util.loadWords(goldFile).toBuffer

    //we find the first pair of tokens that don't match
    val zip_result = result.zip(gold);
    val mismatch = result.zip(gold).find { case (a, b) => a != b }

    //Your goal is to make sure that mismatch is None
    val regex_test =
    mismatch match {
      case None => println("Success!")
      case Some(pair) => println("The following tokens still don't match: " + pair)
    }

  }

}
