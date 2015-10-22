package uk.ac.ucl.cs.mr.statnlp2015.assignment1

import java.io.File
import uk.ac.ucl.cs.mr.statnlpbook.Tokenizer
import ml.wolfe.nlp.Document
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
    val _Document = Document.fromString("Cool");
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
    val split_chorus_block ="""(?=\[(Chorus|Hook)\])"""
    val split_chorus_left_bracket  = """(?=(Chorus|Hook)\])"""
    val split_chorus_right_bracket = """(?=\])(?<=(Chorus|Hook))|(?<=(Chorus|Hook)])"""
    val split_chorus = s"""$split_chorus_block|$split_chorus_left_bracket|$split_chorus_right_bracket"""



    val split_verse_block ="""(?=\[Verse \d+\])"""
                                            //Verse DDD
    val split_verse_left_bracket  = """(?=Verse \d{1,3}\])"""
    val split_verse_right_bracket = """(?=\])(?<=Verse \d{1,3})|(?<=Verse \d{1,3}])"""
    val split_verse = s"""$split_verse_block|$split_verse_left_bracket|$split_verse_right_bracket"""

    //Chunk at |(***)| -> (|*****|)
    val split_all_braces = """(?=[\(\{].*?[\)\}])|(?<=[\(\{])|(?=[\}\)])"""
    val split_all_bracks = """(?=\[.*?\])|(?<=\[)(?!(BAR|/BAR))|(?<!(BAR|/BAR))(?=\])|(?<=\](?<!(BAR|/BAR)))"""
    //can't, won't couldn't {this is due to the grouping being (pre, n'*)
    val split_unparsed_contractions = ("""(?i)(?=can't)|(?i)(?<=can't)""")
    val contraction_cant = ("""(?i)(?<=\w{2,6})(?=n't)""")

    //who's, it's, i'm {this is due to the grouping being (pre, '*)   ve|d|s|m|re|ll
    val pro_nouns = "(?i)(it|he|she|how|let|some(body|one|thing)|that|there|what|where|when|who|why|\\d|mother|pj|\\bt|\\b\\w)"

    //There is a case where "[man's / T's]" is not split into (man,'s) making it an outlier
    val contraction_s =  (s"(?i)(?<=$pro_nouns|\\w(\\w{0,4}|-.{0,4})\\w)(?<!\\b(man|plan|penetration|Tradition|vision|Devon|John|harlequin|brown))(?='s)")

    val contraction_rest_ignore = ("(?!(s|t|ta|y|tcha)\\W)")
    val special_rest_ignore = "(?<!tryin)(?!'a)"
    val contraction_rest =  (s"(?i)(?<={1,5})(?='(?=\\w{1,5})(?!s\\W)(?!t\\W)($contraction_rest_ignore))")

    //cases: God' , lo'                         //'[NOT A WORD]
    val contraction_lo_ignore = "in|en|\\b(gon|n|an)"
    val contraction_lo  = s"((?i)(?<!($contraction_lo_ignore))(?<=\\w{1,9})(?='\\W))"
    val split_contractions = (s"""$split_all_bracks|$split_unparsed_contractions|$contraction_cant|$contraction_s|$contraction_rest|$contraction_lo""")
    //                      if comma/ not work
    val punctuation = """[\*\?\!\;\:\+\,\"\_]"""
                //Titles and one Letter words.
    val titles = "(Mr|Mrs|Dr)"          //<----- arrow!
    val is_no_title = s"(?=\\.)(?<!$titles)"
    val split_period_in_single_letter = "(?<=\\b\\w(?=\\.))|(?<=\\.)(?=\\w.?\\W)"
    val clean_punctuation = s"(?=\\')(?<=\\W)|(?<=$punctuation)(?<!$titles)(?=\\.)|(?<=\\.)(?=($punctuation))|$split_period_in_single_letter"
    // punc ".,\?"
    val split_punctuation = s"""(?=$punctuation)|$is_no_title|(?<=$punctuation|\\.)|(?<=\")"""
    //val split_chorus = """(?=\[Chorus\])|(?=Chorus\])|(?=\])(?<=Chorus)|(?<=Chorus])"""
    //TODO: Create a 'seperate_regx(str_patten)' method, which returns (?=pattern|?<=pattern)



    val tokenizer = Tokenizer.fromRegEx(s"\\s|$split_verse|$split_all_braces|$split_end_bar|$split_start_bar|$split_chorus|$split_contractions|$split_punctuation|$clean_punctuation")

    //the result of the tokenization
    val result = Util.words(raw map tokenizer)

    //the gold tokenization file
    val gold = Assignment1Util.loadWords(goldFile).toBuffer

    //we find the first pair of tokens that don't match
    val zip_result = result.zip(gold);

    def find_mismatch(list_tuple: IndexedSeq[(String,String)]): Any ={
      var count = 0;
      var some_list = List(("","",0));
      for ((r, g) <-list_tuple) {
        count += 1;
        if( r != g) {
           return (r, g, count)
           some_list = (r,g,count) :: some_list
        }
      }
      return some_list

    }

    val mismatch = find_mismatch(zip_result)

    def check_regex(test_str: String, regex_expr: String = ""): Any = {
      var temp_tokenizer = tokenizer;
      if(!regex_expr.isEmpty()) {
        temp_tokenizer = Tokenizer.fromRegEx(regex_expr)
      }
      return Util.words(List(temp_tokenizer(Document.fromString(test_str))))
    }
    //Your goal is to make sure that mismatch is None
    val regex_test =
    mismatch match {
      case None => println("Success!")
      case Some(pair) => println("The following tokens still don't match: " + pair)
    }

  }

}
