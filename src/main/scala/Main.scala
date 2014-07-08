
package idio.spotlight
/**
 * Copyright 2014 Idio
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import jp.kenkov.smt.ibmmodel.{IBMModel1, IBMModel, IBMModel2, Alignment}
import jp.kenkov.smt.{TargetSentence, TokenizedCorpus, mkTokenizedCorpus, Corpus}
import java.io.FileWriter


/**
 * @author David Przybilla david.przybilla@idioplatform.com
 **/


object Corpus{

  val acronymPattern = "[A-Z.]+".r

  private def tokenizeAcronym(word: String): Array[String] = {
    word match {
      case acronymPattern() =>  word.replace(".", "").split("")
      case _ => Array[String](word)
    }
  }

   def sentenceTokenizer(sentence: String): List[String] = {
    sentence.split("[ ]+").map(tokenizeAcronym).flatten.toList
  }

  def createTokenizedCorpus(corpus: Corpus): TokenizedCorpus = {
     corpus.map{
       case(es, fs) => (sentenceTokenizer(es), sentenceTokenizer(fs))
     }
  }
}

class NameEntityAligment(pathToFile: String) {

  /*
  * Cleans a dbpedia uri
  * */
  private def transformUri(uri: String): String = {
    uri.replace("_"," ").replace("("," ").replace(")"," ")
  }



  /*
  * Read a tsv file:
  *  dbpedia_id <tab> surfaceForm <tab> annotatedCount
  * */
  private def readFile(pathToFile: String): TokenizedCorpus ={
    println("reading file...")
    val parsedLines:Seq[(String, String, String)] = scala.io.Source.fromFile(pathToFile).getLines().map{

      line: String =>
        val splitLine = line.trim().split("\t")
        try{
           Some(splitLine(0), splitLine(1), splitLine(2))
        }catch{

          case _=>{
             None
          }
        }

    }.flatten.toSeq


    println("generating training data..")
    val trainingData = parsedLines.par.map{
      case (dbpediaID:String, surfaceForm:String, annotatedCounts:String) =>
        val targetSentence = transformUri(dbpediaID)
        val sourceSentence = surfaceForm
        (targetSentence, sourceSentence)
    }.toList

    println("tokenizing corpus")
    val tokenizedCorpus = Corpus.createTokenizedCorpus(trainingData)
    println("finished tokenizing corpus")


    tokenizedCorpus

  }


  val wordsProbabilities = {

      val tokenizedCorpus = readFile(pathToFile)
      println("training IBM model...")
      val model = new IBMModel1(tokenizedCorpus, 2)
      model.train

  }

  /*
  * Playing a bit with the Generated IBM model
  * */
  def getProbability(source:String, target:String){
   0.0
  }

 /*
 * Exports the Aligment probabilities to a file
 * */
  def exportAlignmentProbabilities(pathToOutputFile: String){
    val fw = new FileWriter(pathToOutputFile)
    wordsProbabilities.foreach{
      case ((targetWord:String, sourceWord:String), probability:Double ) =>

        val lineData = Array[String](targetWord, sourceWord, probability.toString)
        fw.write(lineData.mkString("\t")+"\n")

    }
    fw.close()
  }
}

object NameEntityAligment{

  def Main(args: Array[String]){
    val pathTofile = args(0)
    val neAligner = new NameEntityAligment(pathTofile)
  }

}



// Just playing a bit with the IBM aligments..
object Main{


  def main(args : Array[String]){
    println("sbt project")

    val sourceSentence1 = List[String]("david", "come", "pollo")
    val targetSentence1 = List[String]("david","eats", "chicken")


    val sourceSentence2 = List[String]("el","pollo", "es","dulce")
    val targetSentence2 = List[String]("chicken", "is", "sweet" )


    val sourceSentence3 = List[String]("Michael", "come", "sandia")
    val targetSentence3 = List[String]("Michael", "eats", "watermelon")


    val sourceSentence4 = List[String]("sandia", "dulce")
    val targetSentence4 = List[String]("sweet", "watermelon")

    val sourceSentence5 = List[String]("el","sandia", "es","buena")
    val targetSentence5 = List[String]("watermelon.","is", "good")

    val sourceSentence6 = List[String]("el","dijo", "es","bueno")
    val targetSentence6 = List[String]("he.","said","it's", "good")


    val corpus = List[(List[String], List[String])](

      (targetSentence1, sourceSentence1),
      (targetSentence2, sourceSentence2),
      (targetSentence3, sourceSentence3),
      (targetSentence4, sourceSentence4),
      (targetSentence5, sourceSentence5),
      (targetSentence6, sourceSentence6)

    )

    val model = new IBMModel2(corpus, 1000)

    val trainedModel = model.train

    val aligmentResult = Alignment.viterbiAlignment(
                             List[String]("chicken","good","sweet", "is", "watermelon", "eats"),
                              List[String]("el","pollo","es", "bueno"),

                               trainedModel._1 ,trainedModel._2

    )



    aligmentResult.foreach{ case(k,v) =>

    println(k+"-->"+v)

    }

  }

}
