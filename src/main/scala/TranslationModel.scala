import idio.spotlight.Corpus

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

/**
 * @author David Przybilla david.przybilla@idioplatform.com
 **/


class TranslationModel(pathToAligmentFile : String){


  val translationsProbabilities = {

     scala.io.Source.fromFile(pathToAligmentFile).getLines().map{

      line: String =>
           val splitLine = line.split("\t")
           val targetWord = splitLine(0)
           val sourceWord = splitLine(1)
           val probability = splitLine(2).toDouble
           ((targetWord, sourceWord) , probability)
    }.toMap
  }


  def translationProbability(source: String, target: String): Double ={

     // tokenize
     val tokenizedSource = Corpus.sentenceTokenizer(source)
     val tokenizedTarget = Corpus.sentenceTokenizer(target)
     val normalizationConstant = 1.0

     val factor =  normalizationConstant/ math.pow( (tokenizedTarget.size + 1), tokenizedSource.size)


     val sumOfScores = tokenizedTarget.par.map{
         targetToken: String =>
            tokenizedSource.par.map{
                sourceToken: String =>
                  translationsProbabilities.getOrElse((targetToken, sourceToken), 0.0)
            }.sum
     }.toList

    val mutiplicationOfProbabilities = sumOfScores.foldLeft(1.0){ (total, n) => total * n }


   val score = factor * mutiplicationOfProbabilities

   score

  }

}
