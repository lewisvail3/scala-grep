package com.lewisvail3.grep

import io.Source
import util.matching.Regex
import java.io.FileNotFoundException
import scala.annotation.tailrec

object Grep {

  private val SEPARATOR = "--"

  private def printUsage() {
    // Full usage print out
    //    println("usage: grep [-abcDEFGHhIiJLlmnOoqRSsUVvwxZ] [-A num] [-B num] [-C[num]]")
    //    println("\t[-e pattern] [-f file] [--binary-files=value] [--color=when]")
    //    println("\t[--context[=num]] [--directories=action] [--label] [--line-buffered]")
    //    println("\t[--null] [pattern] [file ...]")

    // implemented usage print out
    println("usage: grep [-A num] [pattern] [file ...]")
  }

  def main(args: Array[String]) {

    if (args.length == 0) {
      printUsage()
    } else {

      // parse args
      var pattern = "";
      var fileNames: List[String] = Nil
      def nextOption(map: Map[Symbol, String], list: List[String]): Map[Symbol, String] = {
        list match {
          case Nil => map
          case "-A" :: value :: tail =>
            nextOption(map ++ Map('linesAfterMatch -> value), tail)
          // TODO: uncomment these as they become implemented
          // case "-B" :: value :: tail =>
          //   nextOption(map ++ Map('linesBeforeMatch -> value), tail)
          case option :: tail if option.startsWith("-") =>
            Console.err.println("option " + option + " is not supported")
            throw new IllegalArgumentException
          case patternArg :: tail if pattern == "" =>
            pattern = patternArg
            nextOption(map, tail)
          case filename :: tail =>
            fileNames = fileNames :+ filename
            nextOption(map, tail)
        }
      }

      try {
        var options = nextOption(Map(), args.toList)

        val linesAfterMatch = options.getOrElse('linesAfterMatch, "0")
        fileNames.flatMap(filename => grepFile(filename, pattern,
            options.getOrElse('linesAfterMatch, "0").toInt, fileNames.size > 1))
          .foreach(println)
      } catch {
        case ex: IllegalArgumentException => System.exit(1)
      }
    }
  }
  
  def grepFile(filename: String, pattern: String) : List[String] = {
    grepFile(filename, pattern, false)
  }
  
  def grepFile(filename: String, pattern: String, linesAfterMatch: Int) : List[String] = {
		  grepFile(filename, pattern, linesAfterMatch, false)
  }

  def grepFile(filename: String, pattern: String, prependFilename: Boolean) : List[String] = {
    grepFile(filename, pattern, 0, prependFilename)
  }

  def grepFile(filename: String, pattern: String, linesAfterMatch: Int, 
      prependFilename: Boolean): List[String] = {
    
    def isMatch(line: String) = pattern.r().findFirstIn(line).nonEmpty

    def decorateLine(line: String) = {
      if (prependFilename) {
        filename + ":" + line
      } else {
        line
      }
    }

    @tailrec
    def filterLines(fileStream: Stream[String], matches: List[String], linesToRead: Int, printBreak: Boolean): List[String] = {
      fileStream match {
        case line #:: tail if isMatch(line) && printBreak =>
          filterLines(line +: tail, matches :+ SEPARATOR, linesAfterMatch, false)
        case line #:: tail if isMatch(line) =>
          filterLines(tail, matches :+ decorateLine(line), linesAfterMatch, false)
        case line #:: tail if linesToRead > 0 =>
          filterLines(tail, matches :+ decorateLine(line), linesToRead - 1, linesToRead == 1)
        case line #:: tail =>
          filterLines(tail, matches, 0, printBreak)
        case Stream.Empty => matches
      }
    }
    
    try {
      val source = Source.fromFile(filename)
      try {
        
        
        filterLines(source.getLines().toStream, Nil, 0, false)
      } finally {
        source.close()
      }
    } catch {
      case ex: FileNotFoundException => {
        Console.err.println(filename + ": No such file or directory")
        Nil
      }
    }
  }
}
