package com.lewisvail3.grep

import io.Source
import util.matching.Regex
import java.io.FileNotFoundException

object Grep {

  private def printUsage() {
    // Full usage print out
    //    println("usage: grep [-abcDEFGHhIiJLlmnOoqRSsUVvwxZ] [-A num] [-B num] [-C[num]]")
    //    println("\t[-e pattern] [-f file] [--binary-files=value] [--color=when]")
    //    println("\t[--context[=num]] [--directories=action] [--label] [--line-buffered]")
    //    println("\t[--null] [pattern] [file ...]")

    // implemented usage print out
    println("usage: grep [pattern] [file ...]")
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
          // TODO: uncomment these as they become implemented
          // case "-A" :: value :: tail =>
          //   nextOption(map ++ Map('linesAfterMatch -> value), tail)
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

        fileNames.flatMap(filename => grepFile(filename, pattern, fileNames.size > 1))
          .foreach(println)
      } catch {
        case ex: IllegalArgumentException => System.exit(1)
      }
    }
  }

  def grepFile(filename: String, pattern: String, prependFilename: Boolean) : List[String] = {
    try {
      val source = Source.fromFile(filename)
      try {
        source.getLines().filter(line => pattern.r().findFirstIn(line).nonEmpty)
          .map(line => if (prependFilename) filename + ":" + line else line)
          .toList
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
