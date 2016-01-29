package com.lewisvail3.grep

import org.scalatest._
import Matchers._
//import org.junit.runner.RunWith
//import org.scalatest.junit.JUnitRunner

//@RunWith(classOf[JUnitRunner])
class GrepSpec extends FlatSpec {
  
  val TEST_FILE = "src/test/resources/greptest.txt"
  val PATTERN = "lewisvail3"
  
  "Grep.grepFile" should "return lines 6 and 8" in {
    val results = Grep.grepFile(TEST_FILE, PATTERN)
    val expected = List("line6 - lewisvail3", "line8 - lewisvail3")
    expected should equal (results)
  }
  
  "Grep.grepFile -A 1" should "return lines 6 & 7, --, lines 8 & 9" in {
    val results = Grep.grepFile(TEST_FILE, PATTERN, 1)
    val expected = List("line6 - lewisvail3", "line7", "--", "line8 - lewisvail3", "line9")
    expected should equal (results)
  }
  
  "Grep.grepFile -A 1 w/file names" should "return PATH:lines 6 & 7, --, PATH:lines 8 & 9" in {
    val results = Grep.grepFile(TEST_FILE, PATTERN, 1, true)
    val expected = List(TEST_FILE + ":line6 - lewisvail3", TEST_FILE + ":line7", "--",
        TEST_FILE + ":line8 - lewisvail3", TEST_FILE + ":line9")
    expected should equal (results)
  }
  
  "Grep.grepFile -A 2" should "return lines 6, 7, 8, 9, 10" in {
    val results = Grep.grepFile(TEST_FILE, PATTERN, 2)
    val expected = List("line6 - lewisvail3", "line7", "line8 - lewisvail3", "line9", "line10")
    expected should equal (results)
  }
  
}
