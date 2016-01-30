package com.lewisvail3.grep

import org.scalatest._
import Matchers._
//import org.junit.runner.RunWith
//import org.scalatest.junit.JUnitRunner

//@RunWith(classOf[JUnitRunner])
class GrepSpec extends FlatSpec {
  
  val TEST_FILE = "src/test/resources/greptest.txt"
  val PATTERN = "lewisvail3"
  
  "Grep.grepFile" should "return lines 1, 6, 8, and 20" in {
    val results = Grep.grepFile(TEST_FILE, PATTERN)
    val expected = List("line1 - lewisvail3", "line6 - lewisvail3", "line8 - lewisvail3",
        "line20 - lewisvail3")
    expected should equal (results)
  }
  
  "Grep.grepFile -A 1" should "return lines 1 & 2, --, lines 6 & 7, --, " +
      "lines 8 & 9, --, line 20" in {
    val results = Grep.grepFile(TEST_FILE, PATTERN, 1)
    val expected = List("line1 - lewisvail3", "line2", "--", "line6 - lewisvail3", "line7", "--",
        "line8 - lewisvail3", "line9", "--", "line20 - lewisvail3")
    expected should equal (results)
  }
  
  "Grep.grepFile -A 1 w/file names" should "return PATH:lines 1 & 2, --, " +
      "PATH:lines 6 & 7, --, PATH:lines 8 & 9, --, PATH:line 20" in {
    val results = Grep.grepFile(TEST_FILE, PATTERN, 1, true)
    val expected = List(TEST_FILE + ":line1 - lewisvail3", TEST_FILE + ":line2",
        "--", TEST_FILE + ":line6 - lewisvail3", TEST_FILE + ":line7", "--",
        TEST_FILE + ":line8 - lewisvail3", TEST_FILE + ":line9", "--",
        TEST_FILE + ":line20 - lewisvail3")
    expected should equal (results)
  }
  
  "Grep.grepFile -A 2" should "return lines 1, 2, 6, 7, 8, 9, 10, 20" in {
    val results = Grep.grepFile(TEST_FILE, PATTERN, 2)
    val expected = List("line1 - lewisvail3", "line2", "line3", "--", "line6 - lewisvail3",
        "line7", "line8 - lewisvail3", "line9", "line10", "--", "line20 - lewisvail3")
    expected should equal (results)
  }
  
}
