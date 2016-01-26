package com.lewisvail3.grep

import org.scalatest._

class GrepSpec extends FlatSpec {
  
  val TEST_FILE = "src/test/resources/greptest.txt"
  
  "Grep.grepFile" should "return lines 6 and 8" in {
    val results = Grep.grepFile(TEST_FILE, "lewisvail3")
    assert(results.size === 2);
  }
  
}
