package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
      assert(weight(Leaf('a',1)) === 1)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton detect if one or more") {
    val llong = makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3)))
    val lshort = makeOrderedLeafList(List(('t', 2)))
    assert(!singleton(llong))
    assert(singleton(lshort))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
    assert(combine(Nil)==Nil )

  }
  test("decode short text should be identity") {
    new TestTrees {
      assert(decode(t1, List(0,0,1,0))===List('a','a','b','a'))
      //print(decode(frenchCode, secret))
    }
  }



  test("decode and encode a very short text should be identity") {
    new TestTrees {
      val x = List('t', 'u', 'r', 'e', ' ', 'f', 'r', 'o', 'm', ' ', '4', '5', ' ', 'B', 'C', ',', ' ', 'm', 'a', 'k', 'i', 'n', 'g', ' ', 'i', 't', ' ', 'o', 'v', 'e', 'r', ' ', '2', '0', '0', '0', ' ', 'y', 'e', 'a', 'r', 's', ' ', 'o', 'l', 'd', '.', ' ', 'R', 'i', 'c', 'h', 'a', 'r', 'd', ' ', 'M', 'c')
      val y = createCodeTree(x)
      encode(y)(x).foreach(x => print(x.toString))
      println("")
      quickEncode(y)(x).foreach(print)
      assert(decode(y, encode(y)(x.toList)) === x.toList)
      assert(decode(y, quickEncode(y)(x.toList)) === x.toList)
    }
  }

  test("create times list.") {
    new TestTrees {
      val x = times("Huffman with Scala".toList)
      val first = x.head
      val second = x.tail.head
      assert(first._1 == 'n')
      assert(first._2 == 1)
      assert(second._1 == 't')
      assert(second._2 == 1)
    }
  }

  test("Convert code table works correct.") {
    new TestTrees {
      val x = convert(t2)
      assert(x==List(('a',List(0, 0)), ('b',List(0, 1)), ('d',List(1))))
    }
  }

}
