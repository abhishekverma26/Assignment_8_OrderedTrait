import org.scalatest.funsuite.AnyFunSuite

class SetsTest extends AnyFunSuite {

  test("Test contains for EmptySet") {
    val set1 = new EmptySet[NewSet]
    assert(!set1.contains(NewSet(1)))
  }

  test("Test incl to insert elements in EmptySet") {
    val set1 = new EmptySet[NewSet].incl(NewSet(1))
    assert(set1.contains(NewSet(1)))
  }

  test("Test contains for Set that does not contain the element") {
    val set1 = new EmptySet[NewSet].incl(NewSet(1)).incl(NewSet(2))
    assert(!set1.contains(NewSet(3)))
  }

  test("Test incl for inserting elements in NonEmptySet") {
    val set1= new EmptySet[NewSet]
    val set2 = new NonEmptySet[NewSet](NewSet(1),set1.incl(NewSet(2)),set1.incl(NewSet(3)))

    val set3 = set2.incl(NewSet(4)).incl(NewSet(8))
    assert(set3.contains(NewSet(8)))
  }

}