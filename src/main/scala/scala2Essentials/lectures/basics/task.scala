package scala2Essentials.lectures.basics

object task extends App {

  def countChange(sum: Int, nums: List[Int]): Int =
    if (sum == 0) 1
    else if (sum < 0) 0
    else if (nums.isEmpty) 0
    else countChange(sum - nums.head, nums) + countChange(sum, nums.tail)
  println(countChange(4, List(1, 5)))
}