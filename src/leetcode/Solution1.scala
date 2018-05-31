package leetcode

/**
 * Created by Ming on 2018/5/20.
 */
object Solution1 {
  def main(args:Array[String]):Unit = {
    var x = findMedianSortedArrays(Array[Int](6),Array[Int](3,4,8))
    println(reverse(-123456))
  }

  /**
    *
    */
  def myAtoi(str: String): Int = {
    return 0;
  }

  /** Reverse Integer
    * Given a 32-bit signed integer, reverse digits of an integer.
    *
    * Assume we are dealing with an environment which could only store integers within the 32-bit signed integer range:
    * [−231,  231 − 1]. For the purpose of this problem, assume that your function returns 0 when the reversed integer overflows.
    */
  def reverse(x: Int): Int = {
    if(x > -10 && x < 10)
        return x;
    var str = x.toString
    var fuhao:Char = 0;
    while((str.length-1) == str.lastIndexOf("0")){
      str = str.substring(0,str.length-1)
    }
    if(str.startsWith("-") || str.startsWith("+")){
      fuhao = str.charAt(0)
      str = str.substring(1,str.length)
    }
    str = str.reverse
    if(fuhao != 0)
      str = fuhao.toString + str

    str.reverse
    val L:Long = str.toLong
    if(L < Integer.MIN_VALUE || L > Integer.MAX_VALUE)
      return 0;

    return str.toInt;
  }

  /** ZigZag Conversion
    * The string "PAYPALISHIRING" is written in a zigzag pattern on a given number of rows like this:
    * (you may want to display this pattern in a fixed font for better legibility)
P   A   H   N
A P L S I I G
Y   I   R
    * And then read line by line: "PAHNAPLSIIGYIR"
    *
    * eg2. s = "PAYPALISHIRING", numRows = 4
P     I    N
A   L S  I G
Y A   H R
P     I
    *
    * Output: "PINALSIGYAHRPI"
    */
  //将其转换成横的形式，方便存储，即将其反转过来，以eg.2为例，第一行变成PAYP
  def convert(s: String, numRows: Int): String = {
    if(s == null || s.length == 0)
      return "";
    if(numRows == 0 || numRows == 1)
      return s;
    var list = scala.collection.mutable.ListBuffer[Array[Char]]();
    var i = 0;
    val sLen = s.length();
    var remainder = 0;
    while(i < sLen){
      var tem = new Array[Char](numRows);
      remainder = list.size % (numRows - 1);
      //需要填充一整行的情况，即锯齿的边
      if(remainder == 0){
        var j = 0;
        while(i < sLen && j < numRows){
          tem(j) = s.charAt(i);
          i += 1;
          j += 1;
        }

        i -= 1;
      }else{
        //只需填充一个的情况
        tem(numRows - 1 - remainder) = s.charAt(i);
      }
      list += tem;
      i += 1;
    }
    val sBuff = new StringBuffer();
    val listSize = list.size;
    i = 0;
    var j = 0;
    while(i < numRows){
      j = 0
      while(j < listSize){
        if(list(j)(i) > 0){
          sBuff.append(list(j)(i))
        }
        j += 1;
      }
      i += 1;
    }


    return sBuff.toString;
  }


  /** Median of Two Sorted Arrays
   *There are two sorted arrays nums1 and nums2 of size m and n respectively.

Find the median of the two sorted arrays. The overall run time complexity should be O(log (m+n)).
   */
  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
    //使用归并的思想解决此题
    import util.control.Breaks._
    if(nums1.length == 0 && nums2.length == 0)
      return 0;
    val nums1L = nums1.length;
    val nums2L = nums2.length;
    var result:Double = 0;
    //当两个数组加起来长度为奇数时
    if((nums1L+nums2L) % 2 != 0){
      val center:Int = (nums1L+nums2L) / 2;
      val tem:Array[Int] = new Array[Int](nums1L+nums2L);
      var fir = 0;
      var sec = 0;
      var i = 0;
      breakable(
        while(true){
          if(nums1L == 0 || nums2L == 0)
            break();
          if(nums1(fir) < nums2(sec)){
            tem(i) = nums1(fir)
            fir += 1;
          }else{
            tem(i) = nums2(sec);
            sec += 1;
          }
          if(i == center)
            return tem(i);
          i += 1;
          if(sec == nums2L || fir == nums1L)
            break();

        }
      )
      while(fir < nums1L){
        tem(i) = nums1(fir);
        if(i == center)
          return tem(i);
        i += 1;
        fir += 1;
      }
      while(sec < nums2L){
        tem(i) = nums2(sec);
        if(i == center)
          return tem(i);
        i += 1;
        sec += 1;
      }

    }else{
      val center:Int = (nums1L+nums2L) / 2;
      val tem:Array[Int] = new Array[Int](nums1L+nums2L);
      var fir = 0;
      var sec = 0;
      var i:Int = 0;
      breakable(
        while(true){
          if(nums1L == 0 || nums2L == 0)
            break();
          if(nums1(fir) < nums2(sec)){
            tem(i) = nums1(fir)
            fir += 1;
          }else{
            tem(i) = nums2(sec);
            sec += 1;
          }
          if(i == center) {
            val d:Double = (tem(i) + tem(i - 1))
            return d / 2
          }
          i += 1;
          if(sec == nums2L || fir == nums1L)
            break();

        }
      )
      while(fir < nums1L){
        tem(i) = nums1(fir);
        if(i == center) {
          val d:Double = (tem(i) + tem(i - 1))
          return d / 2
        }
        i += 1;
        fir += 1;
      }
      while(sec < nums2L){
        tem(i) = nums2(sec);
        if(i == center) {
          val d:Double = (tem(i) + tem(i - 1))
          return d / 2
        }
        i += 1;
        sec += 1;
      }
    }

    return 0;
  }


  /** Longest Substring Without Repeating Characters
    *
Given a string, find the length of the longest substring without repeating characters.
Examples:
Given "abcabcbb", the answer is "abc", which the length is 3.
Given "bbbbb", the answer is "b", with the length of 1.
Given "pwwkew", the answer is "wke", with the length of 3. Note that the answer must be a substring, "pwke" is a subsequence and not a substring.
   * https://leetcode.com/problems/longest-substring-without-repeating-characters/description/
   *
   * @param s
   * @return
   */
  def lengthOfLongestSubstring(s: String): Int = {
    if(s == None || s.length == 0)
      return 0;
    //用一个数组来存储字符的位置，下标即为字符，值为字符位置
    val array = new Array[Int](128);
    var arr = array.map(_ - 1);
    var i = 0;
    var sBuff = new StringBuffer();
    val list = scala.collection.mutable.ListBuffer[String]();
    for( i <- 0 to (s.length-1)){
      var char = s.charAt(i);
      //若值为-1，则非重复，只需加入到sBuff即可
      if(arr(char) == -1){
        arr(char) = i;
        sBuff.append(char);
        //若不是-1，则表示是重复的字符，需要将sBuff分成两部分，前半部分为重复字符以前，将其array中的值都复位，后半部分不变
      }else{
        list += sBuff.toString;
        var dup = sBuff.indexOf(char.toString);
        //前部分
        var before = sBuff.substring(0,dup);
        //后部分
        var rear = sBuff.substring(dup+1);
        for(c <- before)
          arr(c) = -1;

        sBuff = new StringBuffer(rear)
        sBuff.append(char)
        arr(char) = i;
      }
    }
    list += sBuff.toString
    var max = 0;
    var result = "";
    for( str <- list){
//      println(str)
      if(str.length > max) {
        result = str;
        max = result.length;
      }
    }
    return max;
  }


}
