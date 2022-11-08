object Main {

  def merge_sort(lst: List[Double]): List[Double] = {

    
    
    def merge(l: List[Double], r: List[Double]): List[Double] =
      (l, r) match {
        case (l, Nil)  => l
        case (Nil, r) => r
        
        case ( lh::lt , rh :: rt) =>
          if ( lh < rh)  lh :: merge(r, lt)
          else rh :: merge(l, rt)
      }

    lst match {
      case Nil      => Nil
      case h :: Nil => List(h)
      case h :: t =>
        val (l, r) = lst splitAt lst.length / 2
        merge(merge_sort(l), merge_sort(r))

    }

  }

  def selection_sort(lst: List[Double]): List[Double] = {
    
    //uses list functions to avoid messy pattern matching used in past for selection sort
    
    val min = lst.min
    val temp = lst.filter(_ > min)
    
    lst match {
      case Nil      => Nil
      case h :: Nil => List(h)
      case h :: t   => min::selection_sort(temp)
      
    }

  }

  def insertion_sort(lst: List[Double]): List[Double] = {
    
    

    def insert(n: Double, list: List[Double]): List[Double] =
      list match {
        case Nil => List(n)
        case h :: t =>
          if (n < h) n :: list
          else h :: (insert(n, t))

      }

    lst match {
      case Nil      => Nil
      case h :: Nil => List(h)
      case h :: t   => insert(h, insertion_sort(t))

    }
  }

  def main(args: Array[String]): Unit = {
    val lst = List(5.1, 4.2, 11.0, 2.3, 3.5, 1.5, 0.6, 9.7)
    println(merge_sort(lst))
    println(selection_sort(lst))
    println(insertion_sort(lst))
  }
}
