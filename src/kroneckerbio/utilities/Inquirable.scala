/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.utilities

import kroneckerbio.utilities.RichAny._

object Inquirable {

  sealed trait Inquirable[+A] {
    def inquire[B >: A](converter: B => Boolean): Boolean

    def leaves: Seq[A]

    def replaceLeaves[B >: A, C](leafMap: Map[B, C]): Inquirable[C]

    def replaceLeavesBy[C](f: A => C) = replaceLeaves((leaves zip leaves.map(f)).toMap)

    def graftBranches[B >: A, C](leafMap: Map[B, Inquirable[C]]): Inquirable[C]

    def graftBranchesBy[C](f: A => Inquirable[C]): Inquirable[C] = graftBranches((leaves zip leaves.map(f)).toMap)

    def simplified: Inquirable[A]

    protected def is_single(element: Inquirable[Any]) = {
      element match {
        case
          InquirableLeaf(_) |
          True              |
          False             |
          And(Seq(_))       |
          Or(Seq(_))        => true
        case _              => false
      }
    }
  }

  object Inquirable {
    implicit def any2Inquirable[A](a: A) = InquirableLeaf[A](a)
  }


  final case class InquirableLeaf[+A](content: A) extends Inquirable[A] {
    def inquire[B >: A](converter: B => Boolean): Boolean = converter(content)

    def leaves: Seq[A] = Seq(content)

    def replaceLeaves[B >: A, C](leafMap: Map[B, C]): Inquirable[C] = InquirableLeaf[C](leafMap(content))

    def graftBranches[B >: A, C](leafMap: Map[B, Inquirable[C]]): Inquirable[C] = leafMap(content)

    def simplified = this

    override def toString = "" + content
  }


  final case class Not[+A](branch: Inquirable[A]) extends Inquirable[A] {
    def inquire[B >: A](converter: B => Boolean): Boolean = branch.inquire(converter)

    def leaves: Seq[A] = branch.leaves

    def replaceLeaves[B >: A, C](leafMap: Map[B, C]): Inquirable[C] = Not[C](branch.replaceLeaves(leafMap))

    def graftBranches[B >: A, C](leafMap: Map[B, Inquirable[C]]): Inquirable[C] = Not[C](branch.graftBranches(leafMap))

    def simplified = {
      branch.simplified match {
        case True => False
        case False => True
        case And(list) => Nand(list)
        case Nand(list) => And(list)
        case Or(list) => Nor(list)
        case Nor(list) => Or(list)
          // TODO: bug? case Not() needed?
        case simp => Not(simp)
      }
    }

    override def toString = {
      branch match {
        case _ if is_single(branch) => "!" + branch
        case _                      => "!(" + branch + ")"
      }
    }
  }


  final case class And[+A](list: Traversable[Inquirable[A]]) extends Inquirable[A] {
    def this(list: Inquirable[A]*) = this(list)

    def inquire[B >: A](converter: B => Boolean): Boolean = list.foldLeft(true)(_ && _.inquire(converter))

    def leaves: Seq[A] = list.flatMap(_.leaves).toSeq

    def replaceLeaves[B >: A, C](leafMap: Map[B, C]): Inquirable[C] = And[C](list.map(_.replaceLeaves(leafMap)))

    def graftBranches[B >: A, C](leafMap: Map[B, Inquirable[C]]): Inquirable[C] = And[C](list.map(_.graftBranches(leafMap)))

    def simplified = {
      // Simplify each branch and remove the True branches
      list.map(_.simplified).filterNot(_ == True) match {
        // Always true if all elements are true
        case Seq() => True

        // Always false if any elements are false
        case simp if(simp.exists(_ == False)) => False

        // Condense single element
        case Seq(element) => element

        // Otherwise, return the filtered list
        case simp => And(simp)
      }
    }

    override def toString =
      list.size match {
        case 0 => "" + False
        case _ => list.map(add_parentheses(_)).mkString("&")
      }

    private def add_parentheses(element: Inquirable[Any]): String = {
      element match {
        case _ if is_single(element) => "" + element
        case Xor(_,_)                => "" + element
        case _                       => "(" + element + ")"
      }
    }
  }

  object And {
    def apply[A](list: Inquirable[A]*): And[A] = And(list)
  }


  final case class Nand[+A](list: Traversable[Inquirable[A]]) extends Inquirable[A] {
    def this(list: Inquirable[A]*) = this(list)

    def inquire[B >: A](converter: B => Boolean): Boolean = !list.foldLeft(true)(_ && _.inquire(converter))

    def leaves: Seq[A] = list.flatMap(_.leaves).toSeq

    def replaceLeaves[B >: A, C](leafMap: Map[B, C]): Inquirable[C] = Nand[C](list.map(_.replaceLeaves(leafMap)))

    def graftBranches[B >: A, C](leafMap: Map[B, Inquirable[C]]): Inquirable[C] = Nand[C](list.map(_.graftBranches(leafMap)))

    def simplified = {
      list.map(_.simplified).filterNot(_ == True) match {
        // Always true if any elements are false
        case simp if(simp.exists(_ == False)) => True

        // Always false if all elements are true
        case Seq() => False

        // Condense single element
        case Seq(element) => Not(element)

        // Otherwise, filter for true and return
        case simp => Nand(simp)
      }
    }

    override def toString =
      list.size match {
        case 0 => "" + False
        case 1 => "!" + list.head.pipe(add_parentheses(_))
        case _ => "!(" + And(list) + ")"
      }

    def add_parentheses(element: Inquirable[Any]): String = {
      element match {
        case _ if is_single(element) => "" + element
        case _                       => "(" + element + ")"
      }
    }
  }

  object Nand {
    def apply[A](list: Inquirable[A]*): Nand[A] = Nand(list)
  }


  final case class Or[+A](list: Traversable[Inquirable[A]]) extends Inquirable[A] {
    def this(list: Inquirable[A]*) = this(list)

    def inquire[B >: A](converter: B => Boolean): Boolean = list.foldLeft(false)(_ || _.inquire(converter))

    def leaves: Seq[A] = list.flatMap(_.leaves).toSeq

    def replaceLeaves[B >: A, C](leafMap: Map[B, C]): Inquirable[C] = And[C](list.map(_.replaceLeaves(leafMap)))

    def graftBranches[B >: A, C](leafMap: Map[B, Inquirable[C]]): Inquirable[C] = Or[C](list.map(_.graftBranches(leafMap)))

    def simplified = {
      list.map(_.simplified).filterNot(_ == False) match {
        // Always true if any elements are true
        case simp if(simp.exists(_ == True)) => True

        // Always false if all elements are false
        case Seq() => False

        // Condense single element
        case Seq(element) => element

        // Otherwise, filter for false and return
        case simp => Or(simp)
      }
    }

    override def toString =
      list.size match {
        case 0 => "" + False
        case _ => list.map(add_parentheses(_)).mkString("|")
      }

    private def add_parentheses(element: Inquirable[Any]): String = {
      element match {
        case _ if is_single(element) => "" + element
        case Xor(_,_)                => "" + element
        case And(_)                  => "" + element
        case _                       => "(" + element + ")"
      }
    }
  }

  object Or {
    def apply[A](list: Inquirable[A]*): Or[A] = Or(list)
  }


  final case class Nor[+A](list: Traversable[Inquirable[A]]) extends Inquirable[A] {
    def this(list: Inquirable[A]*) = this(list)

    def inquire[B >: A](converter: B => Boolean): Boolean = !list.foldLeft(false)(_ || _.inquire(converter))

    def leaves: Seq[A] = list.flatMap(_.leaves).toSeq

    def replaceLeaves[B >: A, C](leafMap: Map[B, C]): Inquirable[C] = Nor[C](list.map(_.replaceLeaves(leafMap)))

    def graftBranches[B >: A, C](leafMap: Map[B, Inquirable[C]]): Inquirable[C] = Nor[C](list.map(_.graftBranches(leafMap)))

    def simplified = {
      list.map(_.simplified).filterNot(_ == False) match {
        // Always false if any elements are true
        case simp if(simp.exists(_ == True)) => False

        // Always true if all elements are false
        case Seq() => True

        // Condense single element
        case Seq(element) => Not(element)

        // Otherwise, filter for false and return
        case simp => Nor(simp)
      }
    }

    override def toString =
      list.size match {
        case 0 => "" + True
        case 1 => "!" + list.head.pipe(add_parentheses(_))
        case _ => "!(" + Or(list) + ")"
      }

    private def add_parentheses(element: Inquirable[Any]): String = {
      element match {
        case _ if is_single(element) => "" + element
        case _                       => "(" + element + ")"
      }
    }
  }

  object Nor {
    def apply[A](list: Inquirable[A]*): Nor[A] = Nor(list)
  }


  final case class Xor[+A](first: Inquirable[A], second: Inquirable[A]) extends Inquirable[A] {
    def inquire[B >: A](converter: B => Boolean): Boolean = first.inquire(converter) ^ second.inquire(converter)

    def leaves: Seq[A] = first.leaves ++ second.leaves

    def replaceLeaves[B >: A, C](leafMap: Map[B, C]): Inquirable[C] = Xor[C](first.replaceLeaves(leafMap), second.replaceLeaves(leafMap))

    def graftBranches[B >: A, C](leafMap: Map[B, Inquirable[C]]): Inquirable[C] = Xor[C](first.graftBranches(leafMap), second.graftBranches(leafMap))

    override def toString = add_parentheses(first) + "^" + add_parentheses(second)

    def simplified = {
      (first.simplified, second.simplified) match {
        case (True, other)  => simplified_one_already_true(other)
        case (other, True)  => simplified_one_already_true(other)
        case (False, other) => other
        case (other, False) => other
        case (simp1, simp2) => Xor(simp1, simp2)
      }
    }

    private def simplified_one_already_true[B >: A](other: Inquirable[B]) = {
      other match {
        case True => False
        case False => True
        case _ => Not(other)
      }
    }

    private def add_parentheses(element: Inquirable[Any]): String = {
      element match {
        case _ if is_single(element) => "" + element
        case _                       => "(" + element + ")"
      }
    }
  }


  case object True extends Inquirable[Nothing] {
    def inquire[B](converter: B => Boolean) = true

    def leaves: Seq[Nothing] = Seq()

    def replaceLeaves[B, C](leafMap: Map[B, C]) = True

    def graftBranches[B, C](leafMap: Map[B, Inquirable[C]]) = True

    def simplified = this

    override def toString = "true"
  }


  case object False extends Inquirable[Nothing] {
    def inquire[B](converter: B => Boolean) = false

    def leaves: Seq[Nothing] = Seq()

    def replaceLeaves[B, C](leafMap: Map[B, C]) = False

    def graftBranches[B, C](leafMap: Map[B, Inquirable[C]]) = False

    def simplified = this

    override def toString = "false"
  }
}
