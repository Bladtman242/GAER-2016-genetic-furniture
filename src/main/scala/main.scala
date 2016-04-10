package main.scala
import scala.util.Random
import genetic._
import util.Functional.{nApply}

object GeneticFurniture {
  //A dummy-main that runs a GA on a stupid problem
  def main (args: Array[String]): Unit = {
    val env = new Environment.FindAs()
    val initPop = env.seedPop(100)

    print("Best of initial population (value, genome): ")
    println(initPop.map((x :List[Char]) => (env.fitness(x), x)).maxBy(_._1))

    val finalPop = nApply(initPop, env.evoProcess, 200)

    print("Best of final population (value, genome): ")
    println(finalPop.map((x :List[Char]) => (env.fitness(x), x)).maxBy(_._1))
  }
}
