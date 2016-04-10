package genetic
import scala.util.Random
import util.Functional.{nApply, nList}

trait Genome[A] {
  def mutate(genome: A): A //random mutation (if supported?)
  def crossover (a: A, b: A): (A,A) // sexual reproduction (if supported?)
}

trait Environment[A] {
  val genome: Genome[A]
  def fitness(genome: A): Double
  def seedPop(n:Int): List[A]
  val evoProcess: (List[A]) => List[A]
}


object Genome {
  class PrintableStringGenome extends Genome[List[Char]] {
    def mutate(genome: List[Char]) : List[Char] = {
      val r = new Random()
      val idx = r.nextInt(genome.length)
      genome.updated(idx, r.nextPrintableChar())
    }
    def crossover(a: List[Char], b : List[Char]) : (List[Char], List[Char]) = {
      val r = new Random()
      val idx = r.nextInt(a.length)
      (a.take(idx) ++ b.drop(idx), b.take(idx) ++ a.drop(idx))
    }
  }
}

object Environment {
  class FindAs extends Environment[List[Char]] {
    val genome = new Genome.PrintableStringGenome()
    def fitness(g: List[Char]) = g.count(_.toUpper == 'A')
    def seedPop(n:Int): List[List[Char]] = {
      val r = new Random()
      nList(() => nList(() => r.nextPrintableChar(), 10), n)
    }

    val evoProcess = (pop : List[List[Char]]) => Genetic.constantSizeCrossover(pop, this, 5)
  }
}

object Genetic {
  // makes a new generation of (approx) the same size, using crossover on the best half of the population, and
  // mutations on mutPct percent of the new population.
  def constantSizeCrossover[A] (population: List[A], env: Environment[A], mutPct: Int) : List[A] = {
    //fitness
    val genFitList = population.map(x => (x,env.fitness(x)))

    //selection
    val genByFitness = genFitList.sortBy(_._2).map(_._1).reverse
    val numSurvivors = genByFitness.length/4
    val firstQuart = genByFitness.take(numSurvivors)
    val secondQuart = genByFitness.drop(numSurvivors).take(numSurvivors)

    //mutation
    val offspring : List[A] = firstQuart.zip(secondQuart.reverse)
      .map(x => env.genome.crossover _ tupled x)
      .flatMap(x=> List(x._1, x._2))

    val parrentsAndOffspring : List[A] = firstQuart ++ secondQuart ++ offspring
    val r = new Random()
    def mutatePop (pop: List[A]) : List[A] = {
      val idx = r.nextInt(pop.length)
      pop.updated(idx, env.genome.mutate(pop(idx)))
    }
    val newGen = nApply(parrentsAndOffspring, mutatePop, parrentsAndOffspring.length/(100/mutPct))
    newGen
  }
}
