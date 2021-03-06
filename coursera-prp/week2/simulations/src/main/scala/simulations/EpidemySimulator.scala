package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation

    val prevalenceRate = 0.01
    val transRate = 0.4
    val dieRate = 0.25

    //period after infected
    val incubationTime = 6
    val dieTime = 14
    val immuneTime = 16
    val healTime = 18

    //move in rounds
    val moveInTime = 5
  }

  import SimConfig._

  case class Room(row: Int, col: Int)

  val persons: List[Person] = {

    val totalInfected = population * prevalenceRate

    val range = 1 to population

    def manifestPerson(id: Int) = {
      val p = new Person(id)
      if (id <= totalInfected) p.setInfected()
      p.scheduleMove()
      p
    }
    
    def buildPersonList : List[Person] = {
      val sample = range map (id => { manifestPerson(id) })
      sample.toList
    }

    buildPersonList
  }

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //

    private var actions: List[Action] = List()

    def resetToHeal() = {
      infected = false
      sick = false
      immune = false
      dead = false
    }

    def moveTo(room: Room) = {
      row = room.row
      col = room.col

      if (!infected && random <= transRate) setInfected()
    }

    def addAction(a: Action) {
      actions = a :: actions
      a()
    }

    def setInfected() = {
      def infectedAction(): Unit = {
        infected = true
        afterDelay(incubationTime) {
          sick = true
        }
        afterDelay(dieTime) {
          if (random.toDouble <= dieRate) dead = true
        }
        afterDelay(immuneTime) {
          if (!dead) {
            immune = true
            sick = false
          }
        }
        afterDelay(healTime) {
          if (!dead) resetToHeal()
        }
      }

      this.addAction(infectedAction)
    }

    def scheduleMove() = {
      //Action to move in 1-5 days
      //At that time, choose a random neighbor room
      //If surrounded by infected rooms, then add action of next move
      def moveAction(): Unit = {
        val moveInDay = (random * moveInTime).toInt + 1
        afterDelay(moveInDay) {
          if (!dead) {
            val randomNeighbor = unInfectedRoom(neighbors(Room(row, col)))
            randomNeighbor match {
              case Some(r) => moveTo(r)
              case None =>
            }
            this.addAction(moveAction)
          }
        }
      }

      this.addAction(moveAction)
    }

    def visiblyInfected = sick || dead

    def inRoom(room: Room) = this.row == room.row && this.col == room.col
  }

  //Is there anybody visibly infected in room (row, col)?
  def infectedInRoom(room: Room) = persons.exists(p => p.inRoom(room) && p.visiblyInfected)

  //Is there anybody in incubation period
  def incubatedInRoom(room: Room) = persons.exists(p => p.inRoom(room) && p.infected)

  //Find 4 neighbor rooms
  def neighbors(room: Room): List[Room] = {
    val left = (room.col - 1 + roomRows) % roomRows
    val right = (room.col + 1) % roomRows
    val top = (room.row - 1 + roomColumns) % roomColumns
    val bottom = (room.row + 1) % roomColumns
    for {
      c <- List(left, right)
      r <- List(top, bottom)
    } yield Room(r, c)
  }

  //Randomly return a not infected room, None if no such room
  def unInfectedRoom(rooms: List[Room]): Option[Room] = rooms match {
    case Nil => None
    case rs => {
      val rid = (random * rs.length).toInt
      val rm = rooms(rid)
      if (!infectedInRoom(rm)) Some(rm)
      else unInfectedRoom(rooms diff List(rm))
    }
  }
}
