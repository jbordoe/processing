import scala.collection.mutable.Queue

val width = 700
val height = 700

size(width, height)
smooth

background(0)

val grav = 10

val rand = new scala.util.Random()

val trailLength = 15
val trailStrength = 0.3

val maxSpd = 12

object Source {
  var x: Int = width/2
  var y: Int = height/2
  
  var power: Double = 0.0
  
  var alive = true
  
  var lifetime = 50.0
  
  def go() = {
    if (alive) {
      power += 1/lifetime
    } else {
      power -= 1/lifetime
    }
    
     
    if (power > 1) {
      alive = false
    }
    if (power < 0) {
      alive = true
      this.move( rand.nextInt(width), rand.nextInt(height) )
    }
  }
 
  def move(newX: Int, newY: Int) = {
    this.x = newX
    this.y = newY
    alive = true
    lifetime = (rand.nextDouble * 100) + 20
  }
  
  def render {
    fill (255.0 * power, 255.0 * power)
    strokeWeight(0)
    ellipse(this.x, this.y, 30*power, 30*power)
    //ellipse(this.x, this.y, 50, 50)
  }

}


class Speck (var x: Double, var y: Double) {
  
 var acc = new PVector(rand.nextInt(10), rand.nextInt(10))
 var spd = new PVector(rand.nextInt(10), rand.nextInt(10))
 
 val diam = rand.nextInt(3)+1
 
 val r = rand.nextInt(155) + 100
 val g = rand.nextInt(155) + 100
 val b = rand.nextInt(155) + 100
 
 val trails = new Queue[PVector]
 
 def go() = {

   // WTF no pow() function !? 
   var dx = this.x - Source.x
   var dy = this.y - Source.y
   
   var dist = sqrt( dx*dx + dy*dy )/(Source.power+0.01)
   

   acc.x = -dx/dist
   acc.y = -dy/dist
   
   
   if (spd.x > maxSpd) { spd.x = maxSpd } 
   if (spd.y > maxSpd) { spd.y = maxSpd }
   if (spd.x < -maxSpd) { spd.x = -maxSpd } 
   if (spd.y < -maxSpd) { spd.y = -maxSpd } 
   
   spd.x += acc.x
   spd.y += acc.y
   
   x += spd.x
   y += spd.y
   
   if (x > width) {
     x = 2*width - x
     spd.x = -spd.x*0.5
     spd.y += rand.nextDouble*2 - 1
   }
   if (y > height) {
     y = 2*height - y
     spd.y = -spd.y*0.5
     spd.x += rand.nextDouble*2 - 1
   }
   if (x < 0) {
     x = -x
     spd.x = -spd.x*0.5
     spd.y += rand.nextDouble*2 - 1
    }
   if (y < 0) {
     y = -y
     spd.y = -spd.y*0.5
     spd.x += rand.nextDouble*2 - 1
   }
   
 }
 
 def render() = {
   fill(this.r,this.g,this.b)
   ellipse(x, y, diam, diam)
   
   if(trails.length > trailLength) {
     trails.dequeue     
   } 
   var opacity = 0.0
   
   var previousX = this.x
   var previousY = this.y
   trails.enqueue(new PVector(this.x, this.y))
   for (trail <- trails.toList) {

     
     opacity += (255*trailStrength)/trails.length
     stroke(this.r,this.g,this.b,opacity)
     strokeWeight(this.diam * (opacity/255) )
     line(trail.x, trail.y, previousX, previousY)
     
     previousX = trail.x
     previousY = trail.y
   }
   
 }

}

var specks: Array[Speck] = Array[Speck]()

for( a <- 1 to 150) {
  specks = specks ++ Array(new Speck(rand.nextInt(width), rand.nextInt(height) ) )
}

def draw() {

 //noStroke()
 //fill(0, 0, 0, 30)
 // rect(0, 0, width, height)
  
  background(0,0,0,0.1)
  
  specks.map(x => x.go() )
  specks.map(x => x.render() )
  
  Source.go()
  Source.render  
  
}
