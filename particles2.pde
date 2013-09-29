import scala.collection.mutable.Queue

val width = 400
val height = 400
size(width, height)

smooth
background(0)

val grav = 10

val rand = new scala.util.Random()

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
    fill (255, 255.0 * power)
    strokeWeight(0)
    ellipse(this.x, this.y, 30*power, 30*power)
  }
}


class Speck (var x: Double, var y: Double) {
  
 var acc = new PVector(rand.nextInt(10), rand.nextInt(10))
 var spd = new PVector(rand.nextInt(10), rand.nextInt(10))
 
 val diam = rand.nextDouble()*0.5
 
 val r = rand.nextInt(155) + 100
 val g = rand.nextInt(155) + 100
 val b = rand.nextInt(155) + 100
 
 var previousX = this.x
 var previousY = this.y
 
 def go() = {

   var dx = this.x - Source.x
   var dy = this.y - Source.y
   
   // WTF no pow() function?! 
   var dist = sqrt( dx*dx + dy*dy )/(Source.power+0.01)
   
   acc.x = -dx/dist
   acc.y = -dy/dist
   
   spd.x += acc.x
   spd.y += acc.y
   
   if (spd.x > maxSpd) { spd.x = maxSpd } 
   if (spd.y > maxSpd) { spd.y = maxSpd }
   if (spd.x < -maxSpd) { spd.x = -maxSpd } 
   if (spd.y < -maxSpd) { spd.y = -maxSpd } 
   
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
   stroke(this.r,this.g,this.b)
   strokeWeight(diam)
   line(this.x, this.y, previousX, previousY)

   previousX = this.x
   previousY = this.y
 }

}

var specks: Array[Speck] = Array[Speck]()

for( a <- 1 to 1000) {
  specks = specks ++ Array(new Speck(rand.nextInt(width), rand.nextInt(height) ) )
}

def draw() {
  //background(0)
  //tint(255,100)
  //image(buffer.get(), 0, 0)

  // Darken previous frame...
  noStroke()
  fill(0, 0, 0, 140)
  rect(0,0,width,height)  

  // ...copy it and blur it...
  val blurredFrame = get()  
  blurredFrame.filter(BLUR,1)

  specks.foreach(x => x.go() )
  specks.foreach(x => x.render() )
  
  Source.go()
  Source.render  
  
  // additive blend of blurred previous frame
  blend(blurredFrame, 0, 0, width, height, 0, 0, width, height, ADD)
}
