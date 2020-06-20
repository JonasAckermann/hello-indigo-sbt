package io.indigo.pluto

import indigo.{AssetType, _}
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("IndigoGame")
object HelloIndigo extends IndigoSandbox[Unit, Model] {

  val config: indigo.GameConfig =
    GameConfig.default.withMagnification(magnification).withViewport(width, height).withClearColor(ClearColor.fromRGB(111, 111, 111))

  val animations: Set[indigo.Animation] =
    Set()

  val plutoAssetName = AssetName("pluto")
  val lineAssetName = AssetName("centerStrip")
  val passengerAssetName = AssetName("passenger")
  val deliveryAssetName = AssetName("delivery")

  val assets: Set[indigo.AssetType] =
    Set(
      AssetType.Image(AssetName("pluto"), AssetPath("assets/pluto.png")),
      AssetType.Image(AssetName("centerStrip"), AssetPath("assets/line.png")),
      AssetType.Image(AssetName("passenger"), AssetPath("assets/robert.png")),
      AssetType.Image(AssetName("delivery"), AssetPath("assets/delivery.png"))
    )

  val fonts: Set[indigo.FontInfo] =
    Set()

  def setup(
      assetCollection: indigo.AssetCollection,
      dice: indigo.Dice
  ): indigo.Startup[indigo.StartupErrors, Unit] =
    Startup.Success(())

  def initialModel(startupData: Unit): Model =
    Model.initial(
      config.viewport.giveDimensions(magnification).center
    )

  def updateModel(
      context: indigo.FrameContext,
      model: Model
  ): indigo.GlobalEvent => indigo.Outcome[Model] = {
    case KeyboardEvent.KeyDown(Keys.LEFT_ARROW) =>
      val change = model.pluto.velocity match {
        case a if a == 0  => -3
        case c if c > -5  => -2
        case _ => -1
      }
      Outcome(model.update(change, false))

    case KeyboardEvent.KeyDown(Keys.RIGHT_ARROW) =>
      val change = model.pluto.velocity match {
        case a if a == 0  => 3
        case c if c < 5  => 2
        case _ => 1
      }
      Outcome(model.update(change, false))

    case KeyboardEvent.KeyDown(Keys.SHIFT) =>
      Outcome(model.update(0, true))

    case FrameTick =>
      val change = if(model.ftct % 10 == 0) {
        model.pluto.velocity match {
        case a if a > 0  => -1
        case c if c < 0  => 1
        case _ => 0
        }
      } else 0
      Outcome(model.update(change, false))

    case _ =>
      Outcome(model)
  }

  def present(
      context: indigo.FrameContext,
      model: Model
  ): indigo.SceneUpdateFragment =
    SceneUpdateFragment(
    ).addGameLayerNodes(
      drawScene(model.pluto, model.lines, model.passenger , model.deliveryStop)
    )

  def drawScene(
      pluto: Pluto,
      lines: List[CenterStrip],
      passenger: Passenger,
      delivery: DeliveryStop
  ): List[Graphic] = {

    lines.map(line =>Graphic(Rectangle(0, 0, 10, 50), 1, Material.Textured(lineAssetName)).withTint(255,255,255)
      .moveTo(line.location)) ++
    List(Graphic(Rectangle(0, 0, 20, 60), 1, Material.Textured(passengerAssetName))
      .moveTo(passenger.drawLocation)) ++
    List(Graphic(Rectangle(0, 0, 150, 150), 1, Material.Textured(deliveryAssetName))
        .moveTo(delivery.location)) ++
    List(Graphic(Rectangle(0, 0, 120, 75), 1, Material.Textured(plutoAssetName))
      .withRef(60, 38)
      .moveTo(pluto.location))
  }

}

case class Model(pluto: Pluto, lines: List[CenterStrip], passenger: Passenger, deliveryStop: DeliveryStop, ftct: Int) {
  def update(acc: Int, pickupAttempt: Boolean): Model = {
    val newPluto = pluto.update(acc)
    val newLines = lines.map(_.update(ftct))
    val newPassenger = passenger.update(pluto.location, pickupAttempt)
    val newDelivery = deliveryStop.update
    this.copy(newPluto, newLines, newPassenger, newDelivery, ftct = (ftct % 10) + 1 )
  }
}
object Model {
  def initial(center: Point): Model = Model(Pluto(center, 0), (0 to 970 by 88).map(y => CenterStrip(Point(center.x, y), - gamespeed)).toList, Passenger.initial(center.x - 100), DeliveryStop(Point(center.x, center.y)),  0)}
case class Pluto(location: Point, velocity: Int) {
  def update(acc: Int): Pluto = {
    this.copy(location = Point(location.x + velocity, location.y), velocity = velocity + acc)
  }
}
case class CenterStrip(location: Point, velocity: Double){
  def update(ftct: Int): CenterStrip = {
    val (newLocation: Double, velocityChange: Double) = location.y match {
      case a if a <= -88 => (height, 0D)
      case _ if (ftct % 10 == 0) => (location.y + velocity, 0D) //TODO fix
      case _ => (location.y + velocity, 0D)
    }
    this.copy(location = Point(location.x, Math.floor(newLocation).toInt),  velocity = velocity + velocityChange)
  }
}

case class Passenger(state: Pickup, location: Point, drawLocation: Point){
  def update(plutoPos: Point, button: Boolean): Passenger ={
    val (newState, newLocation, newDrawLocation) = plutoPos match {
      // higher than view, reset
      case _ if location.y < - 25 => Passenger.placeRandom
      // higher than pluto, maybe missed
      case a if a.y > (location.y + 100) => state match {
        case Pickup.Success => (Pickup.Success, Point(location.x, location.y - gamespeed), Point(-100, -100))
        case _ => (Pickup.Missed, Point(location.x, location.y - gamespeed), Point(location.x, location.y - gamespeed))
      }
      // out of reach, still possible
      case a if a.y < (location.y - 20) => (Pickup.NotYet, Point(location.x, location.y - gamespeed), Point(location.x, location.y - gamespeed))
      // possible pickup, depending on position and button
      case l if l.x >= location.x - 20 && l.x <= location.x + 95 && state != Pickup.Success => button match {
        // picked up, hide
        case true => (Pickup.Success, Point(location.x, location.y - gamespeed), Point(-100, -100))
        // not picked up, try again
        case false => (Pickup.NotYet, Point(location.x, location.y - gamespeed), Point(location.x, location.y - gamespeed))
      }
      // don't override success state
      case _ if state == Pickup.Success => (Pickup.Success, Point(location.x, location.y - gamespeed), Point(-100, -100))
      case _ => (Pickup.NotYet, Point(location.x, location.y - gamespeed), Point(location.x, location.y- gamespeed))
    }

    this.copy(newState, newLocation, newDrawLocation)

  }
}
object Passenger {
  def initial(xLoc: Int) = {
    val (state, loc, dloc) = reset(xLoc)
    Passenger(state, loc, dloc)
  }
  def reset(xLoc: Int) = (Pickup.NotYet, Point(xLoc, height), Point(xLoc, height))
  def placeRandom = {
    val xLoc = scala.util.Random.between(10, width-10)
    (Pickup.NotYet, Point(xLoc, height), Point(xLoc, height))
  }
}

case class DeliveryStop(location: Point){
  val update = this.copy(Point(location.x, location.y - gamespeed))
}
object DeliveryStop {
  //def initial(xLoc: Int) = DeliveryStop(Point(xLoc, height))
  //def randomX = scala.util.Random.between(10, width-10)
}

sealed trait Pickup
object Pickup {
  case object Missed extends Pickup
  case object Success extends Pickup
  case object NotYet extends Pickup
  case object Delivered extends Pickup
}