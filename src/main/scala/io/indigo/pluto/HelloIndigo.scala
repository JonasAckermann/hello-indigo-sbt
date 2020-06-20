package io.indigo.pluto

import indigo.{AssetType, _}
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("IndigoGame")
object HelloIndigo extends IndigoSandbox[Unit, Model] {

  val config: indigo.GameConfig =
    GameConfig.default
      .withMagnification(magnification)
      .withViewport(width, height)
      .withClearColor(ClearColor.fromRGB(111, 111, 111))

  val animations: Set[indigo.Animation] =
    Set()

  val plutoAssetName     = AssetName("pluto")
  val lineAssetName      = AssetName("centerStrip")
  val passengerAssetName = AssetName("passenger")
  val deliveryAssetName  = AssetName("delivery")
  val fontAssetName = AssetName("font")

  val assets: Set[indigo.AssetType] =
    Set(
      AssetType.Image(AssetName("pluto"), AssetPath("assets/pluto.png")),
      AssetType.Image(AssetName("centerStrip"), AssetPath("assets/line.png")),
      AssetType.Image(AssetName("passenger"), AssetPath("assets/robert.png")),
      AssetType.Image(AssetName("delivery"), AssetPath("assets/delivery.png")),
      AssetType.Image(AssetName("font"), AssetPath("assets/boxy_font.png"))
    )

  val fontKey: FontKey = FontKey("font")

  def fontInfo: FontInfo =
    FontInfo(fontKey, Material.Textured(fontAssetName), 320, 230, FontChar("?", 93, 52, 23, 23))
      .addChar(FontChar("A", 3, 78, 23, 23))
      .addChar(FontChar("B", 26, 78, 23, 23))
      .addChar(FontChar("C", 50, 78, 23, 23))
      .addChar(FontChar("D", 73, 78, 23, 23))
      .addChar(FontChar("E", 96, 78, 23, 23))
      .addChar(FontChar("F", 119, 78, 23, 23))
      .addChar(FontChar("G", 142, 78, 23, 23))
      .addChar(FontChar("H", 165, 78, 23, 23))
      .addChar(FontChar("I", 188, 78, 15, 23))
      .addChar(FontChar("J", 202, 78, 23, 23))
      .addChar(FontChar("K", 225, 78, 23, 23))
      .addChar(FontChar("L", 248, 78, 23, 23))
      .addChar(FontChar("M", 271, 78, 23, 23))
      .addChar(FontChar("N", 3, 104, 23, 23))
      .addChar(FontChar("O", 29, 104, 23, 23))
      .addChar(FontChar("P", 54, 104, 23, 23))
      .addChar(FontChar("Q", 75, 104, 23, 23))
      .addChar(FontChar("R", 101, 104, 23, 23))
      .addChar(FontChar("S", 124, 104, 23, 23))
      .addChar(FontChar("T", 148, 104, 23, 23))
      .addChar(FontChar("U", 173, 104, 23, 23))
      .addChar(FontChar("V", 197, 104, 23, 23))
      .addChar(FontChar("W", 220, 104, 23, 23))
      .addChar(FontChar("X", 248, 104, 23, 23))
      .addChar(FontChar("Y", 271, 104, 23, 23))
      .addChar(FontChar("Z", 297, 104, 23, 23))
      .addChar(FontChar("0", 3, 26, 23, 23))
      .addChar(FontChar("1", 26, 26, 15, 23))
      .addChar(FontChar("2", 41, 26, 23, 23))
      .addChar(FontChar("3", 64, 26, 23, 23))
      .addChar(FontChar("4", 87, 26, 23, 23))
      .addChar(FontChar("5", 110, 26, 23, 23))
      .addChar(FontChar("6", 133, 26, 23, 23))
      .addChar(FontChar("7", 156, 26, 23, 23))
      .addChar(FontChar("8", 179, 26, 23, 23))
      .addChar(FontChar("9", 202, 26, 23, 23))
      .addChar(FontChar("?", 93, 52, 23, 23))
      .addChar(FontChar("!", 3, 0, 15, 23))
      .addChar(FontChar(".", 286, 0, 15, 23))
      .addChar(FontChar(",", 248, 0, 15, 23))
      .addChar(FontChar(" ", 145, 52, 23, 23))

  val fonts: Set[indigo.FontInfo] =
    Set(fontInfo)

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
        case a if a == 0 => -3
        case c if c > -5 => -2
        case _           => -1
      }
      Outcome(model.update(change, Command.NoOp))

    case KeyboardEvent.KeyDown(Keys.RIGHT_ARROW) =>
      val change = model.pluto.velocity match {
        case a if a == 0 => 3
        case c if c < 5  => 2
        case _           => 1
      }
      Outcome(model.update(change, Command.NoOp))

    case KeyboardEvent.KeyDown(Keys.SHIFT) =>
      Outcome(model.update(0, Command.Pickup))

    case KeyboardEvent.KeyDown(Keys.CTRL) =>
      Outcome(model.update(0, Command.Delivery))

    case FrameTick =>
      val change =
        if (model.ftct % 10 == 0)
          model.pluto.velocity match {
            case a if a > 0 => -1
            case c if c < 0 => 1
            case _          => 0
          }
        else 0
      Outcome(model.update(change, Command.NoOp))

    case _ =>
      Outcome(model)
  }

  def present(
      context: indigo.FrameContext,
      model: Model
  ): indigo.SceneUpdateFragment =
    SceneUpdateFragment.empty
    .addGameLayerNodes(
      drawScene(model.pluto, model.lines, model.passenger, model.deliveryStop)
    )
    .addGameLayerNodes(drawText(model.passenger.score))

  def drawScene(
      pluto: Pluto,
      lines: List[CenterStrip],
      passenger: Passenger,
      delivery: DeliveryStop
  ): List[Graphic] =
    lines.map(line =>
      Graphic(Rectangle(0, 0, 10, 50), 1, Material.Textured(lineAssetName))
        .withTint(255, 255, 255)
        .moveTo(line.location)
    ) ++
      List(
        Graphic(Rectangle(0, 0, 150, 150), 1, Material.Textured(deliveryAssetName))
          .withAlpha(0.8)
          .scaleBy(0.7, 0.7)
          .moveTo(delivery.location)
      ) ++
      List(
        Graphic(Rectangle(0, 0, 20, 60), 1, Material.Textured(passengerAssetName))
          .moveTo(passenger.drawLocation)
      ) ++
      List(
        Graphic(Rectangle(0, 0, 120, 75), 1, Material.Textured(plutoAssetName))
          .withRef(60, 38)
          .moveTo(pluto.location)
      )

  def drawText(score: Int) = Text(s"Score $score", 10, 20, 1, fontKey).alignLeft

}

case class Model(pluto: Pluto, lines: List[CenterStrip], passenger: Passenger, deliveryStop: DeliveryStop, ftct: Int) {
  def update(acc: Int, input: Command): Model = {
    val newPluto     = pluto.update(acc)
    val newLines     = lines.map(_.update(ftct))
    val newPassenger = passenger.update(pluto.location, deliveryStop.location, input)
    val newDelivery  = deliveryStop.update(passenger.state)
    this.copy(newPluto, newLines, newPassenger, newDelivery, ftct = (ftct % 10) + 1)
  }
}
object Model {
  def initial(center: Point): Model =
    Model(
      Pluto(Point(center.x, center.y - 300), 0),
      (0 to 970 by 88).map(y => CenterStrip(Point(center.x, y), -gamespeed)).toList,
      Passenger.initial(center.x - 100),
      DeliveryStop(Point(center.x, center.y)),
      0
    )
}
case class Pluto(location: Point, velocity: Int) {
  def update(acc: Int): Pluto =
    this.copy(location = Point(location.x + velocity, location.y), velocity = velocity + acc)
}
case class CenterStrip(location: Point, velocity: Double) {
  def update(ftct: Int): CenterStrip = {
    val (newLocation: Double, velocityChange: Double) = location.y match {
      case a if a <= -88 => (height, 0d)
      case _ if ftct % 10 == 0 => (location.y + velocity, 0d) //TODO fix
      case _ => (location.y + velocity, 0d)
    }
    this.copy(location = Point(location.x, Math.floor(newLocation).toInt), velocity = velocity + velocityChange)
  }
}

case class Passenger(state: Pickup, location: Point, drawLocation: Point, score: Int) {
  def update(plutoPos: Point, deliveryPos: Point, input: Command): Passenger = {
    val (newState, newLocation, newDrawLocation, newScore): Tuple4[Pickup, Point, Point, Int] = plutoPos match {
      // don't override success state, check if delivery
      case _ if state == Pickup.Success =>
        input match {
          case Command.Delivery if checkInDelivery(plutoPos, deliveryPos) =>
            (
              Pickup.Delivered,
              Point(deliveryPos.x + 40, deliveryPos.y + 25),
              Point(deliveryPos.x + 40, deliveryPos.y + 25),
            score + 10
            )
          case _ => (Pickup.Success, Point(location.x, location.y - gamespeed), Point(-100, -100), score)
        }
      // higher than view, reset
      case _ if location.y < -25 && state != Pickup.Success => Passenger.placeRandom(score)
      // higher than pluto, maybe missed
      case a if a.y > (location.y + 100) =>
        state match {
          case Pickup.Success => (Pickup.Success, Point(location.x, location.y - gamespeed), Point(-100, -100), score)
          case _ =>
            (Pickup.Missed, Point(location.x, location.y - gamespeed), Point(location.x, location.y - gamespeed), score)
        }
      // out of reach, still possible
      case a if a.y < (location.y - 20) =>
        (Pickup.NotYet, Point(location.x, location.y - gamespeed), Point(location.x, location.y - gamespeed), score)
      // possible pickup, depending on position and button
      case l if l.x >= location.x - 20 && l.x <= location.x + 95 && state != Pickup.Success =>
        input match {
          // picked up, hide
          case Command.Pickup => (Pickup.Success, Point(location.x, location.y - gamespeed), Point(-100, -100), score)
          // not picked up, try again
          case _ =>
            (Pickup.NotYet, Point(location.x, location.y - gamespeed), Point(location.x, location.y - gamespeed), score)
        }
      case _ => (Pickup.NotYet, Point(location.x, location.y - gamespeed), Point(location.x, location.y - gamespeed), score)
    }

    //println(s"State: $state, plutoPos: $plutoPos, deliveryPos: $deliveryPos, passenger: $location, input: $input")
    this.copy(newState, newLocation, newDrawLocation, newScore)

  }

  def checkInDelivery(plutoPos: Point, deliveryPos: Point): Boolean =
    if (
      // Not left
      (plutoPos.x >= deliveryPos.x - 120) &&
      // Not right
      (plutoPos.x <= deliveryPos.x + 150) &&
      // Not over
      (plutoPos.y >= deliveryPos.y - 75) &&
      // Not under
      (plutoPos.y <= deliveryPos.y + 150)
    ) true
    else false
}
object Passenger {
  def initial(xLoc: Int) = {
    val (state, loc, dloc) = reset(xLoc)
    Passenger(state, loc, dloc, 0)
  }
  def reset(xLoc: Int) = (Pickup.NotYet, Point(xLoc, height), Point(xLoc, height))
  def placeRandom(score: Int) = {
    val xLoc = scala.util.Random.between(10, width - 10)
    (Pickup.NotYet, Point(xLoc, height), Point(xLoc, height), score)
  }
}

case class DeliveryStop(location: Point) {
  def update(passengerState: Pickup) = if (passengerState == Pickup.Success)
    location.y match {
      case a if a < -150 => DeliveryStop.initial(DeliveryStop.randomX)
      case _             => this.copy(Point(location.x, location.y - gamespeed))
    } else DeliveryStop.hidden
}
object DeliveryStop {
  def initial(xLoc: Int) = DeliveryStop(Point(xLoc, height))
  def randomX            = scala.util.Random.between(10, width - 10)
  def hidden = DeliveryStop(Point(0, -200))
}

sealed trait Pickup
object Pickup {
  case object Missed    extends Pickup
  case object Success   extends Pickup
  case object NotYet    extends Pickup
  case object Delivered extends Pickup
}

sealed trait Command
object Command {
  case object Pickup   extends Command
  case object Delivery extends Command
  case object NoOp     extends Command
}
