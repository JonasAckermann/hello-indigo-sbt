import indigo._
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("IndigoGame")
object HelloIndigo extends IndigoSandbox[Unit, Model] {

  val magnification = 1

  val config: indigo.GameConfig =
    GameConfig.default.withMagnification(magnification).withViewport(1680, 972)

  val animations: Set[indigo.Animation] =
    Set()

  val plutoAssetName = AssetName("pluto")
  val lineAssetName = AssetName("centerStrip")

  val assets: Set[indigo.AssetType] =
    Set(
      AssetType.Image(AssetName("pluto"), AssetPath("assets/pluto.png")),
      AssetType.Image(AssetName("centerStrip"), AssetPath("assets/line.png"))
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
      Outcome(model.update(-1))

    case KeyboardEvent.KeyDown(Keys.RIGHT_ARROW) =>
      Outcome(model.update(1))

    case FrameTick =>
      val change = if(model.ftct % 10 == 0) {
        model.pluto.velocity match {
        case a if a > 0  => -1
        case c if c < 0  => 1
        case _ => 0
        }
      } else 0
      Outcome(model.update(change))

    case _ =>
      Outcome(model)
  }

  def present(
      context: indigo.FrameContext,
      model: Model
  ): indigo.SceneUpdateFragment =
    SceneUpdateFragment(
    ).addGameLayerNodes(
      drawScene(model.pluto, model.lines)
    )

  def drawScene(
      pluto: Pluto,
      lines: List[CenterStrip]
  ): List[Graphic] = {

    lines.map(line =>Graphic(Rectangle(0, 0, 10, 50), 1, Material.Textured(lineAssetName)).withTint(255,255,255)
      .moveTo(line.location)) ++
    List(Graphic(Rectangle(0, 0, 120, 75), 1, Material.Textured(plutoAssetName))
      .withRef(60, 38)
      .moveTo(pluto.location))
  }

}

case class Model(pluto: Pluto, lines: List[CenterStrip], ftct: Int) {
  def update(acc: Int): Model =
    this.copy(pluto = pluto.update(acc), lines.map(_.update), ftct = (ftct % 10) + 1 )
}
object Model {
  def initial(center: Point): Model = Model(Pluto(center, 0, ""), (0 to 970 by 88).map(y => CenterStrip(Point(center.x, y))).toList, 0)}
case class Pluto(location: Point, velocity: Int, infoText: String) {
  def update(acc: Int): Pluto = {
    println(infoText)
    this.copy(location = Point(location.x + velocity, location.y), velocity = velocity + acc, infoText = s"velocity: $velocity, acc: $acc")
  }
}
case class CenterStrip(location: Point){
  def update: CenterStrip = {
    val newLocation = location.y match {
      case a if a <= -88 => 972
      case _ => location.y - 3
    }

    this.copy(Point(location.x, newLocation))
  }
}