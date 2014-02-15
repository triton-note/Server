package models

package object db {
  val simple = scala.slick.driver.PostgresDriver.simple
  lazy val DB = {
    import simple._
    import play.api.Play.current
    val dc = Database.forDataSource(play.api.db.DB.getDataSource())
    dc withSession { implicit session =>
      List(
        Users, UserAliases,
        //Photo, Image, ImageRelation,
        Comments
        //VolatileToken,
        //Geographic
      ).foreach { t =>
          import scala.slick.jdbc.meta.MTable
          val table = MTable.getTables(t.baseTableRow.tableName).firstOption
          if (table.isEmpty) t.ddl.create
        }
    }
    dc
  }
  def currentTimestamp = new java.sql.Timestamp(System.currentTimeMillis)
  implicit def dateToTimestamp(date: java.util.Date) = new java.sql.Timestamp(date.getTime)
}