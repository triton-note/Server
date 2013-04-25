package models

package db {
  object DB {
    val simple = scala.slick.driver.PostgresDriver.simple
    import simple._
    import Database.threadLocalSession
    lazy val db = {
      import play.api.Play.current
      Database.forDataSource(play.api.db.DB.getDataSource())
    }
    val prepared = {
      db withSession {
        def createTable(t: Table[_]) = {
          import scala.slick.jdbc.meta.MTable
          val table = MTable.getTables(t.tableName).firstOption
          val d = if (table.isEmpty) Some(t.ddl) else None
          d.map(_.create)
          d
        }
        List(
          User, Photo, Album,
          UserAlias, VolatileToken,
          PhotoAlbum, PhotoOwner, AlbumOwner,
          Comment, CommentPhoto, CommentAlbum).flatMap(createTable(_))
      }
    }
  }
}
package object db {
  def withSession[T](f: => T) = DB.db.withSession(f)
  def withTransaction[T](f: => T) = DB.db.withTransaction(f)
  def currentTimestamp = new java.sql.Timestamp(System.currentTimeMillis)
  implicit def dateToTimestamp(date: java.util.Date) = new java.sql.Timestamp(date.getTime)
}