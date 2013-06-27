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
          UserAlias,
          User, PhotoOwner, AlbumOwner,
          Photo, Image, ImageRelation,
          Album, PhotoAlbum,
          Comment, CommentPhoto, CommentAlbum,
          VolatileToken,
          Geographic
        ).flatMap(createTable(_))
      }
    }
    type HasColumnID = { def id: scala.slick.lifted.Column[Long] }
    def getById[D](table: DB.simple.Table[D] with HasColumnID)(givenId: Long): Option[D] = withSession {
      val q = for {
        o <- table
        if o.id is givenId
      } yield o
      q.firstOption
    }
  }
}
package object db {
  def withSession[T](f: => T) = DB.db.withSession(f)
  def withTransaction[T](f: => T) = DB.db.withTransaction(f)
  def currentTimestamp = new java.sql.Timestamp(System.currentTimeMillis)
  implicit def dateToTimestamp(date: java.util.Date) = new java.sql.Timestamp(date.getTime)
}