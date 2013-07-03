package models.db

import play.api.test._
import play.api.test.Helpers._

class AlbumSpec extends org.specs2.mutable.Specification {
  "Album" should {
    "enumerate photos" in {
      running(FakeApplication()) {
        withTransaction {
          val photos = List.fill(10)(Photo.addNew(None, None)).sortBy(_.id)
          val albums = List("A", "B", "C", "D", "E").map(i => (i -> Album.addNew(currentTimestamp, f"Point$i"))).toMap
          def pa(p: Int, a: String*) = a.map(albums(_)) map {
            PhotoAlbum.addNew(photos(p), _)
          }
          pa(0, "A")
          pa(1, "A")
          pa(2, "A")
          pa(3, "C", "B")
          pa(4, "C", "B")
          pa(5, "C", "D")
          pa(6, "E", "D")
          pa(7, "E", "D")
          pa(8, "E")
          pa(9, "E")
          // TEST
          albums("A").photos.sortBy(_.id) must_== List(0, 1, 2).map(photos(_))
          albums("B").photos.sortBy(_.id) must_== List(3, 4).map(photos(_))
          albums("C").photos.sortBy(_.id) must_== List(3, 4, 5).map(photos(_))
          albums("D").photos.sortBy(_.id) must_== List(5, 6, 7).map(photos(_))
          albums("E").photos.sortBy(_.id) must_== List(6, 7, 8, 9).map(photos(_))
        }
      }
    }
  }
}
