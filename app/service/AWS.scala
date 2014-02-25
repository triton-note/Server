package service

import com.amazonaws.auth.BasicAWSCredentials

object AWS {
  lazy val endpoint = System.getenv("AWS_ENDPOINT")
  lazy val credential = {
    val id = System.getenv("AWS_ACCESS_KEY_ID")
    val key = System.getenv("AWS_SECRET_ACCESS_KEY")
    new BasicAWSCredentials(id, key)
  }
  object DynamoDB {
    lazy val client = {
      val c = new com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient(credential)
      c.setEndpoint(endpoint)
      c
    }
  }
  object S3 {
    lazy val bucketName = System.getenv("S3_BUCKET_NAME")
    lazy val client = {
      val c = new com.amazonaws.services.s3.AmazonS3Client(credential)
      c.setEndpoint(endpoint)
      c
    }
  }
}