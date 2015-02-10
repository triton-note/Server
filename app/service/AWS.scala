package service

import com.amazonaws.auth.BasicAWSCredentials

object AWS {
  lazy val region = Settings.AWS_REGION
  lazy val credential = {
    val id = Settings.AWS_ACCESS_KEY_ID
    val key = Settings.AWS_SECRET_KEY
    new BasicAWSCredentials(id, key)
  }
  object DynamoDB {
    lazy val client = {
      val c = new com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient(credential)
      c.setEndpoint(f"dynamodb.${region}.amazonaws.com")
      c
    }
  }
  object S3 {
    lazy val bucketName = Settings.AWS_S3_BUCKET_NAME
    lazy val client = {
      val c = new com.amazonaws.services.s3.AmazonS3Client(credential)
      val ep = if (region == "us-east-1") "" else f"-$region"
      c.setEndpoint(f"s3${ep}.amazonaws.com")
      c
    }
    object ClientSide {
      lazy val targetUrl = f"https://${bucketName}.s3.amazonaws.com/"
      lazy val accessKey = Settings.AWS_ACCESS_KEY_ID_CLIENTSIDE
      lazy val seacretKey = Settings.AWS_SECRET_KEY_CLIENTSIDE
      val acl = "bucket-owner-full-control"
    }
  }
}