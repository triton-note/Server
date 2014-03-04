package service

import com.amazonaws.auth.BasicAWSCredentials

object AWS {
  lazy val region = System.getenv("AWS_REGION")
  lazy val credential = {
    val id = System.getenv("AWS_ACCESS_KEY_ID")
    val key = System.getenv("AWS_SECRET_ACCESS_KEY")
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
    lazy val bucketName = System.getenv("AWS_S3_BUCKET_NAME")
    lazy val client = {
      val c = new com.amazonaws.services.s3.AmazonS3Client(credential)
      val ep = if (region == "us-east-1") "" else f"-$region"
      c.setEndpoint(f"s3${ep}.amazonaws.com")
      c
    }
  }
}