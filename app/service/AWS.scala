package service

import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.dynamodbv2.document.DynamoDB

object AWS {
  val credential = new BasicAWSCredentials(settings.AWS.accessKey, settings.AWS.secretKey)
  object DynamoDB {
    lazy val client = {
      val c = new com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient(credential)
      c.setEndpoint(f"dynamodb.${settings.AWS.region}.amazonaws.com")
      c
    }
    lazy val delegate = new DynamoDB(service.AWS.DynamoDB.client)
    def getTable(name: String) = delegate.getTable(name)
  }
  object S3 {
    lazy val client = {
      val c = new com.amazonaws.services.s3.AmazonS3Client(credential)
      val ep = if (settings.AWS.region == "us-east-1") "" else f"-${settings.AWS.region}"
      c.setEndpoint(f"s3${ep}.amazonaws.com")
      c
    }
  }
}