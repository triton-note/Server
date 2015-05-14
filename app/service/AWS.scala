package service

import scala.collection.JavaConversions._

import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.cognitoidentity.model.GetCredentialsForIdentityRequest
import com.amazonaws.services.dynamodbv2.document.DynamoDB

object AWS {
  val credential = new BasicAWSCredentials(settings.AWS.accessKey, settings.AWS.secretKey)
  object Cognito {
    lazy val client = new com.amazonaws.services.cognitoidentity.AmazonCognitoIdentityClient(credential);
    def checkId(id: String, logins: Map[String, String]) = {
      try {
        val cred = client.getCredentialsForIdentity(new GetCredentialsForIdentityRequest().withIdentityId(id).withLogins(logins));
        cred.getCredentials.getSessionToken != null
      } catch {
        case ex: Exception =>
          ex.printStackTrace()
          false
      }
    }
  }
  object DynamoDB {
    lazy val client = {
      val c = new com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient(credential)
      c.setEndpoint(f"dynamodb.${settings.AWS.region}.amazonaws.com")
      c
    }
    lazy val delegate = new DynamoDB(service.AWS.DynamoDB.client)
    def getTable(name: String) = delegate.getTable(f"${settings.appName}.${name}")
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
