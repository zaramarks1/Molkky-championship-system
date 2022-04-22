pipeline {
    agent any
    environment {
        MYSQL_DB = "molkky_test"
        MYSQL_HOST = "localhost"
        DB_PASSWORD = "password"
        DB_USER = "sa"
        CHROME_DRIVER = "/bin/chrome_driver/chromedriver"
        SERVER_PORT = 8175
        DB_URL = "jdbc:h2:mem:testdb"
        DRIVER_CLASS_NAME = "org.h2.Driver"
    }
    stages {
        stage('Build') {
            steps {
                sh 'mvn clean install'
            }
        }
        stage('Sonar'){
            steps {
                sh 'printenv'
                sh 'mvn sonar:sonar'
            }
        }
        stage('War') {
            steps {
                sh 'mvn compile war:war'
                sh 'cp /srv/tomcat9/jenkins/workspace/Multibranch_Devops_Lucien/target/molkky.war /srv/tomcat9/webapps/ROOT.war'
            }
        }
    }
    post {
        always {
            junit 'target/surefire-reports/*.xml'
        }
    }

}