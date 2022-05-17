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
        OPENSSL_CONF = "/dev/null"
        HIBERNATE_DDL = "create-drop"
    }
    stages {
        stage('Build') {
            steps {
                sh 'chmod +x bin/geckodriver'
                sh 'mvn clean install'
            }
        }
        stage('Sonar'){
            steps {
                sh 'printenv'
                sh 'mvn sonar:sonar'
            }
        }
        stage('Deploy') {
            when {
                branch 'PreProd'
            }
            steps {
                sh 'mvn package -DskipTests=true'
                sh 'rm -rf /srv/tomcat9/webapps/ROOT*'
                sh 'cp /srv/tomcat9/jenkins/workspace/Multibranch_PreProd/target/molkky.war /srv/tomcat9/webapps/ROOT.war'
            }
        }
    }
    post {
        always {
            junit 'target/surefire-reports/*.xml'
        }
    }

}