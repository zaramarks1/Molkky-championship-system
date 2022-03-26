pipeline {
    agent any
    environment {
        MYSQL_DB = "molkky_test"
        MYSQL_HOST = "localhost"
        MYSQL_PASSWORD = "GL2022"
        MYSQL_USER = "calembel"
    }
    stages {
        stage('Build') {
            steps {
                sh 'mvn clean install'
            }
        }
    }
    post {
        always {
            junit 'target/surefire-reports/*.xml'
        }
    }
}