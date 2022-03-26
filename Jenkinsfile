pipeline {
    agent any
    environment {
        MYSQL_DB = "molkky_test"
        MYSQL_HOST = "localhost"
        MYSQL_PASSWORD = "GL2022"
        MYSQL_USER = "calembel"
    }
    stages {
        stage('test') {
            steps {
                sh 'echo lol'
            }
        }
        stage('Build') {
            steps {
                sh 'mvn clean install'
            }
        }
    }
}