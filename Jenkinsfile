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
        stage('Sonar'){
//         5da1e6ee849091ca8fec6cf16062bc190df5382e
            steps {
                sh 'printenv'
                sh 'mvn sonar:sonar'
            }
        }
    }
    post {
        always {
            junit 'target/surefire-reports/*.xml'
        }
    }

}