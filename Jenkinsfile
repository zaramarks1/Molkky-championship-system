pipeline {
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