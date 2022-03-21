# Informations sur l'exécution du projet

Dû au fait qu'on ait pas de base de donnée de développement la solution d'avoir une base de donnée qui tourne localement est préférée.
Il existe plusieurs façons de faire tourner une base de données pour le projet, ça n'importe peu tant que les identifiants sont les mêmes à la fin.
### Docker
Un fichier docker-compose est fourni à la racine du projet, pour le faire tourner il suffit d'installer Docker.
L'avantage c'est que ce sera spécifique au projet et que phpMyAdmin sera inclut direct.
##### Lancer le container docker
    docker-compose up -d
##### Arrêter le container
    docker-compose down
Sinon vous pouvez utiliser l'interface Docker Desktop une fois que le container a été lancé une fois.
PhpMyAdmin est dispo à l'url http://localhost:8000 avec le serveur "mysql", user "root", mot de passe "molkky_db". Les paramètres de la base de données sont déjà renseignés dans un fichier de ressources.
### Installer MySql
# Nomenclature
### Noms des commits
Insérez un des emojis selon cette liste:
https://gist.github.com/parmentf/035de27d6ed1dce0b36a
Au début de chacun de vos commits, cela permettra de se repérer facilement dans la liste.
Exemple pour un commit d'avancement basique: 

> ⚡Added the Teams entity

Écrire en anglais est préféré.
### Noms des branches

 - master, branche à merge une fois par sprint
	 - PreProd, branche d'avancement à merge à la fin de chaque US
		 - USX, (ex: US2) branche d'avancement d'une User story avec le numéro de l'user story dans l'openproject
			 - USXDEV_NOM, (ex: US2DEV_Lucien), branche typique de dev individuelle.

