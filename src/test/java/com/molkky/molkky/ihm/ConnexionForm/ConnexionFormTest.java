package com.molkky.molkky.ihm.ConnexionForm;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.junit.jupiter.api.*;
import org.openqa.selenium.By;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import type.UserRole;

@SpringBootTest(classes = MolkkyApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class ConnexionFormTest {

    @Autowired
    private UserRepository userRepository;
    @Autowired
    private TeamRepository teamRepository;
    @Autowired
    private TournamentRepository tournamentRepository;
    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;
    private SeleniumConfig config;
    @Value("${server.port}")
    private Integer port;
    private String url;
    private String emailPlayer = "connexion.player@gmail.com";
    private String passwordPlayer = "test123";
    private String teamName = "TeamConnexionPlayer";
    private String teamCode = "TeamCode";
    private String tournamentName = "TournamentConnexion";
    private String tournamentName2 = "TournamentConnexion2";
    private String emailAdmin = "connexion.admin@gmail.com";
    private String passwordAdmin = "admin123";
    private String emailStaff = "connexion.staff@gmail.com";
    private String passwordStaff = "staff123";


    @BeforeAll
    void setUp() {
        config = new SeleniumConfig();
        url = String.format("http://localhost:%s", port.toString());
        if(userRepository.findUserByEmail(emailPlayer)==null) {
            User player = new User();
            player.setEmail(emailPlayer);
            player.setPassword(passwordPlayer);
            userRepository.save(player);
            Team team = new Team();
            team.setName(teamName);
            team.setCode(teamCode);
            teamRepository.save(team);
            Tournament tournament = new Tournament();
            tournament.setName(tournamentName);
            tournament.setVisible(true);
            tournamentRepository.save(tournament);
            Tournament tournament2 = new Tournament();
            tournament2.setName(tournamentName2);
            tournament2.setVisible(true);
            tournamentRepository.save(tournament2);
            UserTournamentRole userTournamentRolePlayer = new UserTournamentRole();
            userTournamentRolePlayer.setRole(UserRole.PLAYER);
            userTournamentRolePlayer.setUser(player);
            userTournamentRolePlayer.setTournament(tournament);
            userTournamentRolePlayer.setTeam(team);
            userTournamentRoleRepository.save(userTournamentRolePlayer);
            User admin = new User();
            admin.setEmail(emailAdmin);
            admin.setPassword(passwordAdmin);
            userRepository.save(admin);
            UserTournamentRole userTournamentRoleAdmin = new UserTournamentRole();
            userTournamentRoleAdmin.setRole(UserRole.ADM);
            userTournamentRoleAdmin.setUser(admin);
            userTournamentRoleAdmin.setTournament(tournament);
            userTournamentRoleRepository.save(userTournamentRoleAdmin);
            UserTournamentRole userTournamentRoleAdmin2 = new UserTournamentRole();
            userTournamentRoleAdmin2.setRole(UserRole.ADM);
            userTournamentRoleAdmin2.setUser(admin);
            userTournamentRoleAdmin2.setTournament(tournament2);
            userTournamentRoleRepository.save(userTournamentRoleAdmin2);
            User staff = new User();
            staff.setEmail(emailStaff);
            staff.setPassword(passwordStaff);
            userRepository.save(staff);
            UserTournamentRole userTournamentRoleStaff = new UserTournamentRole();
            userTournamentRoleStaff.setRole(UserRole.STAFF);
            userTournamentRoleStaff.setUser(staff);
            userTournamentRoleStaff.setTournament(tournament);
            userTournamentRoleRepository.save(userTournamentRoleStaff);

        }
    }

    /*@Test
    void gotToConnexion(){
        Assertions.assertEquals("Page de connexion", config.getDriver().getTitle());
    }*/

    @Test
    void testConnexionFormIsDisplayed(){
        config.getDriver().get(url + "/connexion");
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("email")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("password")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("teamCode")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("connexion")).isDisplayed());
    }

    @Test
    void testCheckConnexionExistingPlayer(){
        config.getDriver().get(url + "/connexion");
        WebDriverWait wait = new WebDriverWait(config.getDriver(), 30);
        config.getDriver().findElement(new By.ById("email")).sendKeys(emailPlayer);
        config.getDriver().findElement(new By.ById("password")).sendKeys(passwordPlayer);
        config.getDriver().findElement(new By.ById("teamCode")).sendKeys(teamCode);
        config.getDriver().findElement(new By.ById("connexion")).click();
        wait.until(ExpectedConditions.visibilityOf(config.getDriver().findElement(new By.ById("homeDescription"))));
        Assertions.assertEquals("Accueil", config.getDriver().getTitle());
        config.getDriver().findElement(new By.ById("infos_sidebar")).click();
        Assertions.assertEquals(emailPlayer, config.getDriver().findElement(new By.ById("email")).getText());
        Assertions.assertEquals(teamName, config.getDriver().findElement(new By.ById("team")).getText());
        Assertions.assertEquals("PLAYER", config.getDriver().findElement(new By.ById("role")).getText());
        Assertions.assertEquals(tournamentName, config.getDriver().findElement(new By.ById("tournament")).getText());

    }

    @Test
    void testCheckConnexionWrongPasswordUser(){
        config.getDriver().get(url + "/connexion");
        config.getDriver().findElement(new By.ById("email")).sendKeys(emailPlayer);
        config.getDriver().findElement(new By.ById("password")).sendKeys("wgPwd");
        config.getDriver().findElement(new By.ById("teamCode")).sendKeys(teamCode);
        config.getDriver().findElement(new By.ById("connexion")).click();
        Assertions.assertEquals("Page de connexion", config.getDriver().getTitle());
    }
    @Test
    void testConnexionExistingAdmin2Tournaments(){
        config.getDriver().get(url + "/connexion");
        WebDriverWait wait = new WebDriverWait(config.getDriver(), 30);
        config.getDriver().findElement(new By.ById("email")).sendKeys(emailAdmin);
        config.getDriver().findElement(new By.ById("password")).sendKeys(passwordAdmin);
        config.getDriver().findElement(new By.ById("connexion")).click();
        wait.until(ExpectedConditions.visibilityOf(config.getDriver().findElement(new By.ById("tournamentChoice"))));
        Assertions.assertEquals("Choix du Role/Tournoi", config.getDriver().getTitle());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("buttonTournament")).isDisplayed());
        config.getDriver().findElement(new By.ById("buttonTournament")).click();
        wait.until(ExpectedConditions.visibilityOf(config.getDriver().findElement(new By.ById("rolesChoice"))));
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("buttonRole")).isDisplayed());
        config.getDriver().findElement(new By.ById("buttonRole")).click();
        wait.until(ExpectedConditions.visibilityOf(config.getDriver().findElement(new By.ById("homeDescription"))));
        Assertions.assertEquals("Accueil", config.getDriver().getTitle());
        config.getDriver().findElement(new By.ById("infos_sidebar")).click();
        Assertions.assertEquals(emailAdmin, config.getDriver().findElement(new By.ById("email")).getText());
        Assertions.assertEquals("ADM", config.getDriver().findElement(new By.ById("role")).getText());
        Assertions.assertEquals(tournamentName, config.getDriver().findElement(new By.ById("tournament")).getText());
    }


/*
    @Test
    void testPlayerFormGetPage() {
        enterTeam("1");
        Assertions.assertEquals("Create Team", config.getDriver().getTitle());
    }

    @Test
    void testFormIsDisplayed(){
        enterTeam("1");

        Assertions.assertTrue(config.getDriver().findElement(new By.ByClassName("contentTitle")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByXPath("/html/body/div/div[2]/form/div[1]/a")).isDisplayed());

        //Joueur 1
        Assertions.assertTrue(config.getDriver().findElement(new By.ByXPath("/html/body/div/div[2]/form/div[3]/div/b")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("players[0].forename")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("players[0].surname")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("players[0].mail")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("players[0].club")).isDisplayed());

        Assertions.assertTrue(config.getDriver().findElement(new By.ById("sendTeam")).isDisplayed());
    }

    @Test
    void testFormAddInfo(){
        String nameTeam = enterTeam("1");
        String nom = this.generateName();
        String prenom = this.generateName();
        String mail = prenom+"."+nom+"@gmail.com";

        config.getDriver().findElement(new By.ByName("players[0].forename")).sendKeys(prenom);
        config.getDriver().findElement(new By.ByName("players[0].surname")).sendKeys(nom);
        config.getDriver().findElement(new By.ByName("players[0].mail")).sendKeys(mail);
        config.getDriver().findElement(new By.ByName("players[0].club")).sendKeys("Molkky Angers");

        config.getDriver().findElement(new By.ById("sendTeam")).click();

        Team team = teamRepository.findByName(nameTeam);
        User user = userRepository.findUsersByEmail(mail);

        Assertions.assertNotNull(user);
        Assertions.assertEquals(nom,user.getSurname());
        Assertions.assertEquals(prenom,user.getForename());
        Assertions.assertEquals(mail,user.getEmail());
        Assertions.assertEquals("Molkky Angers",user.getClub());
        Assertions.assertEquals(team.getId(),user.getTeam().getId());
    }

    @Test
    void testFormErrorSameMail(){
        enterTeam("2");
        String nom = this.generateName();
        String prenom = this.generateName();
        String mail = prenom+"."+nom+"@gmail.com";

        config.getDriver().findElement(new By.ByName("players[0].forename")).sendKeys(nom);
        config.getDriver().findElement(new By.ByName("players[0].surname")).sendKeys(prenom);
        config.getDriver().findElement(new By.ByName("players[0].mail")).sendKeys(mail);
        config.getDriver().findElement(new By.ByName("players[0].club")).sendKeys("Molkky Angers");

        config.getDriver().findElement(new By.ByName("players[1].forename")).sendKeys(nom);
        config.getDriver().findElement(new By.ByName("players[1].surname")).sendKeys(prenom);
        config.getDriver().findElement(new By.ByName("players[1].mail")).sendKeys(mail);
        config.getDriver().findElement(new By.ByName("players[1].club")).sendKeys("Molkky Angers");

        config.getDriver().findElement(new By.ById("sendTeam")).click();

        Assertions.assertTrue(config.getDriver().findElement(new By.ByXPath("/html/body/div/div[2]/form/div[2]/span")).isDisplayed());
    }

    @AfterAll
    void tearDown() {
        config.getDriver().quit();
    }

    private String enterTeam(String nbPlayer){
        config.getDriver().get(url + "/team/create");
        String teamName = "Test" + Math.floor(Math.random() * 1000);
        config.getDriver().findElement(new By.ById("nom")).sendKeys(teamName);
        Select select = new Select(config.getDriver().findElement(new By.ById("tournament")));
        select.selectByIndex(select.getOptions().size() - 1);
        String idTournament = config.getDriver().findElement(new By.ById("tournament")).getAttribute("value");
        config.getDriver().findElement(new By.ByName("nbPlayers")).sendKeys(nbPlayer);
        config.getDriver().findElement(new By.ById("sendTeam")).click();
        return teamName;
    }

    private String generateName(){
        String str = "";
        for(int i = 0; i < 5 ; i++){
            char c = (char)(new Random().nextInt(25)+'a');
            str +=c;
        }
        return str;
    }*/
    @AfterAll
    void tearDown() {
        config.getDriver().quit();
    }
}

