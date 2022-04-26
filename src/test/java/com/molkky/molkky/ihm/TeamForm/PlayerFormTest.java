package com.molkky.molkky.ihm.TeamForm;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.UserRepository;
import org.junit.jupiter.api.*;
import org.openqa.selenium.By;
import org.openqa.selenium.support.ui.Select;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Random;

@SpringBootTest(classes = MolkkyApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class PlayerFormTest {
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private TeamRepository teamRepository;
    private SeleniumConfig config;
    @Value("${server.port}")
    private Integer port;
    private String url;

    @BeforeAll
    void setUp() {
        config = new SeleniumConfig();
        url = String.format("http://localhost:%s", port.toString());
    }

    @BeforeAll
    void createTournament(){
        config.getDriver().get(url + "/tournament/create");
        String randomName = "Tournoi " + Math.floor(Math.random() * 1000);
        String randomLocation = "location de test";
        String randomDateTournoi = "01/01/2020";
        String randomCutOffDate = "01/01/2020";
        String randomMinTeam = "5";
        String randomMaxTeam = "20";
        String randomNbRounds = "1";
        String randomNbCounts = "1";

        config.getDriver().findElement(new By.ById("nom")).sendKeys(randomName);
        config.getDriver().findElement(new By.ById("location")).sendKeys(randomLocation);
        config.getDriver().findElement(new By.ById("dateTournoi")).sendKeys(randomDateTournoi);
        config.getDriver().findElement(new By.ById("cutOffDate")).sendKeys(randomCutOffDate);
        config.getDriver().findElement(new By.ById("minTeam")).sendKeys(randomMinTeam);
        config.getDriver().findElement(new By.ById("maxTeam")).sendKeys(randomMaxTeam);
        config.getDriver().findElement(new By.ById("visible")).click();
        config.getDriver().findElement(new By.ById("nbRounds")).sendKeys(randomNbRounds);
        config.getDriver().findElement(new By.ById("nbCourts")).sendKeys(randomNbCounts);
        config.getDriver().findElement(new By.ById("sendTournament")).click();
    }

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
    }
}

