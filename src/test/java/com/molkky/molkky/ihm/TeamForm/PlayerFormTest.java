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

@SpringBootTest(classes = MolkkyApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
public class PlayerFormTest {
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

    @Test
    void testPlayerFormGetPage() {
        enterTeam("1");
        Assertions.assertEquals("Create Team", config.getDriver().getTitle());
    }

    @Test
    void testFormIsDisplayed(){
        enterTeam("1");

        Assertions.assertTrue(config.getDriver().findElement(new By.ByClassName("contentTitle")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByXPath("/html/body/div[2]/div[2]/form/div[1]/a")).isDisplayed());

        //Joueur 1
        Assertions.assertTrue(config.getDriver().findElement(new By.ByXPath("/html/body/div[2]/div[2]/form/div[3]/div[1]/b")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("players[0].forename")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("players[0].surname")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("players[0].mail")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("players[0].club")).isDisplayed());

        Assertions.assertTrue(config.getDriver().findElement(new By.ById("sendTeam")).isDisplayed());
    }

    //@Test
    void testFormAddInfo(){
        String nameTeam = enterTeam("1");

        config.getDriver().findElement(new By.ByName("players[0].forename")).sendKeys("Aurelien");
        config.getDriver().findElement(new By.ByName("players[0].surname")).sendKeys("Masson");
        config.getDriver().findElement(new By.ByName("players[0].mail")).sendKeys("aurel1749@gmail.com");
        config.getDriver().findElement(new By.ByName("players[0].club")).sendKeys("Molkky Angers");

        config.getDriver().findElement(new By.ById("sendTeam")).click();

        Team team = teamRepository.findByName(nameTeam);
        User user = userRepository.findUsersByEmail("aurelien.masson@reseau.eseo.fr");

        Assertions.assertNotNull(user);
        Assertions.assertEquals("Masson",user.getSurname());
        Assertions.assertEquals("Aurelien",user.getForename());
        Assertions.assertEquals("aurelien.masson@reseau.eseo.fr",user.getEmail());
        Assertions.assertEquals("Molkky Angers",user.getClub());
        Assertions.assertEquals(team.getId(),user.getTeam());
    }

    //@Test
    void testFormErrorSameMail(){
        String nameTeam = enterTeam("2");

        config.getDriver().findElement(new By.ByName("players[0].forename")).sendKeys("Aurelien");
        config.getDriver().findElement(new By.ByName("players[0].surname")).sendKeys("Masson");
        config.getDriver().findElement(new By.ByName("players[0].mail")).sendKeys("aurelien.masson@reseau.eseo.fr");
        config.getDriver().findElement(new By.ByName("players[0].club")).sendKeys("Molkky Angers");

        config.getDriver().findElement(new By.ByName("players[0].forename")).sendKeys("Zara");
        config.getDriver().findElement(new By.ByName("players[0].surname")).sendKeys("Marks");
        config.getDriver().findElement(new By.ByName("players[0].mail")).sendKeys("aurelien.masson@reseau.eseo.fr");
        config.getDriver().findElement(new By.ByName("players[0].club")).sendKeys("Molkky Angers");

        config.getDriver().findElement(new By.ById("sendTeam")).click();

        Assertions.assertTrue(config.getDriver().findElement(new By.ByLinkText("S'il vous plait, entrez des mails différents pour chaque joueur !")).isDisplayed());
    }

    @AfterAll
    void tearDown() {
        config.getDriver().quit();
    }

    String enterTeam(String nbPlayer){
        config.getDriver().get(url + "/team/create");
        String teamName = "Test" + Math.floor(Math.random() * 100);
        config.getDriver().findElement(new By.ById("nom")).sendKeys(teamName);
        Select select = new Select(config.getDriver().findElement(new By.ById("tournament")));
        select.selectByIndex(1);
        String idTournament = config.getDriver().findElement(new By.ById("tournament")).getAttribute("value");
        config.getDriver().findElement(new By.ByName("nbPlayers")).sendKeys(nbPlayer);
        config.getDriver().findElement(new By.ById("sendTeam")).click();

        return teamName;
    }
}

