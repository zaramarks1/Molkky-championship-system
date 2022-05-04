package com.molkky.molkky.ihm.TeamForm;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.repository.TeamRepository;
import org.junit.Assert;
import org.junit.jupiter.api.*;
import org.openqa.selenium.By;
import org.openqa.selenium.support.ui.Select;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(classes = MolkkyApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class TeamFormTest {
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
    void testTournamentFormGetPage() {
        config.getDriver().get(url + "/team/create");
        Assertions.assertEquals("Création d'une nouvelle équipe", config.getDriver().getTitle());
    }

    @Test
    void testFormIsDisplayed(){
        config.getDriver().get(url + "/team/create");
        Assertions.assertTrue(config.getDriver().findElement(new By.ByClassName("contentTitle")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("nom")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("tournament")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("sendTeam")).isDisplayed());
    }

    @Test
    void testTeamFormAddInfo(){
        config.getDriver().get(url + "/team/create");
        String teamName = "Test" + Math.floor(Math.random() * 100);
        String numberOfPlayer = "2";


        config.getDriver().findElement(new By.ById("nom")).sendKeys(teamName);
        Select select = new Select(config.getDriver().findElement(new By.ById("tournament")));
        select.selectByIndex(1);
        String idTournament = config.getDriver().findElement(new By.ById("tournament")).getAttribute("value");
        config.getDriver().findElement(new By.ById("sendTeam")).click();

        Team team = teamRepository.findByName(teamName);

        Assertions.assertNotNull(team,"Team not save");
        Assertions.assertEquals(teamName,team.getName(),"Name different");
        Assertions.assertEquals(idTournament,String.valueOf(team.getTournament().getId()),"IdTournament different");
    }


    @AfterAll
    void tearDown() {
        config.getDriver().quit();
    }
}
