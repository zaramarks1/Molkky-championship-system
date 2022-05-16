package com.molkky.molkky.ihm;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.repository.TournamentRepository;
import org.junit.jupiter.api.*;
import org.openqa.selenium.By;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(classes = MolkkyApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
public class allTournamentFormTest {
    @Autowired
    private TournamentRepository tournamentRepository;
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
     void testAllTournamentGetPage(){
        config.getDriver().get(url+"/tournament/allTournament");
        Assertions.assertEquals("Affichage des tournois",config.getDriver().getTitle());
    }
    @Test
    void testAllBoutonIsDisplayed(){
        config.getDriver().get(url+"/tournament/allTournament");
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("open")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("closed")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("inProgress")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("valider")).isDisplayed());
    }
    @Test
    void testButtonTypeTournament(){
        config.getDriver().get(url + "/tournament/allTournament");
        config.getDriver().findElement(new By.ById("open")).click();
        Assertions.assertEquals("http://localhost:8080/tournament/TournamentOpen", config.getDriver().getCurrentUrl());
        config.getDriver().findElement(new By.ById("closed")).click();
        Assertions.assertEquals("http://localhost:8080/tournament/TournamentClose", config.getDriver().getCurrentUrl());
        config.getDriver().findElement(new By.ById("inProgress")).click();
        Assertions.assertEquals("http://localhost:8080/tournament/TournamentInProgress", config.getDriver().getCurrentUrl());
        config.getDriver().findElement(new By.ById("valider")).click();
        Assertions.assertEquals("http://localhost:8080/tournament/create", config.getDriver().getCurrentUrl());
    }

    @Test
    void testAllTournament(){
        //tournamentRepository
        config.getDriver().get(url+"/tournament/allTournament");

    }


    @AfterAll
    void tearDown(){
        config.getDriver().quit();
    }

}
