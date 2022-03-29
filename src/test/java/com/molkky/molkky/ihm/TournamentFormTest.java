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
class TournamentFormTest {
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
    void testTournamentFormGetPage() {
        config.getDriver().get(url + "/tournament/create");
        Assertions.assertEquals("Creation Nouveau Tournoi", config.getDriver().getTitle());
    }

    @Test
    void testTournamentFormAddInfo() {
        config.getDriver().get(url + "/tournament/create");
        String randomName = "Tournoi " + Math.floor(Math.random() * 100000);
        config.getDriver().findElement(new By.ById("nom")).sendKeys(randomName);
        config.getDriver().findElement(new By.ById("location")).sendKeys("location de test");
        config.getDriver().findElement(new By.ById("dateTournoi")).sendKeys("01/01/2020");
        config.getDriver().findElement(new By.ById("cutOffDate")).sendKeys("01/01/2020");
        config.getDriver().findElement(new By.ById("minTeam")).sendKeys("1");
        config.getDriver().findElement(new By.ById("maxTeam")).sendKeys("1");
        config.getDriver().findElement(new By.ById("visible")).click();
        config.getDriver().findElement(new By.ById("nbRounds")).sendKeys("1");
        config.getDriver().findElement(new By.ById("nbCounts")).sendKeys("1");
        config.getDriver().findElement(new By.ById("sendTournament")).click();
        Tournament tournament = tournamentRepository.findByName(randomName);
        Assertions.assertNotNull(tournament);
    }

    @AfterAll
    void tearDown() {
        config.getDriver().quit();
    }
}
