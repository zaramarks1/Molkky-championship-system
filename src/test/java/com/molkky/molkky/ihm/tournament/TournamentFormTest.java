package com.molkky.molkky.ihm.tournament;

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
    void testFormIsDisplayed(){
        config.getDriver().get(url + "/tournament/create");
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("nom")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("location")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("dateTournoi")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("cutOffDate")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("minTeam")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("maxTeam")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("visible")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("nbRounds")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("nbCourts")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ById("sendTournament")).isDisplayed());
    }

    @Test
    void testTournamentFormAddInfo() {
        config.getDriver().get(url + "/tournament/create");
        String randomName = "Tournoi " + Math.floor(Math.random() * 100000);
        String randomLocation = "location de test";
        String randomDateTournoi = "01/01/2020";
        String randomCutOffDate = "01/01/2020";
        String randomMinTeam = "5";
        String randomMaxTeam = "20";
        String randomNbRounds = "1";
        String randomNbCounts = "1";
        String randomNbPlayersPerTeam = "3";

        config.getDriver().findElement(new By.ById("nom")).sendKeys(randomName);
        config.getDriver().findElement(new By.ById("location")).sendKeys(randomLocation);
        config.getDriver().findElement(new By.ById("dateTournoi")).sendKeys(randomDateTournoi);
        config.getDriver().findElement(new By.ById("cutOffDate")).sendKeys(randomCutOffDate);
        config.getDriver().findElement(new By.ById("nbPlayersPerTeam")).sendKeys(randomNbPlayersPerTeam);
        config.getDriver().findElement(new By.ById("minTeam")).sendKeys(randomMinTeam);
        config.getDriver().findElement(new By.ById("maxTeam")).sendKeys(randomMaxTeam);
        config.getDriver().findElement(new By.ById("visible")).click();
        config.getDriver().findElement(new By.ById("nbRounds")).sendKeys(randomNbRounds);
        config.getDriver().findElement(new By.ById("nbCourts")).sendKeys(randomNbCounts);
        config.getDriver().findElement(new By.ById("sendTournament")).click();

        Tournament tournament = tournamentRepository.findByName(randomName);

        String dateTournoiSQL = transformDate(tournament.getDate().toString());
        String cutOffSQL = transformDate(tournament.getDate().toString());

        Assertions.assertNotNull(tournament);
        Assertions.assertEquals(randomName,tournament.getName());
        Assertions.assertEquals(randomLocation,tournament.getLocation());
        Assertions.assertEquals(randomDateTournoi,dateTournoiSQL);
        Assertions.assertEquals(randomCutOffDate,cutOffSQL);
        Assertions.assertEquals(randomMinTeam,String.valueOf(tournament.getMinTeam()));
        Assertions.assertEquals(randomMaxTeam,String.valueOf(tournament.getMaxTeam()));
        Assertions.assertTrue(tournament.isVisible());
        Assertions.assertEquals(randomNbRounds,String.valueOf(tournament.getNbRounds()));
        Assertions.assertEquals(randomNbCounts,String.valueOf(tournament.getNbCourts()));
    }

    public String transformDate(String date){
        String[] table = date.split(" |-");
        return table[2]+"/"+table[1]+"/"+table[0];
    }

    @AfterAll
    void tearDown() {
        config.getDriver().quit();
    }
}
