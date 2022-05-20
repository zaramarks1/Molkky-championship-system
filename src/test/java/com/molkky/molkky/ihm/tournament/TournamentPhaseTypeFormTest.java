package com.molkky.molkky.ihm.tournament;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.SeleniumConfig;
import com.molkky.molkky.domain.Phase;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.rounds.*;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.PhaseRepository;
import com.molkky.molkky.repository.TournamentRepository;
import org.junit.jupiter.api.*;
import org.openqa.selenium.By;
import org.openqa.selenium.support.ui.Select;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;

import java.text.ParseException;
import java.util.List;

@SpringBootTest(classes = MolkkyApplication.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
 class TournamentPhaseTypeFormTest {

    @Autowired
    private TournamentRepository tournamentRepository;



    private SeleniumConfig config;
    @Value("${server.port}")
    private Integer port;
    private String url;

    @BeforeAll
    void setUp()  throws ParseException {
        config = new SeleniumConfig();
        url = String.format("http://localhost:%s", port.toString());

    }



    @BeforeEach
    void configTournament(){
        //List<Tournament> tournaments = tournamentRepository.findAll();

       // config.getDriver().get(url + "/phase/choosePhases?idTournament= " + tournaments.get(tournaments.size()-1).getId();
        config.getDriver().get(url + "/tournament/create");
        String randomName = "Tournoi " + Math.floor(Math.random() * 100000);
        String randomLocation = "location de test";
        String randomDateTournoi = "01/01/2020";
        String randomCutOffDate = "01/01/2020";
        String randomMinTeam = "5";
        String randomMaxTeam = "20";
        String randomNbRounds = "2";
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
    }

    @Test
    void testPlayerFormGetPage(){
        Assertions.assertEquals("Choix de la/des phase(s)", config.getDriver().getTitle());
    }

    @Test
    void testFormIsDisplayed(){
        Assertions.assertTrue(config.getDriver().findElement(new By.ByClassName("contentTitle")).isDisplayed());
        Assertions.assertEquals("Veuillez choisir la/les phase(s) du tournoi",config.getDriver().findElement
                (new By.ByClassName("contentTitle")).getText());

        Assertions.assertTrue(config.getDriver().findElement
                (new By.ByXPath("/html/body/div/div[2]/form/div[1]/strong")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement
                (new By.ByXPath("/html/body/div/div[2]/form/div[2]/strong")).isDisplayed());

        Assertions.assertEquals("Phase 1",config.getDriver().findElement
                (new By.ByXPath("/html/body/div/div[2]/form/div[1]/strong")).getText());

        Assertions.assertEquals("Phase 2",config.getDriver().findElement
                (new By.ByXPath("/html/body/div/div[2]/form/div[2]/strong")).getText());

        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[0].phaseType")).isDisplayed());
        Assertions.assertTrue(config.getDriver().findElement(new By.ByName("phases[1].phaseType")).isDisplayed());

        Assertions.assertTrue(config.getDriver().findElement(new By.ById("choosePhases")).isDisplayed());
        Assertions.assertEquals("Definir Phase",config.getDriver().findElement(new By.ById("choosePhases")).getText());

    }

    @Test
    void testFormAddInfo() {


        Select select = new Select(config.getDriver().findElement(new By.ByName("phases[0].phaseType")));
        select.selectByIndex(0);

        Select select1 = new Select(config.getDriver().findElement(new By.ByName("phases[1].phaseType")));
        select1.selectByIndex(1);

        String idTournament = config.getDriver().findElement(new By.ById("tournament")).getAttribute("value");

        config.getDriver().findElement(new By.ById("choosePhases")).click();

        Tournament tournament = tournamentRepository.findById(Integer.valueOf(idTournament));
        Assertions.assertNotNull(tournament,"Tournament not found");





    }


    @AfterAll
    void tearDown() {
        config.getDriver().quit();
    }


    void createTournament() throws ParseException {
        TournamentModel tournament = new TournamentModel();
        tournament.setName("Tournoi " + Math.floor(Math.random() * 1000));
        tournament.setLocation("location de test");
        tournament.setDate("2019-01-01");
        tournament.setCutOffDate("2019-01-01");
        tournament.setMinTeam(5);
        tournament.setMaxTeam(20);
        tournament.setNbRounds(2);
        tournament.setNbCourts(1);
        tournament.setNbPlayersPerTeam(2);
        tournament.setVisible(true);

        Tournament tournament1 = new Tournament(tournament);
        tournamentRepository.save(tournament1);
    }


}
